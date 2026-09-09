#' Match production taxa to FishBase / SeaLifeBase hierarchical rank classifications and other attributes
#'
#' @description
#' Loads FishBase and SeaLifeBase data tables, performs hierarchical
#' `inner_join` matching of production scientific names, runs a synonym
#' resolution loop to reconcile unmatched species names, optionally applies
#' manual name corrections (`corr_tbl`), joins aquarium-trade habitat info,
#' and assembles the raw (pre-gap-fill) taxa classification table.
#'
#' Intended to be called **twice** in `01-clean-input-data.R`:
#' - **Pass 1** (`corr_tbl = NULL`): surfaces unmatched names via
#'   `$taxa_need_corrections` so the developer can update
#'   [build_corr_tbl_prod_sciname()] as needed.
#' - **Pass 2** (`corr_tbl = build_corr_tbl_prod_sciname()`): applies
#'   corrections then produces the final matched outputs.
#'
#' Unlike [match_prod_taxa_to_fbslb()], `prod_data` is treated as
#' **read-only** throughout this function. All SciName corrections (from
#' `corr_tbl` and synonym resolution) accumulate on `prod_taxa$SciName`. The
#' original name from `prod_data` is preserved in `prod_taxa$SciName_prod` as
#' a stable join key. The caller joins corrections back to `prod_data` via
#' `SciName_prod` in `01-clean-input-data.R`.
#'
#' The `perciformes/` symbol fix is applied here so that
#' [fill_taxa_classification_gaps()] remains a pure function.
#'
#' @param prod_data Data frame. Output of [clean_prod_dat()]. Treated as
#'   read-only; it is never mutated by this function. Both Pass 1 and Pass 2
#'   should receive the **same original** uncorrected `prod_data`.
#' @param fb_slb_dir Character. Directory containing FishBase/SeaLifeBase
#'   taxonomy and synonym CSVs (`fb_taxa_info.csv`, `slb_taxa_info.csv`,
#'   `fb_synonyms_clean.csv`, `slb_synonyms_clean.csv`, `fb_aquarium.csv`,
#'   `slb_aquarium.csv`).
#' @param corr_tbl Data frame or `NULL`. Manual name-correction table as
#'   returned by [build_corr_tbl_prod_sciname()]. Pass `NULL` (default) for
#'   Pass 1 (no corrections); pass the table for Pass 2.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{`prod_taxa`}{Unified taxa table derived from `prod_data`, combining
#'     what was previously split across `prod_taxa` and `prod_taxa_classification`.
#'     Columns: `SciName_prod` (original value from `prod_data`, never modified
#'     — the stable join key back to `prod_data`); `SciName` (final accepted
#'     name after `corr_tbl` corrections and synonym resolution); `CommonName`;
#'     binary matching columns (`Species01`, `Genus01`, `Family01`, `Other01`);
#'     full hierarchical classification (`Genus`, `Subfamily`, `Family`,
#'     `Order`, `Class`, `Superclass`, `Phylum`, `Kingdom`); aquarium habitat
#'     columns (`Aquarium`, `Fresh01`, `Brack01`, `Saltwater01`); and the
#'     derived `habitat_fb` field. Rows for taxa that never matched FB/SLB will
#'     have `NA` in all classification columns. Join this to `prod_data` via
#'     `SciName_prod` to apply corrected names and bring in classification info
#'     in one step.}
#'   \item{`synonym_resolution`}{Data frame returned by [resolve_synonyms()].
#'     One row per species-level unmatched name, documenting resolution outcome
#'     via `status`: `"resolved_fb"`, `"resolved_slb"`, `"unresolved"`,
#'     `"assumption_violation_fb"`, or `"assumption_violation_slb"`. Use this
#'     on Pass 1 to identify names requiring manual correction.}
#'   \item{`taxa_need_corrections`}{Character vector of `SciName`s present in
#'     `prod_data` but absent from both FishBase and SeaLifeBase after synonym
#'     resolution. Includes non-species-level names that never enter the
#'     synonym loop. Ideally empty on Pass 2.}
#' }
#'
#' @seealso
#' * [clean_prod_dat()] — produces the `prod_data` input for this function
#' * [build_corr_tbl_prod_sciname()] — builds the `corr_tbl` applied on Pass 2
#' * [resolve_synonyms()] — called internally for synonym resolution; returns
#'   `$synonym_resolution`
#' * [warn_fbslb_taxa_join()] — called after each hierarchical FB/SLB join
#' * [fill_taxa_classification_gaps()] — receives `$prod_taxa_classification`
#'   for gap-filling
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import stringr
#' @import data.table
#' @export

match_prod_taxa_to_fbslb_2 <- function(
  prod_data,
  fb_slb_dir,
  corr_tbl = NULL
) {

  # Load FishBase and SeaLifeBase reference tables -------------------------
  fb_taxa_df   <- fread(file.path(fb_slb_dir, "fb_taxa_info.csv"),       data.table = FALSE)
  slb_taxa_df  <- fread(file.path(fb_slb_dir, "slb_taxa_info.csv"),      data.table = FALSE)

  fb_synonyms  <- fread(file.path(fb_slb_dir, "fb_synonyms_clean.csv"),  data.table = FALSE)
  slb_synonyms <- fread(file.path(fb_slb_dir, "slb_synonyms_clean.csv"), data.table = FALSE)

  # Assemble distinct taxa names from prod_data (prod_data is read-only) ----
  # SciName_prod captures the original value from prod_data before any corrections;
  # it must not be modified anywhere in this function. It is the stable join key
  # the caller uses to match corrected names back to prod_data.
  prod_taxa <- prod_data %>%
    select(SciName, CommonName, Species01, Genus01, Family01, Other01) %>%
    arrange(SciName) %>%
    distinct() %>%
    mutate(SciName_prod = SciName)

  # Optionally apply manual corrections to prod_taxa$SciName ---------------
  # All corrections accumulate on prod_taxa; prod_data is never mutated.
  if (!is.null(corr_tbl)) {
    prod_taxa <- prod_taxa %>%
      left_join(
        corr_tbl %>%
          select(
            SciName = sciname_prod,
            sciname_corrected,
            Species01,
            Genus01,
            Family01,
            Other01
          ) %>% 
          mutate(correction_source = "manual_correction_table"),
        join_by(SciName)
      ) %>%
      # collapse original production values and correction table value (prefer correction table .y)
      mutate(
        SciName = coalesce(sciname_corrected, SciName),
        Species01 = coalesce(Species01.y, Species01.x),
        Genus01 = coalesce(Genus01.y, Genus01.x),
        Family01 = coalesce(Family01.y, Family01.x),
        Other01 = coalesce(Other01.y, Other01.x)
      ) %>%
      select(-sciname_corrected, -ends_with(".x"), -ends_with(".y"))
  }

  # Hierarchical FB inner_joins --------------------------------------------

  # For each SciName in prod_taxa, attach taxonomic classification from either fishbase or sealifebase
  # - Discard native FAO and SAU taxonomic classifications - Defer to fishbase/sealifebase  (more trustworthy)
  # - Perform joins hierarchically - match species to species, genus to genus, etc.
  # - Use `Other01` encoding for Order, Class, and Superclass joins 
  # "many-to-many" matches are NOT expected - these should be flagged in the raw data assessment - need to be fixed. 

  # Match Species rank values only
  prod_fb_species <- prod_taxa %>%
    filter(Species01 == 1) %>%
    inner_join(fb_taxa_df, join_by(SciName == Species)) %>%
    warn_fbslb_taxa_join(
      matched_rank = "Species",
      fb_or_slb    = "fishbase"
    )

  # Match Genus rank values only
  prod_fb_genus <- prod_taxa %>%
    filter(Genus01 == 1) %>%
    inner_join(
      # remove lower rank columns - confounds matching
      fb_taxa_df %>%
        select(-Species) %>%
        distinct(),
      join_by(SciName == Genus)
    ) %>%
    mutate(Genus = SciName) %>%
    distinct() %>%
    warn_fbslb_taxa_join(
      matched_rank = "Genus",
      fb_or_slb    = "fishbase"
    )

  # Match Family rank values only
  prod_fb_family <- prod_taxa %>%
    filter(Family01 == 1) %>%
    inner_join(
      fb_taxa_df %>%
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily)) %>%
        distinct(),
      join_by(SciName == Family)
    ) %>%
    mutate(Family = SciName) %>%
    distinct() %>%
    warn_fbslb_taxa_join(
      matched_rank = "Family",
      fb_or_slb    = "fishbase"
    )

  # Match Other to Order rank values only
  prod_fb_order <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      fb_taxa_df %>%
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family)) %>%
        distinct(),
      join_by(SciName == Order)
    ) %>%
    mutate(Order = SciName) %>%
    distinct() %>%
    #mutate(Order01 = 1) %>%
    #select(-Other01) %>%
    warn_fbslb_taxa_join(
      matched_rank = "Order",
      fb_or_slb    = "fishbase"
    )

  # Match Other to Class rank values only
  prod_fb_class <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      fb_taxa_df %>%
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order)) %>%
        distinct(),
      join_by(SciName == Class)
    ) %>%
    mutate(Class = SciName) %>%
    distinct() %>%
    #mutate(Class01 = 1) %>%
    #select(-Other01) %>%
    warn_fbslb_taxa_join(
      matched_rank = "Class",
      fb_or_slb    = "fishbase"
    )

  # Match Other to Superclass rank values only
  prod_fb_superclass <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      fb_taxa_df %>%
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order, Class)) %>%
        distinct(),
      join_by(SciName == SuperClass)
    ) %>%
    mutate(SuperClass = SciName) %>%
    distinct() %>%
    #mutate(Superclass01 = 1) %>%
    #select(-Other01) %>%
    warn_fbslb_taxa_join(
      matched_rank = "SuperClass",
      fb_or_slb    = "fishbase"
    )

  # Hierarchical SLB inner_joins -------------------------------------------
  # Same process as Hierarchical FB inner_joins

  # Match Species rank values only
  prod_slb_species <- prod_taxa %>%
    filter(Species01 == 1) %>%
    inner_join(slb_taxa_df, join_by(SciName == Species)) %>%
    warn_fbslb_taxa_join(
      matched_rank = "Species",
      fb_or_slb    = "sealifebase"
    )

  # Match Genus rank values only
  prod_slb_genus <- prod_taxa %>%
    filter(Genus01 == 1) %>%
    inner_join(
      slb_taxa_df %>%
        # remove lower rank columns - confounds matching
        select(-Species) %>%
        distinct(),
      join_by(SciName == Genus)
    ) %>%
    mutate(Genus = SciName) %>%
    distinct() %>%
    warn_fbslb_taxa_join(
      matched_rank = "Genus",
      fb_or_slb    = "sealifebase"
    )

  # Match Family rank values only
  prod_slb_family <- prod_taxa %>%
    filter(Family01 == 1) %>%
    inner_join(
      slb_taxa_df %>%
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily)) %>%
        distinct(),
      join_by(SciName == Family)
    ) %>%
    mutate(Family = SciName) %>%
    warn_fbslb_taxa_join(
      matched_rank = "Family",
      fb_or_slb    = "sealifebase"
    )

  # Match Other to Order rank values only
  prod_slb_order <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      slb_taxa_df %>%
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family)) %>%
        distinct(),
      join_by(SciName == Order)
    ) %>%
    mutate(Order = SciName) %>%
    distinct() %>%
    #mutate(Order01 = 1) %>%
    #select(-Other01) %>%
    warn_fbslb_taxa_join(
      matched_rank = "Order",
      fb_or_slb    = "sealifebase"
    )

  # Match Other to Class rank values only
  prod_slb_class <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      slb_taxa_df %>%
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order)) %>%
        distinct(),
      join_by(SciName == Class)
    ) %>%
    mutate(Class = SciName) %>%
    distinct() %>%
    #mutate(Class01 = 1) %>%
    #select(-Other01) %>%
    warn_fbslb_taxa_join(
      matched_rank = "Class",
      fb_or_slb    = "sealifebase"
    )

  # Match Other to Phylum rank values only
  prod_slb_phylum <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      slb_taxa_df %>%
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order, Class)) %>%
        distinct(),
      join_by(SciName == Phylum)
    ) %>%
    mutate(Phylum = SciName) %>%
    distinct() %>%
    #mutate(Phylum01 = 1) %>%
    #select(-Other01) %>%
    warn_fbslb_taxa_join(
      matched_rank = "Phylum",
      fb_or_slb    = "sealifebase"
    )

  # Assemble Prod taxa FB and SLB tables ----------------------------------------
  prod_taxa_class_fb <- prod_fb_species %>%
    full_join(prod_fb_genus,      by = intersect(names(prod_fb_species), names(prod_fb_genus))) %>%
    full_join(prod_fb_family,     by = intersect(names(.), names(prod_fb_family))) %>%
    full_join(prod_fb_order,      by = intersect(names(.), names(prod_fb_order))) %>%
    full_join(prod_fb_class,      by = intersect(names(.), names(prod_fb_class))) %>%
    full_join(prod_fb_superclass, by = intersect(names(.), names(prod_fb_superclass))) %>%
    # remove binary encoded columns used for initial taxa matching to fb/slb classification info
    select(-c(Species01, Genus01, Family01, Other01)) %>%
    arrange(SciName)

  prod_taxa_class_slb <- prod_slb_species %>%
    full_join(prod_slb_genus,  by = intersect(names(prod_slb_species), names(prod_slb_genus))) %>%
    full_join(prod_slb_family, by = intersect(names(.), names(prod_slb_family))) %>%
    full_join(prod_slb_order,  by = intersect(names(.), names(prod_slb_order))) %>%
    full_join(prod_slb_class,  by = intersect(names(.), names(prod_slb_class))) %>%
    full_join(prod_slb_phylum, by = intersect(names(.), names(prod_slb_phylum))) %>%
    # remove binary encoded columns used for initial taxa matching to fb/slb classification info
    select(-c(Species01, Genus01, Family01, Other01)) %>%
    arrange(SciName)

  # Identify unmatched taxa ------------------------------------------------

  # Prod taxa scinames not in full joined fishbase dataframe
  nomatch_fb <- prod_taxa$SciName[
    prod_taxa$SciName %in% prod_taxa_class_fb$SciName == FALSE
  ]
  # Exclude taxa scinames that matched to slb_taxa_df to get scinames not matched at all.
  # Note: prod_taxa is allowed to have duplicate scinames (each has a different commonname);
  # only need list of unique scinames for synonym matching below.
  nomatch_fb_and_slb <- unique(
    nomatch_fb[nomatch_fb %in% prod_taxa_class_slb$SciName == FALSE]
  )

  # Synonym resolution -----------------------------------------------------

  synonym_results <- resolve_synonyms(
    scinames     = nomatch_fb_and_slb,
    fb_synonyms  = fb_synonyms,
    slb_synonyms = slb_synonyms
  ) 

  # align column names to prod_taxa and only retain solutions
  synonym_resolutions <- synonym_results %>% 
    filter(resolved) %>% 
    select(
      # Side Note - there is a possibility that these sciname_original values are not exactly SciName_prod values from prod_taxa
      # if a manual correction was made above to correct to a fb/slb synonym name that would be picked up here. Not the design
      # intention of the manual correction table, but it is possible.
      SciName_prod = sciname_original, 
      SciName = sciname_accepted, 
      correction_source)

  # Apply resolved synonyms to prod_taxa$SciName ---------------------------
  # prod_data is read-only at this point; synonym resolution updates prod_taxa$SciName only.
  # resolved_names <- synonym_resolution %>%
  #   filter(resolved) %>%
  #   select(
  #     sciname_original, 
  #     sciname_accepted, 
  #     correction_source)

  # if (nrow(resolved_names) > 0) {
  #   prod_taxa <- prod_taxa %>%
  #     left_join(
  #       resolved_names,
  #       join_by(SciName == sciname_original)
  #     # apply accepted name when data available from resolved_names join - 
  #     # preferring first vector (sciname_accepted) if both exist
  #     ) %>%
  #     mutate(
  #       SciName = coalesce(sciname_accepted, SciName),
  #       correction_source = coalesce(correction_source.y, correction_source.x)
  #     ) %>%
  #     select(-sciname_accepted, -ends_with(".x"), -ends_with(".y"))
  # }

  # Append accepted names to prod taxa classification tables ----------------

  ### prod_taxa_class_fb -------------------------------------------

  fb_resolved <- synonym_resolutions %>%
    filter(correction_source == "synonym_table_fb")

  if (nrow(fb_resolved) > 0) {
    # Filter by original SciName_prod that were resolved to accepted names in the synonym table.
    # fb_resolved_replacements <- prod_taxa %>%
    #   filter(SciName_prod %in% fb_resolved$sciname_original) 

    # Get taxa classification info from FB/SLB taxa tables - divided up by classification rank for matching purposes
    prod_taxa_class_fb_syns <- bind_rows(

      # resolved species with classification rank info joined on
      fb_resolved %>%
        inner_join(
          fb_taxa_df, 
          join_by(SciName == Species)),

      # resolved genera
      fb_resolved %>%
        inner_join(
          fb_taxa_df %>%
            select(-Species) %>%
            distinct(),
          join_by(SciName == Genus)
        ) %>%
        mutate(Genus = SciName),

      # resolved families
      fb_resolved %>%
        inner_join(
          fb_taxa_df %>%
            select(-Species, -Genus, -Subfamily) %>%
            distinct(),
          join_by(SciName == Family)
        ) %>%
        mutate(Family = SciName)
    )

    # Integrate into the existing prod taxa classification FB table
    # Note - any synonym correction will not have a CommonName value at this point
    prod_taxa_class_fb <- prod_taxa_class_fb %>%
      full_join(
        prod_taxa_class_fb_syns,
        by = intersect(names(prod_taxa_class_fb), names(prod_taxa_class_fb_syns))
      )
  }

  ### prod_taxa_class_slb -------------------------------------------

  slb_resolved <- synonym_resolutions %>%
    filter(correction_source == "synonym_table_slb") 

  if (nrow(slb_resolved) > 0) {
    # Filter by SciName_prod (original pre-synonym names that were unmatched).
    # prod_taxa$SciName is already the accepted name at this point
    # slb_resolved_replacements <- prod_taxa %>%
    #   filter(SciName_prod %in% slb_resolved$sciname_original)

    prod_taxa_class_slb_syns <- bind_rows(
      
      # resolved species
      slb_resolved %>%
        inner_join(
          slb_taxa_df, 
          join_by(SciName == Species)),
      
      # resolved genera
      slb_resolved %>%
        inner_join(
          slb_taxa_df %>%
            select(-Species) %>%
            distinct(),
          join_by(SciName == Genus)
        ) %>%
        mutate(Genus = SciName),
      
      # resolved families
      slb_resolved %>%
        inner_join(
          slb_taxa_df %>%
            select(-Species, -Genus, -Subfamily) %>%
            distinct(),
          join_by(SciName == Family)
        ) %>%
        mutate(Family = SciName)
    )

    prod_taxa_class_slb <- prod_taxa_class_slb %>%
      full_join(
        prod_taxa_class_slb_syns,
        by = intersect(names(prod_taxa_class_slb), names(prod_taxa_class_slb_syns))
      )
  }

  # No match scinames after synonym resolution ------------------------------

  # Scinames not resolved by synonym matching - compare to successful synonyms in synonym_resolutions 
  # - may require manual corrections downstream
  missing_scinames_post_syn <- nomatch_fb_and_slb[
    !nomatch_fb_and_slb %in% synonym_resolutions$SciName_prod
  ]

  # Add aquarium trade / habitat info -------------------------------------

  fb_aquarium_info <- fread(file.path(fb_slb_dir, "fb_aquarium.csv"), data.table = FALSE)

  prod_taxa_class_fb <- prod_taxa_class_fb %>%
    left_join(
      fb_aquarium_info, 
      by = "SciName") %>%
    rename(
      Fresh01 = Fresh, 
      Brack01 = Brack, 
      Saltwater01 = Saltwater)

  slb_aquarium_info <- fread(file.path(fb_slb_dir, "slb_aquarium.csv"), data.table = FALSE)

  prod_taxa_class_slb <- prod_taxa_class_slb %>%
    left_join(
      slb_aquarium_info, 
      by = "SciName") %>%
    rename(
      Fresh01 = Fresh, 
      Brack01 = Brack, 
      Saltwater01 = Saltwater)

  # Assemble prod_taxa_classification () -----------------------
  prod_taxa_classification <- bind_rows(
    prod_taxa_class_fb,
    prod_taxa_class_slb) %>%
    select(
      SciName,
      SciName_prod,
      CommonName,
      Genus,
      Subfamily,
      Family,
      Order,
      Class,
      Superclass = SuperClass,
      Phylum,
      Kingdom,
      Aquarium,
      Fresh01,
      Brack01,
      Saltwater01,
      correction_source
    ) %>%
    arrange(SciName) %>%
    mutate(
      habitat_fb = case_when(
        Fresh01 == 1 & Saltwater01 == 0 ~ "inland",
        Fresh01 == 0 & Saltwater01 == 1 ~ "marine",
        Fresh01 == 1 & Saltwater01 == 1 ~ "diadromous",
        # If a species just exists in brackish water we classify as marine
        Brack01 == 1 & Fresh01 == 0 & Saltwater01 == 0 ~ "marine",
        TRUE ~ as.character(NA)
      )
    )

  # Fold classification columns into prod_taxa ---------------------------------
  # prod_taxa supersedes the old prod_taxa + prod_taxa_classification split.
  # CommonName is already in prod_taxa; all other classification columns are
  # joined on SciName (accepted name). Rows with no FB/SLB match will have NA
  # in classification columns.
  prod_taxa <- prod_taxa %>%
    left_join(
      prod_taxa_classification %>% select(-CommonName),
      join_by(SciName)
    )

  # Replace empty strings with NA
  prod_taxa[prod_taxa == ""] <- NA

  # Output messages ---------------------------------------------------------------
  n_missing  <- length(missing_scinames_post_syn)
  n_resolved <- nrow(synonym_resolutions %>% filter(resolved))

  cli::cli_h2("Results: Fishbase / Sealifebase matching and synonym resolution")

  if (n_resolved > 0) {
    cli::cli_alert_success("{.val {n_resolved}} taxa were synonyms resolved to accepted names")
  } else {
    cli::cli_alert_warning("{.val {no(n_resolved)}} taxa were identified as synonyms; no names were resolved")
  }

  if (n_missing > 0) {
    cli::cli_alert_warning("Found {.val {no(n_missing)}} unmatched production taxa")
    cli::cli_alert_info("{.strong Developer Notes}:")
    cli::cli_ul(c(
      "Manual corrections required for taxa names returned in {.var taxa_need_corrections} dataframe",
      "Open {.file ./R/build_corr_tbl_prod_sciname.R} to add manual corrections - follow instructions in help page {.code ?build_corr_tbl_prod_sciname()}",
      "Open Fishbase taxa table with {.code fb_taxa <- fread(file.path(current_fb_slb_dir, 'fb_taxa_info.csv'), data.table = FALSE)}",
      "Open Sealifebase taxa table with {.code slb_taxa <- fread(file.path(current_fb_slb_dir, 'slb_taxa_info.csv'), data.table = FALSE)}",
      "Run {.code devtools::load_all} or {.code devtools::install} and {.code library(artis)} to integrate changes",
      "Proceed running {.file 01-clean-input-data.R}; the second pass of {.fun match_prod_taxa_to_fbslb_2} will apply new corrections"
    ))
  } else if (n_missing == 0) {
    cli::cli_alert_success("All production taxa matched to Fishbase / Sealifebase")
    cli::cli_alert_info("No further manual corrections required - proceed with clean input data script")
  }

  list(
    prod_taxa             = prod_taxa,
    synonym_results    = synonym_results,
    taxa_need_corrections = missing_scinames_post_syn
  )
}
