#' Match production taxa to FishBase and SeaLifeBase classifications
#'
#' @description
#' Performs hierarchical `inner_join` matching of production scientific names
#' against FishBase and SeaLifeBase taxonomy tables across six rank levels
#' (Species, Genus, Family, Order, Class, and SuperClass for FishBase; Species,
#' Genus, Family, Order, Class, and Phylum for SeaLifeBase), resolves unmatched
#' binomial names via [resolve_synonyms()], optionally applies manual name
#' corrections from `corr_tbl`, and joins aquarium-trade habitat information.
#'
#' @details
#' Called inside `01-clean-input-data.R` **twice** in sequence:
#'
#' * **Pass 1** (`corr_tbl = NULL`): Runs matching and synonym resolution with
#'   no corrections applied. Inspect `$taxa_need_corrections` to identify names
#'   that require manual corrections in [build_corr_tbl_prod_sciname()].
#' * **Pass 2** (`corr_tbl = build_corr_tbl_prod_sciname()`): Applies manual
#'   corrections before matching, then produces the final matched output.
#'   The `$prod_taxa_classification` result is passed to [fill_prod_taxa_gaps()]
#'   for gap-filling and finalization.
#'
#' `prod_data` is treated as **read-only** throughout. The original `SciName`
#' values from `prod_data` are preserved in `SciName_prod` as a stable join key.
#' All corrections (from `corr_tbl` and synonym resolution) accumulate on a
#' working copy and do not modify `prod_data`.
#'
#' ## Hierarchical rank matching
#'
#' For each rank level, only taxa encoded at that rank (via `Species01`,
#' `Genus01`, `Family01`, `Other01`) are joined to the FB/SLB classification
#' table. Lower-rank columns are removed before each join to prevent cross-rank
#' mismatches. The rank-level tables are then combined with `full_join()` before
#' synonym resolution rows are appended.
#'
#' ## Manual corrections
#'
#' When `corr_tbl` is non-`NULL`, corrections from [build_corr_tbl_prod_sciname()]
#' are applied via `left_join()` on `SciName_prod`. The corrected name
#' (`sciname_corrected`) and updated rank indicators take precedence over
#' original `prod_data` values via `coalesce()`.
#'
#' @param prod_data Data frame. Output of [clean_prod_dat()]. Treated as
#'   read-only; never mutated by this function. Both Pass 1 and Pass 2
#'   should receive the **same original** uncorrected `prod_data`.
#' @param fb_slb_dir Character. Directory containing FishBase/SeaLifeBase
#'   taxonomy, synonym, and aquarium CSVs: `fb_taxa_info.csv`,
#'   `slb_taxa_info.csv`, `fb_synonyms_clean.csv`, `slb_synonyms_clean.csv`,
#'   `fb_aquarium.csv`, `slb_aquarium.csv`.
#' @param corr_tbl Data frame or `NULL`. Manual name-correction table as
#'   returned by [build_corr_tbl_prod_sciname()]. Pass `NULL` (default) for
#'   Pass 1 (no corrections); pass the table for Pass 2.
#'
#' @return
#' A named list with three elements:
#'
#' * `prod_taxa_classification` — Distinct taxa table derived from `prod_data`. Columns:
#'   `SciName_prod` (original value from `prod_data`, the stable join key back
#'   to `prod_data`); `Species01`, `Genus01`, `Family01`, `Other01` (binary
#'   rank indicators). Empty strings replaced with `NA`.
#' * `synonym_results` — Full data frame returned by [resolve_synonyms()]. One
#'   row per species-level unmatched name. Key columns: `sciname_original`,
#'   `sciname_accepted`, `correction_source`, `resolved`, and `status`
#'   (`"resolved_fb"`, `"resolved_slb"`, `"unresolved"`,
#'   `"assumption_violation_fb"`, `"assumption_violation_slb"`). 
#' * `taxa_need_corrections` — Character vector of `SciName`s present in
#'   `prod_data` but absent from both FishBase and SeaLifeBase after synonym
#'   resolution; candidates for manual corrections in build_corr_tbl_prod_sciname(). Ideally empty on Pass 2.
#'
#' @note
#' The return list does not currently match what `01-clean-input-data.R` and
#' downstream functions expect. The caller accesses `$prod_data`,
#' `$prod_taxa_classification`, `$synonym_resolution`, and `$prod_ts` — none of
#' which are present in the current return list. [fill_prod_taxa_gaps()] also
#' documents receiving `$prod_taxa_classification` and `$prod_data` from this
#' function. The return list requires reconciliation with the caller and
#' downstream functions before the two-pass workflow will run as designed.
#'
#' @seealso
#' * [clean_prod_dat()] — produces the `prod_data` input
#' * [build_corr_tbl_prod_sciname()] — builds the `corr_tbl` applied on Pass 2
#' * [resolve_synonyms()] — called internally for synonym resolution; result
#'   returned as `$synonym_results`
#' * [warn_fbslb_taxa_join()] — called after each hierarchical FB/SLB join to
#'   flag many-to-many matches
#' * [fill_prod_taxa_gaps()] — intended downstream consumer of
#'   `$prod_taxa_classification` and `$prod_data` (Pass 2)
#'
#' @import dplyr
#' @import cli
#' @importFrom magrittr %>%
#' @import data.table
#' @export

match_prod_taxa_to_fb_slb <- function(
  prod_data,
  fb_slb_dir,
  corr_tbl = NULL
) {

  # Load FishBase and SeaLifeBase reference tables -------------------------
  fb_taxa_df   <- fread(file.path(fb_slb_dir, "fb_taxa_info.csv"), data.table = FALSE)
  slb_taxa_df  <- fread(file.path(fb_slb_dir, "slb_taxa_info.csv"), data.table = FALSE)

  fb_synonyms  <- fread(file.path(fb_slb_dir, "fb_synonyms_clean.csv"), data.table = FALSE)
  slb_synonyms <- fread(file.path(fb_slb_dir, "slb_synonyms_clean.csv"), data.table = FALSE)

  # Assemble distinct taxa names from prod_data (prod_data is read-only) ----
  # SciName_prod captures the original value from prod_data before any corrections;
  # it must not be modified anywhere in this function. It is the stable join key
  # the caller uses to match corrected names back to prod_data.
  prod_taxa <- prod_data %>%
    select(
      SciName_prod = SciName, 
      Species01, 
      Genus01, 
      Family01, 
      Other01
    ) %>%
    arrange(SciName_prod) %>%
    distinct() 

  # Optionally apply manual corrections ---------------
  # All corrections accumulate on prod_taxa_corr
  if (!is.null(corr_tbl)) {
    prod_taxa_corr <- prod_taxa %>%
      left_join(
        corr_tbl %>%
          select(
            SciName_prod = sciname_prod,
            sciname_corrected,
            Species01,
            Genus01,
            Family01,
            Other01
          ) %>% 
          mutate(correction_source = "manual_correction_table"),
        join_by(SciName_prod)
      ) %>%
      # collapse original production values and correction table value 
      # prefer correction table (.y) as taxa rank may have changed with manual correction
      mutate(
        SciName = coalesce(sciname_corrected, SciName_prod),
        Species01 = coalesce(Species01.y, Species01.x),
        Genus01 = coalesce(Genus01.y, Genus01.x),
        Family01 = coalesce(Family01.y, Family01.x),
        Other01 = coalesce(Other01.y, Other01.x)
      ) %>%
      select(-sciname_corrected, -ends_with(".x"), -ends_with(".y"))
  } else if (is.null(corr_tbl)) {
    # No corrections applied if no correction table supplied in argument
    prod_taxa_corr <- prod_taxa
  }

  # Hierarchical FB inner_joins --------------------------------------------

  # For each SciName (corrected) in prod_taxa_corr, attach taxonomic classification from either fishbase or sealifebase
  # - Discard native FAO and SAU taxonomic classifications - Defer to fishbase/sealifebase  (more trustworthy)
  # - Perform joins hierarchically - match species to species, genus to genus, etc.
  # - Use `Other01` encoding for Order, Class, and Superclass joins 
  # "many-to-many" matches are NOT expected - these should be flagged in the raw data assessment - need to be fixed. 

  # Match Species rank values only
  prod_fb_species <- prod_taxa_corr %>%
    filter(Species01 == 1) %>%
    inner_join(
      fb_taxa_df, 
      join_by(SciName == Species)) %>%
    warn_fbslb_taxa_join(
      matched_rank = "Species",
      fb_or_slb    = "fishbase"
    )

  # Match Genus rank values only
  prod_fb_genus <- prod_taxa_corr %>%
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
  prod_fb_family <- prod_taxa_corr %>%
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
  prod_fb_order <- prod_taxa_corr %>%
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
  prod_fb_class <- prod_taxa_corr %>%
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
  prod_fb_superclass <- prod_taxa_corr %>%
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
  prod_slb_species <- prod_taxa_corr %>%
    filter(Species01 == 1) %>%
    inner_join(slb_taxa_df, join_by(SciName == Species)) %>%
    warn_fbslb_taxa_join(
      matched_rank = "Species",
      fb_or_slb    = "sealifebase"
    )

  # Match Genus rank values only
  prod_slb_genus <- prod_taxa_corr %>%
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
  prod_slb_family <- prod_taxa_corr %>%
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
  prod_slb_order <- prod_taxa_corr %>%
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
  prod_slb_class <- prod_taxa_corr %>%
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
  prod_slb_phylum <- prod_taxa_corr %>%
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
  nomatch_fb <- prod_taxa_corr$SciName[
    prod_taxa_corr$SciName %in% prod_taxa_class_fb$SciName == FALSE
  ]
  # Exclude taxa scinames that matched to slb_taxa_df to get scinames not matched at all.
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

  # Add resolved synonyms to FB / SLB tables with classification info ----------------

  ## prod_taxa_class_fb -------------------------------------------

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
    prod_taxa_class_fb <- prod_taxa_class_fb %>%
      full_join(
        prod_taxa_class_fb_syns,
        by = intersect(names(prod_taxa_class_fb), names(prod_taxa_class_fb_syns))
      )
  }

  ## prod_taxa_class_slb -------------------------------------------

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

  # Add aquarium trade / habitat info to FB / SLB -------------------------------------

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

  # Assemble all classification info -----------------------
  prod_taxa_classification <- bind_rows(
    prod_taxa_class_fb,
    prod_taxa_class_slb) %>%
    select(
      SciName,
      SciName_prod,
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

  # Update prod_taxa with corrections and classification info ---------------------------------
  # joined by original production SciNames before corrections as key. 
  # Rows with no FB/SLB match will have NA in classification columns.
  prod_taxa_classification <- prod_taxa %>%
    select(-c(Species01, Genus01, Family01, Other01)) %>% 
    left_join(
      prod_taxa_classification,
      join_by(SciName_prod)
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
      "Proceed running {.file 01-clean-input-data.R}; the second pass of {.fun match_prod_taxa_to_fb_slb} will apply new corrections"
    ))
  } else if (n_missing == 0) {
    cli::cli_alert_success("All production taxa matched to Fishbase / Sealifebase")
    cli::cli_alert_info("No further manual corrections required - proceed with clean input data script")
  }

  return(
    list(
      prod_taxa = prod_taxa_classification,
      synonym_results = synonym_results,
      taxa_need_corrections = missing_scinames_post_syn
    )
  )
}
