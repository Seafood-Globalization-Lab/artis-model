#' Match production taxa to FishBase / SeaLifeBase hierarchtical rank classifications and other attributes
#' 
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
#'   `$unmatched_scinames` so the developer can update
#'   [build_corr_tbl_prod_sciname()] as needed.
#' - **Pass 2** (`corr_tbl = build_corr_tbl_prod_sciname()`): applies
#'   corrections then produces the final matched outputs.
#'
#' The `perciformes/` symbol fix is applied here so that
#' [fill_taxa_classification_gaps()] remains a pure function.
#'
#' @param prod_df Data frame. Output of [clean_prod_dat()]. Both Pass 1 and
#'   Pass 2 should receive the **same original** uncorrected `prod_df`; Pass 2
#'   does not build on Pass 1's `prod_df`.
#' @param fb_slb_dir Character. Directory containing FishBase/SeaLifeBase
#'   taxonomy and synonym CSVs (`fb_taxa_info.csv`, `slb_taxa_info.csv`,
#'   `fb_synonyms_clean.csv`, `slb_synonyms_clean.csv`, `fb_aquarium.csv`,
#'   `slb_aquarium.csv`).
#' @param corr_tbl Data frame or `NULL`. Manual name-correction table as
#'   returned by [build_corr_tbl_prod_sciname()]. Pass `NULL` (default) for
#'   Pass 1 (no corrections); pass the table for Pass 2.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{`prod_ts`}{The input `prod_ts` with `SciName` updated by synonym
#'     resolution (and corrections if `corr_tbl` supplied).}
#'   \item{`prod_taxa_classification`}{Raw combined FB/SLB classification
#'     table, pre-gap-filling, ready for [fill_taxa_classification_gaps()].}
#'   \item{`synonym_resolution`}{Data frame returned by [resolve_synonyms()].
#'     One row per species-level unmatched name, documenting resolution
#'     outcome via `status`: `"resolved_fb"`, `"resolved_slb"`,
#'     `"unresolved"`, `"assumption_violation_fb"`, or
#'     `"assumption_violation_slb"`. Use this on Pass 1 to identify names
#'     requiring manual correction.}
#'   \item{`unmatched_scinames`}{Character vector of `SciName`s present in
#'     `prod_df` but absent from both FishBase and SeaLifeBase after synonym
#'     resolution. Includes non-species-level names that never enter the
#'     synonym loop. Ideally empty on Pass 2.}
#' }
#'
#' @seealso
#' * [clean_prod_dat()] — produces the `prod_df` input for this function
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

match_prod_taxa_to_fbslb <- function(
  prod_data,
  fb_slb_dir,
  corr_tbl = NULL
) {

  # Load FishBase and SeaLifeBase reference tables -------------------------
  fb_taxa_df <- fread(file.path(fb_slb_dir, "fb_taxa_info.csv"), data.table = FALSE) 
  slb_taxa_df <- fread(file.path(fb_slb_dir, "slb_taxa_info.csv"), data.table = FALSE) 

  fb_synonyms  <- fread(file.path(fb_slb_dir, "fb_synonyms_clean.csv"),  data.table = FALSE)
  slb_synonyms <- fread(file.path(fb_slb_dir, "slb_synonyms_clean.csv"), data.table = FALSE)

  # Optionally apply manual corrections to SciName -------------------------
  if (!is.null(corr_tbl)) {
    prod_data <- prod_data %>%
      left_join(
        corr_tbl %>%
          select(sciname_raw, sciname_corrected, Species01, Genus01, Family01, Other01),
        join_by(SciName == sciname_raw)
      ) %>%
    # consolidate records to use .y columns when there is a join match (a value in the correction .y columns)
    mutate(
      SciName   = coalesce(sciname_corrected, SciName),
      Species01 = coalesce(Species01.y, Species01.x),
      Genus01   = coalesce(Genus01.y,   Genus01.x),
      Family01  = coalesce(Family01.y,  Family01.x),
      Other01   = coalesce(Other01.y,   Other01.x)
    ) %>%
    select(-sciname_corrected, -ends_with(".x"), -ends_with(".y"))

# FIXIT - capture successfully joined corrections (applied corrections) to document original and corrected prod taxa names for this version. 
    # pass to code below for complete record. 

  }

  # Assemble distinct taxa names from prod_data ------------------------------
  # origin of the reference sciname table
  prod_taxa <- prod_data %>%
    select(SciName, CommonName, Species01, Genus01, Family01, Other01) %>%
    arrange(SciName) %>%
    distinct()

  # Hierarchical FB inner_joins --------------------------------------------

  # For each SciName in prod_taxa, attach taxonomic classification from either fishbase or sealifebase
  # - Discard native FAO and SAU taxonomic classifications - Defer to fishbase/sealifebase  (more trustworthy)
  # - Perform joins hierarchically - match species to species, genus to genus, etc.
  # - Use `Other01` encoding for Order, Class, and Superclass joins 
  # "many-to-many" matches are NOT expected - these should be flagged in the raw data assessment - need to be fixed. 
  
  # Match Species rank values only
  prod_fb_species <- prod_taxa %>%
    filter(Species01 == 1) %>%
    inner_join(fb_taxa_df, by = c("SciName" = "Species")) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Species",
      fb_or_slb = "fishbase"
    )

  # Match Genus rank values only
  prod_fb_genus <- prod_taxa %>%
    filter(Genus01 == 1) %>%
    inner_join(
      # remove lower rank columns - confounds matching
      fb_taxa_df %>% 
        select(-Species)%>% 
        distinct(),
      by = c("SciName" = "Genus")
    ) %>%
    mutate(Genus = SciName) %>%
    distinct() %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Genus",
      fb_or_slb = "fishbase"
    )

  # Match Family rank values only
  prod_fb_family <- prod_taxa %>%
    filter(Family01 == 1) %>%
    inner_join(
      fb_taxa_df %>% 
        # remove lower rank rank columns - confounds matching
        select(-c(Species, Genus, Subfamily)) %>% 
        distinct(),
      by = c("SciName" = "Family")
    ) %>%
    mutate(Family = SciName) %>%
    distinct() %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Family",
      fb_or_slb = "fishbase"
    )
  
  # Match Other to Order rank values only
  prod_fb_order <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      fb_taxa_df %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family)) %>% 
        distinct(),
      by = c("SciName" = "Order")
    ) %>%
    mutate(Order = SciName) %>%
    distinct() %>%
    #mutate(Order01 = 1) %>%
    #select(-Other01)%>% 
    warn_fbslb_taxa_join(
      matched_rank = "Order",
      fb_or_slb = "fishbase"
    )

  # Match Other to Class rank values only
  prod_fb_class <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      fb_taxa_df %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order)) %>% 
        distinct(),
      by = c("SciName" = "Class")
    ) %>%
    mutate(Class = SciName) %>%
    distinct() %>%
    #mutate(Class01 = 1) %>%
    #select(-Other01) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Class",
      fb_or_slb = "fishbase"
    )

  # Match Other to Superclass rank values only
  prod_fb_superclass <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      fb_taxa_df %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order, Class)) %>% 
        distinct(),
      by = c("SciName" = "SuperClass")
    ) %>%
    mutate(SuperClass = SciName) %>%
    distinct() %>%
    #mutate(Superclass01 = 1) %>%
    #select(-Other01) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "SuperClass",
      fb_or_slb = "fishbase"
    )

  # Hierarchical SLB inner_joins -------------------------------------------
  # Same process as Hierarchical FB inner_joins

  # Match Species rank values only
  prod_slb_species <- prod_taxa %>%
    filter(Species01 == 1) %>%
    inner_join(slb_taxa_df,  
      by = c("SciName" = "Species")) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Species",
      fb_or_slb = "sealifebase"
    )

  # Match Genus rank values only
  prod_slb_genus <- prod_taxa %>%
    filter(Genus01 == 1) %>%
    inner_join(
      slb_taxa_df %>% 
        # remove lower rank columns - confounds matching
        select(-Species) %>% 
        distinct(),
      by = c("SciName" = "Genus")
    ) %>%
    mutate(Genus = SciName) %>%
    distinct() %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Genus",
      fb_or_slb = "sealifebase"
    )

  # Match family rank values only
  prod_slb_family <- prod_taxa %>%
    filter(Family01 == 1) %>%
    inner_join(
      slb_taxa_df %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily)) %>% 
        distinct(),
      by = c("SciName" = "Family")
    ) %>%
    mutate(Family = SciName) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Family",
      fb_or_slb = "sealifebase"
    )
    
  # Match Other to Order rank values only
  prod_slb_order <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      slb_taxa_df %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family)) %>% 
        distinct(),
      by = c("SciName" = "Order")
    ) %>%
    mutate(Order = SciName) %>%
    distinct() %>%
    #mutate(Order01 = 1) %>%
    #select(-Other01) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Order",
      fb_or_slb = "sealifebase"
    )

  # Match Other to Class rank values only
  prod_slb_class <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      slb_taxa_df %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order)) %>% 
        distinct(),
      by = c("SciName" = "Class")
    ) %>%
    mutate(Class = SciName) %>%
    distinct() %>%
    #mutate(Class01 = 1) %>%
    #select(-Other01) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Class",
      fb_or_slb = "sealifebase"
    )

  # Match Other to Phylum rank values only
  prod_slb_phylum <- prod_taxa %>%
    filter(Other01 == 1) %>%
    inner_join(
      slb_taxa_df %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order, Class)) %>% 
        distinct(),
      by = c("SciName" = "Phylum")
    ) %>%
    mutate(Phylum = SciName) %>%
    distinct() %>%
    #mutate(Phylum01 = 1) %>%
    #select(-Other01) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Phylum",
      fb_or_slb = "sealifebase"
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
    # remove binary encoded colums used for initial taxa matching to fb/slb classification info
    select(-c(Species01, Genus01, Family01, Other01)) %>% 
    arrange(SciName)

  # Identify unmatched taxa ------------------------------------------------

  # Prod taxa scinames not in full joined fishbase dataframe
  nomatch_fb <- prod_taxa$SciName[
    prod_taxa$SciName %in% prod_taxa_class_fb$SciName == FALSE
  ]
  # exclude taxa scinames that matched to slb_taxa_df to get scinames not matched at all
  # Note: prod_taxa is allowed to have duplicate scinames (each has a different commonname), 
  # only need list of unique scinames for synonyms matching below
  nomatch_fb_and_slb <- unique(
    nomatch_fb[nomatch_fb %in% prod_taxa_class_slb$SciName == FALSE]
  )

  # Synonym resolution -----------------------------------------------------

  # FIXIT - AM 2026-06-29 - Update warning message with more specifics - table name - maybe diff format than warning message
  synonym_resolution <- resolve_synonyms(
    scinames     = nomatch_fb_and_slb,
    fb_synonyms  = fb_synonyms,
    slb_synonyms = slb_synonyms
  )

  ## Apply resolved synonyms to production data ----------------------------------------
  ### get resolved synonym matches -----------------------------------------------------
  resolved_names <- synonym_resolution %>%
    filter(resolved) %>%
    select(sciname_original, sciname_accepted)

  ### Apply accepted name to prod_data -----------------
  if (nrow(resolved_names) > 0) {
    prod_data <- prod_data %>%
      left_join(
        resolved_names, 
        join_by(SciName == sciname_original)) %>%
      # apply accepted name when data available from resolved_names join - 
      # preferring first vector (sciname_accepted) if both exist
      mutate(
        SciName = coalesce(sciname_accepted, SciName)) %>%
      select(
        -sciname_accepted)
  }

  ## Append accepted names to prod taxa classification tables -----------------------------

  ### prod_taxa_class_fb -------------------------------------------

  # get fishbase specific resolved synonyms
  fb_resolved <- synonym_resolution %>%
    filter(source == "fb", resolved) %>% 
    select(sciname_original, sciname_accepted)

  # create a table (with prod_taxa schema) of only the synonym resolutions to join to prod_taxa
  if (nrow(fb_resolved) > 0) {
    fb_resolved_replacements <- prod_taxa %>%
      # filter down to original scinames that were successfully resolved 
      filter(
        SciName %in% fb_resolved$sciname_original) %>%
      # join on the synonym accepted values
      left_join(
        fb_resolved,
        join_by(SciName == sciname_original)
      ) %>%
      # replace sciname with accepted name
      mutate(SciName = sciname_accepted) %>%
      select(-sciname_accepted) 
      
    # get the fishbase taxa classification info  
    # join by taxa ranks, inner_join acts as a taxa rank filter
    prod_taxa_class_fb_newdat <- bind_rows(
      # resolved species
      fb_resolved_replacements %>%
        inner_join(
          fb_taxa_df, 
          join_by(SciName == Species)),
      # resolved genera
      fb_resolved_replacements %>%
        inner_join(
          fb_taxa_df %>% 
            select(-Species) %>% 
            distinct(),
          # add genus column back in
          join_by(SciName == Genus)
        ) %>%
        mutate(Genus = SciName),
      # resolved families 
      fb_resolved_replacements %>%
        inner_join(
          fb_taxa_df %>% 
            select(-Species, -Genus, -Subfamily) %>% 
            distinct(),
          join_by(SciName == Family)
        ) %>%
        # add family column back in
        mutate(Family = SciName)
    )

    # integrate into the existing prod taxa classification FB table
    prod_taxa_class_fb <- prod_taxa_class_fb %>%
      full_join(
        prod_taxa_class_fb_newdat,
        by = intersect(names(prod_taxa_class_fb), names(prod_taxa_class_fb_newdat))
      )
  }

  ### prod_taxa_class_slb -------------------------------------------

  # get slb specific resolved synonyms
  slb_resolved <- synonym_resolution %>%
    filter(source == "slb", resolved) %>% 
    select(sciname_original, sciname_accepted)

  # create a table (with prod_taxa schema) of only the synonym resolutions to join to prod_taxa
  if (nrow(slb_resolved) > 0) {
    slb_resolved_replacements <- prod_taxa %>%
      # filter down to original scinames that were successfully resolved 
      filter(SciName %in% slb_resolved$sciname_original) %>%
      # join on the synonym accepted values
      left_join(
        slb_resolved %>% select(sciname_original, sciname_accepted),
        join_by(SciName == sciname_original)
      ) %>%
      # replace sciname with accepted name
      mutate(SciName = sciname_accepted) %>%
      select(-sciname_accepted)

    # get the fishbase taxa classification info  
    # join by taxa ranks, inner_join acts as a taxa rank filter
    prod_taxa_class_slb_newdat <- bind_rows(
      # resolved species
      slb_resolved_replacements %>%
        inner_join(
          slb_taxa_df, 
          join_by(SciName == Species)),
      # resolved genera
      slb_resolved_replacements %>%
        inner_join(
          slb_taxa_df %>% 
            select(-Species) %>% 
            distinct(),
          # add genus column back in
          join_by(SciName == Genus)
        ) %>%
        mutate(Genus = SciName),
      # resolved families 
      slb_resolved_replacements %>%
        inner_join(
          slb_taxa_df %>% 
            select(-Species, -Genus, -Subfamily) %>% 
            distinct(),
          join_by(SciName == Family)
        ) %>%
        # add family column back in
        mutate(Family = SciName)
    )

    prod_taxa_class_slb <- prod_taxa_class_slb %>%
      full_join(
        prod_taxa_class_slb_newdat,
        by = intersect(names(prod_taxa_class_slb), names(prod_taxa_class_slb_newdat))
      )
  }

  # No match scinames after synonym resolution ------------------------------
  
  # no match scinames not resolved by synonym matching - require manual corrections downstream
  missing_scinames_post_syn <- nomatch_fb_and_slb[!nomatch_fb_and_slb %in% resolved_names$sciname_original]

  # Add aquarium trade / habitat info -------------------------------------

  fb_aquarium_info <- fread(file.path(fb_slb_dir, "fb_aquarium.csv"), data.table = FALSE)
  fb_aquarium_relevant <- fb_aquarium_info %>%
    filter(SciName %in% prod_taxa_class_fb$SciName)

  prod_taxa_class_fb <- prod_taxa_class_fb %>%
    left_join(fb_aquarium_relevant, by = "SciName") %>%
    rename(Fresh01 = Fresh, Brack01 = Brack, Saltwater01 = Saltwater)

  slb_aquarium_info <- fread(file.path(fb_slb_dir, "slb_aquarium.csv"), data.table = FALSE)
  slb_aquarium_relevant <- slb_aquarium_info %>%
    filter(SciName %in% unique(prod_taxa_class_slb$SciName))

  prod_taxa_class_slb <- prod_taxa_class_slb %>%
    left_join(slb_aquarium_relevant, by = "SciName") %>%
    rename(Fresh01 = Fresh, Brack01 = Brack, Saltwater01 = Saltwater)

  # Assemble prod_taxa_classification (pre-gap-fill) -----------------------
  prod_taxa_classification <- prod_taxa_class_fb %>%
    full_join(
      prod_taxa_class_slb,
      by = intersect(names(prod_taxa_class_fb), names(prod_taxa_class_slb))
    ) %>%
    rename(Superclass = SuperClass) %>%
    select(
      SciName, CommonName, Genus, Subfamily, Family,
      Order, Class, Superclass, Phylum, Kingdom,
      Aquarium, Fresh01, Brack01, Saltwater01
    ) %>%
    arrange(SciName)

  # Replace empty strings with NA
  prod_data[prod_data == ""] <- NA
  prod_taxa_classification[prod_taxa_classification == ""] <- NA


  # Output messages ---------------------------------------------------------------
  n_missing <- length(missing_scinames_post_syn)
  n_resolved <- nrow(synonym_resolution %>% 
    filter(resolved))
  
  cli::cli_h2("Results: Fishbase / Sealifebase matching and synonym resolution")

  # synonym resolution results
  if(n_resolved > 0) {
    cli::cli_alert_success("{.val {n_resolved}} taxa were synonyms resolved to accepted names")
  } else {
    cli::cli_alert_warning("{.val {no(n_resolved)}} taxa were identified as synonyms; no names were resolved")
  }
  # unmatched taxa results
  if(n_missing > 0) {
    cli::cli_alert_warning("Found {.val {no(n_missing)}} unmatched production taxa")
    cli::cli_alert_info("{.strong Developer Notes}:")
    cli::cli_ul(c(
      "Manual corrections required for taxa names returned in {.var taxa_need_corrections} dataframe",
      "Open {.file ./R/build_corr_tbl_prod_sciname.R} to add manual corrections - follow instructions in help page {.code ?build_corr_tbl_prod_sciname()}",
      "Open Fishbase taxa table with {.code fb_taxa <- fread(file.path(current_fb_slb_dir, 'fb_taxa_info.csv'), data.table = FALSE)}",
      "Open Sealifebase taxa table with {.code slb_taxa <- fread(file.path(current_fb_slb_dir, 'slb_taxa_info.csv'), data.table = FALSE)}",
      "Run {.code devtools::load_all} or {.code devtools::install} and {.code library(artis)} to integrate changes",
      "Proceed running {.file 01-clean-input-data.R}; the second pass of {.fun match_prod_taxa_to_fbslb} will apply new corrections"
    ))
  } else if (n_missing == 0) {
    cli::cli_alert_success("All production taxa matched to Fishbase / Sealifebase")
    cli::cli_alert_info("No further manual corrections required - proceed with clean input data script")
  }

  list(
    prod_data                = prod_data,
    prod_taxa_classification = prod_taxa_classification,
    synonym_resolution       = synonym_resolution,
    taxa_need_corrections    = missing_scinames_post_syn
  )
}
