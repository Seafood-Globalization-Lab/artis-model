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
#' @param prod_ts Data frame. Output of [clean_prod_dat()]. Both Pass 1 and
#'   Pass 2 should receive the **same original** uncorrected `prod_ts`; Pass 2
#'   does not build on Pass 1's `prod_ts`.
#' @param fb_slb_dir Character. Directory containing FishBase/SeaLifeBase
#'   taxonomy and synonym CSVs (`fb_taxa_info.csv`, `slb_taxa_info.csv`,
#'   `fb_synonyms_clean.csv`, `slb_synonyms_clean.csv`, `fb_aquarium.csv`,
#'   `slb_aquarium.csv`).
#' @param prod_data_source Character. One of `"FAO"` or `"SAU"`. Used to
#'   filter `corr_tbl` to the relevant rows when `corr_tbl` is not `NULL`.
#' @param corr_tbl Data frame or `NULL`. Manual name-correction table as
#'   returned by [build_corr_tbl_prod_sciname()]. Pass `NULL` (default) for
#'   Pass 1 (no corrections); pass the table for Pass 2.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{`prod_ts`}{The input `prod_ts` with `SciName` updated by synonym
#'     resolution (and corrections if `corr_tbl` supplied).}
#'   \item{`prod_taxa_classification`}{Raw combined FB/SLB classification
#'     table, pre-gap-filling, ready for [fill_taxa_classification_gaps()].}
#'   \item{`unmatched_scinames`}{Character vector of `SciName`s present in
#'     `prod_ts` but absent from both FishBase and SeaLifeBase after synonym
#'     resolution. Ideally empty on Pass 2.}
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import stringr
#' @import data.table
#' @export

match_prod_taxa_to_fbslb <- function(
  prod_ts,
  fb_slb_dir,
  prod_data_source,
  corr_tbl = NULL
) {

  # Load FishBase and SeaLifeBase reference tables -------------------------
  fishbase_taxa <- fread(file.path(fb_slb_dir, "fb_taxa_info.csv"), data.table = FALSE) %>%
    mutate_all(tolower) %>%
    select(-SpecCode)

  sealifebase_taxa <- fread(file.path(fb_slb_dir, "slb_taxa_info.csv"), data.table = FALSE) %>%
    mutate_all(tolower) %>%
    select(-SpecCode)

  fb_synonyms  <- fread(file.path(fb_slb_dir, "fb_synonyms_clean.csv"),  data.table = FALSE)
  slb_synonyms <- fread(file.path(fb_slb_dir, "slb_synonyms_clean.csv"), data.table = FALSE)

  # Optionally apply manual corrections to SciName -------------------------
  if (!is.null(corr_tbl)) {
    prod_ts <- prod_ts %>%
      left_join(
        corr_tbl %>%
          filter(prod_data_type == prod_data_source) %>%
          select(sciname_raw, sciname_corrected),
        join_by(SciName == sciname_raw)
      ) %>%
      mutate(SciName = coalesce(sciname_corrected, SciName)) %>%
      select(-sciname_corrected)
  }

  # Assemble distinct taxa names from prod_ts ------------------------------
  prod_taxa_names <- prod_ts %>%
    select(SciName, CommonName, Species01, Genus01, Family01, Other01) %>%
    arrange(SciName) %>%
    distinct()

  # Hierarchical FB inner_joins --------------------------------------------

  # For each SciName in prod_taxa_names, attach taxonomic classification from either fishbase or sealifebase
  # - Discard native FAO and SAU taxonomic classifications - Defer to fishbase/sealifebase  (more trustworthy)
  # - Perform joins hierarchically - match species to species, genus to genus, etc.
  # - Use `Other01` encoding for Order, Class, and Superclass joins 
  
  # Match Species rank values only
  prod_fb_species <- prod_taxa_names %>%
    filter(Species01 == 1) %>%
    inner_join(fishbase_taxa, by = c("SciName" = "Species"))

  # Match Genus rank values only
  prod_fb_genus <- prod_taxa_names %>%
    filter(Genus01 == 1) %>%
    inner_join(
      # remove lower rank columns - confounds matching
      fishbase_taxa %>% 
        select(-Species)%>% 
        distinct(),
      by = c("SciName" = "Genus")
    ) %>%
    mutate(Genus = SciName) %>%
    distinct()

  # Match Family rank values only
  prod_fb_family <- prod_taxa_names %>%
    filter(Family01 == 1) %>%
    inner_join(
      fishbase_taxa %>% 
        # remove lower rank rank columns - confounds matching
        select(-c(Species, Genus, Subfamily)) %>% 
        distinct(),
      by = c("SciName" = "Family")
    ) %>%
    mutate(Family = SciName) %>%
    distinct()
  
  # Match Other to Order rank values only
  prod_fb_order <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      fishbase_taxa %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family)) %>% 
        distinct(),
      by = c("SciName" = "Order")
    ) %>%
    mutate(Order = SciName) %>%
    distinct() %>%
    mutate(Order01 = 1) %>%
    select(-Other01)

  # Match Other to Class rank values only
  prod_fb_class <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      fishbase_taxa %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order)) %>% 
        distinct(),
      by = c("SciName" = "Class")
    ) %>%
    mutate(Class = SciName) %>%
    distinct() %>%
    mutate(Class01 = 1) %>%
    select(-Other01)

  # Match Other to Superclass rank values only
  prod_fb_superclass <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      fishbase_taxa %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order, Class)) %>% 
        distinct(),
      by = c("SciName" = "SuperClass")
    ) %>%
    mutate(SuperClass = SciName) %>%
    distinct() %>%
    mutate(Superclass01 = 1) %>%
    select(-Other01)

  # Hierarchical SLB inner_joins -------------------------------------------
  # Same process as Hierarchical FB inner_joins

  # Match Species rank values only
  prod_slb_species <- prod_taxa_names %>%
    filter(Species01 == 1) %>%
    inner_join(sealifebase_taxa,  
      by = c("SciName" = "Species")) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Species",
      fb_or_slb = "sealifebase"
    )

  # Match Genus rank values only
  prod_slb_genus <- prod_taxa_names %>%
    filter(Genus01 == 1) %>%
    inner_join(
      sealifebase_taxa %>% 
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
  prod_slb_family <- prod_taxa_names %>%
    filter(Family01 == 1) %>%
    inner_join(
      sealifebase_taxa %>% 
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
  prod_slb_order <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      sealifebase_taxa %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family)) %>% 
        distinct(),
      by = c("SciName" = "Order")
    ) %>%
    mutate(Order = SciName) %>%
    distinct() %>%
    mutate(Order01 = 1) %>%
    select(-Other01) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Order",
      fb_or_slb = "sealifebase"
    )

  # Match Other to Class rank values only
  prod_slb_class <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      sealifebase_taxa %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order)) %>% 
        distinct(),
      by = c("SciName" = "Class")
    ) %>%
    mutate(Class = SciName) %>%
    distinct() %>%
    mutate(Class01 = 1) %>%
    select(-Other01) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Class",
      fb_or_slb = "sealifebase"
    )

  # Match Other to Phylum rank values only
  prod_slb_phylum <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      sealifebase_taxa %>% 
        # remove lower rank columns - confounds matching
        select(-c(Species, Genus, Subfamily, Family, Order, Class)) %>% 
        distinct(),
      by = c("SciName" = "Phylum")
    ) %>%
    mutate(Phylum = SciName) %>%
    distinct() %>%
    mutate(Phylum01 = 1) %>%
    select(-Other01) %>% 
    warn_fbslb_taxa_join(
      matched_rank = "Phylum",
      fb_or_slb = "sealifebase_taxa"
    )

  # Assemble full FB and SLB tables ----------------------------------------
  prod_fb_full <- prod_fb_species %>%
    full_join(prod_fb_genus,      by = intersect(names(prod_fb_species), names(prod_fb_genus))) %>%
    full_join(prod_fb_family,     by = intersect(names(.), names(prod_fb_family))) %>%
    full_join(prod_fb_order,      by = intersect(names(.), names(prod_fb_order))) %>%
    full_join(prod_fb_class,      by = intersect(names(.), names(prod_fb_class))) %>%
    full_join(prod_fb_superclass, by = intersect(names(.), names(prod_fb_superclass))) %>%
    arrange(SciName)

  prod_slb_full <- prod_slb_species %>%
    full_join(prod_slb_genus,  by = intersect(names(prod_slb_species), names(prod_slb_genus))) %>%
    full_join(prod_slb_family, by = intersect(names(.), names(prod_slb_family))) %>%
    full_join(prod_slb_order,  by = intersect(names(.), names(prod_slb_order))) %>%
    full_join(prod_slb_class,  by = intersect(names(.), names(prod_slb_class))) %>%
    full_join(prod_slb_phylum, by = intersect(names(.), names(prod_slb_phylum))) %>%
    arrange(SciName)

  # Identify unmatched taxa ------------------------------------------------

  # Prod taxa scinames not in full joined fishbase dataframe
  nomatch_fb <- prod_taxa_names$SciName[
    prod_taxa_names$SciName %in% prod_fb_full$SciName == FALSE
  ]
  # exclude taxa scinames that matched to sealifebase_taxa to get scinames not matched at all
  # Note: prod_taxa_names is allowed to have duplicate scinames (each has a different commonname), 
  # only need list of unique scinames for synonyms matching below
  nomatch_fb_and_slb <- unique(
    nomatch_fb[nomatch_fb %in% prod_slb_full$SciName == FALSE]
  )

  # Only species-level names (binomials with a space) go through synonym lookup
  nomatch_species <- nomatch_fb_and_slb[grepl(nomatch_fb_and_slb, pattern = " ")]

  # Synonym resolution loop ------------------------------------------------
  fb_switches  <- 0
  slb_switches <- 0

  for (i in seq_along(nomatch_species)) {
    sciname_i <- nomatch_species[i]

    # Match sciname_i to fb_synonyms$synonym - get accepted name(s)
    name_fb_status  <- artis::query_synonyms(fb_synonyms,  sciname_i)
    # Match sciname_i to slb_synonyms$synonym - get accepted name(s)
    name_slb_status <- artis::query_synonyms(slb_synonyms, sciname_i)

    # Check FishBase synonyms
    if (nrow(name_fb_status) > 0) {
      accepted_name <- tolower(name_fb_status$synonym)

      # Replace synonym with accepted name in classification dataframe
      prod_fb_full_newdat <- prod_taxa_names %>%
        filter(SciName == sciname_i) %>%
        mutate(SciName = accepted_name) %>%
        inner_join(fishbase_taxa, by = c("SciName" = "Species"))

      # Replace synonym with accepted name in produciton dataframe
      prod_ts <- prod_ts %>%
        mutate(
          SciName = case_when(
            SciName == sciname_i ~ accepted_name,
            .default = SciName
          )
          # SciName = if_else(
          #   SciName == sciname_i,
          #   true  = accepted_name,
          #   false = SciName
          # )
        )

      if (nrow(prod_fb_full_newdat) > 0) {
        nomatch_species[i] <- accepted_name
        prod_fb_full <- prod_fb_full %>%
          full_join(
            prod_fb_full_newdat,
            by = intersect(names(prod_fb_full), names(prod_fb_full_newdat))
          )
        fb_switches <- fb_switches + 1
      }
    }

    # Check sealifebase synonyms
    if (nrow(name_slb_status) > 0) {
      accepted_name <- tolower(name_slb_status$synonym)

      prod_slb_full_newdat <- prod_taxa_names %>%
        filter(SciName == sciname_i) %>%
        mutate(SciName = accepted_name) %>%
        inner_join(sealifebase_taxa, by = c("SciName" = "Species"))

      prod_ts <- prod_ts %>%
        mutate(
          SciName = if_else(
            SciName == sciname_i,
            true  = accepted_name,
            false = SciName
          )
        )

      if (nrow(prod_slb_full_newdat) > 0) {
        nomatch_species[i] <- accepted_name
        prod_slb_full <- prod_slb_full %>%
          full_join(
            prod_slb_full_newdat,
            by = intersect(names(prod_slb_full), names(prod_slb_full_newdat))
          )
        slb_switches <- slb_switches + 1
      }
    }
  } # end of for loop

  # Identify still-unmatched names after synonym resolution (Bug 2 fix) ----
  post_match_missing_species <- nomatch_species[
    !(nomatch_species %in% prod_fb_full$SciName) &
    !(nomatch_species %in% prod_slb_full$SciName)
  ]
  nomatch_non_species <- nomatch_fb_and_slb[
    !grepl(nomatch_fb_and_slb, pattern = " ")
  ]

  # Bug 2 fix: capture dead-code checks into the return value
  unmatched_scinames <- sort(
    unique(prod_ts$SciName)[
      !(unique(prod_ts$SciName) %in%
          c(prod_fb_full$SciName, prod_slb_full$SciName))
    ]
  )

  # Add aquarium trade / habitat info -------------------------------------
  fb_aquarium_info <- fread(file.path(fb_slb_dir, "fb_aquarium.csv"), data.table = FALSE)
  fb_aquarium_relevant <- fb_aquarium_info %>%
    filter(SciName %in% prod_fb_full$SciName)

  prod_fb_full <- prod_fb_full %>%
    left_join(fb_aquarium_relevant, by = "SciName") %>%
    rename(Fresh01 = Fresh, Brack01 = Brack, Saltwater01 = Saltwater)

  slb_aquarium_info <- fread(file.path(fb_slb_dir, "slb_aquarium.csv"), data.table = FALSE)
  slb_aquarium_relevant <- slb_aquarium_info %>%
    filter(SciName %in% unique(prod_slb_full$SciName))

  prod_slb_full <- prod_slb_full %>%
    left_join(slb_aquarium_relevant, by = "SciName") %>%
    rename(Fresh01 = Fresh, Brack01 = Brack, Saltwater01 = Saltwater)

  # Assemble prod_taxa_classification (pre-gap-fill) -----------------------
  prod_taxa_classification <- prod_fb_full %>%
    full_join(
      prod_slb_full,
      by = intersect(names(prod_fb_full), names(prod_slb_full))
    ) %>%
    rename(Superclass = SuperClass) %>%
    select(
      SciName, CommonName, Genus, Subfamily, Family,
      Order, Class, Superclass, Phylum, Kingdom,
      Aquarium, Fresh01, Brack01, Saltwater01
    ) %>%
    arrange(SciName)

  # Replace empty strings with NA
  prod_ts[prod_ts == ""] <- NA

  list(
    prod_ts                  = prod_ts,
    prod_taxa_classification = prod_taxa_classification,
    unmatched_scinames       = unmatched_scinames
  )
}
