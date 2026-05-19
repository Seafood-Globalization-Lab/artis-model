#' Match production taxa to FishBase / SeaLifeBase classification
#'
#' @description
#' Loads FishBase and SeaLifeBase reference tables, performs hierarchical
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
#' The `perciformes/` symbol fix is applied here (Option A) so that
#' [fill_taxa_classification_gaps()] remains a pure function.
#'
#' **Bug fixes vs `classify_prod_dat()`:**
#' - Bug 1 fixed: `slb_synonyms` (not `fb_synonyms`) is now passed to the
#'   SeaLifeBase synonym query inside the resolution loop.
#' - Bug 2 fixed: unmatched-name checks now populate `$unmatched_scinames`
#'   instead of being discarded.
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
  fishbase <- fread(
    file.path(fb_slb_dir, "fb_taxa_info.csv"),
    data.table = FALSE
  ) %>%
    mutate_all(tolower) %>%
    select(-SpecCode)

  sealifebase <- fread(
    file.path(fb_slb_dir, "slb_taxa_info.csv"),
    data.table = FALSE
  ) %>%
    mutate_all(tolower) %>%
    select(-SpecCode)

  fb_synonyms  <- fread(file.path(fb_slb_dir, "fb_synonyms_clean.csv"),  data.table = FALSE)
  slb_synonyms <- fread(file.path(fb_slb_dir, "slb_synonyms_clean.csv"), data.table = FALSE)

  # Optionally apply manual corrections to SciName -------------------------
  if (!is.null(corr_tbl)) {
    prod_ts <- prod_ts |>
      left_join(
        corr_tbl |>
          filter(prod_data_type == prod_data_source) |>
          select(sciname_raw, sciname_corrected),
        join_by(SciName == sciname_raw)
      ) |>
      mutate(SciName = coalesce(sciname_corrected, SciName)) |>
      select(-sciname_corrected)
  }

  # perciformes/ symbol fix — applied to prod_ts here (Option A)
  # Collapses e.g. "perciformes/percoidei" → "perciformespercoidei"
  prod_ts <- prod_ts %>%
    mutate(
      SciName = case_when(
        str_detect(SciName, regex("^perciformes/", ignore_case = TRUE)) ~
          str_replace(SciName, "/", ""),
        TRUE ~ SciName
      )
    )

  # Assemble distinct taxa names from prod_ts ------------------------------
  prod_taxa_names <- prod_ts %>%
    select(SciName, CommonName, Species01, Genus01, Family01, Other01) %>%
    arrange(SciName) %>%
    distinct()

  # Hierarchical FB inner_joins --------------------------------------------
  prod_fb_species <- prod_taxa_names %>%
    filter(Species01 == 1) %>%
    inner_join(fishbase, by = c("SciName" = "Species"))

  prod_fb_genus <- prod_taxa_names %>%
    filter(Genus01 == 1) %>%
    inner_join(
      fishbase %>% select(-Species),
      by = c("SciName" = "Genus")
    ) %>%
    mutate(Genus = SciName) %>%
    distinct()

  prod_fb_family <- prod_taxa_names %>%
    filter(Family01 == 1) %>%
    inner_join(
      fishbase %>% select(-c(Species, Genus, Subfamily)),
      by = c("SciName" = "Family")
    ) %>%
    mutate(Family = SciName) %>%
    distinct()

  prod_fb_order <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      fishbase %>% select(-c(Species, Genus, Subfamily, Family)),
      by = c("SciName" = "Order")
    ) %>%
    mutate(Order = SciName) %>%
    distinct() %>%
    mutate(Order01 = 1) %>%
    select(-Other01)

  prod_fb_class <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      fishbase %>% select(-c(Species, Genus, Subfamily, Family, Order)),
      by = c("SciName" = "Class")
    ) %>%
    mutate(Class = SciName) %>%
    distinct() %>%
    mutate(Class01 = 1) %>%
    select(-Other01)

  prod_fb_superclass <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      fishbase %>% select(-c(Species, Genus, Subfamily, Family, Order, Class)),
      by = c("SciName" = "SuperClass")
    ) %>%
    mutate(SuperClass = SciName) %>%
    distinct() %>%
    mutate(Superclass01 = 1) %>%
    select(-Other01)

  # Hierarchical SLB inner_joins -------------------------------------------
  prod_slb_species <- prod_taxa_names %>%
    filter(Species01 == 1) %>%
    inner_join(sealifebase, by = c("SciName" = "Species"))

  prod_slb_genus <- prod_taxa_names %>%
    filter(Genus01 == 1) %>%
    inner_join(
      sealifebase %>% select(-Species),
      by = c("SciName" = "Genus")
    ) %>%
    mutate(Genus = SciName) %>%
    distinct()

  prod_slb_family <- prod_taxa_names %>%
    filter(Family01 == 1) %>%
    inner_join(
      sealifebase %>% select(-c(Species, Genus, Subfamily)),
      by = c("SciName" = "Family")
    ) %>%
    mutate(Family = SciName) %>%
    distinct()

  prod_slb_order <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      sealifebase %>% select(-c(Species, Genus, Subfamily, Family)),
      by = c("SciName" = "Order")
    ) %>%
    mutate(Order = SciName) %>%
    distinct() %>%
    mutate(Order01 = 1) %>%
    select(-Other01)

  prod_slb_class <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      sealifebase %>% select(-c(Species, Genus, Subfamily, Family, Order)),
      by = c("SciName" = "Class")
    ) %>%
    mutate(Class = SciName) %>%
    distinct() %>%
    mutate(Class01 = 1) %>%
    select(-Other01)

  prod_slb_phylum <- prod_taxa_names %>%
    filter(Other01 == 1) %>%
    inner_join(
      sealifebase %>% select(-c(Species, Genus, Subfamily, Family, Order, Class)),
      by = c("SciName" = "Phylum")
    ) %>%
    mutate(Phylum = SciName) %>%
    distinct() %>%
    mutate(Phylum01 = 1) %>%
    select(-Other01)

  # Assemble full FB and SLB tables ----------------------------------------
  prod_fb_full <- prod_fb_species %>%
    full_join(prod_fb_genus,     by = intersect(names(prod_fb_species), names(prod_fb_genus))) %>%
    full_join(prod_fb_family,    by = intersect(names(.), names(prod_fb_family))) %>%
    full_join(prod_fb_order,     by = intersect(names(.), names(prod_fb_order))) %>%
    full_join(prod_fb_class,     by = intersect(names(.), names(prod_fb_class))) %>%
    full_join(prod_fb_superclass, by = intersect(names(.), names(prod_fb_superclass))) %>%
    arrange(SciName)

  prod_slb_full <- prod_slb_species %>%
    full_join(prod_slb_genus,  by = intersect(names(prod_slb_species), names(prod_slb_genus))) %>%
    full_join(prod_slb_family, by = intersect(names(.), names(prod_slb_family))) %>%
    full_join(prod_slb_order,  by = intersect(names(.), names(prod_slb_order))) %>%
    full_join(prod_slb_class,  by = intersect(names(.), names(prod_slb_class))) %>%
    full_join(prod_slb_phylum, by = intersect(names(.), names(prod_slb_phylum))) %>%
    # No SLB superclass or kingdom matches exist
    arrange(SciName)

  # Identify unmatched taxa ------------------------------------------------
  nomatch_fb <- prod_taxa_names$SciName[
    prod_taxa_names$SciName %in% prod_fb_full$SciName == FALSE
  ]
  nomatch_fb_and_slb <- unique(
    nomatch_fb[nomatch_fb %in% prod_slb_full$SciName == FALSE]
  )

  # Only species-level names (binomials with a space) go through synonym lookup
  nomatch_species <- nomatch_fb_and_slb[grepl(nomatch_fb_and_slb, pattern = " ")]

  # Synonym resolution loop ------------------------------------------------
  fb_switches  <- 0
  slb_switches <- 0

  for (i in seq_along(nomatch_species)) {
    next_sciname <- nomatch_species[i]

    # Bug 1 fix: slb_synonyms (not fb_synonyms) used for SLB query
    name_fb_status  <- artis::query_synonyms(fb_synonyms,  next_sciname)
    name_slb_status <- artis::query_synonyms(slb_synonyms, next_sciname)

    # Check FishBase synonyms
    if (nrow(name_fb_status) > 0) {
      accepted_name <- tolower(name_fb_status$synonym)

      prod_fb_full_newdat <- prod_taxa_names %>%
        filter(SciName == next_sciname) %>%
        mutate(SciName = accepted_name) %>%
        inner_join(fishbase, by = c("SciName" = "Species"))

      prod_ts <- prod_ts %>%
        mutate(
          SciName = if_else(
            SciName == next_sciname,
            true  = accepted_name,
            false = SciName
          )
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

    # Check SeaLifeBase synonyms
    if (nrow(name_slb_status) > 0) {
      accepted_name <- tolower(name_slb_status$synonym)

      prod_slb_full_newdat <- prod_taxa_names %>%
        filter(SciName == next_sciname) %>%
        mutate(SciName = accepted_name) %>%
        inner_join(sealifebase, by = c("SciName" = "Species"))

      prod_ts <- prod_ts %>%
        mutate(
          SciName = if_else(
            SciName == next_sciname,
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
  }

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
