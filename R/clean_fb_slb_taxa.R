#' Clean FishBase and SeaLifeBase taxonomic classification data
#'
#' @description
#' Cleans the raw FishBase or SeaLifeBase taxa table returned by
#' [rfishbase::load_taxa()]: lowercases all values, converts empty strings to
#' `NA`, drops `SpecCode`, applies snapshot- and server-specific manual
#' corrections, then runs a hierarchical classification assumption check.
#'
#' ## Hierarchical classification assumption
#'
#' ARTIS requires that each unique taxon value maps to exactly one upstream
#' classification scheme across all ranks above it. The rank schemas differ
#' slightly between databases:
#'
#' * **FishBase:** Species → Genus → Subfamily → Family → Order → Class → SuperClass
#' * **SeaLifeBase:** Species → Genus → Subfamily → Family → Order → Class → Phylum → Kingdom
#'
#' A violation means a taxon (e.g. a Genus) appears in the source data with two
#' or more distinct combinations of all upstream ranks (e.g. two different
#' Family + Order + Class + SuperClass schemes). This is a data error in the
#' snapshot that must be resolved by a targeted manual correction before the
#' data can be used reliably downstream.
#'
#' Placeholder taxa values (e.g. `"Incertae sedis"` in Subfamily) are excluded
#' from the scheme check because they are intentionally used across unrelated
#' clades and would produce expected, non-actionable violations. They are
#' replaced with `NA` in the returned data frame.
#'
#' ## Manual corrections
#'
#' Corrections are scoped to a specific snapshot version and server to prevent
#' them from propagating silently into future snapshot data. See the
#' *Apply manual corrections* section of the function body.
#'
#' @param the_df Data frame. Raw taxa table as returned by
#'   `rfishbase::load_taxa(server = ...)`.
#' @param the_snapshot Character. The `rfishbase` snapshot version (e.g.
#'   `"25.04"`). Used to scope manual corrections to the correct snapshot.
#' @param the_server Character. One of `"fishbase"` or `"sealifebase"`.
#'   Controls which rank hierarchy is checked and appears in messages.
#'
#' @return A cleaned data frame with lowercased values, empty strings converted
#'   to `NA`, `SpecCode` removed, and any manual corrections applied.
#'
#' @seealso
#' * [clean_fb_slb_data()] — calls this function for both FB and SLB
#' * [match_prod_taxa_to_fbslb()] — reads the cleaned output CSVs
#' * [warn_fbslb_taxa_join()] — downstream safety-net check; fires if a
#'   violation in the source data reaches the hierarchical join step
#'
#' @import dplyr
#' @import cli
#' @importFrom magrittr %>%
#' @export

clean_fb_slb_taxa <- function(
  the_df,
  the_snapshot,
  the_server
) {

  # Clean raw data -----------------------------------------------------------
  # mutate_all(tolower) is applied at the very end of this function so that
  # manual corrections and violation messages use original source capitalization.
  the_df <- the_df %>%
    select(-SpecCode) %>%
    # Convert empty strings to NA 
    # Must happen before the hierarchy checks so coalesce(., "missing_value")
    # correctly surfaces empty-string vs real-value conflicts.
    mutate(across(where(is.character), ~na_if(., "")))

  # Apply manual corrections to specific versions ----------------------------
  # Corrections are conditional on:
  # 1) the snapshot version, and
  # 2) the server (fishbase or sealifebase)
  # This prevents corrections from propagating quietly into future snapshot data.

  # Corrections for the `rfishbase` pkg sealifebase 25.04 snapshot
  if (the_snapshot == "25.04" & the_server == "sealifebase") {

    # Family veneridae appeared with two Order values (nuculida and venerida).
    # WoRMS confirms venerida is correct. Excluding nuculida restores the
    # one-to-one Family → Order relationship required by ARTIS.
    # FIXIT: This situation is more complex than this fix. May require correcting family 
    # to Nuculidae for instances where Order is equal to Nuculida (congruent with Worms)
    # https://www.marinespecies.org/aphia.php?p=taxdetails&id=506582
    # this version has 102 species that would be effected 
    # the_df <- the_df %>%
    #   filter(!(Family == "Veneridae" & Order == "Nuculida"))

    the_df <- the_df %>% 
      mutate(
        Family = case_when(
          Order == "Nuculida" ~ "Nuculidae",
          .default = Family
        )
      )

    # Genus buccinum has an NA/ empty value in a corresponding subfamily value. 
    # Present for a single species. 
    the_df <- the_df %>% 
      mutate(
        Subfamily = case_when(
          Species == "Buccinum bayani" ~ "Buccininae", 
          .default = Subfamily
    ))
  }

  # Hierarchical classification assumption checks ----------------------------
  # ARTIS requires that each unique taxon value maps to exactly one upstream
  # classification scheme across all ranks above it. Schemas differ between
  # databases:
  #   FishBase:    Species > Genus > Subfamily > Family > Order > Class > SuperClass
  #   SeaLifeBase: Species > Genus > Subfamily > Family > Order > Class > Phylum > Kingdom
  #
  # NA values in any upstream rank are replaced with the sentinel "missing_value"
  # before counting distinct schemes. This ensures a taxon with some records
  # carrying a real upstream value and others carrying NA is flagged as a
  # conflict rather than the NA rows being silently dropped.

  upstream_ranks <- if (the_server == "fishbase") {
    list(
      Species   = c("Genus", "Subfamily", "Family", "Order", "Class", "SuperClass"),
      Genus     = c("Subfamily", "Family", "Order", "Class", "SuperClass"),
      Subfamily = c("Family", "Order", "Class", "SuperClass"),
      Family    = c("Order", "Class", "SuperClass"),
      Order     = c("Class", "SuperClass"),
      Class     = c("SuperClass")
    )
  } else if (the_server == "sealifebase"){
    list(
      Species   = c("Genus", "Subfamily", "Family", "Order", "Class", "Phylum", "Kingdom"),
      Genus     = c("Subfamily", "Family", "Order", "Class", "Phylum", "Kingdom"),
      Subfamily = c("Family", "Order", "Class", "Phylum", "Kingdom"),
      Family    = c("Order", "Class", "Phylum", "Kingdom"),
      Order     = c("Class", "Phylum", "Kingdom"),
      Class     = c("Phylum", "Kingdom"),
      Phylum    = c("Kingdom")
    )
  }

  # Placeholder taxa values to exclude from the scheme check, keyed by rank.
  # These taxa names are used by FishBase/SeaLifeBase across unrelated clades
  # and would produce expected, non-actionable violations if included.
  # They are retained in the_df and replaced with NA at the end of this function.
  rank_placeholder_taxa <- list(
    Subfamily = "Incertae sedis"
  )

  # Warn if any expected rank column is absent from the_df
  expected_cols <- unique(c(names(upstream_ranks), unlist(upstream_ranks)))
  missing_cols  <- expected_cols[!expected_cols %in% names(the_df)]

  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "{.field {the_server}} taxa table is missing expected rank column{?s}: {.val {missing_cols}}.",
      "i" = "This may indicate a schema change in the {.val {the_snapshot}}
             {.field {the_server}} snapshot — verify with {.code rfishbase::load_taxa()}."
    ))
  }

  # Run holistic scheme check for each focal rank and collect violations.
  # Each element of the returned list is a data frame of taxa at that rank
  # whose upstream scheme is not unique (n_schemes > 1).
  hierarchy_violations <- lapply(names(upstream_ranks), function(a_focal_rank) {
    parents <- upstream_ranks[[a_focal_rank]]
    excl    <- rank_placeholder_taxa[[a_focal_rank]]
    if (is.null(excl)) excl <- character(0)

    the_df %>%
      # Drop rows where the focal rank is NA or is a known placeholder (rank_exclude).
      # NA focal-rank rows have no taxon identity to group by, so they would be
      # lumped together arbitrarily and produce false violations.
      # Placeholder values (e.g. "Incertae sedis") span unrelated clades by design
      # and are excluded here; they remain in the_df for downstream handling.
      # Note: NA values in *upstream* (parent) ranks are intentionally kept — they
      # are evaluated by the sentinel coalesce() step below.
      filter(!is.na(.data[[a_focal_rank]]), !(.data[[a_focal_rank]] %in% excl)) %>%
      # Sentinel: replace NA in any upstream rank with the literal "missing_value".
      # This makes a real-value vs NA conflict visible as two distinct scheme entries
      # rather than silently collapsing NA rows out of the distinct() count.
      mutate(across(all_of(parents), ~coalesce(., "missing_value"))) %>%
      # Collapse to unique focal-rank + full upstream combinations.
      # Each row here represents one distinct classification scheme for that taxon.
      distinct(across(all_of(c(a_focal_rank, parents)))) %>%
      # Count how many distinct upstream schemes each focal-rank value has.
      # A well-formed hierarchy has n_schemes == 1 for every taxon.
      summarize(n_schemes = n(), .by = all_of(a_focal_rank)) %>%
      # Keep only taxa with more than one upstream scheme — these are the violations.
      filter(n_schemes > 1) %>%
      rename(taxon = 1) %>%
      mutate(a_focal_rank = a_focal_rank, upstream_ranks = paste(parents, collapse = ", "))
  })

  names(hierarchy_violations) <- names(upstream_ranks)

  # Keep only focal ranks that produced at least one violation
  hierarchy_violations <- Filter(
    function(v) nrow(v) > 0,
    hierarchy_violations
  )

  if (length(hierarchy_violations) > 0) {
    cli::cli_h2("Taxonomic classification assumption violation - {the_server}")
    cli::cli_alert_warning(
      "Each unique taxon value must map to exactly one upstream classification scheme."
    )

    for (v in hierarchy_violations) {
      focal <- unique(v$a_focal_rank)
      ups   <- unique(v$upstream_ranks)
      cli::cli_alert_info(
        "{.val {nrow(v)}} {.field {focal}} value{?s} map{?s/} to multiple upstream schemes \\
        ({ups}). {.field {focal}} value{?s}: {.val {v$taxon}}"
      )
    }

    cli::cli_alert_info("{.strong Developer Notes}:")
    cli::cli_ul(c(
      "Manual correction may not be needed depending on if the taxa in violation is included in production data.",
      "Violations are stored in {.code hierarchy_violations} list — step into {.fn clean_fb_slb_taxa} to inspect.",
      "Open FishBase taxa: {.code fb_taxa_raw <- fread(file.path(current_fb_slb_dir, 'fb_taxa_raw.csv'), data.table = FALSE)}",
      "Open Sealifebase taxa: {.code slb_taxa_raw <- fread(file.path(current_fb_slb_dir, 'slb_taxa_raw.csv'), data.table = FALSE)}",
      "Verify the correct higher-rank assignment on Fishbase, Sealifebase, and WoRMS before applying any fix.",
      "Add a targeted correction in the {.emph Apply manual corrections} section of {.fn clean_fb_slb_taxa},
      scoped to the snapshot and server.",
      "Run {.code devtools::load_all()} and re-run {.fn clean_fb_slb_data} to confirm the \\
      fix resolves the violation before proceeding."
    ))
  }

  # Replace taxonomic placeholder values (rank_exclude) with NA before
  # lowercasing, so comparisons match the original source capitalization in
  # rank_exclude. Scoped to only the columns that have exclusions.
  the_df <- the_df %>%
    mutate(across(
      all_of(names(rank_placeholder_taxa)),
      ~if_else(. %in% rank_placeholder_taxa[[cur_column()]], NA_character_, .)
    )) %>%
    # Lowercase all values as the final step — after corrections and checks so
    # that filter comparisons above match raw source capitalization and violation
    # messages report taxa names as they appear in the original data.
    mutate_all(tolower)


  return(the_df)
}
