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
#' ARTIS requires a strict many-to-one relationship at each rank transition —
#' each unique lower-rank value must map to exactly one value of the next
#' higher rank. The rank schemas differ slightly between databases:
#'
#' * **FishBase:** Species → Genus → Subfamily → Family → Order → Class → SuperClass
#' * **SeaLifeBase:** Species → Genus → Subfamily → Family → Order → Class → Phylum → Kingdom
#'
#' A violation means the same taxon (e.g. a Family) appears in the source data
#' with two conflicting parent-rank values (e.g. two Orders). This is a data
#' error in the snapshot that must be resolved by a targeted manual correction
#' before the data can be used reliably downstream.
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
  # ARTIS assumes a strict many-to-one relationship at each rank transition.
  # Schemas differ between databases:
  #   FishBase:    Species > Genus > Subfamily > Family > Order > Class > SuperClass
  #   SeaLifeBase: Species > Genus > Subfamily > Family > Order > Class > Phylum > Kingdom
  #
  # NA values in the parent rank are replaced with the sentinel "missing_value"
  # before n_distinct() is computed. This ensures that a taxon with one record
  # carrying a real parent-rank value and another carrying NA is correctly
  # flagged as a conflict, rather than the NA row being silently dropped.

  rank_pairs <- if (the_server == "fishbase") {
    list(
      c("Species",   "Genus"),
      c("Genus",     "Subfamily"),
      c("Subfamily", "Family"),
      c("Family",    "Order"),
      c("Order",     "Class"),
      c("Class",     "SuperClass")
    )
  } else {
    list(
      c("Species",   "Genus"),
      c("Genus",     "Subfamily"),
      c("Subfamily", "Family"),
      c("Family",    "Order"),
      c("Order",     "Class"),
      c("Class",     "Phylum"),
      c("Phylum",    "Kingdom")
    )
  }

  # Warn if any expected rank column is absent from the_df
  expected_cols <- unique(unlist(rank_pairs))
  missing_cols  <- expected_cols[!expected_cols %in% names(the_df)]

  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = "{.field {the_server}} taxa table is missing expected rank column{?s}: {.val {missing_cols}}.",
      "i" = "This may indicate a schema change in the {.val {the_snapshot}}
             {.field {the_server}} snapshot — verify with {.code rfishbase::load_taxa()}."
    ))
  }

  # Run check for each rank pair and collect violations
  # Creates list of dataframes. Each df is the results of each child/parent evaluation.
  # Each evaluation may contain multiple violations. 
  hierarchy_violations <- lapply(rank_pairs, function(pair) {
    child_rank  <- pair[1]
    parent_rank <- pair[2]

    the_df %>%
      # Only evaluate rows where the child rank value is known
      filter(!is.na(.data[[child_rank]])) %>%
      # Sentinel: surfaces real-value vs NA conflicts as distinct values
      mutate(across(all_of(parent_rank), ~coalesce(., "missing_value"))) %>%
      distinct(.data[[child_rank]], .data[[parent_rank]]) %>%
      summarize(
        n_parent = n_distinct(.data[[parent_rank]]),
        .by      = all_of(child_rank)
      ) %>%
      filter(n_parent > 1) %>%
      rename(taxon = 1) %>%
      mutate(child_rank = child_rank, parent_rank = parent_rank)
  })

  # Keep only rank pairs that produced at least one violation
  hierarchy_violations <- Filter(
    function(v) nrow(v) > 0,
    hierarchy_violations
  )

  if (length(hierarchy_violations) > 0) {
    cli::cli_h2("Hierarchical classification assumption violation - {the_server}")
    cli::cli_alert_warning(
      "Each unique taxa rank value must map to exactly one value of the next higher rank."
    )

    for (v in hierarchy_violations) {
      child_rank  <- unique(v$child_rank)
      parent_rank <- unique(v$parent_rank)
      cli::cli_alert_info(
        "{.val {nrow(v)}} {.field {child_rank}} values map(s) to multiple {.field {parent_rank}} values. {.field {child_rank}} value(s):
        {.val {v$taxon}}"
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

  # Lowercase all values as the final step — after corrections and checks so
  # that filter comparisons above match raw source capitalization and violation
  # messages report taxa names as they appear in the original data.
  the_df <- the_df %>%
    mutate_all(tolower)

  return(the_df)
}
