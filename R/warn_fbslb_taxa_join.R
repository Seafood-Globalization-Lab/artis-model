#' Warn on taxonomic rank conflicts after FB/SLB hierarchical joins
#'
#' @description
#' After each hierarchical `inner_join` in [match_prod_taxa_to_fbslb()], checks
#' whether any matched taxon maps to multiple values of the next higher rank in
#' the reference data. Emits a `cli` warning listing the conflicting names.
#'
#' @param joined_dat Data frame. The result of the hierarchical `inner_join`
#'   (e.g. `prod_slb_family`).
#' @param matched_rank Character. The rank column that was just matched.
#'   For FishBase: one of `"Species"`, `"Genus"`, `"Family"`, `"Order"`,
#'   `"Class"`, `"SuperClass"`.
#'   For SeaLifeBase: one of `"Species"`, `"Genus"`, `"Family"`, `"Order"`,
#'   `"Class"`, `"Phylum"`, `"Kingdom"`.
#' @param fb_or_slb Character. One of `"fishbase"` or `"sealifebase"`. Controls
#'   which rank hierarchy is used and appears in the warning message.
#'
#' @return `joined_dat`, invisibly, when no conflicts are detected. Aborts
#'   with an ARTIS assumption violation error if any conflicts are found.
#'
#' @importFrom cli cli_h2 cli_abort
#' @importFrom dplyr mutate across all_of distinct summarise n_distinct coalesce
#' @export
warn_fbslb_taxa_join <- function(
  joined_dat,
  matched_rank,
  fb_or_slb = c("fishbase", "sealifebase")
) {

  # Validate the fb_or_slb argument against allowed values
  fb_or_slb <- match.arg(fb_or_slb)

  # Define source-specific rank hierarchies low-to-high.
  # FishBase tops out at SuperClass; SeaLifeBase extends to Phylum and Kingdom.
  rank_hierarchies <- list(
    fishbase    = c("Species", "Genus", "Family", "Order", "Class", "SuperClass"),
    sealifebase = c("Species", "Genus", "Family", "Order", "Class", "Phylum", "Kingdom")
  )

  # Select the hierarchy that matches the source database
  rank_hierarchy <- rank_hierarchies[[fb_or_slb]]

  # Find the position of the matched rank in the source-specific hierarchy
  matched_rank_idx <- match(matched_rank, rank_hierarchy)

  # Decision tree based on provided rank index -----------------------------

  # Invalid matched_rank argument (not part of classification schema defined above) — abort loudly
  if (is.na(matched_rank_idx)) {
    cli::cli_abort(c(
      "x" = "{.arg matched_rank} value {.val {matched_rank}} is not a recognized rank in the \\
            {fb_or_slb} hierarchy.",
      "i" = "Valid ranks: {.val {rank_hierarchy}}"
    ),
    call = match.call())
  }

  # At the top of the hierarchy — no parent rank to check, nothing to do, exit quietly 
  if (matched_rank_idx == length(rank_hierarchy)) {
    return(invisible(joined_dat))
  }

  # Identify the next-higher rank — this is what we check for conflicts
  parent_rank <- rank_hierarchy[matched_rank_idx + 1]

  # For each SciName, count how many distinct parent rank values it maps to.
  # NA values are replaced with the sentinel "missing_value" before counting —
  # this ensures a row with NA and a row with a real value for the same SciName
  # are detected as a conflict rather than masked by filtering NAs out first.
  # Two NAs for the same SciName collapse to one "missing_value" via distinct(),
  # so n_distinct = 1 — correctly not flagged as a conflict.
  conflicts <- joined_dat %>%
    mutate(across(all_of(parent_rank), ~coalesce(., "missing_value"))) %>%
    distinct(SciName, .data[[parent_rank]]) %>%
    summarise(n_parent = n_distinct(.data[[parent_rank]]), .by = SciName) %>%
    filter(n_parent > 1)

  if (nrow(conflicts) > 0) {
    cli::cli_h2("ARTIS Assumption Violation - one hierarchical classification scheme for each sciname")
    cli::cli_abort(c(
      "Each {.field sciname} is expected to map to a single {.field {parent_rank}} value in {.field {fb_or_slb}}.",
      "x" = "{nrow(conflicts)} {.field {matched_rank}} value{?s} in {.strong {fb_or_slb}} taxa map to multiple {.field {parent_rank}} values.",
      "i" = "{.field sciname} values with multiple taxonomic classification schemes: {.val {conflicts$SciName}}",
      "i" = "Inspect the raw data: {.code fb_taxa_raw <- fread(file.path(current_fb_slb_dir, 'fb_taxa_raw.csv'), data.table = FALSE)}",
      "i" = "Inspect the raw data: {.code slb_taxa_raw <- fread(file.path(current_fb_slb_dir, 'slb_taxa_raw.csv'), data.table = FALSE)}",
      "i" = "Add a targeted fix in {.fn clean_fb_slb_taxa} — scoped to the snapshot version and server",
      "i" = "Run {.code devtools::load_all()} or {.code devtools::install()} and re-run {.fn clean_fb_slb_data} to apply the fix before proceeding"
    ),
    call = match.call())
  }

  # Return joined_dat invisibly so the function can be piped without
  # altering the object being assigned
  invisible(joined_dat)
}