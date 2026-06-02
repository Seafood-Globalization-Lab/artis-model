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
#' @return `joined_dat`, invisibly. Called for its side-effect warning.
#'
#' @importFrom cli cli_warn
#' @importFrom dplyr filter distinct summarise n_distinct
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

  # If the rank is unrecognised or is already the highest rank, nothing to check
  if (is.na(matched_rank_idx) || matched_rank_idx == length(rank_hierarchy)) {
    return(invisible(joined_dat))
  }

  # Identify the next-higher rank — this is what we check for conflicts
  parent_rank <- rank_hierarchy[matched_rank_idx + 1]

  # If the parent rank column is not present in the joined data, skip silently
  if (!parent_rank %in% names(joined_dat)) {
    return(invisible(joined_dat))
  }

  # For each SciName, count how many distinct parent rank values it maps to.
  # Any SciName with n_parent > 1 represents a many-to-one conflict.
  conflicts <- joined_dat %>%
    filter(!is.na(.data[[parent_rank]])) %>%
    distinct(SciName, .data[[parent_rank]]) %>%
    summarise(n_parent = n_distinct(.data[[parent_rank]]), .by = SciName) %>%
    filter(n_parent > 1)

  # Only emit a warning when at least one conflict is detected
  if (nrow(conflicts) > 0) {
    cli::cli_warn(c(
      # Lead with the conflict summary
      "!" = "Matching prod taxa to {fb_or_slb}: many-to-one conflict at {matched_rank} to {parent_rank}.",
      # Report the count with automatic singular/plural via cli pluralization
      "i" = "{nrow(conflicts)} {matched_rank}{?s} map to multiple {parent_rank} values:",
      # List the offending SciNames inline
      " " = paste(conflicts$SciName, collapse = ", "),
      # Flag downstream risk
      "i" = "ARTIS expects a single taxonomic record per taxa, multiple taxa classifications will cause errors",
      # Suggest remediation paths
      "i" = "Consider adding corrections to {.fn build_corr_tbl_prod_sciname} or auditing the source data."
    ))
  }

  # Return joined_dat invisibly so the function can be piped without
  # altering the object being assigned
  invisible(joined_dat)
}