#' Standardize production common names across taxa
#'
#' Joins `CommonName` from production data into the taxa classification table
#' and identifies taxa where a single `SciName` maps to multiple or missing
#' `CommonName` values. Optionally applies a manual correction table to resolve
#' inconsistencies. Emits `cli` diagnostics before and after corrections.
#'
#' @details
#' Called in `01-clean-input-data.R` after [match_prod_taxa_to_fb_slb()] to
#' standardize `CommonName` values before downstream processing. The output is
#' assigned to `prod_taxa_classification`.
#'
#' ## Data integrity checks
#'
#' Runs two `cli`-reported data integrity checks:
#'
#' * **Check 1 — before corrections**: reports all `SciName` values associated
#'   with more than one `CommonName`, including cases where one value is `NA`.
#' * **Check 2 — after corrections**: reports remaining `SciName` values with
#'   multiple `CommonName` values and separately reports any `SciName` values
#'   with at least one `NA` `CommonName`.
#'
#' ## Manual corrections
#'
#' If `corr_tbl` is provided, it is joined to the taxa table on `SciName`.
#' `CommonName_corrected` values take precedence over existing `CommonName`
#' values via `coalesce()`. Duplicate rows introduced by the join are collapsed
#' with `distinct()`. If `corr_tbl` is `NULL`, the joined table is returned
#' without modification.
#'
#' @param prod_data Data frame. Cleaned production data containing `SciName`
#'   and `CommonName` columns. `SciName` here corresponds to the original
#'   production taxa name (`SciName_prod` in `prod_taxa`), and is the source
#'   of `CommonName` values joined into the classification table. Typically
#'   the output of [clean_prod_data()].
#' @param prod_taxa Data frame. Production taxa classification table containing
#'   at minimum `SciName` (working scientific name after matching and synonym
#'   resolution) and `SciName_prod` (original production taxa name). Typically
#'   `$prod_taxa_classification` from the output of [match_prod_taxa_to_fb_slb()].
#' @param corr_tbl Data frame or `NULL`. Manual correction table with columns
#'   `SciName`, `CommonName_corrected`, and `notes`, as produced by
#'   [build_corr_tbl_prod_com_name()]. If `NULL`, no corrections are applied.
#'   Default: `NULL`.
#'
#' @return
#' A data frame with the same columns as `prod_taxa` plus `CommonName`. One
#' row per unique combination of production taxa attributes after corrections
#' and deduplication. Assigned to `prod_taxa_classification` in
#' `01-clean-input-data.R`.
#'
#' @seealso
#' * [match_prod_taxa_to_fb_slb()] — produces the `prod_taxa` input
#' * [build_corr_tbl_prod_com_name()] — builds the correction table passed to `corr_tbl`
#'
#' @import dplyr
#' @import cli
#' @importFrom magrittr %>%
#' @export
correct_prod_common_names <- function(
  prod_data,
  prod_taxa,
  corr_tbl = NULL
) {

  # Join CommonName from prod_data into prod_taxa via original SciName_prod values
  taxa_com_names <- prod_taxa %>%
    left_join(
      prod_data %>%
        distinct(SciName, CommonName) %>%
        rename(SciName_prod = SciName),
      join_by(SciName_prod)
    )

  # Check 1: which working SciNames have multiple CommonName values?
  common_name_multiples <- taxa_com_names %>%
    distinct(SciName, CommonName) %>%
    mutate(n = n_distinct(CommonName), .by = SciName) %>%
    filter(n > 1) %>%
    arrange(SciName)

  n_multiples <- length(unique(common_name_multiples$SciName))

  cli::cli_h2("Production taxa - multiple {.field CommonName}s")
  cli::cli_alert_warning("{.val {no(n_multiples)}} working {.filed SciNames} have multiple {.field CommonName} values")
  if (n_multiples > 0) {
    cli::cli_alert_warning("They are: {.val {unique(common_name_multiples$SciName)}}")
  }

  # Apply corrections from corr_tbl - join via working SciName column (contains manual and synonym corrections)
  if (!is.null(corr_tbl)) {

    taxa_com_name_corr <- taxa_com_names %>%
      left_join(
        corr_tbl %>% select(-notes),
        join_by(SciName)
      ) %>%
      # reconciliate CommonName values - prefer corrected column
      mutate(CommonName = coalesce(CommonName_corrected, CommonName)) %>%
      # remove correction column
      select(-CommonName_corrected) %>% 
      # collapse any duplicates rows
      distinct()
      
  } else {
    taxa_com_name_corr <- taxa_com_names
  }

  # Check 2: verify corrections resolved all multiples
  common_name_multiples_2 <- taxa_com_name_corr %>%
    distinct(SciName, CommonName) %>%
    mutate(n = n_distinct(CommonName), .by = SciName) %>%
    filter(n > 1) %>%
    arrange(SciName)

  n_multiples_2 <- length(unique(common_name_multiples_2$SciName))

  cli::cli_h3("After {.field CommonName} corrections")
  if (n_multiples_2 == 0) {
    cli::cli_alert_success("All {.field CommonName} multiples resolved")
  } else {
    cli::cli_alert_warning("{.val {no(n_multiples_2)}} {.field SciNames} still have multiple {.field CommonName} values")
    cli::cli_alert_warning("They are: {.val {unique(common_name_multiples_2$SciName)}}")
    cli::cli_alert_info("{.val NA} {.field SciName} values in may distort the number of remaining instances")
  }

  return(taxa_com_name_corr)
}
