#' Standardize production common names across taxa
#'
#' Joins `CommonName` from production data into the taxa classification table
#' and identifies taxa where a single `SciName` maps to multiple or missing
#' `CommonName` values. Optionally applies a manual correction table to resolve
#' inconsistencies, then replaces any remaining `NA` `CommonName` values with a
#' fallback string. Emits `cli` diagnostics at each stage.
#'
#' @details
#' Called in `01-clean-input-data.R` after [match_prod_taxa_to_fb_slb()] to
#' standardize `CommonName` values before downstream processing. The output is
#' assigned to `prod_taxa_classification` in `01-clean-input-data.R`.
#'
#' ## Data integrity checks
#'
#' Runs two `cli`-reported checks before corrections are applied:
#'
#' * **Check 1 — multiple CommonNames**: reports all `SciName` values associated
#'   with more than one `CommonName`, including cases where one value is `NA`.
#' * **Check 2 — NA CommonNames**: reports all `SciName` values with at least
#'   one `NA` `CommonName`.
#'
#' After corrections are applied, a third check re-runs Check 1 to report any
#' `SciName` values that still have multiple `CommonName` values.
#'
#' ## Manual corrections
#'
#' If `corr_tbl` is provided, it is joined to the taxa table on `SciName`.
#' `CommonName_corrected` values take precedence over existing `CommonName`
#' values via `coalesce()`. Duplicate rows introduced by the join are collapsed
#' with `distinct()`. If `corr_tbl` is `NULL`, the joined table is returned
#' without modification.
#'
#' ## NA fallback
#'
#' After corrections, any remaining `NA` `CommonName` values are replaced with
#' `paste0(SciName, " no common")`. This ensures no `NA` values propagate
#' downstream. The `" no common"` suffix is arbitrary and can be changed in the
#' function body.
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
#' and deduplication. `CommonName` contains no `NA` values — any remaining
#' `NA`s after correction are replaced with `paste0(SciName, " no common")`.
#' Assigned to `prod_taxa_classification` in `01-clean-input-data.R`.
#'
#' @seealso
#' * [match_prod_taxa_to_fb_slb()] — produces the `prod_taxa` input
#' * [build_corr_tbl_prod_com_name()] — builds the correction table passed to `corr_tbl`
#'
#' @import dplyr
#' @import cli
#' @importFrom magrittr %>%
#' @export
correct_common_names <- function(
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

  cli::cli_h2("Production taxa {.field CommonName} check 1 - multiple values")
  cli::cli_alert_warning("{.val {no(n_multiples)}} working {.filed SciNames} have multiple {.field CommonName} values")
  if (n_multiples > 0) {
    cli::cli_alert_warning("They are: {.val {unique(common_name_multiples$SciName)}}")
  }

  # Check 2: which SciNames have NA CommonName values?
  common_name_na <- taxa_com_names %>% 
    distinct(SciName, CommonName) %>% 
    filter(is.na(CommonName))

  n_na <- length(common_name_na$CommonName)

  cli::cli_h2("Production taxa {.field CommonName} check 2 - {.val NA} values")
  cli::cli_alert_warning("{.val {no(n_na)}} {.field CommonName}{?s} have {.val NA}{?s}")
  if (n_na > 0) {
    cli::cli_alert_warning("They are: {.val {unique(common_name_na$SciName)}}")
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

  cli::cli_h3("Post {.field CommonName} corrections:")
  if (n_multiples_2 == 0) {
    cli::cli_alert_success("All {.field CommonName} multiples resolved")
  } else {
    cli::cli_alert_warning("{.val {no(n_multiples_2)}} {.field SciNames} still have multiple {.field CommonName} values")
    cli::cli_alert_warning("They are: {.val {unique(common_name_multiples_2$SciName)}}")
    cli::cli_alert_info("{.val NA} {.field SciName} values in may distort the number of remaining instances")
  }

  # Apply correction for NA values
  # NOTE: The "correction term" is arbitrary - can change in the paste0 function below
  
  taxa_com_name_corr <- taxa_com_name_corr %>%
    mutate(
      CommonName = case_when(
        is.na(CommonName) ~ paste0(SciName, " no common"),
        .default = CommonName
      )
    )
  
    # Check 2: which SciNames have NA CommonName values?
  common_name_na_2 <- taxa_com_name_corr %>% 
    distinct(SciName, CommonName) %>% 
    filter(is.na(CommonName))

  n_na_2 <- length(common_name_na_2$CommonName)

  if(n_na_2 == 0){
    cli::cli_alert_success("All {.field CommonName} {.val NA}s resolved")
  } else {
    cli::cli_alert_warning("{.val {no(n_na_2)}} {.field SciNames} still have {.val NA} {.field CommonName} values")
    cli::cli_alert_warning("They are: {.val {unique(common_name_na_2$SciName)}}")
  }
  

  return(taxa_com_name_corr)
}
