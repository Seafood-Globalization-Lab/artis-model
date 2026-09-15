#' Title
#'
#' @param prod_data
#' @param prod_taxa
#' @param corr_tbl
#'
#' @returns
#'
#' @export
#' @examples
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

  cli::cli_h2("Production taxa - multiple{.field CommonName}")
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

  # Check 2: verify corrections resolved all multipless
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
