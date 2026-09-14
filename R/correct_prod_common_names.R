correct_prod_common_names <- function(
  prod_data,
  prod_taxa,
  corr_tbl = NULL
) {

  # Join CommonName from prod_data into prod_taxa via SciName_prod
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

  cli::cli_h2("Production data multiple CommonNames")
  cli::cli_alert_warning("{.val {no(n_multiples)}} SciNames have multiple CommonName values")
  if (n_multiples > 0) {
    cli::cli_alert_warning("They are: {.val {unique(common_name_multiples$SciName)}}")
  }

  # Apply corrections from corr_tbl
  if (!is.null(corr_tbl)) {
    taxa_com_name_corr <- taxa_com_names %>%
      left_join(
        corr_tbl %>% select(-notes),
        join_by(SciName)
      ) %>%
      mutate(CommonName = coalesce(CommonName_corrected, CommonName)) %>%
      select(-CommonName_corrected)
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

  cli::cli_h2("After corrections")
  if (n_multiples_2 == 0) {
    cli::cli_alert_success("All CommonName multiples resolved")
  } else {
    cli::cli_alert_warning(
      "{.val {no(n_multiples_2)}} SciNames still have multiple CommonName values"
    )
    cli::cli_alert_warning(
      "They are: {.val {unique(common_name_multiples_2$SciName)}}"
    )
  }

  # FIXIT: This is not exactly what we want to return right? Double check that we don't want to 
  # join taxa_common_names_corr back to prod_taxa. Currently, returned df assigned to prod_taxa_classification in 01-clean-data-input.R
  return(taxa_com_name_corr)
}
