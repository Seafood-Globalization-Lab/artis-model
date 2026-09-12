correct_prod_common_names <- function(
  prod_data,
  prod_taxa,
  corr_tbl = NULL
) {
  
  # Test which sciname have multiple common name values - keep CommonName values for reference
  common_name_multiples <- prod_data %>% 
    distinct(SciName, CommonName) %>% 
    mutate(n = n_distinct(CommonName), .by = SciName) %>% 
    filter(n > 1) %>% 
    arrange(SciName)

  n_multiples <- length(unique(common_name_multiples$SciName))

  # Correct by joining correction table to prod_taxa 

  prod_taxa_corr <- prod_taxa %>% 
    left_join(
      corr_tbl,
      join_by()
    )


  # Message output scinames 

  cli::cli_h2("Production data multiple CommonNames")
  cli::cli_alert_warning("{.val {no(n_multiples)}} original SciNames have multiple CommonName values")
  if (n_multiples > 0) {
    cli::cli_alert_warning("They are: 
      {.val {unique(common_name_multiples$SciName)}}")
  }



  # Join prod_data corrected subset to prod_taxa

  # final message of correction results

  # return updated prod_taxa
}