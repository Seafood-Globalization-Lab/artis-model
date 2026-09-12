correct_prod_common_names <- function(
  prod_data,
  prod_taxa,
  corr_tbl = NULL
) {
  
  # Test which sciname have multiple common name values - keep CommonName values for reference

   prod_com_names <- prod_data %>% 
    distinct(SciName, CommonName) %>% 
    rename(SciName_prod = SciName)

  common_name_multiples <- prod_com_names %>% 
    mutate(n = n_distinct(CommonName), .by = SciName_prod) %>% 
    filter(n > 1) %>% 
    arrange(SciName_prod) %>% 
    # Join prod_taxa scinames to get updated "working" scinames to report
    left_join(
      prod_taxa %>% 
        select(SciName_prod, SciName)) %>% 
    # reconciliate the two SciName versions - prefer the working version from prod_taxa
    mutate(SciName = coalesce(SciName, SciName_prod)) %>% 
    select(-SciName_prod)

  n_multiples <- length(unique(common_name_multiples$SciName))

  # Message output scinames 

  cli::cli_h2("Production data multiple CommonNames")
  cli::cli_alert_warning("{.val {no(n_multiples)}} SciNames have multiple CommonName values")
  if (n_multiples > 0) {
    cli::cli_alert_warning("They are: 
      {.val {unique(common_name_multiples$SciName)}}")
  }

  # Take distinct pairs of Sciname and common name from prod_data and 
  # join to prod_taxa

  taxa_com_name_corr <- prod_taxa %>% 
    left_join(
      prod_com_names,
      join_by(SciName_prod)
    ) %>% 
    left_join(
      corr_tbl %>% 
        select(-notes),
      join_by(SciName)
    ) %>% 
    # reconcile CommonName columns - prefer corrections
    mutate(CommonName = coalesce(CommonName_corrected, CommonName)) %>% 
    select(-CommonName_corrected)

  # final message of correction results

  common_name_multiples_2 <- taxa_com_name_corr %>% 
    distinct(SciName, CommonName) %>% 
    mutate(n = n_distinct(CommonName), .by = SciName) %>% 
    filter(n > 1) %>% 
    arrange(SciName)

  # return updated prod_taxa
}