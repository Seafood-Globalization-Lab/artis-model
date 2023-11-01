#' @export
create_reweight_X_long <- function(country_est, V1, V2) {
  
  countries_to_analyze <- names(country_est) 
  
  reweight_X_long <- data.frame()
  for(i in 1:length(countries_to_analyze)){
    tmp <- reweight_X(country_est, countries_to_analyze[i], V1, V2)
    reweight_X_long <- reweight_X_long %>%
      bind_rows(tmp)
  }
  
  check_reweight_X_long <- reweight_X_long %>%
    group_by(iso3c, hs6) %>%
    summarize(reweighted_X = sum(reweighted_X, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(difference = 1 - reweighted_X) %>%
    filter(abs(difference) > 1e-9)
  
  if (nrow(check_reweight_X_long)) {
    warning("Reweighted X long proportions DO NOT sum to 1.")
    return(NULL)
  }
  
  return(reweight_X_long)
}