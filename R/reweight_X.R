#' @export
reweight_X <- function(country_est, country_j){
  tmp_p <- data.frame(SciName = colnames(country_est[[country_j]]$X), 
                      production = country_est[[country_j]]$p)
  
  tmp_X <- as.data.frame(country_est[[country_j]]$X)
  tmp_X <- tmp_X %>%
    rownames_to_column(var = "hs6") %>%
    pivot_longer(2:(ncol(tmp_X)+1), 
                 names_to = "SciName", 
                 values_to = "estimated_X") %>%
    filter(estimated_X > 0) %>% 
    mutate(iso3c = str_extract(hs6, "[[:alpha:]]+"),
           hs6 = str_extract(hs6, "[[:digit:]]+")) %>%
    # Join with production to reweight
    left_join(tmp_p, by = "SciName") %>% 
    # Multiply by production to reweight by code
    mutate(species_commod_production = estimated_X*production) %>%
    # Total production of each commod
    group_by(hs6) %>%
    mutate(total_commod_production = sum(species_commod_production)) %>%
    # Calculate proportion of each species within each code
    mutate(reweighted_X = species_commod_production/total_commod_production) %>%
    select(iso3c, hs6, SciName, reweighted_X)
  
  return(tmp_X)
}