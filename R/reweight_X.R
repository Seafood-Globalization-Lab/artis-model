#' Reweight X matrix by species production volumes for a single country
#'
#' @param country_est Named list. Per-country optimization results.
#' @param country_j Character. ISO3c code of the country to process.
#' @param V1 Matrix. Live-weight-to-product conversion factors (products x species).
#' @param V2 Matrix. Product-to-product conversion factors (products x products).
#' @return Data frame with columns iso3c, hs6, SciName, and reweighted_X
#'   (proportion of each HS6 product attributable to each species).
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter mutate left_join group_by ungroup select
#' @importFrom stringr str_extract
#' @export
reweight_X <- function(country_est, country_j, V1, V2){
  # Live weight conversion factors from live weight to product weight
  # maximum V1 values is 1
  V1_long <- data.frame(V1) %>%
    mutate(hs6 = colnames(V2)) %>%
    pivot_longer(cols = -hs6, names_to = "SciName", values_to = "live_weight_cf") %>%
    filter(live_weight_cf > 0)
  
  tmp_p <- data.frame(SciName = colnames(country_est[[country_j]]$X), 
                      production = country_est[[country_j]]$p)
  
  tmp_X <- as.data.frame(country_est[[country_j]]$X)
  tmp_X <- tmp_X %>%
    rownames_to_column(var = "hs6") %>%
    pivot_longer(2:(ncol(tmp_X)+1), 
                 names_to = "SciName", 
                 values_to = "estimated_X") %>%
    filter(estimated_X > 1e-9) %>% 
    mutate(iso3c = str_extract(hs6, "[[:alpha:]]+"),
           hs6 = str_extract(hs6, "[[:digit:]]+")) %>%
    # Join with production to reweight
    left_join(tmp_p, by = "SciName") %>%
    # CFs from live weight to product weight
    left_join(
      V1_long,
      by = c("hs6", "SciName")
    ) %>%
    # Converting production from live weight to product weight to match calculations
    # further down the pipeline in calculating snet
    mutate(production = production * live_weight_cf) %>%
    # Multiply by production to reweight by code
    mutate(species_commod_production = estimated_X * production) %>%
    # Total production of each commod
    group_by(hs6) %>%
    mutate(total_commod_production = sum(species_commod_production)) %>%
    # Calculate proportion of each species within each code
    mutate(reweighted_X = species_commod_production/total_commod_production) %>%
    select(iso3c, hs6, SciName, reweighted_X)
  
  return(tmp_X)
}