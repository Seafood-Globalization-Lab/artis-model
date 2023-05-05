#' @export
add_prod_source <- function(prod_data, analysis_year){
  
  prod_key <- prod_data %>%
    # Clean data
    select(year, country_iso3_alpha, SciName, prod_method, habitat, quantity) %>%
    filter(year == analysis_year) %>%
    filter(quantity != 0) %>%
    filter(country_iso3_alpha != "") %>%
    arrange(country_iso3_alpha, SciName, prod_method, habitat) %>%
    # Standardize prod_method levels as lowercase
    mutate(prod_method = str_to_lower(prod_method)) %>%
    # Collapse brackish + freshwater + marine aquaculture into aquaculture
    mutate(prod_method = case_when(str_detect(prod_method, "marine|brackishwater|freshwater") ~ "aquaculture",
                                   str_detect(prod_method, "capture") ~ "capture",
                                   TRUE ~ "unspecified")) %>%
    # Calculate new quantity based on these new groupings
    group_by(country_iso3_alpha, SciName, prod_method, habitat) %>%
    summarise(quantity = sum(quantity)) %>%
    ungroup() %>%
    # Now get TOTAL quantity per country, species (irrespective of prod_method and habitat)
    group_by(country_iso3_alpha, SciName) %>%
    mutate(total_sciname_quantity = sum(quantity)) %>%
    ungroup() %>%
    # Calculate proportion of the total quantity for each source+habitat combo 
    mutate(prop_by_source = quantity / total_sciname_quantity) %>%
    select(country_iso3_alpha, SciName, prod_method, habitat, prop_by_source)
  
  return(prod_key)
    
}