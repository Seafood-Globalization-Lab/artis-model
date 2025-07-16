#' standardize_countries_draft
#'
#' @return
#' @export
#'
#' @examples
standardize_countries_draft <- function() {
  # add conditional here for prod type. SAU needs additional column cleaning 
  prod_sau_clean %>%
    
    # remove a trailing parenthetical phrase from a string
    # Removes entire parenthetical phrase from chr value
    mutate(country_name_en = str_remove(country_name_en, ' \\(.+\\)$')) %>%
    mutate(country_iso3_alpha = countrycode(country_name_en, 
                                            origin = 'country.name', 
                                            destination = 'iso3c'))
}