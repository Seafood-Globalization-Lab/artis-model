#' standardize_countries_draft
#'
#' This function standardizes country English names and ISO3c codes to ARTIS
#' specifications. Add more 
#' 
#' @param df dataframe.
#' @param country_id character string.
#' @param col_name character string. Check that "charater string" is valid
#' 
#' @return a dataframe with standardized country name and iso3c columns. 
#' @export
#'
standardize_countries_draft <- function(
  df,
  country_id = c("name_en", "iso3c"),
  col_name = ""
) {

  # get dataframe with country corrections
  corrections_df <- artis::standardize_country_data()

    std_df <- df %>% 
      left_join(corrections_df, by = "country_name_en")


  # add conditional here for prod type. SAU needs additional column cleaning 
  # prod_sau_clean %>%
    
    # remove a trailing parenthetical phrase from a string
    # Removes entire parenthetical phrase from chr value
    # mutate(country_name_en = str_remove(country_name_en, ' \\(.+\\)$')) %>%
    # mutate(country_iso3_alpha = countrycode(country_name_en, 
    #                                         origin = 'country.name', 
    #                                         destination = 'iso3c'))

return(std_df)

}
