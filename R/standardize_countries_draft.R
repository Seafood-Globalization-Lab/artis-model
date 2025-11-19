#' standardize_countries_draft
#'
#' This function standardizes country English names and ISO3c codes to ARTIS
#' specifications. Add more 
#' 
#' @param data dataframe. Input dataframe that will get country standardized
#' @param country_id_type character. Denote whether to standardize based on 
#' either the input data's country name column or iso3c column
#' @param country_col_name character. Name of the input country col name to be joined to standardization data
#' @param year_col_name character. Name of the year col name to be joined to standardization data
#' @return a dataframe with standardized country name and iso3c columns. 
#' @importFrom countrycode countrycode
#' @export
#'
standardize_countries_draft <- function(
  data,
  country_id_type = c("name_en", "iso3c"),
  country_col_name = "",
  year_col_name = ""
) {

  # get dataframe with country corrections
  corrections_df <- artis::standardize_country_data()

  # Set up join by naming to match input data column names to the standardization column names
  # by_cols <- setNames(c("iso3c", "year"), c(country_col_name, year_col_name))
    
  # Join input data to standardization data frame based on country_id_type
    if (country_id_type == "name_en") {

      # set up join by naming to match input data column names to the standardization column names
      by_cols <- setNames(c("country_name", "year"), c(country_col_name, year_col_name))
      
      std_df <- data %>% 
        # Join to ARTIS corrections table
        left_join(corrections_df, by = by_cols) %>% 
        # Standardize countries that ARTIS corrections table did not correct (NA values in artis_* columns)
        # pull values from given country column
        mutate(artis_iso3c = case_when(is.na(artis_iso3c) ~ 
                                         countrycode(!!sym(country_col_name),
                                                     origin = "country.name",
                                                     destination = "iso3c"),
                                       .default = artis_iso3c), 
              # If already a country that ARTIS did not correct, add in regular country
               artis_country_name = case_when(is.na(artis_country_name) ~ 
                                                countrycode(!!sym(country_col_name),
                                                          origin = "country.name",
                                                          destination = "country.name"),
                                              .default = artis_country_name)) %>%
        # Remove original country iso3c column if it exists
        select(-iso3c)
    } 
    else if (country_id_type == "iso3c") {
      # set up join by naming to match input data column names to the standardization column names
      by_cols <- setNames(c("iso3c", "year"), c(country_col_name, year_col_name))
      # Join input data to standardization data frame based on iso3c
      std_df <- data %>% 
        # Join to ARTIS corrections table
        left_join(corrections_df, by = by_cols) %>% 
        # Standardize countries that ARTIS corrections table did not correct (NA values in artis_* columns)
        # pull values from given country column
        mutate(artis_iso3c = case_when(is.na(artis_iso3c) ~ 
                                          countrycode(!!sym(country_col_name),
                                                      origin = "iso3c",
                                                      destination = "iso3c"),
                                       .default = artis_iso3c),
               artis_country_name = case_when(is.na(artis_country_name) ~
                                                countrycode(!!sym(country_col_name), 
                                                            origin = "iso3c",
                                                            destination = 'country.name'),
                                              .default = artis_country_name)) %>%
        # Remove original country name column if it exists
        select(-country_name)
    }
  


  # add conditional here for prod type. SAU needs additional column cleaning 
  # prod_sau_clean %>%
    
    # remove a trailing parenthetical phrase from a string
    # Removes entire parenthetical phrase from chr value
    # mutate(country_name_en = str_remove(country_name_en, ' \\(.+\\)$')) %>%
    # mutate(country_iso3_alpha = countrycode(country_name_en, 
    #                                         origin = 'country.name', 
    #                                         destination = 'iso3c'))

  # my_function <- function(data, country_col_name = "country_name_en") {
  #   result <- data |>
  #     group_by(.data[[country_col_name]]) |>
  #     summarize(count = n())
  #   
  #   return(result)
  # }
  
return(std_df)

}
