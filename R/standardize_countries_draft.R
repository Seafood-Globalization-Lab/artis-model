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
#' @importFrom stats setNames
#' @importFrom stringr str_remove
#' @import dplyr
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
      by_cols <- stats::setNames(c("country_name", "year"), c(country_col_name, year_col_name))
      
      std_df <- data %>% 
        # remove any trailing parenthetical phrase from string values
        dplyr::mutate(!!country_col_name := stringr::str_remove(.data[[country_col_name]], "\\(.+\\)$"))
        # Join to ARTIS corrections table
        dplyr::left_join(corrections_df, by = by_cols) %>% 
        # Standardize countries that ARTIS corrections table did not correct (NA values in artis_* columns)
        # pull values from given country column
        dplyr::mutate(artis_iso3c = dplyr::case_when(base::is.na(artis_iso3c) ~ countrycode::countrycode(.data[[country_col_name]],
                                                     origin = "country.name",
                                                     destination = "iso3c"),
                                       .default = artis_iso3c), 
              # If already a country that ARTIS did not correct, add in regular country
               artis_country_name = dplyr::case_when(base::is.na(artis_country_name) ~ countrycode::countrycode(.data[[country_col_name]],
                                                          origin = "country.name",
                                                          destination = "country.name"),
                                              .default = artis_country_name)) %>%
        # Remove leftover corrections_df column
        dplyr::select(-iso3c)
      
      # Get list of names that weren't standardized
      list <- std_df %>%
        dplyr::filter(base::is.na(artis_country_name)) %>%
        dplyr::select(country_col_name) %>%
        dplyr::distinct() %>%
        dplyr::pull(country_col_name)
      
    } 
    else if (country_id_type == "iso3c") {
      
      # filter out duplicate entires (i.e., multiple country names matched to the same input iso3c - we don't need these input country names since we will get an output country name)
      corrections_df_iso3c <- corrections_df %>%
        dplyr::select(-country_name) %>%
        dplyr::distinct()
      
      # set up join by naming to match input data column names to the standardization column names
      by_cols <- stats::setNames(c("iso3c", "year"), c(country_col_name, year_col_name))

      # Join input data to standardization data frame based on iso3c
      std_df <- data %>% 
        # remove any trailing parenthetical phrase from string values
        dplyr::mutate(!!country_col_name := stringr::str_remove(.data[[country_col_name]], '\\(.+\\)$')) %>%
        # Join to ARTIS corrections table
        dplyr::left_join(corrections_df_iso3c, by = by_cols) %>% 
        # Standardize countries that ARTIS corrections table did not correct (NA values in artis_* columns)
        # pull values from given country column
        dplyr::mutate(artis_iso3c = dplyr::case_when(is.na(artis_iso3c) ~ 
                                          countrycode::countrycode(.data[[country_col_name]],
                                                      origin = "iso3c",
                                                      destination = "iso3c"),
                                       .default = artis_iso3c),
               artis_country_name = dplyr::case_when(base::is.na(artis_country_name) ~
                                                countrycode::countrycode(.data[[country_col_name]], 
                                                            origin = "iso3c",
                                                            destination = 'country.name'),
                                              .default = artis_country_name))
      
      # Get list of names that weren't standardized
      list <- std_df %>%
        dplyr::filter(base::is.na(artis_iso3c)) %>%
        dplyr::select(country_col_name) %>%
        dplyr::distinct() %>%
        dplyr::pull(country_col_name)
    }
  
  
# Only return a warning for unstandardized values if the list for which unstandardized values are reported is greater than 0  
  if (!length(list) == 0) {
    warning(base::paste0(
      "List of names that weren't standardized by either the corrections dataframe or by countrycode: ",
      base::dQuote(list)
    ))
  }
  
  std_df <- std_df %>%
    mutate(artis_iso3c = case_when(
      is.na(artis_iso3c) ~ "",
      .default = artis_iso3c))
  
return(std_df)

}
