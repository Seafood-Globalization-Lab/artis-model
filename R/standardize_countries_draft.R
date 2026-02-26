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
#' @import cli
#' @export
#'
standardize_countries_draft <- function(
  data,
  country_id_type = c("name_en", "iso3c"),
  country_col_name = "",
  year_col_name = ""
) {

    # Check incoming data for missing values
    na_count <- sum(is.na(data[[country_col_name]]))
    missing_count <- sum(data[[country_col_name]] == "")
    
    # --- Validate input column types before doing anything ---
    # Validate country column type
    if (!is.character(data[[country_col_name]])) {
      cli::cli_abort(c(
        "x" = "The input column {.field {country_col_name}} you supplied as {.var country_col_name} does not appear to be a character type",
        "i" = "Country names or ISO3c codes should be character strings (e.g., 'USA', 'FRA')."
      ))
    }
    
    # Validate year column type
    if (!is.numeric(data[[year_col_name]])) {
      cli::cli_abort(c(
        "x" = "The input column {.field {year_col_name}} you supplied as {.var year_col_name} does not appear to be numeric.",
        "i" = "Year values must be numeric (e.g., 2010, 2015)."
      ))
    }
    
    # Validate country_id_type
    valid_types <- c("name_en", "iso3c")
    if (!country_id_type %in% valid_types) {
      cli::cli_abort(c(
        "x" = "Invalid value supplied for {.arg country_id_type}: '{country_id_type}'.",
        "i" = "Accepted values are {.val name_en} or {.val iso3c}."
      ))
    }

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
        dplyr::mutate(!!country_col_name := stringr::str_remove(.data[[country_col_name]], "\\(.+\\)$")) %>%
        # Join to ARTIS corrections table
        dplyr::left_join(corrections_df, by = by_cols) %>% 
        # Standardize countries that ARTIS corrections table did not correct (NA values in artis_* columns)
        # pull values from given country column
        dplyr::mutate(artis_iso3c = dplyr::case_when(base::is.na(artis_iso3c) ~ countrycode::countrycode(.data[[country_col_name]],
                                                     origin = "country.name",
                                                     destination = "iso3c",
                                                     warn = FALSE),
                                       .default = artis_iso3c), 
              # If already a country that ARTIS did not correct, add in regular country
               artis_country_name = dplyr::case_when(base::is.na(artis_country_name) ~ countrycode::countrycode(.data[[country_col_name]],
                                                          origin = "country.name",
                                                          destination = "country.name",
                                                          warn = FALSE),
                                              .default = artis_country_name)) %>%
        # Remove leftover corrections_df column
        dplyr::select(-iso3c)
      
      # Get list of names that weren't standardized
      list <- std_df %>%
        dplyr::filter(base::is.na(artis_country_name)) %>%
        dplyr::select(all_of(country_col_name)) %>%
        dplyr::distinct() %>%
        dplyr::pull(country_col_name)
      
    } 
    else if (country_id_type == "iso3c") {
      
      # filter out duplicate entires (i.e., multiple country names matched to the same input iso3c - we don't need these input country names since we will get an output country name)
      corrections_df_iso3c <- corrections_df %>%
        dplyr::select(-country_name) %>%
        dplyr::distinct() %>%
        tidyr::drop_na()
      
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
                                                      destination = "iso3c",
                                                      warn = FALSE),
                                       .default = artis_iso3c),
               artis_country_name = dplyr::case_when(base::is.na(artis_country_name) ~
                                                countrycode::countrycode(.data[[country_col_name]], 
                                                            origin = "iso3c",
                                                            destination = 'country.name',
                                                            warn = FALSE),
                                              .default = artis_country_name))
      
      # Get list of names that weren't standardized
      list <- std_df %>%
        dplyr::filter(base::is.na(artis_iso3c)) %>%
        dplyr::select(all_of(country_col_name)) %>%
        dplyr::distinct() %>%
        dplyr::pull(country_col_name)
    }
  
  
  # 1️⃣ NA/missing values warning
  if (na_count > 0 | missing_count > 0) {
    cli::cli_alert_danger("Found {missing_count} missing values and {na_count} NA values in column {.field {country_col_name}}.")
    cli::cli_alert_info("These NAs will not be standardized.")
  }
  
  # 2️⃣ Print a blank line to separate warnings
  cli::cli_text("")
  
  # 3️⃣ Unstandardized values warning
  non_na_list <- list[!is.na(list)]
  if (length(non_na_list) > 0) {
    visible_list <- sapply(non_na_list, function(x) if (x == "") dQuote("") else dQuote(x))
    
    # First part of the warning
    cli::cli_alert_warning("Some values in {.field {country_col_name}} were not standardized.")
    cli::cli_alert_info("These values were neither corrected by the ARTIS corrections table nor found by `countrycode`.")
    
    # Print the unstandardized list on its own line with attention emoji
    cli::cli_text("⚠ Unstandardized values: {paste(visible_list, collapse = ', ')}")
  }
  
  # Explicitly print tibble before returning
  # print(std_df)
  
  # Return invisibly so assignment doesn’t double-print
  return(std_df)

}
