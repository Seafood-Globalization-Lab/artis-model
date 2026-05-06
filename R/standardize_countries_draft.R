#' Standardize country identifiers to ARTIS naming conventions
#'
#' This function harmonizes country identifiers in an input dataset to the
#' ARTIS standard using a combination of the ARTIS corrections table and
#' fallback mappings from the `countrycode` package. It supports workflows
#' where the incoming country identifier is either an English country name
#' or an ISO3c code, and returns a consistent pair of standardized fields:
#' `artis_country_name` and `artis_iso3c`.
#'
#' The function:
#' (1) Cleans input country strings by removing trailing parenthetical phrases.
#' (2) Joins the input data to the ARTIS corrections table using user‑specified
#'   country and year column names.
#' (3) Applies ARTIS overrides where available, and uses `countrycode` to fill
#'   in any remaining unmapped identifiers.
#' (4) Warns when values cannot be standardized by either source.
#' (5) Preserves all original columns and appends standardized identifiers.
#'
#' @param data A data frame containing country and year identifiers to be standardized.
#' @param country_id_type Character string indicating the type of identifier
#'   supplied in `country_col_name`. Must be either `"name_en"` for English
#'   country names or `"iso3c"` for ISO3c codes.
#' @param country_col_name Character string giving the name of the column in
#'   `data` that contains the country identifier to standardize.
#' @param year_col_name Character string giving the name of the column in
#'   `data` that contains the year used for joining to the ARTIS corrections table.
#'
#' @return A data frame containing all original columns plus:
#'   (1) `artis_country_name`: standardized ARTIS country name
#'   (2) `artis_iso3c`: standardized ARTIS ISO3c code (empty string if unresolved)
#'
#' @details
#' The ARTIS corrections table may contain multiple historical mappings for a
#' given country-year combination. The join uses both the country identifier
#' and year to ensure the correct mapping is applied. When no ARTIS mapping
#' exists, the function falls back to `countrycode` to infer a standardized
#' name and ISO3c code. Any identifiers that cannot be resolved by either
#' source are reported in a warning.
#'
#' @importFrom countrycode countrycode
#' @importFrom stats setNames
#' @importFrom stringr str_remove
#' @import dplyr
#' @import cli
#' @export

standardize_countries_draft <- function(
  data,
  country_id_type = c("name_en", "iso3c"),
  country_col_name,
  year_col_name 
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
        dplyr::mutate(
          flag = dplyr::case_when(base::is.na(artis_iso3c) ~ TRUE,
                                              .default = FALSE)) %>% #,
              # If missing country name value (i.e. not corrected by ARTIS corrections join) add std country name via country code from original/supplied country name
          # artis_country_name = dplyr::case_when(
            # base::is.na(artis_country_name) ~ countrycode::countrycode(.data[[country_col_name]],
            #                                               origin = "country.name",
            #                                               destination = "country.name",
            #                                               warn = FALSE),
                                              # .default = artis_country_name)) %>%
        # Remove leftover corrections_df column
        dplyr::select(-iso3c)
    
      # Problem: Correcting some of the input territory names that don't have 
      # in house ARTIS respective matches get corrected via countrycode, and
      # countrycode corrects to the territory iso3c instead of the soverign.
      # we have flagged these rows to rejoin corrected countrycode iso3c to
      # artis corrections to receive their soverign variants.
      
      # Sets the names (i.e., the input column variable names) of the corrections_df
      # column names to be joined (see ?setNames()) 
      by_cols <- stats::setNames(c("iso3c", "year"), c("artis_iso3c", year_col_name))
      
      # Rejoin flagged data and df_corrections by iso3c and year
      flagged_data <- std_df %>%
        dplyr::filter(flag == TRUE) %>%
        dplyr::mutate(
          artis_iso3c = dplyr::case_when(
            base::is.na(artis_iso3c) ~ countrycode::countrycode(
              .data[[country_col_name]],
              origin = "country.name",
              destination = "iso3c",
              warn = FALSE
            ),
            .default = artis_iso3c
          )
        ) %>% 
        dplyr::left_join(corrections_df %>%
                             select(-artis_country_name), by = by_cols) %>%
        dplyr::select(-artis_country_name, -country_name) %>%
        select(-artis_iso3c) %>%
        rename(artis_iso3c = artis_iso3c.y) %>%
        mutate(artis_country_name = countrycode(artis_iso3c,
                                                origin = "iso3c",
                                                destination = "country.name")) %>%
        select(-flag)
      
      nonflagged_data <- std_df %>%
        dplyr::filter(flag == FALSE) %>%
        select(-flag)
      
      std_df <- bind_rows(flagged_data,nonflagged_data)
      
      # Get vector of country names that weren't standardized (i.e. have NA values)
      not_std_vec <- std_df %>%
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
        # Join to ARTIS corrections table
        dplyr::left_join(corrections_df_iso3c, by = by_cols) %>% 
        # Standardize countries that ARTIS corrections table did not correct (NA values in artis_* columns)
        # pull values from given country column
        dplyr::mutate(artis_iso3c = dplyr::case_when(
          is.na(artis_iso3c) ~ countrycode::countrycode(.data[[country_col_name]],
                                                      origin = "iso3c",
                                                      destination = "iso3c",
                                                      warn = FALSE),
                                       .default = artis_iso3c)) %>%
        dplyr::mutate(artis_country_name = dplyr::case_when(
          
          # FIXIT: take a look at to see whether we should apply this to all names
          base::is.na(artis_country_name) ~ countrycode::countrycode(.data[[country_col_name]], 
                                                            origin = "iso3c",
                                                            destination = 'country.name',
                                                            warn = FALSE),
                                              .default = artis_country_name))
      
      # Get vector of country names that weren't standardized (i.e. have NA values)
      not_std_vec <- std_df %>%
        dplyr::filter(base::is.na(artis_iso3c)) %>%
        dplyr::select(all_of(country_col_name)) %>%
        dplyr::distinct() %>%
        dplyr::pull(country_col_name)
    }
  
  
  # NA/missing values warning
  if (na_count > 0 | missing_count > 0) {
    cli::cli_alert_danger("Found {missing_count} missing values and {na_count} NA values in column {.field {country_col_name}}.")
    cli::cli_alert_info("These NAs will not be standardized.")
  }
  
  # Print a blank line to separate warnings
  cli::cli_text("")
  
  # list of country names that did not successfully get assigned iso3c codes
  not_std_countries <- not_std_vec[!is.na(not_std_vec)]
  if (length(not_std_vec) > 0) {
    visible_list <- sapply(not_std_countries, function(x) if (x == "") dQuote("") else dQuote(x))
    
    # First part of the warning
    cli::cli_alert_warning("Some values in {.field {country_col_name}} were not standardized.")
    cli::cli_alert_info("These values were neither corrected by the ARTIS corrections table nor found by `countrycode`.")
    
    # Print the unstandardized list on its own line with attention emoji
    cli::cli_alert_info("Unstandardized values: {paste(visible_list, collapse = ', ')}")
  }
  
  # Explicitly print tibble before returning
  # print(std_df)
  
  # Return invisibly so assignment doesn’t double-print
  return(std_df)

}
