#' Standardize country identifiers to ARTIS naming conventions
#'
#' @description
#' Harmonizes country identifiers in an input data frame to the ARTIS standard
#' using the ARTIS corrections table and fallback mappings from the
#' \code{countrycode} package. Accepts either English country names or ISO3c
#' codes and appends two standardized output columns: \code{artis_country_name}
#' and \code{artis_iso3c}.
#'
#' @details
#' Called by \code{\link{std_artis_input_countries}} as the core standardization
#' step applied to each ARTIS data source. The corrections table is produced by
#' \code{\link{build_std_countries_tbl}} and joined to the input data by country
#' identifier and year, ensuring the historically correct sovereign-country
#' mapping is applied for time-dependent political changes.
#'
#' \strong{Input validation}
#'
#' Aborts with an informative error if the country column is not character type,
#' the year column is not numeric, or \code{country_id_format} is not one of
#' \code{"name_en"} or \code{"iso3c"}.
#'
#' \strong{Standardization logic}
#'
#' When \code{country_id_format = "name_en"}, trailing parenthetical phrases are
#' stripped from country name strings before joining. Rows not matched by the
#' ARTIS corrections table are flagged and re-processed: \code{countrycode} is
#' used to infer an ISO3c code, which is then re-joined to the corrections table
#' to resolve the correct sovereign country. This two-pass approach handles
#' territory names that \code{countrycode} maps to a territory ISO3c rather than
#' a sovereign one.
#'
#' When \code{country_id_format = "iso3c"}, the join is performed directly on the
#' ISO3c code and year. Unmatched rows fall back to \code{countrycode} for both
#' \code{artis_iso3c} and \code{artis_country_name}.
#'
#' In both paths, country identifiers that cannot be resolved by either source
#' are reported as a \code{cli} warning and returned as \code{NA} in the output
#' columns.
#'
#' @param data Data frame. Input table containing country and year identifiers
#'   to be standardized.
#' @param country_id_format Character. Type of country identifier supplied in
#'   \code{country_col}. One of \code{"name_en"} (English country name) or
#'   \code{"iso3c"} (ISO 3166-1 alpha-3 code).
#' @param country_col Character. Name of the column in \code{data}
#'   containing the country identifier to standardize.
#' @param year_col Character. Name of the column in \code{data} containing
#'   the year, used to join the time-dependent ARTIS corrections table.
#'
#' @return
#' A data frame with all original columns from \code{data} plus:
#' \describe{
#'   \item{artis_country_name}{Standardized ARTIS sovereign country name.
#'     \code{NA} if the identifier could not be resolved.}
#'   \item{artis_iso3c}{Standardized ARTIS ISO3c code. \code{NA} if the
#'     identifier could not be resolved.}
#' }
#'
#' @note
#' When \code{country_id_format = "name_en"}, the \code{iso3c} column introduced
#' by the corrections join is removed from the output. Unresolvable country
#' identifiers produce \code{NA} in output columns and are reported via
#' \code{cli} warning rather than causing an error.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{build_std_countries_tbl}} — produces the ARTIS corrections
#'     table used internally by this function
#'   \item \code{\link{std_artis_input_countries}} — calls this function as the
#'     core standardization step for FAO, BACI, and SAU pipeline inputs
#' }
#'
#' @importFrom countrycode countrycode
#' @importFrom stats setNames
#' @importFrom stringr str_remove str_trim
#' @importFrom tidyr drop_na
#' @import dplyr
#' @import cli
#' @export

standardize_countries <- function(
  data,
  country_id_format = c("name_en", "iso3c"),
  country_col,
  year_col 
) {

    # Check incoming data for missing values
    na_count <- sum(is.na(data[[country_col]]))
    missing_count <- sum(na.omit(data[[country_col]] == ""))
    
    # --- Validate input column types before doing anything ---
    # Validate country column type
    if (!is.character(data[[country_col]])) {
      cli::cli_abort(c(
        "x" = "The input column {.field {country_col}} you supplied as {.var country_col} does not appear to be a character type",
        "i" = "Country names or ISO3c codes should be character strings (e.g., 'USA', 'FRA')."
      ))
    }
    
    # Validate year column type
    if (!is.numeric(data[[year_col]])) {
      cli::cli_abort(c(
        "x" = "The input column {.field {year_col}} you supplied as {.var year_col} does not appear to be numeric.",
        "i" = "Year values must be numeric (e.g., 2010, 2015)."
      ))
    }
    
    # Validate country_id_format
    valid_types <- c("name_en", "iso3c")
    if (!country_id_format %in% valid_types) {
      cli::cli_abort(c(
        "x" = "Invalid value supplied for {.arg country_id_format}: '{country_id_format}'.",
        "i" = "Accepted values are {.val name_en} or {.val iso3c}."
      ))
    }

  # get dataframe with country corrections
  corrections_df <- artis::build_std_countries_tbl()

  # Set up join by naming to match input data column names to the standardization column names
  # by_cols <- setNames(c("iso3c", "year"), c(country_col, year_col))
    
  # Join input data to standardization data frame based on country_id_format
    if (country_id_format == "name_en") {
      
      # Need full corrections data frame to correct by country name
      corrections_df_name <- corrections_df

      # set up join by naming to match input data column names to the standardization column names
      by_cols <- stats::setNames(c("country_name", "year"), c(country_col, year_col))
      
      std_df <- data %>%
        # remove any trailing parenthetical phrase from string values
        dplyr::mutate(
          !!country_col := stringr::str_trim(
            stringr::str_remove(.data[[country_col]], "\\(.+\\)$"))) %>%
        #Join to ARTIS corrections table
        dplyr::left_join(
          corrections_df_name, by = by_cols) %>%
        # Flag countries that ARTIS corrections table did not correct (NA values in artis_* columns)
        dplyr::mutate(
          flag = dplyr::case_when(base::is.na(artis_iso3c) ~ TRUE,
                                              .default = FALSE)) %>% #,
              # If missing country name value (i.e. not corrected by ARTIS corrections join) add std country name via country code from original/supplied country name
          # artis_country_name = dplyr::case_when(
            # base::is.na(artis_country_name) ~ countrycode::countrycode(.data[[country_col]],
            #                                               origin = "country.name",
            #                                               destination = "country.name",
            #                                               warn = FALSE),
                                              # .default = artis_country_name)) %>%
        # Remove leftover corrections_df column
        dplyr::select(-iso3c)

        # Join to ARTIS corrections table
        # dplyr::left_join(
        #   corrections_df, by = by_cols) %>%
        # # Standardize countries that ARTIS corrections table did not correct (NA values in artis_* columns)
        # # pull values from given country column
        # dplyr::mutate(
        #   flag = dplyr::case_when(base::is.na(artis_iso3c) ~ TRUE,
        #                                       .default = FALSE)) %>% #,
        #       # If missing country name value (i.e. not corrected by ARTIS corrections join) add std country name via country code from original/supplied country name
        #   # artis_country_name = dplyr::case_when(
        #     # base::is.na(artis_country_name) ~ countrycode::countrycode(.data[[country_col]],
        #     #                                               origin = "country.name",
        #     #                                               destination = "country.name",
        #     #                                               warn = FALSE),
        #                                       # .default = artis_country_name)) %>%
        # # Remove leftover corrections_df column
        # dplyr::select(-iso3c)
    
      # Problem: Correcting some of the input territory names that don't have 
      # in house ARTIS matches get corrected via countrycode, and
      # countrycode corrects to the territory iso3c instead of the sovereign.
      # we have flagged these rows to rejoin corrected countrycode iso3c to
      # artis corrections to receive their sovereign variants.
      
      # Sets the names (i.e., the input column variable names) of the corrections_df
      # column names to be joined (see ?setNames()) 
      by_cols <- stats::setNames(
        c("iso3c", "year"), 
        c("artis_iso3c", year_col))
      
      # Rejoin flagged data and df_corrections by iso3c and year
      # flagged_data <- std_df %>%
      #   dplyr::filter(flag == TRUE) %>%
      #   dplyr::mutate(
      #     artis_iso3c = dplyr::case_when(
      #       base::is.na(artis_iso3c) ~ countrycode::countrycode(
      #         .data[[country_col]],
      #         origin = "country.name",
      #         destination = "iso3c",
      #         warn = FALSE
      #       ),
      #       .default = artis_iso3c
      #     )
      #   ) %>% 
      #   dplyr::left_join(corrections_df %>%
      #                      select(-artis_country_name), by = by_cols) %>%
      #   dplyr::select(-artis_country_name, -country_name) %>%
      #   select(-artis_iso3c) %>%
      #   rename(artis_iso3c = artis_iso3c.y) %>%
      #   mutate(artis_country_name = countrycode(artis_iso3c,
      #                                           origin = "iso3c",
      #                                           destination = "country.name")) %>%
      #   select(-flag)
      
      # Two scenarios of flags: territory that needs to be corrected to sovereign
      # 2. Sovereign that needs sovereign naming
      flagged_data <- std_df %>%
        dplyr::filter(flag == TRUE) %>%
        dplyr::mutate(
          artis_iso3c = dplyr::case_when(
            base::is.na(artis_iso3c) ~ countrycode::countrycode(
              .data[[country_col]],
              origin = "country.name",
              destination = "iso3c",
              warn = FALSE
            ),
            .default = artis_iso3c
          )
        ) %>% 
        # Convert territory to sovereign (will fix sovereign to sovereign later)
        dplyr::left_join(corrections_df_name %>%
                           select(-artis_country_name), by = by_cols) %>%
        dplyr::select(-artis_country_name, -country_name) %>%
        # Update artis_iso3c values in two ways:
        # When both artis_iso3c and artis_iso3c.y, prioritize & keep artis_iso3c.y value,
        # otherwise fill artis_iso3c.y NAs with artis_iso3c value
        # (i.e. correct artis_iso3c column through our artis corrections dataframe to
        # correct territories to sovereign)
        dplyr::mutate(
          artis_iso3c = dplyr::coalesce(artis_iso3c.y, artis_iso3c)
        ) %>%
        dplyr::select(-artis_iso3c.y) %>%
        dplyr::mutate(
          artis_country_name = countrycode::countrycode(
            artis_iso3c, origin = "iso3c", destination = "country.name")
        )
        # select(-artis_iso3c) %>%
        # rename(artis_iso3c = artis_iso3c.y)
      
      nonflagged_data <- std_df %>%
        dplyr::filter(flag == FALSE) %>%
        select(-flag)
      
      std_df <- dplyr::bind_rows(flagged_data, nonflagged_data) %>%
        dplyr::select(-flag)
      
      # Get vector of country names that weren't standardized (i.e. have NA values)
      not_std_vec <- std_df %>%
        dplyr::filter(base::is.na(artis_country_name)) %>%
        tidyr::drop_na(country_col) %>%
        dplyr::select(country_col) %>%
        dplyr::distinct() %>%
        dplyr::pull(country_col)
      
    } else if (country_id_format == "iso3c") {
      
      # filter out duplicate entires (i.e., multiple country names matched to the same input iso3c - we don't need these input country names since we will get an output country name)
      corrections_df_iso3c <- corrections_df %>%
        dplyr::select(-country_name) %>%
        dplyr::distinct() %>%
        tidyr::drop_na()
      
      # set up join by naming to match input data column names to the standardization column names
      by_cols <- stats::setNames(c("iso3c", "year"), c(country_col, year_col))

      # Join input data to standardization data frame based on iso3c
      std_df <- data %>% 
        # remove any trailing parenthetical phrase from string values
        # Join to ARTIS corrections table
        dplyr::left_join(corrections_df_iso3c, by = by_cols) %>% 
        # Standardize countries that ARTIS corrections table did not correct (NA values in artis_* columns)
        # Probably not common to need this, but will output an NA when countrycode() does not register. 
        dplyr::mutate(artis_iso3c = dplyr::case_when(
          is.na(artis_iso3c) ~ countrycode::countrycode(.data[[country_col]],
                                                      origin = "iso3c",
                                                      destination = "iso3c",
                                                      warn = FALSE),
                                       .default = artis_iso3c)) %>%
        dplyr::mutate(artis_country_name = dplyr::case_when(
          # FIXIT: take a look at to see whether we should apply this to all names
          base::is.na(artis_country_name) ~ countrycode::countrycode(.data[[country_col]], 
                                                            origin = "iso3c",
                                                            destination = 'country.name',
                                                            warn = FALSE),
                                              .default = artis_country_name))
      
      # Get vector of country names that weren't standardized (i.e. have NA values)
      not_std_vec <- std_df %>%
        dplyr::filter(base::is.na(artis_iso3c)) %>%
        tidyr::drop_na(country_col) %>%
        dplyr::select(country_col) %>%
        dplyr::distinct() %>%
        dplyr::pull(country_col)
    }
  
  
  # NA/missing values warning
  if (na_count > 0 | missing_count > 0) {
    cli::cli_alert_warning(c("Column {.field {country_col}} contains {.val {missing_count}} missing string value{?s} and {.val {na_count}} {.val NA} value{?s}."))
    cli::cli_alert_info("These values will be turned into {.val NA}s in the output correction columns {.field artis_country_name} and {.field artis_iso3c}.")
    # cli::cli_alert_danger("Found {missing_count} missing string values and {na_count} NA values in column {.field {country_col}}.")
    # cli::cli_alert_info("These values will not be standardized.")
  }
  
  # Print a blank line to separate warnings
  cli::cli_text("")
  
  # list of country names that did not successfully get assigned iso3c codes
  if (length(not_std_vec) > 0) {
    visible_list <- sapply(na.omit(not_std_vec), function(x) if (x == "") dQuote("") else dQuote(x))
    
    cli::cli_alert_warning("Some values in user column {.field {country_col}} were not standardized.")
    cli::cli_alert_info("These values were not in the ARTIS corrections table or found by {.pkg countrycode}:\n{.val {paste(visible_list, collapse = ', ')}}")
    
    # Print the unstandardized list on its own line with attention emoji
    # cli::cli_alert_info("Unstandardized values: ")
  }
  
  # Return invisibly so assignment doesn’t double-print
  return(std_df)

}
