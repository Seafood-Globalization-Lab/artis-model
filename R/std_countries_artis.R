#' Title
#'
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
std_countries_artis <- function(the_data, data_type = c("FAO", "BACI", "SAU")) {
  
  # data type must be one of FAO, BACI, or SAU. Return an error if the user
  # inputs something outside these three data sources
  data_type <- match.arg(data_type)
  
  if (data_type == "FAO") {
    
    # 1) Correct by iso3
    fao_iso3c <- artis::standardize_countries(
      data = the_data,
      country_id_type = "iso3c",
      country_col_name = "country_iso3_alpha",
      year_col_name = "year"
    )
    
    # 2) filter out NA values produced
    fao_no_na <- fao_iso3c %>%
      filter(!is.na(country_iso3_alpha))
    
    # 3) Filter to NA values - rerun standardization on country name column
    fao_country_name <- fao_iso3c %>%
      filter(is.na(country_iso3_alpha)) %>%
      select(country_iso3_alpha, country_name_en, year) %>%
      artis::standardize_countries(country_id_type = "name_en",
                                   country_col_name = "country_name_en",
                                   year_col_name = "year")
    
    # 4) bind iso3c corrections and country name corrections
    the_std_data <- bind_rows(fao_no_na, fao_country_name)
    
  } else if (data_type == "BACI") {
    
    # Step A. Correct by iso3c.
    baci_iso3c <-
      # 1. standardize exports
      artis::standardize_countries(
        data = the_data,
        country_id_type = "iso3c",
        country_col_name = "exporter_iso3c",
        year_col_name = "year"
      ) %>%
      select(
        exporter_iso3c = artis_iso3c,
        exporter_country,
        # Keep original raw country name in data - don't overwrite with ARTIS countryname correction
        importer_iso3c, 
        # Keep original raw country name in data - don't overwrite with ARTIS countryname correction
        importer_country,
        year
        ) %>%
      # 2. standardize imports
      artis::standardize_countries(
        # Next run through importer iso3c corrections
        country_id_type = "iso3c",
        country_col_name = "importer_iso3c",
        year_col_name = "year"
      ) %>%
      select(exporter_iso3c,
             exporter_country,
             importer_iso3c = artis_iso3c,
             importer_country,
             year)
    
    # Problem: NA's are produced, which will be solved in steps B and C. We remove NA's in this dataset.
    # Obtain data that contains non NA's that standardize on the first go
    baci_no_na <- baci_iso3c %>%
      filter(
        !is.na(exporter_iso3c),
        !is.na(importer_iso3c)
        ) %>%
      select(exporter_iso3c, importer_iso3c, year)
    
    # Step B. create standardized table that only includes NA values from iso3c corrections 
    # so we can use that output to recorrect by countryname in C, producing non NA iso3c values.
    data_for_na_std <- base_df %>%
      filter(is.na(exporter_iso3c) | is.na(importer_iso3c))
    
    # Step C. use standardized table to recorrect by countryname, converting NA iso3c values to their countryname assigned non-NA values.
    baci_std_name <- data_for_na_std %>%
      # 1. standardize exports
      # Gets rid of NA iso3c values in exporter_iso3c column
      artis::standardize_countries(
        country_id_type = "name_en",
        country_col_name = "exporter_country",
        year_col_name = "year"
      ) %>%
      # we overwrite exporter_iso3c to artis_iso3c
      select(
        exporter_iso3c = artis_iso3c,
        # Keep as it's needed for the next correction
        importer_iso3c, 
        # Keep as it's needed for the next correction
        importer_country,
        year
        ) %>%
      # 2. standardize exports
      # Next run through importer country name corrections
      artis::standardize_countries(
        country_id_type = "name_en",
        country_col_name = "importer_country",
        year_col_name = "year"
      ) %>%
      select(
        exporter_iso3c,
        importer_iso3c = artis_iso3c,
        year
        )
    
    # Bind datasets from step A and step C (i.e., corrections by iso3c & recorrections by countryname)
    the_std_data <- bind_rows(
      baci_no_na, # Rows that had no problems standardizing
      baci_std_name # Rows that needed standardization by countryname
    ) %>%
      # Remove circular tradeflows (e.g., India exports to India importing country)
      filter(exporter_iso3c != importer_iso3c) 
    # Also need to add a group_by() and summarize
    
    ## End of workflow
  } else if (data_type == "SAU") {
    
    #### Correct by name - only name in SAU data
    the_std_data <- artis::standardize_countries(
      data = the_data,
      country_id_type = "name_en",
      country_col_name = "country_name_en",
      year_col_name = "year"
    )
    
  }
  
  return(the_std_data)
  
}
