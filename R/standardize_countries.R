#' @importFrom countrycode countrycode
#' @export
standardize_countries <- function(df, 
                                  data_source = NA,
                                  all_sau_cols = FALSE) {
  
  standard_df <- data.frame()
  
  if (data_source == "FAO") {
    # standardize country names
    standard_df <- standardize_prod(data = df, 
                                    col_iso3 = "country_iso3_alpha", 
                                    col_country_name = "country_name_en") %>%
      # remove original FAO country names and rename
      select(-c(country_iso3_alpha, country_name_en)) %>%
      rename(country_iso3_alpha = artis_iso3c, 
             country_name_en = artis_country_name) %>%
      mutate(country_iso3_numeric = countrycode(country_iso3_alpha, "iso3c", "iso3n")) %>%
      
      # group by all raw SAU data columns if all_sau_cols is TRUE
      { if (all_sau_cols == TRUE) {
        group_by(., country_iso3_alpha, SciName, CommonName, taxa_source, year, 
                 Species01, Genus01, Family01, Other01, habitat, prod_method, gear,
                 eez, sector, end_use)
        # otherwise if all_sau_cols argument is empty or explicitly FALSE do this:
      } else {
        group_by(., country_iso3_alpha, SciName, CommonName, taxa_source, year, 
                 Species01, Genus01, Family01, Other01, habitat, prod_method)
      }
      } %>% 

      summarise(quantity = sum(quantity)) %>%
      ungroup() %>%
      # match the order of species in V_1
      arrange(SciName, country_iso3_alpha)
    
  } else if (data_source == "BACI") {
    
      # standardize country names - exporter
    standard_df <- standardize_baci(data = df, 
                                    col_iso3 = "exporter_iso3c", 
                                    col_country_name = "exporter_country") %>%
      # remove original BACI country names and rename
      select(-c(exporter_iso3c, exporter_country)) %>%
      rename(exporter_iso3c = artis_iso3,
             exporter_country = artis_country_name) %>%
      # standardize country names - importer
      standardize_baci(col_iso3 = "importer_iso3c", 
                       col_country_name = "importer_country") %>%
      # remove original BACI country names and rename
      select(-c(importer_iso3c, importer_country)) %>%
      rename(importer_iso3c = artis_iso3,
             importer_country = artis_country_name) %>%
      # aggregate total_q and total_v to account for territories converted to sovereign states
      group_by(exporter_iso3c, importer_iso3c, hs6, year, hs_version) %>%
      summarize(total_q = sum(total_q, na.rm = TRUE),
                total_v = sum(total_v, na.rm = TRUE)) %>%
      ungroup() %>%
      # remove any circular flows because of country standardization
      filter(exporter_iso3c != importer_iso3c)
      
  } else {
    standard_df <- data.frame()
  }
  
  return(standard_df)
}
