#' @importFrom countrycode countrycode
#' @export
standardize_countries <- function(df, data_source = NA) {
  
  standard_df <- data.frame()
  
  if (data_source == "FAO") {
    
    standard_df <- standardize_prod(df,
                                    "country_iso3_alpha",
                                    "country_name_en") %>%
      select(-c(country_iso3_alpha, country_name_en)) %>%
      rename(country_iso3_alpha = artis_iso3c, country_name_en = artis_country_name) %>%
      mutate(country_iso3_numeric = countrycode(country_iso3_alpha, "iso3c", "iso3n")) %>%
      group_by(country_iso3_alpha, SciName, CommonName, taxa_source, year, Species01, Genus01, Family01, Other01, habitat, prod_method) %>%
      summarise(quantity = sum(quantity)) %>%
      ungroup() %>%
      # match the order of species in V_1
      arrange(SciName, country_iso3_alpha)
    
  } else if (data_source == "BACI") {
    
    standard_df <- standardize_baci(df, "exporter_iso3c", "exporter_country") %>%
      select(-c(exporter_iso3c, exporter_country)) %>%
      rename(exporter_iso3c = artis_iso3,
             exporter_country = artis_country_name) %>%
      standardize_baci("importer_iso3c", "importer_country") %>%
      select(-c(importer_iso3c, importer_country)) %>%
      rename(importer_iso3c = artis_iso3,
             importer_country = artis_country_name) %>%
      # Removing country names so that territories do not get confused with their sovereign nations
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
