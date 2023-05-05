#' @importFrom dplyr filter
#' @export
load_baci <- function(baci_data, hs_codes, baci_country_codes){
  
  df <- baci_data %>%
    rename(hs6 = k) %>% 
    # Filter to hs codes included in analysis
    filter(hs6 %in% hs_codes) %>% 
    # Match Comtrade country codes to ISO3c codes
    full_join(baci_country_codes, by = c("i" = "country_code")) %>%
    select(t, exporter_iso3c = iso_3digit_alpha, exporter_iso3n = i,
           exporter_country = country_name_abbreviation, j, hs6, v, q) %>%
    full_join(baci_country_codes, by = c("j" = "country_code")) %>%
    select(t, hs6, exporter_iso3c, exporter_iso3n, exporter_country,
           importer_iso3c = iso_3digit_alpha, importer_iso3n = j,
           importer_country = country_name_abbreviation, v, q) %>%
    filter(!is.na(t))
  
  # Add leading "0" back to those under length of 6
  df$hs6 <- formatC(as.numeric(df$hs6), width = 6, format='d', flag='0')
  
  df <- df %>% 
    filter(!is.na(q)) %>%
    group_by(exporter_iso3c, exporter_iso3n, exporter_country, importer_iso3c,
             importer_iso3n, importer_country, hs6) %>%
    summarise(total_q = sum(q),
              total_v = sum(v)) %>%
    ungroup() %>%
    mutate(exporter_iso3c = as.character(exporter_iso3c),
           importer_iso3c = as.character(importer_iso3c),
           exporter_country = as.character(exporter_country),
           importer_country = as.character(importer_country))
  
  return(df)
}
