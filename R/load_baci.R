#' Load and standardize BACI bilateral trade data
#'
#' @param baci_data Data frame.
#' @param hs_codes Character vector.
#' @param baci_country_codes Data frame.
#' @return Data frame with columns exporter_iso3c, importer_iso3c, hs6,
#'   total_q, total_v, and country name and numeric code fields.
#' @importFrom dplyr filter rename select full_join group_by summarise ungroup
#'   mutate
#' @export
load_baci <- function(baci_data, hs_codes, baci_country_codes){
  
  df <- baci_data %>%
    rename(hs6 = k) %>% 
    # Filter to hs codes included in analysis
    filter(hs6 %in% hs_codes) %>% 
    # Match Comtrade country codes to ISO3c codes
    full_join(baci_country_codes, by = c("i" = "country_code")) %>%
    select(t, exporter_iso3c = country_iso3, exporter_iso3n = i,
           exporter_country = country_name, j, hs6, v, q) %>%
    full_join(baci_country_codes, by = c("j" = "country_code")) %>%
    select(t, hs6, exporter_iso3c, exporter_iso3n, exporter_country,
           importer_iso3c = country_iso3, importer_iso3n = j,
           importer_country = country_name, v, q) %>%
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
