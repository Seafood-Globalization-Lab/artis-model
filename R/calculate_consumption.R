#' Calculate Consumption
#' This function estimates seafood consumption by country using trade, production, 
#' and processing data. It accounts for domestic and foreign consumption by integrating 
#' trade flow data and conversion factors for processed seafood products.
#'
#' @param artis dataframe. Trade flows from ARTIS, including exports and imports.
#' @param prod dataframe. Seafood production data by country, species, and method.
#' @param curr_year Numeric. the year for which consumption is being calculated.
#' @param curr_hs_version Character. the HS code version used for trade classifications.
#' @param W_long dataframe. containing product reallocation factors for processing seafood products.
#' @param reweight_W_long dataframe. containing reweighted product reallocation factors for processing seafood products.
#' @param X_long dataframe. mapping production species to HS6 codes based on available trade data.
#' @param V1_long dataframe. mapping species to HS6 codes using an alternative approach.
#' @param pop dataframe. containing country population data for per capita consumption calculations.
#' @param code_max_resolved 
#' @param max_percap_consumption Numeric. maximum allowable per capita consumption in kg (default 100).
#' @param consumption_threshold Numeric. minimum threshold for recorded consumption to avoid rounding errors (default 1e-9).
#' @param dev_mode Logical. if TRUE, enables debugging output and writes out discrepancies in consumption estimates.
#'
#' @return A data frame with estimated seafood consumption by country, species, and end use. 
#' The output includes both raw consumption estimates and a capped version adjusted for per capita consumption limits.
#'
#' @export
#'
#' @examples

calculate_consumption <- function(artis = s_net, 
                                  prod = prod_data_analysis_year,
                                  curr_year = analysis_year, 
                                  curr_hs_version = curr_hs,
                                  W_long = W_long, 
                                  reweight_W_long = reweight_W_long,
                                  X_long = X_long, 
                                  V1_long = V1_long, 
                                  V2_long = V2_long,
                                  pop = pop, 
                                  code_max_resolved = code_max_resolved,
                                  max_percap_consumption = 100,
                                  consumption_threshold = 1e-9,
                                  dev_mode = FALSE){
  # FIXIT: add all needed arguments to function (so objects are explicitly called)
  # set of arguments then need updated in main script
  
  
  
  # Format data to match for joins----------------------------------------------
  artis <- artis %>%
    filter(!is.na(live_weight_t)) %>%
    mutate(hs6 = as.numeric(hs6))
  
  V1_long <- V1_long %>%
    mutate(hs6 = as.numeric(hs6)) 
  
  V2_long <- V2_long %>%
    rename(hs6_original = from_hs6,
           hs6_processed = to_hs6,
           processing_cf = product_cf) %>%
    mutate(hs6_original = as.numeric(hs6_original),
           hs6_processed = as.numeric(hs6_processed)) 
  
  X_long <- X_long %>%
    mutate(hs6 = as.numeric(hs6))
  
  W_long <- W_long %>%
    mutate(hs6_original = as.numeric(hs6_original),
           hs6_processed = as.numeric(hs6_processed))
  
  prod <- prod %>%
    rename(sciname = SciName,
           method = prod_method,
           live_weight_t = quantity) %>%
    #FIXIT: This should happen earlier in data cleaning script
    group_by(country_iso3_alpha, sciname, habitat, method) %>%
    summarise(live_weight_t = sum(live_weight_t))
  
  # Calculate Domestic Consumption----------------------------------------------
  # calculating all products exported by countries
  country_exports <- artis %>%
    mutate(hs6 = as.numeric(hs6)) %>%
    group_by(exporter_iso3c, hs6) %>%
    summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  # Note this means  all production under the error is now human consumable
  error_code <- 999999
  
  # change X long to account for when a country does not export a certain code
  X_long <- X_long %>%
    left_join(
      country_exports,
      by = c("iso3c"="exporter_iso3c", "hs6")
    ) %>%
    # if a country does not export the code in a given year, 
    # then change the code to placeholder error code
    mutate(
      hs6 = case_when(
        is.na(live_weight_t) ~ error_code,
        TRUE ~ hs6
      )
    ) %>%
    select(-live_weight_t) %>%
    # re-summarize estimated X to account for multiple codes going to error code
    group_by(iso3c, hs6, sciname, habitat, method) %>%
    summarize(estimated_X = sum(estimated_X)) %>%
    ungroup()
  
  # domestic consumption by hs6 code
  x_p <- prod %>%
    rename(iso3c = country_iso3_alpha) %>%
    # join in proportions to divide species resolution into hs6 products
    left_join(
      X_long,
      by = c("iso3c", "sciname", "habitat", "method")
    ) %>%
    # disaggregate species production to hs6 products
    mutate(production_live_t = live_weight_t * estimated_X) %>%
    group_by(iso3c, hs6, sciname, habitat, method) %>%
    summarize(production_live_t = sum(production_live_t, na.rm = TRUE)) %>%
    ungroup()
  
  # domestic exports by hs6 and species
  exports_domestic <- artis %>%
    filter(dom_source == "domestic") %>%
    group_by(source_country_iso3c, hs6, sciname, habitat, method) %>%
    summarize(domestic_export_live_t = sum(live_weight_t, na.rm = TRUE),
              domestic_export_product_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  # domestic consumption = domestic production - domestic exports
  consumption_domestic <- x_p %>%
    full_join(
      exports_domestic %>%
        mutate(hs6 = as.numeric(hs6)),
      by = c("iso3c"="source_country_iso3c", "hs6", "sciname", "habitat", "method")
    ) %>%
    # deals with cases where there is production but no exports
    replace_na(list(domestic_export_live_t = 0,
                    domestic_export_product_t = 0)) %>%
    mutate(consumption_live_t = production_live_t - domestic_export_live_t) 
  
  # Domestic export = production - domestic consumption
  # This is the total amount of product available for foreign consumption
  domestic_export <- consumption_domestic %>%
    select(source_country_iso3c = iso3c, hs6, sciname, habitat, method,
           # These columns below represent that amount of production that is
           # consumed via foreign consumption by other countries
           domestic_export_product_t, domestic_export_live_t) %>%
    filter(domestic_export_live_t > consumption_threshold)
  
  # DATA CHECK: domestic exports should not exceed domestic production
  domestic_consumption_threshold <- -1e-3
  data_check_domestic <- consumption_domestic %>%
    filter(consumption_live_t < domestic_consumption_threshold)
  if (nrow(data_check_domestic) > 0) {
    warning(paste0("Domestic exports EXCEED domestic production", 
                   " for ", curr_year, " and ", curr_hs_version,
                   ", min difference between production and domestic export is ",
                   min(data_check_domestic$consumption_live_t)))
  }
    
  # DATA CHECK: Domestic export product group by exporter and hs6 == artis filtered to domestic exporters and hs6
  artis_domestic_check <- artis %>%
    filter(dom_source == "domestic") %>%
    group_by(exporter_iso3c, hs6) %>%
    summarize(artis_product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    full_join(
      consumption_domestic %>%
        group_by(iso3c, hs6) %>%
        summarize(domestic_export_product_t = sum(domestic_export_product_t, 
                                                  na.rm = TRUE)) %>%
        ungroup(),
      by = c("exporter_iso3c"="iso3c", "hs6")
    ) %>%
    mutate(difference = artis_product_weight_t - domestic_export_product_t) %>%
    filter(abs(difference) > 1e-3) %>%
    arrange(desc(difference)) 
  
  
  if (nrow(artis_domestic_check) > 0) {
    warning("Domestic consumption estimates of domestic 
          exports DOES NOT MATCH ARTIS domestic exports")
  }
    
  # Calculating Foreign Consumption---------------------------------------------
  # Calculate proportion of imports retained
  # proportion of original hs6 that was retained in country from imports =
  # consumption of that original hs6 divided by the imports of the original hs6
  
  exports_foreign <- artis %>%
    filter(dom_source == "foreign") %>%     
    group_by(exporter_iso3c, hs6) %>%
    summarize(foreign_export_product_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  imports <- artis %>%
    # calculating all imports by hs6 code
    group_by(importer_iso3c, hs6) %>%
    summarize(import_product_t = sum(product_weight_t, na.rm = TRUE),
              import_live_t = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  # convert final exported product code into its original product code equivalent
  unprocessed_consumption <- exports_foreign %>%
    # processing imports into the product form that is available for consumption 
    # or re-export
    left_join(
      reweight_W_long %>%
        mutate(hs6_processed = as.numeric(hs6_processed),
               hs6_original = as.numeric(hs6_original)),
      by = c("exporter_iso3c", "hs6"= "hs6_processed")
    ) %>% 
    mutate(unprocessed_product_t = 
             foreign_export_product_t * reweighted_W * unprocessing_cf) %>%
    # proportion of hs6 processed that can be disagregated into hs6 original
    # will be used to disagregate foreign consumption later on
    group_by(exporter_iso3c, hs6_original) %>%
    summarize(hs6_original_product_t = sum(unprocessed_product_t)) %>%
    ungroup() %>%
    # changing to numeric for join
    mutate(hs6_original = as.numeric(hs6_original)) %>%
    # get imports by original hs6 form
    full_join(
      imports %>%
        select(importer_iso3c, hs6, import_product_t),
      by = c("exporter_iso3c"="importer_iso3c",
             "hs6_original"="hs6")
    ) %>%
    # NAs represent flows where there were imports however no re-exports
    # all product imported in this case was consumed
    replace_na(list(hs6_original_product_t = 0)) %>%
    # calculate foreign consumption of original hs6 form
    mutate(foreign_consumption_original_product_t = import_product_t - hs6_original_product_t) %>%
    mutate(foreign_consumption_original_product_t = case_when(
      # FIXIT: we will want to add a check for magnitude of negative flows (currently fairly small)
      foreign_consumption_original_product_t < 1e-3 ~ 0,
      TRUE ~ foreign_consumption_original_product_t
    )) %>%
    filter(foreign_consumption_original_product_t >= 0) %>%
    # proportion of imports that were retained for unprocessed consumption
    mutate(prop_import_retained_product_t =
             foreign_consumption_original_product_t / import_product_t) %>%
    select(exporter_iso3c, hs6_original, prop_import_retained_product_t)
  
  # use ARTIS flows and retention by code to calculate stage 1 consumption 
  # of foreign products
  consumption_export_1 <- artis %>%
    filter(dom_source %in% c("domestic", "error")) %>%
    left_join(unprocessed_consumption %>%
                select(exporter_iso3c, hs6_original, prop_import_retained_product_t),
              by = c("importer_iso3c" = "exporter_iso3c", "hs6" = "hs6_original")) %>%
    mutate(consumption_product_t = product_weight_t*prop_import_retained_product_t,
           consumption_live_t = live_weight_t*prop_import_retained_product_t,
           reexport_product_weight_t = product_weight_t*(1-prop_import_retained_product_t),
           reexport_live_weight_t = live_weight_t*(1-prop_import_retained_product_t)) 
  
  # calculate proportion of each importer's (consumer at this stage) product by end_use
  # coming from each exporter
  foreign_import_props <- artis %>%
    filter(dom_source == "foreign") %>%
    mutate(end_use = case_when(
      hs6 == 230120 ~ "fishmeal",
      hs6 %in% c(30110, 30111, 30119) ~ "other",
      TRUE ~ "direct human consumption")) %>%
    # importer will now be the consumer at this step, hs6 is hs6_processed
    group_by(exporter_iso3c, importer_iso3c, end_use) %>%
    summarise(product_weight_t = sum(product_weight_t),
              live_weight_t = sum(live_weight_t)) %>%
    group_by(exporter_iso3c, end_use) %>%
    mutate(total_product = sum(product_weight_t), 
           total_live = sum(live_weight_t)) %>%
    group_by(exporter_iso3c, importer_iso3c, end_use) %>%
    mutate(import_prop_product = product_weight_t/total_product,
           import_prop_live = live_weight_t/total_live)
  
  # take calculated re-exports and distribute them to final consumer for stage 2
  consumption_export_2 <- consumption_export_1 %>%
    # remove exporter_iso3c, which is now the original exporter
    select(-exporter_iso3c) %>%
    # rename the importer as it is not the re-exporter
    rename("exporter_iso3c" = "importer_iso3c", "hs6_original" = "hs6") %>%
    group_by(year, hs_version, source_country_iso3c, exporter_iso3c,
             hs6_original, sciname, habitat, method) %>%
    summarise(reexport_product_weight_t = sum(reexport_product_weight_t), 
              reexport_live_weight_t = sum(reexport_live_weight_t)) %>%
    filter(reexport_product_weight_t > consumption_threshold) %>%
    # join W_long to convert original product code to final product code 
    # for each exporter
    left_join(W_long, 
              by = c("exporter_iso3c", "hs6_original"),
              relationship = "many-to-many") %>% 
    mutate(reexport_product_weight_t = reexport_product_weight_t*estimated_W,
           reexport_live_weight_t = reexport_live_weight_t*estimated_W) %>%
    mutate(end_use = case_when(
      hs6_processed == 230120 ~ "fishmeal",
      hs6_processed %in% c(30110, 30111, 30119) ~ "other",
      TRUE ~ "direct human consumption")) %>% 
    group_by(year, hs_version, source_country_iso3c, exporter_iso3c, end_use,
             sciname, habitat, method) %>% 
    summarise(reexport_product_weight_t = sum(reexport_product_weight_t),
              reexport_live_weight_t = sum(reexport_live_weight_t)) %>%
    left_join(foreign_import_props,
              by = c("exporter_iso3c",  "end_use")) %>%
    mutate(consumption_product_t = reexport_product_weight_t*import_prop_product,
           consumption_live_t = reexport_live_weight_t*import_prop_live) %>%
    # There are a bunch of NAs in this join
    group_by(year, hs_version, source_country_iso3c, exporter_iso3c, 
             importer_iso3c,  end_use, 
             sciname, habitat, method) %>%
    summarise(consumption_product_t = sum(consumption_product_t, na.rm = TRUE),
              consumption_live_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup()

    
  # Format and join all consumption-----------------------------------------------
  # clean each consumption file and join all consumption sources
  consumption_domestic <- consumption_domestic %>%
    mutate(year = curr_year, hs_version = curr_hs, 
           source_country_iso3c = iso3c, exporter_iso3c = NA,
           consumption_source = "domestic") %>%
    select(year, hs_version, source_country_iso3c, exporter_iso3c,
           "consumer_iso3c" = "iso3c", consumption_source, hs6, 
           sciname, habitat, method, consumption_live_t) %>%
    filter(consumption_live_t > 0.001)
  
  consumption_export_1 <- consumption_export_1 %>% 
    mutate(consumption_source = "foreign step 1") %>%
    select(year, hs_version, source_country_iso3c, exporter_iso3c, 
           "consumer_iso3c" = "importer_iso3c", 
           consumption_source, hs6, sciname, habitat, method, consumption_live_t) %>% 
    filter(consumption_live_t > 0.001)
  
  consumption_export_2 <- consumption_export_2 %>%
    mutate(consumption_source = "foreign step 2") %>%
    select(year, hs_version, source_country_iso3c, exporter_iso3c, 
           "consumer_iso3c" = "importer_iso3c",  consumption_source, end_use, 
           sciname, habitat, method, consumption_live_t) %>%
    filter(consumption_live_t > 0.001)
  
  
  complete_consumption <- consumption_domestic %>%
    bind_rows(consumption_export_1) %>%
    bind_rows(consumption_export_2)
  
  complete_consumption <- complete_consumption %>%
    mutate(end_use = case_when(
      hs6 == 230120 ~ "fishmeal",
      hs6 %in% c(30110, 30111, 30119) ~ "other",
      TRUE ~ "direct human consumption")) %>%
    group_by(year, hs_version, 
             source_country_iso3c, exporter_iso3c, consumer_iso3c,  
             consumption_source, sciname, habitat, method,
             end_use) %>%
    summarise(consumption_live_t = sum(consumption_live_t)) %>%
    ungroup()
  
  # FIXIT: need to decide how to formalize this test (or what information to 
  # write out related to it)
  # test total consumption compared to production by source country
  test <- complete_consumption %>%
    group_by(source_country_iso3c, sciname, habitat, method) %>% 
    summarise(consumption_live_t_sum = sum(consumption_live_t, na.rm = TRUE)) %>% 
    left_join(prod %>% 
                group_by(country_iso3_alpha, sciname, habitat, method) %>% 
                summarise(live_weight_t = sum(live_weight_t)), 
              by = c("source_country_iso3c" = "country_iso3_alpha", "sciname", "habitat", "method")) %>% 
    mutate(diff = consumption_live_t_sum - live_weight_t)
  
  # if dev_mode enabled - filter and write out large consumption negatives to csv
  if (dev_mode){
    
    diff_large <- complete_consumption %>%
      group_by(source_country_iso3c, sciname, habitat, method) %>% 
      summarise(consumption_live_t_sum = sum(consumption_live_t, na.rm = TRUE)) %>% 
      left_join(prod %>% 
                  group_by(country_iso3_alpha, sciname, habitat, method) %>% 
                  summarise(live_weight_t = sum(live_weight_t)), 
                by = c("source_country_iso3c" = "country_iso3_alpha", 
                       "sciname", 
                       "habitat", 
                       "method")) %>% 
      mutate(diff = consumption_live_t_sum - live_weight_t) %>% 
      filter(abs(diff) > 10)
    
    fwrite(diff_large, 
           file.path("./outputs", 
                     paste0("consumption_large_diffs_",Sys.Date(),".csv")))
           }

  # DATA CHECK
  # make sure there are no NA values in consumption
  na_consumption <- complete_consumption %>%
    filter(is.na(consumption_live_t))
  if (nrow(na_consumption) != 0) {
    warning(paste0(
            "NAs in complete consumption for", curr_year, " and ", curr_hs_version,
            ". Number of NAs is ", nrow(na_consumption)))
  }
  
  if (min(complete_consumption$consumption_live_t) < 0) {
    warning(paste0("Negative consumption values for ", curr_year,
      " and ", curr_hs_version))
  }
  

# Max per capita ----------------------------------------------------------
# add max per capita (default 100 kg) REMINDER THIS IS IN KG
# keep both raw consumption and max percapita scaled consumption

  if (!is.na(max_percap_consumption)) {
    
    # calculating consumption per capita
    consumption_per_capita <- complete_consumption %>%
      filter(end_use == "direct human consumption") %>% 
      group_by(consumer_iso3c, end_use) %>%
      summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE)) %>%
      ungroup() %>%
      left_join(
        pop %>% 
          filter(year == curr_year), # introduced filter by year to remove many-to-many join
        by = c("consumer_iso3c"="iso3c")
      ) %>%
      mutate(consumption_percap_t = consumption_live_t / pop) %>%
      mutate(consumption_percap_kg = 1000 * consumption_percap_t)
    
    percap_outliers <- consumption_per_capita %>%
      filter(consumption_percap_kg > max_percap_consumption) %>%
      mutate(corrected_consumption_live_t = (pop * max_percap_consumption) / 1000)
    
    consumption_outliers <- complete_consumption %>%
      filter(consumer_iso3c %in% unique(percap_outliers$consumer_iso3c)) %>%
      group_by(consumer_iso3c) %>%
      mutate(total = sum(consumption_live_t, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(prop = consumption_live_t / total) %>%
      left_join(
        percap_outliers %>%
          select(consumer_iso3c, corrected_consumption_live_t),
        by = c("consumer_iso3c")
      ) %>%
      mutate(consumption_live_t_capped = prop * corrected_consumption_live_t) %>%
      select(-c(total, corrected_consumption_live_t, prop))
    
    complete_consumption_capped <- complete_consumption %>%
      mutate(consumption_live_t_capped = consumption_live_t) %>% 
      filter(!(consumer_iso3c %in% unique(percap_outliers$consumer_iso3c) & 
                 end_use == "direct human consumption")) %>%
      bind_rows(consumption_outliers) %>% 
      left_join(pop %>% 
                  filter(year == curr_year), # introduced filter by year to remove many-to-many join
                by = c("consumer_iso3c"="iso3c",
                       "year")) %>%
      mutate(consumption_percap_live_kg = case_when(end_use == "direct human consumption" ~ 
                                                 1000 * consumption_live_t / pop,
                                               TRUE ~ NA),
             consumption_percap_live_kg_capped = case_when(end_use == "direct human consumption" ~
                                                        1000 * consumption_live_t_capped / pop,
                                                      TRUE ~ NA)) %>% 
      select(-pop)
    
    return(complete_consumption_capped) 
    
  } else(
    return(complete_consumption)
  )
  
}