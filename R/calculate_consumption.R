#' @export
calculate_consumption <- function(artis, prod, curr_year, curr_hs_version,
                                      W_long, X_long, pop,
                                      code_max_resolved,
                                      max_percap_consumption = 100,
                                      consumption_threshold = 1e-9) {
  
  # Formatting columns so that they match for joins
  artis <- artis %>%
    filter(!is.na(live_weight_t)) %>% 
    mutate(hs6 = as.numeric(hs6)) 

  
  W_long <- W_long %>%
    mutate(hs6_original = as.numeric(hs6_original),
           hs6_processed = as.numeric(hs6_processed))
  
  X_long <- X_long %>%
    mutate(hs6 = as.numeric(hs6))
  
  prod <- prod %>%
    rename(sciname = SciName,
           method = prod_method,
           live_weight_t = quantity)
  
  # Calculating Domestic Consumption----------------------------------------------
  
  # calculating all products exported by countries
  country_exports <- artis %>%
    mutate(hs6 = as.numeric(hs6)) %>%
    group_by(exporter_iso3c, hs6) %>%
    summarize(live_weight_t = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  # Note this will mean that all production under the error is now human consumable
  error_code <- 999999
  # change X long to account for when a country does not export a certain code
  # example COD with 230120
  X_long <- X_long %>%
    left_join(
      country_exports,
      by = c("iso3c"="exporter_iso3c", "hs6")
    ) %>%
    # if a country does not export the code in a given year, then change the code to placeholder error code
    mutate(
      hs6 = case_when(
        is.na(live_weight_t) ~ error_code,
        TRUE ~ hs6
      )
    ) %>%
    select(-live_weight_t) %>%
    # Re-summarize estimated X to account for multiple codes being turned into error code
    group_by(iso3c, hs6, sciname, habitat, method) %>%
    summarize(estimated_X = sum(estimated_X)) %>%
    ungroup()
  
  # Domestic consumption by hs6 code
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
  
  
  # Domestic exports by hs6 and species
  exports_domestic <- artis %>%
    filter(dom_source == "domestic") %>%
    group_by(source_country_iso3c, hs6, sciname, habitat, method) %>%
    summarize(domestic_export_live_t = sum(live_weight_t, na.rm = TRUE),
              domestic_export_product_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  # domestic consumption = domestic production - domestic exports
  consumption_domestic <- x_p %>%
    full_join(
      exports_domestic,
      by = c("iso3c"="source_country_iso3c", "hs6", "sciname", "habitat", "method")
    ) %>%
    # deals with cases where there is production but no exports
    replace_na(list(domestic_export_live_t = 0)) %>%
    mutate(consumption_live_t = production_live_t - domestic_export_live_t)
  
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
  #-----------------------------------------------------------------------------
  
  # Calculating Foreign Consumption-----------------------------------------------
  
  # Wi is the pool of imported products that have been processed and are available 
  # for export or consumption as the final hs6 processed form
  
  exports_foreign <- artis %>%
    filter(dom_source == "foreign") %>% 
    # Q: not sure why these were removed 2024-11-08
    #filter(dom_source == "foreign" & habitat != "unknown" & method != "unknown") %>%
    group_by(exporter_iso3c, hs6) %>%
    summarize(foreign_export_product_t = sum(product_weight_t, na.rm = TRUE),
              foreign_export_live_t = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  imports <- artis %>%
    # calculating all imports by hs6 code
    group_by(importer_iso3c, hs6) %>%
    summarize(import_product_t = sum(product_weight_t, na.rm = TRUE),
              import_live_t = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(est_live_weight_cf = import_live_t / import_product_t)
  
  processed_imports <- imports %>%
    # processing imports into the product form that is available for consumption or re-export
    left_join(
      W_long,
      by = c("importer_iso3c"="exporter_iso3c", "hs6"="hs6_original")
    ) %>%
    mutate(processed_product_t = import_product_t * estimated_W) %>%
    mutate(processed_live_t = processed_product_t * est_live_weight_cf) %>%
    # proportion of hs6 processed that can be disagregated into hs6 original
    # will be used to disagregate foreign consumption later on
    group_by(importer_iso3c, hs6_processed) %>%
    mutate(total_hs6_processed_live = sum(processed_live_t),
           total_hs6_processed_product = sum(processed_product_t)) %>%
    ungroup() %>%
    mutate(prop_processed_to_original_live = processed_live_t / total_hs6_processed_live,
           prop_processed_to_original_product = processed_product_t / total_hs6_processed_product)
  
  # unprocessed_exports <- exports_foreign %>%
  #   # processing imports into the product form that is available for consumption or re-export
  #   left_join(
  #     reweight_W_long %>% 
  #       mutate(hs6_processed = as.numeric(hs6_processed)),
  #     by = c("exporter_iso3c", "hs6"= "hs6_processed")
  #   ) %>%
  #   mutate(processed_product_t = foreign_export_product_t * reweighted_W) %>%
  #   #mutate(processed_live_t = processed_product_t * est_live_weight_cf) %>%
  #   # proportion of hs6 processed that can be disagregated into hs6 original
  #   # will be used to disagregate foreign consumption later on
  #   group_by(exporter_iso3c, hs6) %>%
  #   mutate(# total_hs6_processed_live = sum(processed_live_t),
  #          total_hs6_processed_product = sum(processed_product_t)) %>%
  #   ungroup() %>%
  #   mutate(#prop_processed_to_original_live = processed_live_t / total_hs6_processed_live,
  #          prop_processed_to_original_product = processed_product_t / total_hs6_processed_product)
  
  # DATA CHECK:
  # make sure proportions sum to 1 by the final hs6 product
  threshold <- 1e-9
  data_check_processed <- processed_imports %>%
    group_by(importer_iso3c, hs6_processed) %>%
    summarize(prop = sum(prop_processed_to_original_live)) %>%
    ungroup() %>%
    filter(abs(prop - 1) > threshold)
  if (nrow(data_check_processed) > 0) {
    warning(paste0("hs6 processed to hs6 original proportions DO NOT ADD TO 1", 
                   " for ", curr_year, " and ", curr_hs_version))
  }
  
  w_i <- processed_imports %>%
    # Resummarize by new set of products that are available by country
    # after processing of imports
    group_by(importer_iso3c, hs6_processed) %>%
    summarize(processed_live_t = sum(processed_live_t, na.rm = TRUE),
              processed_product_t = sum(processed_product_t, na.rm = TRUE)) %>%
    ungroup()
  
  # Everything that was consumed in their FINAL product form
  consumption_foreign <- w_i %>%
    # joining foreign exports
    full_join(
      exports_foreign,
      by = c("importer_iso3c"="exporter_iso3c", "hs6_processed"="hs6")
    ) %>%
    # correct for flows where there are imports available but no exports
    replace_na(list(foreign_export_live_t = 0, 
                    processed_live_t = 0, 
                    processed_product_t = 0,
                    foreign_export_product_t = 0)) %>%
    # calculate foreign consumption
    mutate(foreign_consumption_live_t = processed_live_t - foreign_export_live_t,
           foreign_consumption_product_t = processed_product_t - foreign_export_product_t)
  
  # DATA CHECK:
  # foreign consumption should equal domestic exports + error exports
  error_exports <- artis %>%
    filter(dom_source == "error")
  data_check_foreign <- sum(consumption_foreign$foreign_consumption_product_t) - sum(exports_domestic$domestic_export_product_t) - sum(error_exports$product_weight_t)
  if (abs(data_check_foreign) > 1) {
    warning(paste0("Foreign consumption DOES NOT EQUAL domestic exports + error exports ",
                   "for ", curr_year, " and ", curr_hs_version))
  }
  
  # Disagregate foreign consumption from hs6 processed to hs6 original
  disagregate_foreign_consumption <- consumption_foreign %>%
    left_join(
      processed_imports %>%
        select(importer_iso3c, hs6_original=hs6, hs6_processed, prop_processed_to_original_product) %>%
        distinct(),
      by = c("importer_iso3c", "hs6_processed")
    ) %>%
    mutate(foreign_consumption_original_product_t = foreign_consumption_product_t * prop_processed_to_original_product)
  
  # Calculating proportion of imports by original hs6 product
  # to disagregate by intermediate trade partners and sciname, habitat, method
  artis_import_props <- artis %>%
    # totals by importer and original hs6 product imported
    group_by(importer_iso3c, hs6) %>% 
    mutate(total = sum(product_weight_t)) %>%
    # proportion of imported hs6 by trade partners and sciname, habitat, method
    mutate(import_prop = product_weight_t/total) %>%
    ungroup() %>%
    # removing unnecessary rows
    select(-product_weight_t, -live_weight_t, -total)
  
  # record pre-disagregated foreign consumption volume for testing later
  test_foreign_consumption <- sum(disagregate_foreign_consumption$foreign_consumption_original_product_t, 
                                  na.rm = TRUE)
  ## FIXIT: Why are there NAs in disagregate_foreign_consumption$foreign_consumption_original_product_t ? 
  
  hs_cf_means <- V1_long %>% 
    group_by(hs6) %>% 
    summarise(live_weight_cf_mean = mean(live_weight_cf))
  
  # disagregate foreign consumption across all intermediates and sciname habitat method
  disagregate_foreign_consumption_all <- disagregate_foreign_consumption %>%
    # note import props are in the form of the original hs6 product and therefore
    # foreign consumption needs to be aggregated back to importer and hs6 original
    group_by(importer_iso3c, hs6_original) %>%
    summarize(foreign_consumption_product_t = sum(foreign_consumption_original_product_t)) %>%
    ungroup() %>%
    mutate(hs6_original = as.numeric(hs6_original)) %>% 
    # join proportions for disagregation
    left_join(
      artis_import_props, 
      by = c("importer_iso3c", "hs6_original"="hs6")) %>%
    # disagregate foreign consumption by import proportions
    mutate(foreign_consumption_product_t = foreign_consumption_product_t * import_prop,
           SciName = paste0(gsub(" ", ".", sciname), "_", habitat, "_", method)) %>% 
    # FIXIT: need to add code to handle "unknown" habitat and method
    left_join(V1_long %>% 
                mutate(hs6 = as.numeric(hs6)),
              by = c("hs6_original" = "hs6", "SciName")) %>% 
    # Replace NA in live_weight_cf using hs_cf_means
    left_join(
      hs_cf_means %>% mutate(hs6 = as.numeric(hs6)), 
      by = c("hs6_original" = "hs6")) %>% 
    mutate(
      live_weight_cf = replace_na(live_weight_cf_mean),
      foreign_consumption_live_t = foreign_consumption_product_t * live_weight_cf) %>%
    # Remove the mean column if it's no longer needed
    select(-live_weight_cf_mean)  %>% 
    # remove residual NAs rows with no data
    # FIXIT: Where are these NAs introduced - only importer and species 
    filter(!is.na(live_weight_cf))
  
  
  # DATA CHECK:
  # make sure there was no change in volume based on disaggregation
  # make sure this data check occurs before removing any hs6 processed flows
  if (abs(sum(disagregate_foreign_consumption_all$foreign_consumption_product_t) - test_foreign_consumption) > 1e-3) {
    warning(paste0("Disagregated foreign consumption does not match agregated foreign consumption ",
                   "for ", curr_year, " and ", curr_hs_version, 
                   ". The absolute value between total total foreign consumption and aggregated foreign consumption is ", 
                   abs(sum(disagregate_foreign_consumption_all$foreign_consumption_product_t) - test_foreign_consumption)))
  }
  
  # DATA CHECK ONLY INCLUDE THIS DATA CHECK WHEN NO CODES ARE REMOVED FROM CONSUMPTION:
  # domestic consumption + foreign consumption = production + error exports
  total_consumption <- sum(consumption_domestic$consumption_live_t) + 
    sum(disagregate_foreign_consumption_all$foreign_consumption_product_t)
  
  data_check_consumption <- total_consumption / (sum(prod$live_weight_t) + sum(error_exports$live_weight_t))
  if (abs(1 - data_check_consumption) > 1e-3) {
      warning(paste0("(domestic consumption + foreign consumption) DOES NOT EQUAL (production + error exports)", 
                     " for ", curr_year, " and ", curr_hs_version, ". Test value should be 0. Test value is ", 
                     abs(1 - data_check_consumption) ))
  }
  
  #-------------------------------------------------------------------------------
  
  # Join domestic and foreign consumption-----------------------------------------
  
  complete_consumption <- disagregate_foreign_consumption_all %>%
    select(-c(hs_version, import_prop)) %>%
    mutate(consumption_type = "foreign") %>%
    rename(consumer_iso3c = importer_iso3c,
           hs6 = hs6_original,
           consumption_live_t = foreign_consumption_live_t) %>%
    bind_rows(
      consumption_domestic %>%
        rename(consumer_iso3c = iso3c) %>%
        select(-c(production_live_t, domestic_export_live_t)) %>%
        mutate(consumption_type = "domestic",
               source_country_iso3c = consumer_iso3c,
               exporter_iso3c = as.character(NA),
               year = curr_year)
    ) %>%
    mutate(hs_version = curr_hs_version)
  
  # Note: Adding in hs taxa max resolved for greater taxonomic clarity
  #        make sure to keep both sciname and taxa max resolved name
  #        MAKE SURE THERE ARE NO DUPLICATES
  complete_consumption <- complete_consumption %>%
    left_join(
      code_max_resolved %>% 
        mutate(hs6 = as.numeric(hs6)),
      by = c("hs6", "hs_version", "sciname")
    ) %>% 
    mutate(sciname_hs_modified = ifelse(is.na(sciname_hs_modified), 
                                        sciname, 
                                        sciname_hs_modified))
  
  # Collapse hs6 original column
  # Note: if we wanted to know the a country's consumption of a given product (ie 230120)
  #       this would relate to the hs6 processed, since this is the last form of the product throughout the supply chain
  complete_consumption <- complete_consumption %>%
    mutate(end_use = case_when(
             hs6 == 230120 ~ "fishmeal",
             hs6 %in% c(30110, 30111, 30119) ~ "other",
             TRUE ~ "direct human consumption")) %>% 
    group_by(year, hs_version, source_country_iso3c, exporter_iso3c, consumer_iso3c,
              sciname, sciname_hs_modified, habitat, method, dom_source, consumption_type, end_use) %>%
    summarize(consumption_live_t = sum(consumption_live_t)) %>%
    ungroup()# %>%
    # negative flows are removed so total consumption volume increases
   # filter(consumption_live_t > consumption_threshold)
  
  # DATA CHECK
  # make sure there are no NA values in consumption
  na_consumption <- complete_consumption %>%
    filter(is.na(consumption_live_t))
  if (nrow(na_consumption) != 0) {
    warning(paste0("NAs in complete consumption ", 
                   "for ", curr_year, " and ", curr_hs_version, 
                   ". Number of NAs is ", nrow(na_consumption)))
  }
  
  if (min(complete_consumption$consumption_live_t) < 0) {
    warning(paste0("Negative consumption values ", 
                   "for ", curr_year, " and ", curr_hs_version))
  }
  
  # DATA CHECK
  # does consumption total by source country add back up to production?
  
  test <- complete_consumption %>% 
    group_by(source_country_iso3c, sciname, habitat, method) %>% 
    summarise(consumption_live_t_sum = sum(consumption_live_t)) %>% 
    left_join(prod %>% 
                group_by(country_iso3_alpha, sciname, habitat, method) %>% 
                summarise(live_weight_t = sum(live_weight_t)), 
              by = c("source_country_iso3c" = "country_iso3_alpha", "sciname", "habitat", "method")) %>% 
    mutate(diff = consumption_live_t_sum - live_weight_t)
  
    
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
      mutate(corrected_consumption_t = (pop * max_percap_consumption) / 1000)
    
    consumption_outliers <- complete_consumption %>%
      filter(consumer_iso3c %in% unique(percap_outliers$consumer_iso3c) & 
               end_use == "direct human consumption") %>%
      group_by(consumer_iso3c) %>%
      mutate(total = sum(consumption_live_t, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(prop = consumption_live_t / total) %>%
      left_join(
        percap_outliers %>%
          select(consumer_iso3c, corrected_consumption_t),
        by = c("consumer_iso3c")
      ) %>%
      mutate(consumption_t_capped = prop * corrected_consumption_t) %>%
      select(-c(total, corrected_consumption_t, prop))
    
    complete_consumption_capped <- complete_consumption %>%
      mutate(consumption_t_capped = consumption_live_t) %>% 
      filter(!(consumer_iso3c %in% unique(percap_outliers$consumer_iso3c) & 
                 end_use == "direct human consumption")) %>%
      bind_rows(consumption_outliers)
    
    complete_consumption <- complete_consumption_capped
  }

  return(complete_consumption)
}
