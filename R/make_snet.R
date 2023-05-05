#' @export
make_snet <- function(baci_data_analysis_year, reweight_X_long, country_est, V1, V2, cc_m, hs_clade_match,
                          country_j, coproduct_codes, threshold = 0.1, dom_source_weight = "midpoint"){
  # Options for dom_source_weight are "min", "max", and "midpoint", with midpoint as the default
  
  # Step 0: Rename product_weight_t column to specify that it is product weight
  baci_data_analysis_year <- baci_data_analysis_year %>%
    rename(product_weight_t = total_q) %>%
    filter(!(hs6 %in% coproduct_codes))
  
  # Step 1: Calculate the domestic, foreign, and error-source weights for exports
  # Replace diag V2 with 1 if imported products can go to exports 
  # Replace with 0 if all non-processed imported products should be considered for domestic consumption
  diag(V2) <- 1
  
  # Get values from country_est for focal country
  X <- country_est[[country_j]]$X
  X[X<0] <- 0 
  p <- country_est[[country_j]]$p
  c_domestic <- country_est[[country_j]]$c
  c_domestic[c_domestic<0] <- 0
  W <- country_est[[country_j]]$W
  W[W<0] <- 0
  imports <- country_est[[country_j]]$imports
  exports <- country_est[[country_j]]$exports
  error <- country_est[[country_j]]$error_term
  error[error<0] <- 0
  
  # Create data frame to calculate min/max export source weight
  export_source_weights <- data.frame(
    exports = exports,
    domestic_prod = (X*V1) %*% p, # max(V1) = 1, meaning conversions go from live weight to product weight
    import_prod = (W * V2) %*% imports,
    error = error
  )
  
  export_source_weights <- export_source_weights %>%
    filter(exports > 0) %>% 
    rownames_to_column(var = "hs6") %>% 
    mutate(hs6 = str_extract(hs6, "[[:digit:]]+")) %>% 
    # Calculate the max exports from domestic and imported sources
    mutate(exports_domestic_max = pmin(domestic_prod, exports), 
           exports_import_max = pmin(import_prod, exports)) %>%
    # Get remaining exports from domestic and imported
    mutate(
      # remaining domestic max = remaining product volume after you have filled exports with the most domestic production
      remaining_domestic_max = domestic_prod - exports_domestic_max,
      # remaining import max = remaining product volume after you have filled exports with the most imports
      remaining_import_max = import_prod - exports_import_max
    ) %>%
    # Calculate the min from domestic and imported sources
    mutate(exports_domestic_min = pmin(domestic_prod, (exports-exports_import_max)),
           exports_import_min = pmin(import_prod, (exports-exports_domestic_max))) %>% 
    # Get remaining exports from domestic and imported
    mutate(
      # remaining domestic min = remaining domestic product volume after you have filled exports with the most imports and
      #                           the rest with domestic product
      remaining_domestic_min = domestic_prod - exports_domestic_min,
      # remaining import min = remaining import product volume after you have filled exports with the most domestic and
      #                           the rest with import product
      remaining_import_min = import_prod - exports_import_min
    ) %>%
    # Calculate the midpoint from domestic and imported sources
    mutate(exports_domestic_midpoint = (exports_domestic_max + exports_domestic_min)/2,
           exports_import_midpoint = (exports_import_max + exports_import_min)/2) %>%
    # Get remaining exports from domestic and imported
    mutate(
      remaining_domestic_midpoint = domestic_prod - exports_domestic_midpoint,
      remaining_import_midpoint = import_prod - exports_import_midpoint
    )
  
  
  consumption_source_weights <- data.frame(consumption = c_domestic,
                    hs6 = cc_m,
                    domestic_prod = (X*V1) %*% p, # max(V1) = 1, meaning conversions go from live weight to product weight
                    import_prod = (W * V2) %*% imports) %>%
    left_join(
      export_source_weights %>%
        select(hs6, starts_with("remaining")),
      by = c("hs6")
    ) %>%
    # if there are no exports then the domestic production is completely consumed
    mutate(
      remaining_domestic_max = case_when(
        is.na(remaining_domestic_max) ~ domestic_prod,
        TRUE ~ remaining_domestic_max),
      remaining_domestic_min = case_when(
        is.na(remaining_domestic_min) ~ domestic_prod,
        TRUE ~ remaining_domestic_min),
      remaining_domestic_midpoint = case_when(
        is.na(remaining_domestic_midpoint) ~ domestic_prod,
        TRUE ~ remaining_domestic_midpoint),
      remaining_import_max = case_when(
        is.na(remaining_import_max) ~ import_prod,
        TRUE ~ remaining_import_max),
      remaining_import_min = case_when(
        is.na(remaining_import_min) ~ import_prod,
        TRUE ~ remaining_import_min),
      remaining_import_midpoint = case_when(
        is.na(remaining_import_midpoint) ~ import_prod,
        TRUE ~ remaining_import_midpoint)
    )
  
    
  
  # Calculate the source weightings based on the dom_source_weight option
  if(dom_source_weight == "midpoint"){
    export_source_weights <- export_source_weights %>% 
      mutate(domestic_weight = exports_domestic_midpoint/exports,
             foreign_weight = exports_import_midpoint/exports,
             error_weight = pmax((exports - exports_domestic_midpoint - exports_import_midpoint),0)/exports) %>%
      select(hs6, domestic_weight, foreign_weight, error_weight)
    
    consumption_source_weights <- consumption_source_weights %>% 
      mutate(domestic_weight = remaining_domestic_midpoint/consumption,
             foreign_weight = remaining_import_midpoint/consumption) %>%
      select(hs6, consumption, domestic_weight, foreign_weight)
  }
  if(dom_source_weight == "max"){
    export_source_weights <- export_source_weights %>% 
      mutate(domestic_weight = exports_domestic_max/exports,
             foreign_weight = exports_import_min/exports,
             error_weight = pmax((exports - exports_domestic_max - exports_import_min),0)/exports) %>%
      select(hs6, domestic_weight, foreign_weight, error_weight)
    
    consumption_source_weights <- consumption_source_weights %>% 
      mutate(domestic_weight = remaining_domestic_max/consumption,
             foreign_weight = remaining_import_min/consumption) %>%
      select(hs6, consumption, domestic_weight, foreign_weight)
    
  }
  if(dom_source_weight == "min"){
    export_source_weights <- export_source_weights %>% 
      mutate(domestic_weight = exports_domestic_min/exports,
             foreign_weight = exports_import_max/exports,
             error_weight = pmax((exports - exports_domestic_min - exports_import_max),0)/exports) %>%
      select(hs6, domestic_weight, foreign_weight, error_weight)
    
    consumption_source_weights <- consumption_source_weights %>% 
      mutate(domestic_weight = remaining_domestic_min/consumption,
             foreign_weight = remaining_import_max/consumption) %>%
      select(hs6, consumption, domestic_weight, foreign_weight)
  }
  
  
  consumption_source_weights <- consumption_source_weights %>%
    # weights that are NaN occured when consumption = 0
    # weights that are infinite occured when 0 / 0
    mutate(
      domestic_weight = case_when(
        is.na(domestic_weight) ~ 0,
        is.infinite(domestic_weight) ~ 0,
        TRUE ~ domestic_weight),
      foreign_weight = case_when(
        is.na(foreign_weight) ~ 0,
        is.infinite(foreign_weight) ~ 0,
        TRUE ~ foreign_weight
      )
    ) %>%
    # identifying country
    mutate(iso3c = country_j)
  
  export_sources <- baci_data_analysis_year %>%
    filter(exporter_iso3c == country_j) %>%
    left_join(export_source_weights, by = "hs6") %>%
    mutate(domestic_product_weight_t = product_weight_t*domestic_weight,
           foreign_product_weight_t = product_weight_t*foreign_weight,
           error_product_weight_t = product_weight_t*error_weight)
  
  # consumption_sources <- baci_data_analysis_year %>%
  #   filter(exporter_iso3c == country_j) %>%
  #   left_join(consumption_source_weights, by = "hs6") %>%
  #   mutate(
  #     domestic_product_weight_t = product_weight_t * domestic_weight,
  #     foreign_product_weight_t = product_weight_t * foreign_weight
  #   )
  
  # Step 2: Format live weight conversion factors
  # Pivot live weight conversion factors long
  V1_long <- as.data.frame(V1) %>%
    mutate(hs6 = colnames(V2)) %>% 
    pivot_longer(cols = -hs6, names_to = "SciName", values_to = "live_weight_cf") %>%
    filter(live_weight_cf > 0) %>%
    # Transform conversion factors to represent conversion from product to live
    mutate(live_weight_cf = 1/live_weight_cf)
  
  # Step 3: Calculate the domestic-sourced portion
  dom_exp <- export_sources %>%
    select(importer_iso3c, exporter_iso3c, hs6, product_weight_t = domestic_product_weight_t) %>%
    # Remove zero product weights
    filter(product_weight_t > 0) %>%
    # Join estimated species proportions within each code
    left_join(reweight_X_long %>% 
                filter(iso3c == country_j), by = c("hs6" = "hs6", "exporter_iso3c" = "iso3c")) %>%
    # Multiply domestic exports by estimated proportion of species that go in each code
    mutate(product_weight_t = product_weight_t * reweighted_X) %>%
    # Join live weight conversion factor data
    left_join(V1_long, by = c("hs6", "SciName")) %>%
    # Remove flows below threshold values
    filter(product_weight_t > threshold) %>%
    # Calculate trade in live weight equivalents
    mutate(live_weight_t = live_weight_cf*product_weight_t) %>%
    # Add column to identify as a domestic export
    mutate(dom_source = "domestic export", 
           source_country_iso3c = country_j) %>%
    # Split SciName into the taxa name, environment, and method
    separate(SciName, into = c("SciName", "environment", "method"), sep = "_") %>%
    select(-reweighted_X) %>%
    mutate(SciName = str_replace(SciName, "\\.", " "))
  
  # Step 4: Calculate the foreign-sourced portion
  # Step 4a: Apply the reweighted X for the source country to get their species mix by code
  imp_source <- baci_data_analysis_year %>% 
    # Select imports for focal country
    filter(importer_iso3c == country_j) %>%
    # Calculate proportion exporter source by code 
    left_join(reweight_X_long, by = c("hs6", "exporter_iso3c" = "iso3c")) %>%
    # Remove NAs generated from species not produced by exporting country
    filter(!is.na(reweighted_X)) %>% 
    mutate(product_weight_t = product_weight_t*reweighted_X) %>% 
    select(-reweighted_X)
    
  # Step 4b: Calculate the processing from one code to another
  taxa_processed <- as.data.frame(country_est[[country_j]]$W)
  taxa_processed <- taxa_processed %>%
    # Reformat W as a long data frame
    # Original imported product form is on the columns and
    # processed form is on the rows
    rownames_to_column(var = "hs6_processed") %>%
    pivot_longer(2:(ncol(taxa_processed)+1), 
                 names_to = "hs6_original", 
                 values_to = "estimated_W") %>%
    filter(estimated_W > 10^-9) %>%
    # Separate hs codes
    mutate(hs6_original = str_extract(hs6_original, "[[:digit:]]+"),
           hs6_processed = str_extract(hs6_processed, "[[:digit:]]+")) %>%
    # Add on the species mixes by code for imports
    left_join(imp_source, by = c("hs6_original" = "hs6")) %>%
    # Calculate the quantity in each processing combo
    # Note: Processing loss factors are not needed because we need species mix within each code
    # and these are constant with each code pair
    mutate(product_weight_t = product_weight_t*estimated_W) %>%
    # Removing instances of estimated processing of a product where
    # no matched species are associated with that product
    filter(!is.na(product_weight_t)) %>%
    group_by(hs6_processed) %>%
    mutate(product_weight_t_hs6 = sum(product_weight_t)) %>%
    filter(product_weight_t_hs6 > 0) %>%
    ungroup() %>%
    # Getting product weight by SciName
    group_by(hs6_processed, SciName, exporter_iso3c, product_weight_t_hs6) %>%
    summarize(product_weight_t = sum(product_weight_t)) %>%
    # Calculate the proportion of each code from each taxa
    summarise(prop_taxa_W = product_weight_t/product_weight_t_hs6) %>%
    rename("hs6" = "hs6_processed", "source_country_iso3c" = "exporter_iso3c") 
 
  # Step 4c: Apply the species mixes to import data
  foreign_exp <- export_sources %>%
    select(importer_iso3c, exporter_iso3c, hs6, product_weight_t = foreign_product_weight_t) %>%
    # Remove zero product weights
    filter(product_weight_t > 0) %>%
    # Join with estimated taxa within processed products from each source country
    left_join(taxa_processed, by = "hs6") %>%
    # Join with live weight conversion factors
    left_join(V1_long, by = c("hs6", "SciName")) %>%
    # Calculate flow by species as product weight
    mutate(product_weight_t = product_weight_t*prop_taxa_W) %>%
    # Drop NAs that appear for codes for which no domestic production was estimated
    # And flows below threshold values
    filter(!is.na(product_weight_t), product_weight_t > threshold) %>%
    # Calculate trade in live weight equivalents
    mutate(live_weight_t = live_weight_cf*product_weight_t) %>%
    # Add column to identify as a domestic export
    mutate(dom_source = "foreign export") %>%
    # Split SciName into the taxa name, environment, and method
    separate(SciName, into = c("SciName", "environment", "method"), sep = "_") %>%
    mutate(SciName = str_replace(SciName, "\\.", " ")) %>%
    select(-prop_taxa_W)
    
  
  # Step 5: Calculate the portion associated with the error term
  # Step 5a: Average conversion factors by code for matching with closest clade
  V1_long <- V1_long %>%
    group_by(hs6) %>%
    summarise(live_weight_cf = mean(live_weight_cf))
    
  error_exp <- export_sources %>%
    select(importer_iso3c, exporter_iso3c, hs6, product_weight_t = error_product_weight_t) %>%
    # Remove zero product weights
    filter(product_weight_t > threshold) %>%
    # Join broad group name
    left_join(hs_clade_match, by = c("hs6" = "Code")) %>%
    # Join live weight conversion factors
    left_join(V1_long, by = "hs6") %>%
    mutate(live_weight_t = live_weight_cf*product_weight_t) %>%
    # Add production source, country source, environment, and method info
    mutate(dom_source = "error export",
           source_country_iso3c = "unknown",
           environment = "unknown", method = "unknown") %>%
    rename("SciName" = "hs_clade") %>%
    select(-classification_level)
  
  # Step 6: Combine the three sources of exports
  s_net <- dom_exp %>%
    bind_rows(foreign_exp) %>%
    bind_rows(error_exp) %>%
    select(exporter_iso3c, importer_iso3c, dom_source, source_country_iso3c, 
           hs6, SciName, environment, method, product_weight_t, live_weight_t)
  
  # Step 7: Format consumption to convert from product to live weight volumes
  # Note: this could be moved over to the supply calculation script if we want to retain species specific conversion factors
  consumption_source_weights <- consumption_source_weights %>%
    left_join(
      V1_long,
      by = c("hs6")
    ) %>%
    mutate(
      consumption = consumption * live_weight_cf
    )
  
  return(list(s_net, consumption_source_weights))
}
