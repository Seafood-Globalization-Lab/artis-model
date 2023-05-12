#' @export
create_export_source_weights <- function(baci_data_analysis_year, countries_to_analyze, country_est, V1, V2, cc_m,
                                         coproduct_codes, dom_source_weight = "midpoint") {
  
  # Rename product_weight_t column to specify that it is product weight
  # remove coproduct codes
  baci_data_analysis_year <- baci_data_analysis_year %>%
    # rename(product_weight_t = total_q) %>%
    filter(!(hs6 %in% coproduct_codes))
  
  # Calculate the domestic, foreign, and error-source weights for exports
  # Replace diag V2 with 1 if imported products can go to exports 
  # Replace with 0 if all non-processed imported products should be considered for domestic consumption
  diag(V2) <- 1
  
  export_source_weights <- data.frame()
  consumption_source_weights <- data.frame()
  
  for (i in 1:length(countries_to_analyze)) {
    curr_country <- countries_to_analyze[i]
    
    # Get values from country_est for focal country
    X <- country_est[[curr_country]]$X
    X[X<0] <- 0 
    p <- country_est[[curr_country]]$p
    c_domestic <- country_est[[curr_country]]$c
    c_domestic[c_domestic<0] <- 0
    W <- country_est[[curr_country]]$W
    W[W<0] <- 0
    imports <- country_est[[curr_country]]$imports
    exports <- country_est[[curr_country]]$exports
    error <- country_est[[curr_country]]$error_term
    error[error<0] <- 0
    
    # Create data frame to calculate min/max export source weight
    curr_export_source_weights <- data.frame(
      exports = exports,
      domestic_prod = (X*V1) %*% p, # max(V1) = 1, meaning conversions go from live weight to product weight
      import_prod = (W*V2) %*% imports,
      error = error
    )
    
    curr_export_source_weights <- curr_export_source_weights %>%
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
    
    curr_consumption_source_weights <- data.frame(consumption = c_domestic,
                                                  hs6 = cc_m,
                                                  domestic_prod = (X*V1) %*% p, # max(V1) = 1, meaning conversions go from live weight to product weight
                                                  import_prod = (W * V2) %*% imports) %>%
      left_join(
        curr_export_source_weights %>%
          select(matches("^remaining|^hs6")),
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
      curr_export_source_weights <- curr_export_source_weights %>% 
        mutate(domestic_weight = exports_domestic_midpoint/exports,
               foreign_weight = exports_import_midpoint/exports,
               error_weight = pmax((exports - exports_domestic_midpoint - exports_import_midpoint),0)/exports) %>%
        select(hs6, domestic_weight, foreign_weight, error_weight)
      
      curr_consumption_source_weights <- curr_consumption_source_weights %>% 
        mutate(domestic_weight = remaining_domestic_midpoint/consumption,
               foreign_weight = remaining_import_midpoint/consumption) %>%
        select(hs6, consumption, domestic_weight, foreign_weight)
    }
    if(dom_source_weight == "max"){
      curr_export_source_weights <- curr_export_source_weights %>% 
        mutate(domestic_weight = exports_domestic_max/exports,
               foreign_weight = exports_import_min/exports,
               error_weight = pmax((exports - exports_domestic_max - exports_import_min),0)/exports) %>%
        select(hs6, domestic_weight, foreign_weight, error_weight)
      
      curr_consumption_source_weights <- curr_consumption_source_weights %>% 
        mutate(domestic_weight = remaining_domestic_max/consumption,
               foreign_weight = remaining_import_min/consumption) %>%
        select(hs6, consumption, domestic_weight, foreign_weight)
      
    }
    if(dom_source_weight == "min"){
      curr_export_source_weights <- curr_export_source_weights %>% 
        mutate(domestic_weight = exports_domestic_min/exports,
               foreign_weight = exports_import_max/exports,
               error_weight = pmax((exports - exports_domestic_min - exports_import_max),0)/exports) %>%
        select(hs6, domestic_weight, foreign_weight, error_weight)
      
      curr_consumption_source_weights <- curr_consumption_source_weights %>% 
        mutate(domestic_weight = remaining_domestic_min/consumption,
               foreign_weight = remaining_import_max/consumption) %>%
        select(hs6, consumption, domestic_weight, foreign_weight)
    }
    
    curr_export_source_weights <- curr_export_source_weights %>%
      mutate(iso3c = curr_country)
    
    curr_consumption_source_weights <- curr_consumption_source_weights %>%
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
      mutate(iso3c = curr_country)
    
    
    # Build up the export and consumption source weights
    export_source_weights <- export_source_weights %>%
      bind_rows(curr_export_source_weights)
    
    consumption_source_weights <- consumption_source_weights %>%
      bind_rows(curr_consumption_source_weights)
  }
  
  return(list(export_source_weights, consumption_source_weights))
}

