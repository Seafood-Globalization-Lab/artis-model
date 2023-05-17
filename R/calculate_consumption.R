#' @export
calculate_consumption <- function(consumption_source_weights, artis, reweight_X_long, reweight_W_long,
                                  V1_long, pop_data, outdir, estimate_type = "midpoint", consumption_threshold = 0.0001) {
  
  consumption <- consumption_source_weights %>%
    mutate(hs_version = paste("HS", hs_version, sep = "")) %>%
    mutate(consumption = case_when(
      consumption < consumption_threshold ~ 0,
      TRUE ~ consumption)) %>%
    mutate(domestic_weight = case_when(
      consumption == 0 ~ 0,
      TRUE ~ domestic_weight
    )) %>%
    mutate(foreign_weight = case_when(
      consumption == 0 ~ 0,
      TRUE ~ foreign_weight
    )) %>%
    filter(consumption > 0)
  
  consumption <- consumption %>%
    # average live weight cfs used to determine live weight consumption
    left_join(V1_long %>%
                mutate(hs6 = as.character(hs6)) %>%
                mutate(hs6 = case_when(
                  str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
                  TRUE ~ hs6
                )) %>%
                group_by(hs6) %>%
                summarize(live_weight_cf = mean(live_weight_cf, na.rm = TRUE)),
              by = c("hs6")) %>%
    mutate(consumption = consumption * live_weight_cf) %>%
    select(-live_weight_cf)
  
  write.csv(
    consumption,
    file.path(outdir, paste("consumption_live_weight_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
  
  consumption <- consumption %>%
    # remove non human consumption codes
    filter(!(hs6 %in% c("230120", "051191", "030110", "030111", "030119")))
  
  write.csv(
    consumption,
    file.path(outdir, paste("consumption_human_only_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
  
  consumption_per_capita <- consumption %>%
    filter(iso3c != "NEI") %>%
    filter(!(iso3c == "SDN" & year <= 2011)) %>%
    group_by(iso3c, year) %>%
    summarize(consumption = sum(consumption, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(
      pop_data %>%
        select(iso3c, year, pop) %>%
        group_by(iso3c, year) %>%
        summarize(pop = sum(pop)) %>%
        ungroup(),
      by = c("iso3c", "year")
    ) %>%
    # Note: there are some countries that have no population data associated with it
    filter(!is.na(pop)) %>%
    group_by(year, iso3c) %>%
    summarize(per_capita = 1000 * sum(consumption, na.rm = TRUE) / sum(pop, na.rm = TRUE))
  
  # Remove records where per capita consumption is over 100 kg
  outliers_per_cap <- consumption_per_capita %>%
    filter(per_capita > 100)
  
  write.csv(
    outliers_per_cap,
    file.path(outdir, paste("outliers_per_cap_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
  
  consumption_outliers <- outliers_per_cap %>%
    left_join(
      consumption,
      by = c("iso3c", "year")
    ) %>%
    group_by(iso3c, year) %>%
    mutate(total_consumption = sum(consumption)) %>%
    ungroup() %>%
    mutate(prop_consumption = consumption / total_consumption) %>%
    left_join(
      pop_data %>%
        select(iso3c, pop, year),
      by = c("iso3c", "year")
    ) %>%
    # 100 kg = 0.1 tonnes
    mutate(total_consumption = 0.1 * pop) %>%
    mutate(consumption = prop_consumption * total_consumption) %>%
    select(-pop) %>%
    select(iso3c, year, hs_version, hs6, consumption, domestic_weight, foreign_weight)
  
  consumption_no_outliers <- consumption %>%
    anti_join(
      outliers_per_cap %>%
        select(iso3c, year) %>%
        distinct(),
      by = c("iso3c")
    )
  
  consumption <- consumption_no_outliers %>%
    bind_rows(consumption_outliers)
  
  write.csv(
    consumption,
    file.path(outdir, paste("consumption_max_100kg_per_capita_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
  
  # revert consumption back to product weight
  consumption <- consumption %>%
    left_join(
      V1_long %>%
        mutate(hs6 = as.character(hs6)) %>%
        mutate(hs6 = case_when(
          str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
          TRUE ~ hs6
        )) %>%
        # conversion from live to product weight
        mutate(live_weight_cf = 1 / live_weight_cf) %>%
        group_by(hs6) %>%
        summarize(live_weight_cf = mean(live_weight_cf, na.rm = TRUE)),
      by = c("hs6")
    ) %>%
    mutate(consumption = consumption * live_weight_cf) %>%
    select(-live_weight_cf)
  
  reweight_X_long <- reweight_X_long %>%
    separate(SciName, c("sciname", "habitat", "method"), sep = "_") %>%
    mutate(sciname = gsub("\\.", " ", sciname))
  
  domestic_consumption <- consumption %>%
    filter(!(hs6 %in% c("230120", "051191", "030110", "030111", "030119"))) %>%
    select(-foreign_weight) %>%
    filter(domestic_weight > 0) %>%
    left_join(
      reweight_X_long,
      by = c("iso3c", "hs6", "year", "hs_version")
    ) %>%
    # prop of consumption from domestic prod * prop of domestic prod that goes into each species
    mutate(reweighted_domestic_weight = domestic_weight * reweighted_X) %>%
    mutate(domestic_consumption_t = consumption * reweighted_domestic_weight) %>%
    mutate(hs6 = as.character(hs6)) %>%
    mutate(hs6 = case_when(
      str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
      TRUE ~ hs6
    )) %>%
    left_join(
      V1_long %>%
        mutate(hs6 = as.character(hs6)) %>%
        mutate(hs6 = case_when(
          str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
          TRUE ~ hs6
        )) %>%
        separate(SciName, c("sciname", "habitat", "method"), sep = "_") %>%
        mutate(sciname = gsub("\\.", " ", sciname)),
      by = c("hs6", "sciname", "habitat", "method")
    ) %>%
    # convert consumption back to live weight
    mutate(domestic_consumption_t = domestic_consumption_t * live_weight_cf)
  
  
  write.csv(
    domestic_consumption,
    file.path(outdir, paste("domestic_consumption_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
  
  # Connect consumption final HS6 code to original hs6 that it was imported under before processing
  consumption <- consumption %>%
    rename(hs6_processed = hs6) %>%
    left_join(
      reweight_W_long,
      by = c("hs6_processed","iso3c"="exporter_iso3c") # , "year", "hs_version"
    ) %>%
    mutate(consumption = consumption * reweighted_W * foreign_weight) %>%
    group_by(iso3c, hs6_original, year, hs_version) %>%
    summarize(consumption = sum(consumption, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(hs6 = hs6_original)
  
  prop_imports <- artis %>%
    # Get total imports by importer
    group_by(importer_iso3c, hs6) %>%
    mutate(total_import = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(importer_prop = product_weight_t / total_import)
  
  write.csv(
    prop_imports,
    file.path(outdir, paste("prop_imports_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
  
  foreign_consumption <- consumption %>%
    # Note: we can leave this in
    filter(!(hs6 %in% c("230120", "051191", "030110", "030111", "030119"))) %>%
    mutate(hs6 = as.character(hs6)) %>%
    mutate(
      hs6 = case_when(
        str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
        TRUE ~ hs6)
    ) %>%
    # prop of consumption from imports * 
    left_join(
      prop_imports %>%
        mutate(hs6 = as.character(hs6)) %>%
        mutate(hs6 = case_when(
          str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
          TRUE ~ hs6
        )) %>%
        select(source_country_iso3c, exporter_iso3c, importer_iso3c, dom_source, hs6, sciname, habitat, method, year, importer_prop),
      by = c("iso3c" = "importer_iso3c", "hs6", "year")
    ) %>%
    # special case minor percentage of consumption does not have a match with ARTIS
    filter(!is.na(source_country_iso3c)) %>%
    # prop of consumption from imports * prop of imports by all categories
    mutate(foreign_consumption_t = consumption * importer_prop) %>%
    left_join(
      V1_long %>%
        mutate(hs6 = as.character(hs6)) %>%
        mutate(hs6 = case_when(
          str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
          TRUE ~ hs6
        )) %>%
        separate(SciName, c("sciname", "habitat", "method"), sep = "_") %>%
        mutate(sciname = gsub("\\.", " ", sciname)),
      by = c("hs6", "sciname", "habitat", "method")
    ) %>%
    left_join(
      V1_long %>%
        mutate(hs6 = as.character(hs6)) %>%
        mutate(hs6 = case_when(
          str_length(hs6) == 5 ~ paste("0", hs6, sep = ""),
          TRUE ~ hs6
        )) %>%
        group_by(hs6) %>%
        summarize(avg_live_weight_cf = mean(live_weight_cf, na.rm = TRUE)),
      by = c("hs6")
    ) %>%
    mutate(foreign_consumption_t = case_when(
      !is.na(live_weight_cf) ~ foreign_consumption_t * live_weight_cf,
      TRUE ~ foreign_consumption_t * avg_live_weight_cf)) %>%
    select(-c(live_weight_cf, avg_live_weight_cf))
  
  write.csv(
    foreign_consumption,
    file.path(outdir, paste("foreign_consumption_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
  
  summary_consumption <- domestic_consumption %>%
    group_by(hs6, iso3c, sciname, habitat, method, year) %>%
    summarize(domestic_consumption_t = sum(domestic_consumption_t, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(sciname)) %>%
    full_join(
      foreign_consumption %>%
        group_by(hs6, iso3c, sciname, habitat, method, year) %>%
        summarize(foreign_consumption_t = sum(foreign_consumption_t, na.rm = TRUE)) %>%
        ungroup(),
      by = c("hs6", "iso3c", "sciname", "habitat", "method", "year")
    ) %>%
    mutate(domestic_consumption_t = case_when(
      is.na(domestic_consumption_t) ~ 0,
      TRUE ~ domestic_consumption_t
    )) %>%
    mutate(foreign_consumption_t = case_when(
      is.na(foreign_consumption_t) ~ 0,
      TRUE ~ foreign_consumption_t
    )) %>%
    mutate(supply = domestic_consumption_t + foreign_consumption_t)
  
  
  write.csv(
    summary_consumption,
    file.path(outdir, paste("summary_consumption_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
}