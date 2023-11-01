#' @export
calculate_consumption <- function(artis, prod_data, analysis_year, X_long, reweight_X_long, reweight_W_long,
                                  V1_long, pop_data, outdir, estimate_type = "midpoint", end_use = "human", 
                                  consumption_threshold = 0.001) {
  
  artis <- artis %>%
    filter(year == analysis_year)
  
  prod_data <- prod_data %>%
    filter(year == analysis_year)
  
  pop_data <- pop_data %>%
    filter(year == analysis_year)
  
  # Fixing production data habitat and method classifications
  prod_data <- prod_data %>%
    select(country_iso3_alpha, taxa_source, quantity, year) %>%
    separate(taxa_source, c("sciname", "habitat", "method"), sep = "_") %>%
    mutate(sciname = gsub("\\.", " ", sciname))
  
  # Domestic Consumption--------------------------------------------------------
  domestic_production <- prod_data %>%
    select(iso3c=country_iso3_alpha, sciname, habitat, method, quantity, year) %>%
    group_by(iso3c, sciname, habitat, method, year) %>%
    summarize(production = sum(quantity, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(
      X_long,
      by = c("iso3c", "sciname", "habitat", "method")
    ) %>%
    mutate(production = production * estimated_X) %>%
    group_by(iso3c, hs6, sciname, habitat, method, year) %>%
    summarize(production = sum(production, na.rm = TRUE)) %>%
    ungroup()
  
  domestic_exports <- artis %>%
    filter(dom_source == "domestic") %>%
    group_by(exporter_iso3c, hs6, sciname, habitat, method, year) %>%
    summarize(dom_exports_live = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  domestic_consumption <- domestic_exports %>%
    rename(iso3c = exporter_iso3c) %>%
    full_join(
      domestic_production,
      by = c("iso3c", "hs6", "sciname", "habitat", "method", "year")
    ) %>%
    mutate(
      dom_exports_live = case_when(
        is.na(dom_exports_live) ~ 0,
        TRUE ~ dom_exports_live
      ),
      production = case_when(
        is.na(production) ~ 0,
        TRUE ~ production
      )
    ) %>%
    mutate(dom_consumption = production - dom_exports_live,
           source_country_iso3c = iso3c)
  
  # Foreign Consumption---------------------------------------------------------
  import_production <- artis %>%
    group_by(importer_iso3c, hs6, year) %>%
    summarize(import_prod = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  foreign_exports <- artis %>%
    filter(dom_source == "foreign") %>%
    group_by(exporter_iso3c, hs6) %>%
    summarize(foreign_exports_product = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(
      reweight_W_long,
      by = c("exporter_iso3c", "hs6"="hs6_processed")
    ) %>%
    mutate(foreign_exports_reweighted = foreign_exports_product * reweighted_W) %>%
    # Re summarize foreign exports by their original imported form
    group_by(exporter_iso3c, hs6_original) %>%
    summarize(foreign_exports_reweighted = sum(foreign_exports_reweighted)) %>%
    ungroup() %>%
    left_join(
      V1_long %>%
        group_by(hs6) %>%
        summarize(avg_live_weight_cf = mean(live_weight_cf)),
      by = c("hs6_original"="hs6")
    ) %>%
    mutate(foreign_exports_reweighted_live = foreign_exports_reweighted * avg_live_weight_cf)
  
  foreign_consumption <- foreign_exports %>%
    rename(iso3c = exporter_iso3c) %>%
    full_join(
      import_production,
      by = c("iso3c"="importer_iso3c", "hs6_original"="hs6")
    ) %>%
    mutate(foreign_consumption_t = import_prod - foreign_exports_reweighted) %>%
    rename(hs6 = hs6_original) %>%
    # Note that for small flows (less than 0.01) do not have an import production record
    filter(!is.na(import_prod)) %>%
    # Note there are some flows where product was imported but
    # there are no corresponding foreign exports
    filter(!is.na(foreign_exports_reweighted))
  
  artis_import_props <- artis %>%
    group_by(importer_iso3c, hs6, sciname, habitat, method, source_country_iso3c, year) %>%
    summarize(imports_live = sum(live_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(importer_iso3c, hs6) %>%
    mutate(total = sum(imports_live, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(prop_import = imports_live / total)
  
  foreign_consumption <- foreign_consumption %>%
    select(iso3c, hs6, foreign_consumption_t, year) %>%
    left_join(
      artis_import_props %>%
        select(-c(imports_live, total)),
      by = c("iso3c"="importer_iso3c", "hs6", "year")
    ) %>%
    mutate(foreign_consumption_resolved = foreign_consumption_t * prop_import)
  
  # Bring together a complete consumption dataframe including domestic and foreign consumption
  complete_consumption <- domestic_consumption %>%
    mutate(dom_source = "domestic",
           source_country_iso3c = iso3c) %>%
    rename(consumption_live_t = dom_consumption) %>%
    select(-c(dom_exports_live, production)) %>%
    bind_rows(
      foreign_consumption %>%
        mutate(dom_source = "foreign") %>%
        select(-c(foreign_consumption_t, prop_import)) %>%
        rename(consumption_live_t = foreign_consumption_resolved)
    )
  
  write.csv(
    complete_consumption,
    file.path(outdir, paste("complete_consumption_raw_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
  
  # Summary Consumption (legacy code matching file)
  summary_consumption <- complete_consumption %>%
    group_by(iso3c, hs6, sciname, habitat, method, year, dom_source) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = dom_source, values_from = consumption_live_t, values_fill = 0) %>%
    rename(domestic_consumption_t = domestic,
           foreign_consumption_t = foreign) %>%
    mutate(supply = domestic_consumption_t + foreign_consumption_t)
  
  write.csv(
    summary_consumption,
    file.path(outdir, paste("summary_consumption_raw_", estimate_type, ".csv", sep = "")),
    row.names = FALSE  
  )
  
  # Removing non human consumption codes
  if (end_use == "human") {
    complete_consumption <- complete_consumption %>%
      filter(!(hs6 %in% c("230120", "051191", "030110", "030111", "030119")))
  }
  
  # Percent of consumption that is domestic vs foreign sourced by country
  percent_consumption <- complete_consumption %>%
    group_by(iso3c, dom_source) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(iso3c) %>%
    mutate(total_consumption_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    mutate(percent = 100 * consumption_live_t / total_consumption_t)
  
  # Standardizing all consumption to be at most 100 kg per capita
  consumption_per_capita <- complete_consumption %>%
    group_by(iso3c, year) %>%
    summarize(total_consumption_kg = 1000 * sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(
      pop_data,
      by = c("iso3c", "year")
    ) %>%
    mutate(consumption_kg_per_capita = total_consumption_kg / pop)
  
  non_outliers <- consumption_per_capita %>%
    filter(consumption_kg_per_capita <= 100)
  
  outliers <- consumption_per_capita %>%
    filter(consumption_kg_per_capita > 100) %>%
    # 100 kg * pop / (1000 kg / 1 tonne)
    mutate(correct_total_consumption_t = (100 * pop) / 1000)
  
  outlier_consumption_edits <- complete_consumption %>%
    filter(iso3c %in% unique(outliers$iso3c)) %>%
    group_by(iso3c) %>%
    mutate(country_total_consumption_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(flow_prop = consumption_live_t / country_total_consumption_t) %>%
    left_join(
      outliers %>%
        select(iso3c, year, correct_total_consumption_t),
      by = c("iso3c", "year")
    ) %>%
    mutate(consumption_live_t = correct_total_consumption_t * flow_prop) %>%
    select(-c(country_total_consumption_t, flow_prop, correct_total_consumption_t))
  
  consumption_100kg_per_capita <- complete_consumption %>%
    filter(!(iso3c %in% unique(outliers$iso3c))) %>%
    bind_rows(outlier_consumption_edits)
  
  # Test all countries have at most 100 kg per capita consumption
  test_per_capita_consumption <- consumption_100kg_per_capita %>%
    group_by(iso3c, year) %>%
    summarize(total_consumption_kg = 1000 * sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(
      pop_data,
      by = c("iso3c", "year")
    ) %>%
    mutate(consumption_kg_per_capita = total_consumption_kg / pop) %>%
    filter(consumption_kg_per_capita > 100)
    
  if (nrow(test_per_capita_consumption) > 100) {
    warning("ERROR: 100 kg consumption per capita threshold exceeded")
  }
  
  test_percent_match <- consumption_100kg_per_capita %>%
    group_by(iso3c, dom_source) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(iso3c) %>%
    mutate(total_consumption_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    mutate(new_percent = 100 * consumption_live_t / total_consumption_t) %>%
    select(-c(total_consumption_t, consumption_live_t)) %>%
    left_join(
      percent_consumption %>%
        select(-c(total_consumption_t, consumption_live_t)),
      by = c("iso3c", "dom_source")
    ) %>%
    mutate(difference = percent - new_percent) %>%
    filter(abs(difference) > 1e-9)
  
  if (nrow(test_percent_match) > 0) {
    warning("ERROR: 100 kg consumption per capita correction not keeping previous domestic/foreign proportions")
  }
  
  test_props <- consumption_100kg_per_capita %>%
    group_by(iso3c) %>%
    mutate(country_total_consumption_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(new_flow_prop = consumption_live_t / country_total_consumption_t) %>%
    left_join(
      complete_consumption %>%
        group_by(iso3c) %>%
        mutate(country_total_consumption_t = sum(consumption_live_t, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(flow_prop = consumption_live_t / country_total_consumption_t)
    ) %>%
    mutate(difference = flow_prop - new_flow_prop) %>%
    filter(abs(difference) > 1e-9)
  
  if (nrow(test_props) > 0) {
    warning("ERROR: 100 kg consumption per capita correction not keeping previous domestic/foreign proportions")
  }
  
  write.csv(
    consumption_100kg_per_capita,
    file.path(outdir, paste("complete_consumption_100kg_per_capita_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
  
  # Summary Consumption (legacy code matching file)
  summary_consumption_100kg_per_capita <- consumption_100kg_per_capita %>%
    group_by(iso3c, hs6, sciname, habitat, method, year, dom_source) %>%
    summarize(consumption_live_t = sum(consumption_live_t, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = dom_source, values_from = consumption_live_t, values_fill = 0) %>%
    rename(domestic_consumption_t = domestic,
           foreign_consumption_t = foreign) %>%
    mutate(supply = domestic_consumption_t + foreign_consumption_t)
  
  write.csv(
    summary_consumption_100kg_per_capita,
    file.path(outdir, paste("summary_consumption_", estimate_type, ".csv", sep = "")),
    row.names = FALSE
  )
  
}
