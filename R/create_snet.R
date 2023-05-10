#' @export
create_snet <- function(baci_data_analysis_year, export_source_weights,
                        reweight_W_long, reweight_X_long, V1_long, hs_clade_match, num_cores, snet_threshold = 0.1) {
  # Creates an ARTIS snet that goes back 2 stages in the supply chain
  # specifically further resolving foreign exports down
  
  # Dividing total product weight exports into domestic, foreign, and error exports
  # returns dataframe of the w:
  #   importer_iso3c, exporter_iso3c, hs6, product_weight_t, source_type and source_prop
  #   (where source type is domestic/foreign/error and source prop is proportion of export by dom_source)
  export_sources <- baci_data_analysis_year %>%
    # proportion of domestic/foreign/error exports determined by create_export_source_weights function
    left_join(
      export_source_weights %>%
        pivot_longer(c("domestic_weight", "foreign_weight", "error_weight"),
                     names_to = "source_type", values_to = "source_prop") %>%
        mutate(source_type = str_extract(source_type, ".+(?=_weight)")),
      by = c("exporter_iso3c" = "iso3c", "hs6")
    ) %>%
    filter(source_prop > 0) %>%
    mutate(product_weight_t = product_weight_t * source_prop)
  
  # Calculating all bilateral trade that originated from domestic exports
  dom_exp <- export_sources %>%
    # Formatting domestic export to match final ARTIS structure
    select(-c(source_prop)) %>%
    filter(source_type == "domestic") %>%
    mutate(source_country_iso3c = exporter_iso3c) %>%
    # Joining on proportion of species production going into each product by country
    left_join(
      reweight_X_long %>%
        select(-c(hs_version, year)),
      by = c("exporter_iso3c" = "iso3c", "hs6")
    ) %>%
    # disaggregating product weights from final hs6 form to hs6 and sciname
    mutate(product_weight_t = product_weight_t * reweighted_X) %>%
    select(-c(reweighted_X))
  
  # Calculating all bilateral trade that originated from error exports
  error_exp <- export_sources %>%
    select(-c(source_prop)) %>%
    filter(source_type == "error") %>%
    # Joining on clade matches to hs6 codes
    # Note: error export hs6 to sciname conversions are 1 to 1
    #       since there is only 1 match per code
    left_join(
      hs_clade_match %>%
        select(-c(classification_level)),
      by = c("hs6"="Code")
    )  %>%
    # matching final ARTIS structure
    rename(SciName = hs_clade) %>%
    mutate(source_country_iso3c = "unknown")
  
  # Separating all bilateral trade that originated from foreign exports for further resolution
  foreign_exports <- export_sources %>%
    filter(source_type == "foreign") %>%
    select(-c(source_type, source_prop))
  
  # Proportion of imports by country and hs6
  # that came from domestic/foreign/error exports
  import_props <- export_sources %>%
    group_by(importer_iso3c, exporter_iso3c, hs6, source_type) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    # get total hs6 imports by importer
    group_by(importer_iso3c, hs6) %>%
    mutate(import_hs6_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    # get proportion of import by importer and hs6 that originated from domestic/foreign/error exports
    mutate(import_prop = product_weight_t / import_hs6_t)
  
  # Resolving foreign exports 1 step back in the supply chain
  first_resolved_exp <- resolve_foreign_exp(foreign_exports, reweight_W_long, import_props,
                                            hs_clade_match)
  
  first_dom_exp <- first_resolved_exp[[1]]
  first_error_exp <- first_resolved_exp[[2]]
  first_foreign_exp <- first_resolved_exp[[3]]
  first_unresolved_foreign_exp <- first_resolved_exp[[4]]
  
  # remove and clear memory
  rm(first_resolved_exp)
  gc()
  
  # Creating link between first and second stage of the supply chain
  foreign_export_original_link <- first_foreign_exp %>%
    # getting total by re exporter (intermediate exporter), current source country, hs6 original
    group_by(re_exporter_iso3c, exporter_iso3c, hs6_original) %>%
    mutate(total = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    # calculating proportion of flow over
    # total by re exporter (intermediate exporter), current source country, hs6 original
    mutate(prop_flow = product_weight_t / total) %>%
    select(importer_iso3c, re_exporter_iso3c, exporter_iso3c, hs6_processed, hs6_original, prop_flow) %>%
    distinct()
  
  # Structuring second stage of supply chain for further resolution
  # resulting dataframe will be summarized by
  # importer (re exporter) exporter (current source country) and hs6 (current hs6 original)
  reformatted_foreign_exports <- first_foreign_exp %>%
    rename(hs6 = hs6_original) %>%
    group_by(re_exporter_iso3c, exporter_iso3c, hs6) %>%
    summarize(product_weight_t = sum(product_weight_t)) %>%
    ungroup() %>%
    rename(importer_iso3c = re_exporter_iso3c)
  
  # Resolving foreign exports at 2 steps back in supply chain
  second_resolved_exp <- resolve_foreign_exp(reformatted_foreign_exports, reweight_W_long,
                                             import_props, hs_clade_match)
  
  second_dom_exp <- second_resolved_exp[[1]]
  second_error_exp <- second_resolved_exp[[2]]
  second_foreign_exp <- second_resolved_exp[[3]]
  second_unresolved_foreign_exp <- second_resolved_exp[[4]]
  
  rm(second_resolved_exp)
  gc()
  
  # linking original importer re exporter and final source country
  # removes intermediate source country found in 1st foreign exports resolution
  relinked_second_dom_exp <- foreign_export_original_link %>%
    rename(hs6_final = hs6_processed) %>%
    rename(hs6_processed = hs6_original) %>%
    left_join(
      second_dom_exp %>%
        rename(source_country_iso3c = exporter_iso3c) %>%
        rename(exporter_iso3c = re_exporter_iso3c) %>%
        rename(re_exporter_iso3c = importer_iso3c),
      by = c("re_exporter_iso3c", "exporter_iso3c", "hs6_processed")
    ) %>%
    # multiply out proportion of overall re exporter and exporter flows from first foreign exp
    mutate(product_weight_t = product_weight_t * prop_flow) %>%
    group_by(importer_iso3c, re_exporter_iso3c, source_country_iso3c, hs6_final, hs6_original) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    # filter out flows that might have become exclusively
    # foreign sourced from foreign or foreign sourced from error
    filter(product_weight_t > 0)
  
  # Building out foreign domestic exports
  overall_foreign_domestic_exports <- first_dom_exp %>%
    rename(hs6_final = hs6_processed) %>%
    bind_rows(relinked_second_dom_exp %>%
                rename(exporter_iso3c = source_country_iso3c)) %>%
    group_by(importer_iso3c, re_exporter_iso3c, exporter_iso3c, hs6_final, hs6_original) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  # Breakdown foreign exports sourced from domestic exports into scinames
  foreign_domestic_sciname_exp <- data.frame()
  
  # Creating cluster of cores for resolving scinames for foreign exports sourced from domestic exports
  sciname_cl <- makeCluster(num_cores, type="FORK")
  # Note: line below redirects print output to different files per cluster worker
  # clusterEvalQ(sciname_cl, sink(file.path(getwd(), paste("qa/foreign_dom_sciname_", Sys.getpid(), ".txt", sep = ""))))
  registerDoParallel(sciname_cl)  # use multicore, set to the number of our cores
  # inital set up for sciname resolution of foreign exports sourced from domestic exports
  foreign_domestic_sciname_exp <- data.frame()
  source_exporters <- sort(unique(overall_foreign_domestic_exports$exporter_iso3c))
  # Parallelizing resolving scinames for foreign exports
  foreign_domestic_sciname_exp <- foreach(i = 1:length(source_exporters), .combine = rbind) %dopar% {
    
    curr_source_exporter <- source_exporters[i]
    
    overall_foreign_domestic_exports %>%
      filter(exporter_iso3c == curr_source_exporter) %>%
      # Get scinames by most resolved source country
      left_join(
        reweight_X_long %>%
          select(-c(hs_version, year)),
        by = c("exporter_iso3c"="iso3c", "hs6_original"="hs6")
      ) %>%
      mutate(product_weight_t = product_weight_t * reweighted_X) %>%
      # Resummarizing scinames by final hs6 code (was previously resolved at hs6 original level)
      group_by(importer_iso3c, re_exporter_iso3c, exporter_iso3c, hs6_final, SciName) %>%
      summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
      ungroup()
    
  }
  
  # Free up cluster workers
  stopCluster(sciname_cl)
  
  # first unresolved foreign exports get categorized as being sourced from error export
  first_unresolved_foreign_sciname <- first_unresolved_foreign_exp %>%
    # scinames are all resolved by hs clade matches
    left_join(
      hs_clade_match %>%
        rename(SciName = hs_clade) %>%
        mutate(SciName = paste(SciName, "unknown", "unknown", sep = "_")) %>%
        select(-c(classification_level)),
      by = c("hs6_original"="Code")
    ) %>%
    # re summarizing by final hs6 code and hs clade matches
    group_by(importer_iso3c, re_exporter_iso3c, hs6_processed, SciName) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    # rename to fit final ARTIS structure
    rename(exporter_iso3c = re_exporter_iso3c,
           hs6 = hs6_processed) %>%
    mutate(source_country_iso3c = "unknown")
  
  
  # Re structuring second unresolved foreign export to 
  second_unresolved_foreign_exp <- second_unresolved_foreign_exp %>%
    # rename for final ARTIS structure
    rename(source_country_iso3c = exporter_iso3c) %>%
    rename(exporter_iso3c = re_exporter_iso3c) %>%
    rename(re_exporter_iso3c = importer_iso3c) %>%
    select(-source_type) %>%
    mutate(source_country_iso3c = "unknown") %>%
    # re summarizing to link with foreign export original link
    group_by(re_exporter_iso3c, exporter_iso3c, source_country_iso3c, hs6_processed, hs6_original) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup()
  
  # second unresolved foreign exports needs to get re-linked and
  # get categorized as being sourced from error export
  second_unresolved_foreign_sciname <- foreign_export_original_link %>%
    rename(hs6_final = hs6_processed) %>%
    rename(hs6_processed = hs6_original) %>%
    # linking first and second stage of supply chain
    left_join(
      second_unresolved_foreign_exp,
      by = c("re_exporter_iso3c", "exporter_iso3c", "hs6_processed")
    ) %>%
    # disaggregating flows between stages
    mutate(product_weight_t = product_weight_t * prop_flow) %>%
    select(-prop_flow) %>%
    filter(!is.na(source_country_iso3c)) %>%
    # re summarize for clade matching
    group_by(importer_iso3c, re_exporter_iso3c, source_country_iso3c, hs6_final, hs6_original) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    # adding hs clade matches
    left_join(
      hs_clade_match %>%
        rename(SciName = hs_clade) %>%
        mutate(SciName = paste(SciName, "unknown", "unknown", sep = "_")) %>%
        select(-classification_level),
      by = c("hs6_original"="Code")
    ) %>%
    # re summarizing for final ARTIS structure
    group_by(importer_iso3c, re_exporter_iso3c, source_country_iso3c, hs6_final, SciName) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(exporter_iso3c = re_exporter_iso3c,
           hs6 = hs6_final)
  
  # Restructuring foreign export sourced from error to link back to first stage of supply chain
  second_error_exp <- second_error_exp %>%
    rename(source_country_iso3c = exporter_iso3c) %>%
    mutate(source_country_iso3c = "unknown") %>%
    rename(exporter_iso3c = re_exporter_iso3c) %>%
    rename(re_exporter_iso3c = importer_iso3c) %>%
    # all exports sourced from error exports get scinames by hs clade match
    left_join(
      hs_clade_match %>%
        rename(SciName = hs_clade) %>%
        mutate(SciName = paste(SciName, "unknown", "unknown", sep = "_")) %>%
        select(-classification_level),
      by = c("hs6_original"="Code")
    )
  # relinking second foreign error with first stage of supply chain
  relinked_second_foreign_error <- foreign_export_original_link %>%
    rename(hs6_final = hs6_processed) %>%
    rename(hs6_processed = hs6_original) %>%
    left_join(
      second_error_exp,
      by = c("re_exporter_iso3c", "exporter_iso3c", "hs6_processed")
    ) %>%
    filter(!is.na(source_country_iso3c)) %>%
    # disaggregating flows when linking back to final hs6
    mutate(product_weight_t = product_weight_t * prop_flow) %>%
    # restructuring for final ARTIS structure
    group_by(importer_iso3c, re_exporter_iso3c, source_country_iso3c, hs6_final, SciName) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(exporter_iso3c = re_exporter_iso3c,
           hs6 = hs6_final)
  
  # foreign exports at final stage of supply chain get classified
  # as getting sourced from error exports
  second_foreign_exp <- second_foreign_exp %>%
    rename(source_country_iso3c = exporter_iso3c) %>%
    mutate(source_country_iso3c = "unknown") %>%
    rename(exporter_iso3c = re_exporter_iso3c) %>%
    rename(re_exporter_iso3c = importer_iso3c) %>%
    # adding scinames through hs clade matches
    left_join(
      hs_clade_match %>%
        rename(SciName = hs_clade) %>%
        mutate(SciName = paste(SciName, "unknown", "unknown", sep = "_")) %>%
        select(-classification_level),
      by = c("hs6_original"="Code")
    )
  
  # relinking final foreign exports back to first stage of supply chain
  relinked_second_foreign_foreign <- foreign_export_original_link %>%
    rename(hs6_final = hs6_processed) %>%
    rename(hs6_processed = hs6_original) %>%
    # connecting final foreign exports
    left_join(
      second_foreign_exp,
      by = c("re_exporter_iso3c", "exporter_iso3c", "hs6_processed")
    ) %>%
    filter(!is.na(source_country_iso3c)) %>%
    # disaggregating foreign exports based on final hs6 code
    mutate(product_weight_t = product_weight_t * prop_flow) %>%
    # re summarizing to fit final ARTIS structure
    group_by(importer_iso3c, re_exporter_iso3c, source_country_iso3c, hs6_final, SciName) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(exporter_iso3c = re_exporter_iso3c,
           hs6 = hs6_final)
  
  # first step back of foreign exports sourced from error exports
  first_error_exp <- first_error_exp %>%
    rename(source_country_iso3c = exporter_iso3c) %>%
    mutate(source_country_iso3c = "unknown") %>%
    rename(exporter_iso3c = re_exporter_iso3c) %>%
    # all scinames come from hs clade matches
    left_join(
      hs_clade_match %>%
        rename(SciName = hs_clade) %>%
        mutate(SciName = paste(SciName, "unknown", "unknown", sep = "_")) %>%
        select(-classification_level),
      by = c("hs6_original"="Code")
    ) %>%
    # restructuring for final ARTIS structure
    group_by(importer_iso3c, exporter_iso3c, source_country_iso3c, hs6_processed, SciName) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(hs6 = hs6_processed)
  
  # Combining all foreign exports
  overall_foreign_exports <- foreign_domestic_sciname_exp %>%
    # starts with all foreign exports resolved to be from domestic exports
    rename(source_country_iso3c = exporter_iso3c) %>%
    rename(exporter_iso3c = re_exporter_iso3c,
           hs6 = hs6_final) %>%
    # foreign exports that were resolved in the first stage to be from error exports
    bind_rows(first_error_exp) %>%
    # first stage unresolved foreign exports treated as sourced from error exports
    bind_rows(first_unresolved_foreign_sciname) %>%
    # second stage relinked foreign exports sourced from error exports
    bind_rows(relinked_second_foreign_error) %>%
    # second stage unresolved foreign exports treated as sourced from error exports
    bind_rows(second_unresolved_foreign_sciname) %>%
    # second stage foreign exports deemed sourced from foreign exports
    # treated as sourced from error exports
    bind_rows(relinked_second_foreign_foreign) %>%
    group_by(importer_iso3c, exporter_iso3c, source_country_iso3c, hs6, SciName) %>%
    summarize(product_weight_t = sum(product_weight_t, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(dom_source = "foreign")
  
  # calculate error V1_long
  V1_long_error <- V1_long %>%
    group_by(hs6) %>%
    summarize(live_weight_cf = mean(live_weight_cf, na.rm = TRUE)) %>%
    ungroup()
  
  # reformatting error exports scinames to have habitat and method
  error_exp <- error_exp %>%
    rename(dom_source = source_type) %>%
    rename(sciname = SciName) %>%
    mutate(sciname = gsub("\\.", " ", sciname),
           habitat = "unknown",
           method = "unknown") %>%
    left_join(
      V1_long_error,
      by = c("hs6")
    ) %>%
    mutate(live_weight_t = product_weight_t * live_weight_cf) %>%
    select(-live_weight_cf) %>%
    # filter out flows that we consider to be zero, default 0.1
    filter(product_weight_t >= snet_threshold)
  
  # Bringing together all exports sourced from domestic, error and foreign
  s_net <- dom_exp %>%
    rename(dom_source = source_type) %>%
    bind_rows(overall_foreign_exports) %>%
    separate(SciName, c("sciname", "habitat", "method"), sep = "_") %>%
    mutate(sciname = gsub("\\.", " ", sciname)) %>%
    # filter out flows that we consider to be zero, default 0.1
    filter(product_weight_t >= snet_threshold) %>%
    # adding live weight conversion factors
    left_join(
      V1_long %>%
        separate(SciName, c("sciname", "habitat", "method"), sep = "_") %>%
        mutate(sciname = gsub("\\.", " ", sciname)),
      by = c("hs6", "sciname", "habitat", "method")
    ) %>%
    mutate(live_weight_t = product_weight_t * live_weight_cf) %>%
    select(-live_weight_cf) %>%
    bind_rows(error_exp) %>%
    # Case when foreign exports are sourced from error exports or unresolved
    left_join(
      V1_long_error,
      by = c("hs6")
    ) %>%
    mutate(live_weight_t = case_when(
      is.na(live_weight_t) ~ product_weight_t * live_weight_cf,
      TRUE ~ live_weight_t
    )) %>%
    select(-live_weight_cf)
  
  return(s_net)
}