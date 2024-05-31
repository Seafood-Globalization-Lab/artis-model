#' @export
get_snet <- function(quadprog_dir, cvxopt_dir, datadir, outdir, num_cores = 10,
                     hs_version = NA, test_years = c(), prod_type = "FAO") {
  
  setup_values <- initial_variable_setup(datadir, outdir, hs_version, test_years, prod_type)
  full_analysis_start <- setup_values[[1]]
  file.date <- setup_values[[2]]
  analysis_info <- setup_values[[3]]
  analysis_setup <- setup_values[[4]]
  df_years <- setup_values[[5]]
  prod_data <- setup_values[[6]]
  prod_taxa_classification <- setup_values[[7]]
  hs_taxa_match <- setup_values[[8]]
  hs_taxa_CF_match <- setup_values[[9]]
  hs_hs_match <- setup_values[[10]]
  coproduct_codes <- setup_values[[11]]
  Xq <- setup_values[[12]]
  X_rows <- setup_values[[13]]
  X_cols <- setup_values[[14]]
  V1 <- setup_values[[15]]
  V2 <- setup_values[[16]]
  W_rows <- setup_values[[17]]
  W_cols <- setup_values[[18]]
  sc_n <- setup_values[[19]]
  cc_m <- setup_values[[20]]
  HS_year_rep <- setup_values[[21]]
  analysis_years_rep <- setup_values[[22]]
  hs_dir <- setup_values[[23]]
  
  rm(setup_values)
  
  # This is used for consumption and diversity calculations post a complete ARTIS
  V1_long <- data.frame(V1) %>%
    mutate(hs6 = colnames(V2)) %>% 
    pivot_longer(cols = -hs6, names_to = "SciName", values_to = "live_weight_cf") %>%
    filter(live_weight_cf > 0) %>%
    # Transform conversion factors to represent conversion from product to live
    mutate(live_weight_cf = 1/live_weight_cf)
  
  write.csv(V1_long, file.path(outdir, paste("HS", hs_version, "/V1_long_HS", hs_version, ".csv", sep = "")), row.names = FALSE)
  
  # Product-to-Product
  # This is going from original product weight to new product weight
  # (ie represents processing losses, all values should be <= 1)
  V2_long <- data.frame(V2) %>%
    mutate(to_hs6 = as.character(cc_m)) %>%
    pivot_longer(cols = -to_hs6, names_to = "from_hs6", values_to = "product_cf") %>%
    mutate(from_hs6 = substr(from_hs6, 2, str_length(from_hs6))) %>%
    filter(product_cf > 0)
  
  write.csv(V2_long, file.path(outdir, paste("HS", hs_version, "/V2_long_HS", hs_version, ".csv", sep = "")), row.names = FALSE)
  #-------------------------------------------------------------------------------
  fao_pop <- read.csv(file.path(datadir, "fao_annual_pop.csv"))
  code_max_resolved <- read.csv(file.path(datadir, "code_max_resolved.csv"))
  
  # Non-human codes
  non_human_codes <- c("230120", "051191", "030110", "030111", "030119")

  # Loop through all analysis years for a given HS version
  for (j in 1:nrow(analysis_years_rep)) {

    analysis_year <- analysis_years_rep$analysis_year[j]
    # output folder
    hs_analysis_year_dir <- file.path(outdir, hs_dir, analysis_year)

    #-----------------------------------------------------------------------------
    # Step 4: Load trade (BACI) data and standardize countries between production and trade data
    baci_data_analysis_year <- read.csv(
      file.path(
        datadir,
        paste("standardized_baci_seafood_hs",
              HS_year_rep, "_y", analysis_year, ".csv", sep = "")
      )) %>%
      # pad hs6 with 0s
      mutate(hs6 = as.character(hs6)) %>%
      mutate(hs6 = if_else(
        str_detect(hs6, "^30"),
        true = str_replace(hs6, pattern = "^30", replacement = "030"),
        if_else(str_detect(hs6, "^511"),
                true = str_replace(hs6, pattern = "^511", replacement = "0511"),
                false = hs6)))

    baci_data_analysis_year <- baci_data_analysis_year %>%
      select(importer_iso3c, exporter_iso3c, hs6, total_q)
    #-----------------------------------------------------------------------------

    # Read in individual country solutions and combine into a list
    quadprog_output_files <- list.files(file.path(quadprog_dir, hs_dir, analysis_year))
    quadprog_output_files <- lapply(quadprog_output_files,
                                    FUN = function(x) file.path(
                                      quadprog_dir, hs_dir, analysis_year, x))

    cvxopt_output_files <- list.files(file.path(cvxopt_dir, hs_dir, analysis_year))
    cvxopt_output_files <- lapply(cvxopt_output_files,
                                  FUN = function(x) file.path(
                                    cvxopt_dir, hs_dir, analysis_year, x))

    output_files <- c(quadprog_output_files, cvxopt_output_files)
    solve_country_files <- output_files[grepl(pattern = "_country-est_", output_files) &
                                          grepl(pattern = analysis_year, output_files) &
                                          grepl(pattern = HS_year_rep, output_files)]

    country_est <- vector(mode = "list", length = length(solve_country_files))
    
    for (i in 1:length(solve_country_files)){
      country_est[[i]] <- readRDS(solve_country_files[[i]])
    }

    # Add country names to country_est
    file_countries <- unlist(lapply(solve_country_files,
                                    FUN = function(x) substr(
                                      str_extract(x, "country-est_[A-Z]{3}_"), 13, 15)))
    names(country_est) <- file_countries
    
    # Add row and column names to X and W
    for(i in 1:length(country_est)){
      if (is.matrix(country_est[[i]]$X)) {
        colnames(country_est[[names(country_est)[i]]]$X) <- X_cols
        rownames(country_est[[names(country_est)[i]]]$X) <- paste(names(country_est)[i],X_rows,sep="_")
        # For countries with no production (i.e., NA for X), insert matrix of zeroes
      } else {
        country_est[[i]]$X <- matrix(0, ncol = length(X_cols), nrow = length(X_rows))
        colnames(country_est[[names(country_est)[i]]]$X) <- X_cols
        rownames(country_est[[names(country_est)[i]]]$X) <- paste(names(country_est)[i],X_rows,sep="_")
      }
      if (is.matrix(country_est[[i]]$W)){
        colnames(country_est[[names(country_est)[i]]]$W) <- paste(names(country_est)[i],W_cols,sep="_")
        rownames(country_est[[names(country_est)[i]]]$W) <- paste(names(country_est)[i],W_rows,sep="_")
        # For countries with no production (i.e., NA for X), insert matrix of zero
      } else {
        country_est[[i]]$W <- matrix(0, ncol = length(W_cols), nrow = length(W_rows))
        colnames(country_est[[names(country_est)[i]]]$W) <- paste(names(country_est)[i],W_cols,sep="_")
        rownames(country_est[[names(country_est)[i]]]$W) <- paste(names(country_est)[i],W_rows,sep="_")
      }
    }
    saveRDS(country_est, file.path(hs_analysis_year_dir, paste(file.date, "_all-country-est_", analysis_year, "_HS", HS_year_rep, ".RDS", sep = "")))
    # We don't believe the final product form provided by solve countries solutions for consumption

    #---------------------------------------------------------------------------

    # Step 6: Make S_net assuming commodity processing only occurs one trade-flow back
    rm(list=ls()[!(ls() %in% c("baci_data_analysis_year", "country_est", "V1",
                               "V2", "V1_long", "countries_to_analyze","analysis_year",
                               "coproduct_codes", "fao_pop", "hs_dir",
                               "analysis_setup", analysis_setup,
                               "analysis_info", analysis_info))])
    gc()
    
    # Determine most specific clade of each HS code (but if clade is not reported
    # in production data (i.e., hs_taxa_match$SciName), return NA)
    # To match to clade, even if not reported in production data, set match_to_prod to FALSE
    hs_clade_match <- match_hs_to_clade(
      hs_taxa_match = read.csv(
        file.path(datadir,
                  paste("hs-taxa-match_HS", HS_year_rep, ".csv", sep = ""))) %>%
        select(-c(sciname_habitat, code_habitat)),
      prod_taxa_classification = read.csv(file.path(datadir, "clean_fao_taxa.csv")),
      match_to_prod = FALSE
    ) %>%
      # pad HS codes with zeroes
      mutate(Code = as.character(Code)) %>%
      mutate(Code = if_else(
        str_detect(Code, "^30"),
        true = str_replace(Code, pattern = "^30", replacement = "030"),
        if_else(str_detect(Code, "^511"),
                true = str_replace(Code, pattern = "^511", replacement = "0511"),
                false = Code))) %>%
      # Filter down to just the codes in cc_m (recreated above from country_est)
      filter(Code %in% cc_m)
    
    write.csv(hs_clade_match,
              file.path(hs_analysis_year_dir, "hs_clade_match.csv"),
              row.names = FALSE)

    # Create new list of countries_to_analyze using only countries with
    # solve_qp solutions (i.e., names(country_est)), some countries have
    # no solution, so using the original list causes error in lapply below
    countries_to_analyze <- names(country_est)

    reweight_X_long <- create_reweight_X_long(country_est, V1, V2)
    
    reweight_X_long <- reweight_X_long %>%
      mutate(hs_version = paste("HS", HS_year_rep, sep = ""),
             year = analysis_year)

    write.csv(reweight_X_long, 
              file.path(hs_analysis_year_dir,
                        paste("reweight_X_long_", analysis_year, "_HS",
                              HS_year_rep, ".csv", sep = "")),
              row.names = FALSE)
    
    W_long <- create_W_long(country_est, num_cores)
    
    write.csv(W_long,
              file.path(hs_analysis_year_dir,
                        paste("W_long_", analysis_year, "_HS", HS_year_rep, ".csv", sep = "")),
              row.names = FALSE)
    
    # Restructure BACI data to specify that it handles product weight
    baci_data_analysis_year <- baci_data_analysis_year %>%
      rename(product_weight_t = total_q) %>%
      filter(!(hs6 %in% coproduct_codes))
    
    reweight_W_long <- create_reweight_W_long(W_long, baci_data_analysis_year)
    
    write.csv(reweight_W_long,
              file.path(hs_analysis_year_dir,
                        paste("reweight_W_long_", analysis_year, "_HS",
                              HS_year_rep, ".csv", sep = "")),
              row.names = FALSE)
    
    # returns the export and consumption weights by
    #   iso3c, hs6, dom_source
    export_source_weights_midpoint <- create_export_source_weights(
      baci_data_analysis_year, countries_to_analyze, country_est, V1, V2, cc_m,
      coproduct_codes, dom_source_weight = "midpoint"
    )
    
    export_source_weights_max <- create_export_source_weights(
      baci_data_analysis_year, countries_to_analyze, country_est, V1, V2, cc_m,
      coproduct_codes, dom_source_weight = "max"
    )
    
    export_source_weights_min <- create_export_source_weights(
      baci_data_analysis_year, countries_to_analyze, country_est, V1, V2, cc_m,
      coproduct_codes, dom_source_weight = "min"
    )
    
    gc()
    
    s_net_midpoint <- create_snet(baci_data_analysis_year, export_source_weights_midpoint,
                                  reweight_W_long, reweight_X_long, V1_long,
                                  hs_clade_match, num_cores, hs_analysis_year_dir, estimate_type = "midpoint") %>%
      mutate(hs_version = paste("HS", HS_year_rep, sep = ""),
             year = analysis_year)

    s_net_max <- create_snet(baci_data_analysis_year, export_source_weights_max,
                             reweight_W_long, reweight_X_long, V1_long,
                             hs_clade_match, num_cores, hs_analysis_year_dir, estimate_type = "max") %>%
      mutate(hs_version = paste("HS", HS_year_rep, sep = ""),
             year = analysis_year)

    s_net_min <- create_snet(baci_data_analysis_year, export_source_weights_min,
                             reweight_W_long, reweight_X_long, V1_long,
                             hs_clade_match, num_cores, hs_analysis_year_dir, estimate_type = "min") %>%
      mutate(hs_version = paste("HS", HS_year_rep, sep = ""),
             year = analysis_year)

    # Save full s_net
    write.csv(
      s_net_midpoint,
      file.path(hs_analysis_year_dir,
                paste(file.date, "_S-net_raw_midpoint_", analysis_year, "_HS",
                      HS_year_rep, ".csv", sep = "")),
      row.names = FALSE
    )
    write.csv(
      s_net_max,
      file.path(hs_analysis_year_dir,
                paste(file.date, "_S-net_raw_max_", analysis_year, "_HS",
                      HS_year_rep, ".csv", sep = "")),
      row.names = FALSE
    )
    write.csv(
      s_net_min,
      file.path(hs_analysis_year_dir,
                paste(file.date, "_S-net_raw_min_", analysis_year, "_HS",
                      HS_year_rep, ".csv", sep = "")),
      row.names = FALSE
    )
    
    X_long <- create_X_long(country_est, num_cores)
    
    write.csv(X_long, file.path(hs_analysis_year_dir, "X_long.csv"), row.names = FALSE)
    
    # Calculate consumption-----------------------------------------------------
    prod_data_analysis_year <- prod_data %>%
      filter(year == analysis_year)
    
    curr_hs <- paste("HS", hs_version, sep = "")
    
    pop <- fao_pop %>%
      filter(year == analysis_year)
    
    consumption_mid <- calculate_consumption(s_net_mid, prod_data_analysis_year,
                                             analysis_year, curr_hs, W_long, X_long,
                                             pop, code_max_resolved)
    
    write.csv(consumption_mid,
              file.path(
                hs_analysis_year_dir,
                paste(file.date, "_consumption_midpoint_", analysis_year,
                      "_HS", HS_year_rep, ".csv", sep = "")),
              row.names = FALSE)
    
    consumption_max <- calculate_consumption(s_net_max, prod_data_analysis_year,
                                             analysis_year, curr_hs, W_long, X_long,
                                             pop, code_max_resolved)
    
    write.csv(consumption_max,
              file.path(
                hs_analysis_year_dir,
                paste(file.date, "_consumption_max_", analysis_year,
                      "_HS", HS_year_rep, ".csv", sep = "")),
              row.names = FALSE)
    
    consumption_min <- calculate_consumption(s_net_min, prod_data_analysis_year,
                                             analysis_year, curr_hs, W_long, X_long,
                                             pop, code_max_resolved)
    
    write.csv(consumption_min,
              file.path(
                hs_analysis_year_dir,
                paste(file.date, "_consumption_min_", analysis_year,
                      "_HS", HS_year_rep, ".csv", sep = "")),
              row.names = FALSE)
    
    #---------------------------------------------------------------------------
    rm(list=ls()[!(ls() %in% c("analysis_setup", analysis_setup, "coproduct_codes",
                               "V1_long", "analysis_info", analysis_info,
                               "fao_pop", "hs_dir"))])

    # Clear current analysis year and output directory before looping to the next analysis year
    rm(analysis_year)
    rm(hs_analysis_year_dir)
    
    gc()
  }
}


