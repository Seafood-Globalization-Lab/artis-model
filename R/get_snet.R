#' @importFrom tibble rownames_to_column
#' @importFrom qs2 qd_save
#' @export
#' 
get_snet <- function(quadprog_dir, 
                     cvxopt_dir, 
                     datadir, 
                     outdir, 
                     num_cores = 10,
                     hs_version = NA, 
                     test_years = c(), 
                     prod_type = "FAO",
                     estimate_type = "midpoint",
                     run_env = "aws", 
                     s3_bucket_name = "", 
                     s3_region = "") {
  
  setup_values <- initial_variable_setup(datadir = datadir, 
                                         outdir = outdir, 
                                         hs_version = hs_version, 
                                         test_years = test_years, 
                                         prod_type = prod_type, 
                                         run_env = run_env,
                                         s3_bucket_name = s3_bucket_name, 
                                         s3_region = s3_region)
  
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
  
  V1_long_fp <- file.path(outdir, paste("HS", hs_version, "/V1_long_HS", hs_version, ".csv", sep = ""))
  
  write.csv(V1_long, V1_long_fp, row.names = FALSE)
  
  # Product-to-Product
  # This is going from original product weight to new product weight
  # (ie represents processing losses, all values should be <= 1)
  
  V2_long <- data.frame(V2) 
  rownames(V2_long) <- colnames(V2)
  V2_long <- V2_long %>% 
    rownames_to_column("to_hs6") %>%
    pivot_longer(cols = -to_hs6, 
                 names_to = "from_hs6", 
                 values_to = "product_cf") %>%
    mutate(from_hs6 = gsub('^.', '', from_hs6)) %>%
    filter(product_cf > 0)
  
  V2_long_fp <- file.path(outdir, 
                          paste0("HS", hs_version, "/V2_long_HS", hs_version, ".csv"))
  
  write.csv(V2_long, V2_long_fp, row.names = FALSE)
  
  if (run_env == "aws") {
    put_object(
      file = V1_long_fp,
      object = V1_long_fp,
      bucket = s3_bucket_name
    )
    
    put_object(
      file = V2_long_fp,
      object= V2_long_fp,
      bucket = s3_bucket_name
    )
  }
  #-------------------------------------------------------------------------------
  pop_fp <- file.path(datadir, "fao_annual_pop.csv")
  
  if (run_env == "aws") {
    save_object(
      object = pop_fp,
      bucket = s3_bucket_name,
      region = s3_region,
      file = pop_fp
    )
  }
  
  fao_pop <- read.csv(pop_fp)
  
  # Non-human codes
  non_human_codes <- c("230120", "051191", "030110", "030111", "030119")


# Start snet loop ---------------------------------------------------------

  # Loop through all analysis years for a given HS version
  for (j in 1:nrow(analysis_years_rep)) {

    analysis_year <- analysis_years_rep$analysis_year[j]
    # output folder
    hs_analysis_year_dir <- file.path(outdir, hs_dir, analysis_year)
    
    message(glue::glue("starting HS{HS_year_rep} {analysis_year} snet"))

    # BACI trade standardize -------------------------------------------------------
    # Load trade (BACI) data and standardize countries between production and trade data
    baci_fp <- file.path(datadir,
                         paste("standardized_baci_seafood_hs", 
                               HS_year_rep, "_y", analysis_year, ".csv", sep = ""))
    
    if (run_env == "aws") {
      save_object(
        baci_fp,
        bucket = s3_bucket_name,
        file = baci_fp
      )
    }
    
    baci_data_analysis_year <- read.csv(baci_fp) %>%
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
    
    # Get country solutions ---------------------------------------------------------------
    # read in all-country-est file in created in get_county_solutions.R for both solver output folers
    
    # build solver‐specific paths
    quad_hs_yr_dir <- file.path(quadprog_dir, hs_dir, analysis_year)
    cvx_hs_yr_dir  <- file.path(cvxopt_dir,  hs_dir, analysis_year)
    
    # pattern to match the combined all country RDS files
    rds_pattern <- paste0(
      ".*_all-country-est_.*",        # any prefix + “_all-country-est_”
      analysis_year,               # “_<year>_”
      "_HS", HS_year_rep,          # “_HS<ver>”
      "\\.RDS$"
    )
    
    # list file path for both solver outputs
    qp_files  <- list.files(quad_hs_yr_dir, pattern = rds_pattern, full.names = TRUE)
    cvx_files <- list.files(cvx_hs_yr_dir,  pattern = rds_pattern, full.names = TRUE)
    all_files <- c(qp_files, cvx_files)
    
    if (length(all_files) == 0) {
      stop("No country‐estimate RDS files found for ", analysis_year, " HS", HS_year_rep)
    }
    
    # read both lists into a single list
    country_est <- lapply(all_files, readRDS)
    # concatenate all elements of the list of lists (into single list with iso3c names)
    country_est <- do.call(c, country_est)
    
    # write out the combined list
    combined_fp <- file.path(
      hs_analysis_year_dir,
      paste0(file.date, "_all-country-est_", analysis_year, "_HS", HS_year_rep, ".RDS")
    )
    saveRDS(country_est, combined_fp)
    
    # upload to s3
    if (run_env == "aws") {
      put_object(
        file      = combined_fp,
        object    = combined_fp,
        bucket    = s3_bucket_name,
        multipart = TRUE
      )
      # remove docker instance local file - use in memory object for snet
      unlink(combined_fp)
    }
    
    
    # We don't believe the final product form provided by solve countries solutions for consumption

    # Step 6: Make S_net assuming commodity processing only occurs one trade-flow back
    rm(list=ls()[!(ls() %in% c("baci_data_analysis_year", "country_est", "V1",
                                "V1_long", "V2", "V2_long", "countries_to_analyze",
                               "analysis_year", "coproduct_codes", "fao_pop", "hs_dir",
                               "prod_taxa_classification",
                               "analysis_setup", analysis_setup,
                               "analysis_info", analysis_info))])
    gc()
    
    # Determine most specific clade of each HS code (but if clade is not reported
    # in production data (i.e., hs_taxa_match$SciName), return NA)
    # To match to clade, even if not reported in production data, set match_to_prod to FALSE
    hs_clade_match <- match_hs_to_clade(
      hs_taxa_match = read.csv(
        file.path(datadir, paste0("hs-taxa-match_HS", HS_year_rep, ".csv"))) %>%
        select(-c(sciname_habitat, code_habitat)),
      # dynamically set to either "clean_fao_taxa.csv" or "clean_taxa_combined.csv" 
      # in initial_variable_setup.R
      prod_taxa_classification = prod_taxa_classification,
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
    
    hs_clade_match_fp <- file.path(hs_analysis_year_dir, "hs_clade_match.csv")
    write.csv(hs_clade_match, hs_clade_match_fp, row.names = FALSE)
    if (run_env == "aws") {
      put_object(
        file = hs_clade_match_fp,
        object = hs_clade_match_fp,
        bucket = s3_bucket_name
      )
    }

    # Create new list of countries_to_analyze using only countries with
    # solve_qp solutions (i.e., names(country_est)), some countries have
    # no solution, so using the original list causes error in lapply below
    countries_to_analyze <- names(country_est)

    reweight_X_long <- create_reweight_X_long(country_est, V1, V2)
    
    reweight_X_long <- reweight_X_long %>%
      mutate(hs_version = paste("HS", HS_year_rep, sep = ""),
             year = analysis_year)
    
    reweight_X_long_fp <- file.path(hs_analysis_year_dir,
                                    paste("reweight_X_long_", analysis_year, "_HS",
                                          HS_year_rep, ".csv", sep = ""))

    write.csv(reweight_X_long, reweight_X_long_fp, row.names = FALSE)
    if (run_env == "aws") {
      put_object(
        file = reweight_X_long_fp,
        object = reweight_X_long_fp,
        bucket = s3_bucket_name
      )
    }
    
    W_long <- create_W_long(country_est, num_cores)
    W_long_fp <- file.path(hs_analysis_year_dir,
                           paste("W_long_", analysis_year, "_HS", HS_year_rep, ".csv", sep = ""))
    write.csv(W_long, W_long_fp, row.names = FALSE)
    
    if (run_env == "aws") {
      put_object(
        file = W_long_fp,
        object = W_long_fp,
        bucket = s3_bucket_name
      )
    }
    
    # Restructure BACI data to specify that it handles product weight
    baci_data_analysis_year <- baci_data_analysis_year %>%
      rename(product_weight_t = total_q) %>%
      filter(!(hs6 %in% coproduct_codes))
    
    reweight_W_long <- create_reweight_W_long(W_long = W_long, 
                                              baci_data_analysis_year = baci_data_analysis_year,
                                              V2_long = V2_long)
    reweight_W_long_fp <- file.path(hs_analysis_year_dir,
                                    paste("reweight_W_long_", analysis_year, "_HS",
                                          HS_year_rep, ".csv", sep = ""))
    write.csv(reweight_W_long, reweight_W_long_fp, row.names = FALSE)
    if (run_env == "aws") {
      put_object(
        file = reweight_W_long_fp,
        object = reweight_W_long_fp,
        bucket = s3_bucket_name
      )
    }
    
    gc()

    X_long_fp <- file.path(hs_analysis_year_dir, "X_long.csv")
    X_long <- create_X_long(country_est, num_cores)
    write.csv(X_long, X_long_fp, row.names = FALSE)

    if (run_env == "aws") {
      put_object(
        file = X_long_fp,
        object = X_long_fp,
        bucket = s3_bucket_name
      )
    }

    prod_data_analysis_year <- prod_data %>%
      filter(year == analysis_year)
    
    curr_hs <- paste("HS", hs_version, sep = "")
    
    pop <- fao_pop %>%
      filter(year == analysis_year)

    # returns the export and consumption weights by
    #   iso3c, hs6, dom_source
    export_source_weights <- create_export_source_weights(
      baci_data_analysis_year, countries_to_analyze, country_est, V1, V2, cc_m,
      coproduct_codes, dom_source_weight = estimate_type
    )


    snet_fp <- file.path(hs_analysis_year_dir,
                         paste0(file.date, "_S-net_raw_", estimate_type, "_", 
                                analysis_year, "_HS", HS_year_rep, ".qs"))

    s_net <- create_snet(baci_data_analysis_year, 
                         export_source_weights,
                         reweight_W_long, 
                         reweight_X_long, 
                         V1_long,
                         hs_clade_match, 
                         num_cores, 
                         hs_analysis_year_dir, 
                         estimate_type = estimate_type,
                         run_env = run_env, 
                         s3_bucket_name = s3_bucket_name, 
                         s3_region = s3_region) %>%
      mutate(hs_version = paste0("HS", HS_year_rep), year = analysis_year)

    # write s_net object to disk (computer or AWS docker instance)
    qs2::qd_save(object = s_net,
                 file = snet_fp)

    rm(export_source_weights)
    gc()

    if (run_env == "aws") {
      put_object(
        file = snet_fp,
        object = snet_fp,
        bucket = s3_bucket_name,
        multipart = TRUE
      )
    }

    message(glue::glue("starting HS{HS_year_rep} {analysis_year} consumption"))
    
    consumption_fp <- file.path(hs_analysis_year_dir,
                                paste0(file.date, "_consumption_", estimate_type, "_", 
                                       analysis_year,"_HS", HS_year_rep, ".qs"))
    
    consumption <- calculate_consumption(artis = s_net, 
                                         prod = prod_data_analysis_year,
                                         curr_year = analysis_year, 
                                         curr_hs_version = curr_hs,
                                         W_long = W_long, 
                                         reweight_W_long = reweight_W_long,
                                         X_long = X_long, 
                                         V1_long = V1_long, 
                                         V2_long = V2_long,
                                         pop = pop, 
                                         max_percap_consumption = 100,
                                         consumption_threshold = 1e-9,
                                         dev_mode = FALSE)
    
    # write consumption object to disk (computer or AWS docker instance)
    qs2::qd_save(object = consumption,
                 file = consumption_fp)

    if (run_env == "aws") {
      put_object(
        file = consumption_fp,
        object = consumption_fp,
        bucket = s3_bucket_name,
        multipart = TRUE
      )
      # remove files on "disk" from docker instance after transfering to s3
      unlink(snet_fp, consumption_fp)
    }
    
    # remove R objects from memory
    rm(list=c("s_net", "consumption"))
    gc()
    
    #---------------------------------------------------------------------------
    rm(list=ls()[!(ls() %in% c("analysis_setup","V2_long", "prod_taxa_classification",
                               analysis_setup, "coproduct_codes",
                               "V1_long", "analysis_info", analysis_info,
                               "fao_pop", "hs_dir"))])
    
    if (run_env == "aws") { unlink(hs_analysis_year_dir) }

    # Clear current analysis year and output directory before looping to the next analysis year
    rm(analysis_year)
    rm(hs_analysis_year_dir)
    
    gc()
  }
}


