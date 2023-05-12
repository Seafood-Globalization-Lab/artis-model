#' @export
get_snet <- function(quadprog_dir, cvxopt_dir, datadir, outdir, num_cores = 10, hs_version = NA, 
                     test_years = NA, prod_type = "FAO") {
  
  #-------------------------------------------------------------------------------
  # Step 0: Setup
  full_analysis_start <- Sys.time()
  
  # Use file.date for all filenames
  # If scripts runs overnight, setting this up top prevents having to deal with two dates before and after midnight
  file.date <- Sys.Date()
  
  # List of variables to retain in memory when environment is cleared
  analysis_info <- c("outdir", "datadir", "file.date", "full_analysis_start", "HS_year_rep", "hs_dir", 
                     "df_years", "analysis_year", "hs_analysis_year_dir",
                     "hs_dir", "quadprog_dir", "cvxopt_dir", "num_cores")
  analysis_setup <- c("prod_data", "V1", "V2", "V1_long", "V2_long", "sc_n", "cc_m", "X_cols", "X_rows", 
                      "W_cols", "W_rows", "Xq", "analysis_years_rep", "HS_year_rep")
  
  # Use file.date for all filenames
  # If scripts runs overnight, setting this up top prevents having to deal with two dates before and after midnight
  file.date <- Sys.Date()
  #-------------------------------------------------------------------------------
  # Step 2: Loop through all years for each HS code year
  
  # List of possible HS versions: HS92, HS96, HS02, HS12, HS17
  # No need to do HS92 when using BACI though as that data starts in 1996
  df_years <- data.frame(HS_year = c(rep("96", length(1996:2020)),
                                     rep("02", length(2002:2020)),
                                     rep("07", length(2007:2020)),
                                     rep("12", length(2012:2020)),
                                     rep("17", length(2017:2020))),
                         analysis_year = c(1996:2020, 2002:2020, 2007:2020,
                                           2012:2020, 2017:2020))
  
  #-------------------------------------------------------------------------------
  # Choose single HS (this will change for each file submitted to Zorro - 
  # run one HS code per Zorro submission)
  
  HS_year_rep <- hs_version
  
  hs_dir <- paste("HS", HS_year_rep, sep = "")
  
  # Get all analysis years for a given HS version
  analysis_years_rep <- df_years %>%
    filter(HS_year == HS_year_rep)
  
  # Check if there is a specific test year to create an snet for
  if (!is.na(test_years)) {
    analysis_years_rep <- analysis_years_rep %>%
      filter(analysis_year %in% test_years)
  }
  
  # Create all output folders
  dir.create(file.path(outdir, hs_dir))
  for (analysis_year in analysis_years_rep$analysis_year){
    dir.create(file.path(outdir, hs_dir, analysis_year))
  }
  
  #-----------------------------------------------------------------------------
  # Step 1: Load production data used for all years
  prod_filename <- NA
  prod_taxa_filename <- NA
  
  if (prod_type == 'FAO') {
    prod_filename <- 'standardized_fao_prod.csv'
    prod_taxa_filename <- 'clean_fao_taxa.csv'
  } else {
    prod_filename <- 'standardized_combined_prod.csv'
    prod_taxa_filename <- 'clean_taxa_combined.csv'
  }
  
  prod_data <- read.csv(file.path(datadir, prod_filename))
  prod_taxa_classification <- read.csv(file.path(datadir, prod_taxa_filename))
  
  #-------------------------------------------------------------------------------
  # Load hs_taxa_match for appropriate HS year
  hs_taxa_match <- read.csv(file.path(datadir, paste("hs-taxa-match_HS", HS_year_rep, ".csv", sep = ""))) %>%
    # select(-c(sciname_habitat, code_habitat)) %>%
    # pad HS codes with zeroes
    mutate(Code = as.character(Code)) %>%
    mutate(Code = if_else(str_detect(Code, "^30"), true = str_replace(Code, pattern = "^30", replacement = "030"),
                          if_else(str_detect(Code, "^511"), true = str_replace(Code, pattern = "^511", replacement = "0511"),
                                  false = Code)))
  
  # Make new version of hs_taxa_match that includes SciName + taxa_source for the full snet estimation
  hs_taxa_match <- hs_taxa_match %>%
    left_join(prod_data %>% select(SciName, taxa_source) %>% distinct(), by = "SciName") %>%
    select(-SciName) %>%
    rename("SciName" = "taxa_source") %>%
    filter(!is.na(SciName)) %>%
    filter(
      str_detect(code_habitat, "marine|diadromous") & str_detect(SciName, "marine") |
        str_detect(code_habitat, "inland|diadromous") & str_detect(SciName, "inland")
    ) %>%
    select(-c(sciname_habitat, code_habitat))
  
  # Load hs_taxa_CF_match for appropriate HS year
  hs_taxa_CF_match <- read.csv(file.path(datadir, paste("hs-taxa-CF_strict-match_HS", HS_year_rep, ".csv", sep = ""))) %>%
    # pad HS codes with zeroes
    mutate(Code = as.character(Code)) %>%
    mutate(Code = if_else(str_detect(Code, "^30"), true = str_replace(Code, pattern = "^30", replacement = "030"),
                          if_else(str_detect(Code, "^511"), true = str_replace(Code, pattern = "^511", replacement = "0511"),
                                  false = Code)))
  
  # Make new version of hs_taxa_CF_match that includes SciName + taxa_source for the full snet estimation
  hs_taxa_CF_match <- hs_taxa_CF_match %>%
    left_join(prod_data %>% select(SciName, taxa_source) %>% distinct(), by = c("Taxa" = "SciName")) %>%
    select(-Taxa) %>%
    rename("Taxa" = "taxa_source") %>%
    filter(!is.na(Taxa)) %>%
    right_join(
      hs_taxa_match %>%
        select("Code", "SciName"), 
      by = c("Taxa" = "SciName", "Code")
    )
  
  # Load hs-hs match for appropriate HS year
  hs_hs_match <- read.csv(file.path(datadir, paste("hs-hs-match_HS", HS_year_rep, ".csv", sep=""))) %>% 
    # pad HS codes with zeroes
    mutate(Code_pre = as.character(Code_pre)) %>%
    mutate(Code_pre = if_else(str_detect(Code_pre, "^30"), true = str_replace(Code_pre, pattern = "^30", replacement = "030"),
                              if_else(str_detect(Code_pre, "^511"), true = str_replace(Code_pre, pattern = "^511", replacement = "0511"),
                                      false = Code_pre))) %>%
    mutate(Code_post = as.character(Code_post)) %>%
    mutate(Code_post = if_else(str_detect(Code_post, "^30"), true = str_replace(Code_post, pattern = "^30", replacement = "030"),
                               if_else(str_detect(Code_post, "^511"), true = str_replace(Code_post, pattern = "^511", replacement = "0511"),
                                       false = Code_post)))
  
  ###############################################################################
  # Step 3: Make V1 and V2
  
  # V1: sparse matrix (products x species) of conversion factors corresponding to the entries of X
  # coproduct_codes are products with CF == 0
  coproduct_codes <- hs_taxa_CF_match %>% select(Code, CF_calc) %>% filter(CF_calc == 0) %>% distinct() %>% pull(Code)
  X_all <- make_v1(hs_taxa_CF_match, coproduct_codes)
  V1 <- X_all[[1]]
  
  X_rows <- X_all[[2]]
  X_cols <- X_all[[3]]
  
  # Make Xq, matrix for controlling strength of species to product estimates in optimization problem (see transform_to_qp_with_python.R)
  Xq <- categorize_hs_to_taxa(hs_taxa_match, coproduct_codes)
  
  # V2: sparse matrix of (products x products) conversion factors corresponding to the entries of W
  # Columns are the original products, Rows are the final product state
  W_all <- make_v2(hs_hs_match = hs_hs_match, hs_taxa_CF_match = hs_taxa_CF_match, coproduct_codes)
  V2 <- W_all[[1]]
  W_rows <- W_all[[2]]
  W_cols <-W_all[[3]]
  
  # Set a max for V2 to prevent gain of mass
  V2[V2>1] <- 1 
  
  # Insert -1 on diagonal if all imports are represented as consumption of foreign goods
  # Insert 1 on diagonal if imports can be exported under the same product code
  diag(V2) <- 1
  
  # Save list of species for analysis from hs_taxa_match
  # Note: hs_taxa_match is what is used to make hs_hs_match, V_1, V_2, so all of these lists should match
  sc_n <- X_cols
  
  # Save list of HS codes for analysis from V_1 matrix, since some codes had to be filtered out
  cc_m <- X_rows
  
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
  
  # FIX IT: remove coproduct codes at beginning of workflow
  # Loop through all analysis years for a given HS version
  for (j in 1:nrow(analysis_years_rep)) {

    analysis_year <- analysis_years_rep$analysis_year[j]
    # output folder
    hs_analysis_year_dir <- file.path(outdir, hs_dir, analysis_year)

    #-----------------------------------------------------------------------------
    # Step 4: Load trade (BACI) data and standardize countries between production and trade data
    baci_data_analysis_year <- read.csv(file = file.path(datadir, paste("standardized_baci_seafood_hs", HS_year_rep, "_y", analysis_year, ".csv", sep = ""))) %>%
      # pad hs6 with 0s
      mutate(hs6 = as.character(hs6)) %>%
      mutate(hs6 = if_else(str_detect(hs6, "^30"), true = str_replace(hs6, pattern = "^30", replacement = "030"),
                           if_else(str_detect(hs6, "^511"), true = str_replace(hs6, pattern = "^511", replacement = "0511"),
                                   false = hs6)))

    # Filter production data to analysis_year
    prod_data_analysis_year <- prod_data %>%
      filter(year == analysis_year)

    # Print message for non-matching countries between baci and production data
    baci_country_list <- unique(c(as.character(baci_data_analysis_year$importer_iso3c), as.character(baci_data_analysis_year$exporter_iso3c)))
    prod_country_list <- unique(prod_data_analysis_year$country_iso3_alpha)

    prod_data_analysis_year <- prod_data_analysis_year %>%
      select(country_iso3_alpha, taxa_source, quantity)

    baci_data_analysis_year <- baci_data_analysis_year %>%
      select(importer_iso3c, exporter_iso3c, hs6, total_q)
    #-----------------------------------------------------------------------------

    # Read in individual country solutions and combine into a list
    quadprog_output_files <- list.files(file.path(quadprog_dir, hs_dir, analysis_year))
    quadprog_output_files <- lapply(quadprog_output_files, FUN = function(x) file.path(quadprog_dir, hs_dir, analysis_year, x))

    cvxopt_output_files <- list.files(file.path(cvxopt_dir, hs_dir, analysis_year))
    cvxopt_output_files <- lapply(cvxopt_output_files, FUN = function(x) file.path(cvxopt_dir, hs_dir, analysis_year, x))

    output_files <- c(quadprog_output_files, cvxopt_output_files)
    solve_country_files <- output_files[grepl(pattern = "_country-est_", output_files) & grepl(pattern = analysis_year, output_files) & grepl(pattern = HS_year_rep, output_files)]

    country_est <- vector(mode = "list", length = length(solve_country_files))
    
    for (i in 1:length(solve_country_files)){
      country_est[[i]] <- readRDS(solve_country_files[[i]])
    }

    # Add country names to country_est
    file_countries <- unlist(lapply(solve_country_files, FUN = function(x) substr(str_extract(x, "country-est_[A-Z]{3}_"), 13, 15)))
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

    # See: Explore_intermediate_results/explore-country-est.R
    # End optimization
    ###############################################################################

    # RESTARTING POINT for make_snet_model_2: If loading old data, might also need to reset file.date and directories
    # load("/Volumes/jgephart/ARTIS/Outputs/S_net/20220207_ARTIS-timeseries/HS12/2012/2022-02-07_all-data-prior-to-solve-country_2012_HS12.RData") # for V2
    # country_est <- readRDS("/Volumes/Dept/CAS/jgephart/ARTIS/Outputs/S_net/20220207_ARTIS-timeseries/HS12/2012/2022-02-07_all-country-est_2012_HS12.RDS") # country_est
    # Reset time and output directory as needed:
    # file.date <- Sys.Date()

    # RESTARTING POINT for JAG
    # load("/Volumes/jgephart/ARTIS/Outputs/S_net/snet_20221129/quadprog_snet/hs12/2016/2022-11-30_all-data-prior-to-solve-country_2016_HS12.RData") # for V2
    # country_est <- readRDS("/Volumes/jgephart/ARTIS/Outputs/S_net/snet_20221129/snet/HS12/2016/2022-12-01_all-country-est_2016_HS12.RDS") # country_est
    # datadir <- "/Volumes/jgephart/ARTIS/Outputs/model_inputs_20220913_hs_hs"


    ###############################################################################

    # Step 6: Make S_net assuming commodity processing only occurs one trade-flow back
    rm(list=ls()[!(ls() %in% c("baci_data_analysis_year", "country_est", "V1", "V2", "V1_long",
                               "countries_to_analyze", "coproduct_codes", "fao_pop", "hs_dir",
                               "analysis_setup", analysis_setup,
                               "analysis_info", analysis_info))])
    gc()
    
    # Determine most specific clade of each HS code (but if clade is not reported in production data (i.e., hs_taxa_match$SciName), return NA)
    # To match to clade, even if not reported in production data, set match_to_prod to FALSE
    hs_clade_match <- match_hs_to_clade(hs_taxa_match = read.csv(file.path(datadir, paste("hs-taxa-match_HS", HS_year_rep, ".csv", sep = ""))) %>%
                                          select(-c(sciname_habitat, code_habitat)),
                                        prod_taxa_classification = read.csv(file.path(datadir, "clean_fao_taxa.csv")),
                                        match_to_prod = FALSE) %>%
      # pad HS codes with zeroes
      mutate(Code = as.character(Code)) %>%
      mutate(Code = if_else(str_detect(Code, "^30"), true = str_replace(Code, pattern = "^30", replacement = "030"),
                            if_else(str_detect(Code, "^511"), true = str_replace(Code, pattern = "^511", replacement = "0511"),
                                    false = Code))) %>%
      # Filter down to just the codes in cc_m (recreated above from country_est)
      filter(Code %in% cc_m)

    # Create new list of countries_to_analyze using only countries with solve_qp solutions (i.e., names(country_est))
    # Some countries have no solution, so using the original list causes error in lapply below
    countries_to_analyze <- names(country_est)

    # writing reweighted X matrix as a dataframe for consumption and diversity calcs
    # post completely built ARTIS
    reweight_X_long <- data.frame()
    for(i in 1:length(countries_to_analyze)){
      tmp <- reweight_X(country_est, countries_to_analyze[i], V1, V2)
      reweight_X_long <- reweight_X_long %>%
        bind_rows(tmp)
    }

    reweight_X_long <- reweight_X_long %>%
      mutate(hs_version = paste("HS", HS_year_rep, sep = ""),
             year = analysis_year)

    write.csv(reweight_X_long, file = file.path(hs_analysis_year_dir, paste("reweight_X_long_", analysis_year, "_HS", HS_year_rep, ".csv", sep = "")), row.names = FALSE)
    
    check_reweight_X_long <- reweight_X_long %>%
      group_by(iso3c, hs6, year) %>%
      summarize(reweighted_X = sum(reweighted_X, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(difference = 1 - reweighted_X) %>%
      filter(abs(difference) > 1e-9)
    
    if (nrow(check_reweight_X_long)) {
      warning("Reweighted X long proportions DO NOT sum to 1.")
    }
    
    # Creating reweighted W long that finds proportion of hs6 processed codes that
    # come from hs6 original codes
    # outlines how much hs6 original code gets transferred to hs6 processed code
    W_long <- data.frame()
    # creating a cluster of cores to parallelize creating a dataframe for W long:
    #   hs6 processed, hs6 original, exporter_iso3c
    w_long_cl <- makeCluster(num_cores, type="FORK")
    # Note this line allows you to pipe output to different files for each parallel cluster worker
    # not needed when in production
    # clusterEvalQ(w_long_cl, sink(file.path(getwd(), paste("qa/w_long_", Sys.getpid(), ".txt", sep = ""))))
    registerDoParallel(w_long_cl)  # use multicore, set to the number of our cores
    # Parallel approach to building W long
    W_long <- foreach(i = 1:length(countries_to_analyze), .combine = rbind) %dopar% {
      curr_country <- countries_to_analyze[i]
      curr_W_long <- as.data.frame(country_est[[curr_country]]$W)
      
      curr_W_long %>%
        # Reformat W as a long data frame
        # Original imported product form is on the columns and
        # processed form is on the rows
        rownames_to_column(var = "hs6_processed") %>%
        pivot_longer(2:(ncol(curr_W_long)+1), 
                     names_to = "hs6_original", 
                     values_to = "estimated_W") %>%
        # Separate hs codes
        mutate(hs6_original = str_extract(hs6_original, "[[:digit:]]+"),
               hs6_processed = str_extract(hs6_processed, "[[:digit:]]+")) %>%
        mutate(exporter_iso3c = curr_country) %>%
        filter(estimated_W > 0)
    }
    # Free up clusters after use
    stopCluster(w_long_cl)
    
    # Restructure BACI data to specify that it handles product weight
    baci_data_analysis_year <- baci_data_analysis_year %>%
      rename(product_weight_t = total_q) %>%
      filter(!(hs6 %in% coproduct_codes))
    
    # reweighted_W = proportion of hs6 processed that came from hs6 original
    # estimated W = proportion of each imported hs6 going into
    #               each processed hs6 which is available for export
    reweight_W_long <- W_long %>%
      # joining processing losses
      left_join(
        V2_long,
        by = c("hs6_original"="from_hs6", "hs6_processed"="to_hs6")
      ) %>%
      # including processing losses in estimated W
      mutate(estimated_W = estimated_W * (1/product_cf)) %>%
      left_join(
        # get total imports by country and hs6 code for weighting
        baci_data_analysis_year %>%
          group_by(importer_iso3c, hs6) %>%
          summarize(product_weight_t = sum(product_weight_t)) %>%
          ungroup() %>%
          rename(iso3c = importer_iso3c),
        by = c("exporter_iso3c"="iso3c", "hs6_original"="hs6")
      ) %>%
      # remove flows where country did not have imports of hs6 original products
      filter(!is.na(product_weight_t)) %>%
      # weight estimated W by the total imports of each hs6 original code by country
      mutate(estimated_W = estimated_W * product_weight_t) %>%
      select(-product_weight_t) %>%
      # reweight estimated W such that you calculate the prop of hs6 processed
      # that came from 'x' hs6 originals
      group_by(exporter_iso3c, hs6_processed) %>%
      mutate(row_sum = sum(estimated_W)) %>%
      ungroup() %>%
      mutate(reweighted_W = estimated_W / row_sum) %>%
      # remove columns that are no longer needed
      select(-c(row_sum, estimated_W))
    
    write.csv(reweight_W_long, file = file.path(hs_analysis_year_dir, paste("reweight_W_long_", analysis_year, "_HS", HS_year_rep, ".csv", sep = "")), row.names = FALSE)
    
    # returns the export and consumption weights by
    #   iso3c, hs6, dom_source
    source_weights_midpoint <- create_export_source_weights(
      baci_data_analysis_year, countries_to_analyze, country_est, V1, V2, cc_m,
      coproduct_codes, dom_source_weight = "midpoint"
    )
    
    source_weights_max <- create_export_source_weights(
      baci_data_analysis_year, countries_to_analyze, country_est, V1, V2, cc_m,
      coproduct_codes, dom_source_weight = "max"
    )
    
    source_weights_min <- create_export_source_weights(
      baci_data_analysis_year, countries_to_analyze, country_est, V1, V2, cc_m,
      coproduct_codes, dom_source_weight = "min"
    )
    
    # volumes by importer, exporter, hs6, dom_source
    export_source_weights_mid <- source_weights_midpoint[[1]]
    export_source_weights_max <- source_weights_max[[1]]
    export_source_weights_min <- source_weights_min[[1]]
    # consumption weighting by iso3c, hs6 and dom_source
    consumption_source_weights_mid <- source_weights_midpoint[[2]] %>%
      mutate(hs_version = HS_year_rep,
             year = analysis_year)
    consumption_source_weights_max <- source_weights_max[[2]] %>%
      mutate(hs_version = HS_year_rep,
             year = analysis_year)
    consumption_source_weights_min <- source_weights_min[[2]] %>%
      mutate(hs_version = HS_year_rep,
             year = analysis_year)
    
    rm(source_weights_midpoint)
    rm(source_weights_max)
    rm(source_weights_min)
    gc()
    
    s_net_midpoint <- create_snet(baci_data_analysis_year, export_source_weights_mid, reweight_W_long,
                         reweight_X_long, V1_long, hs_clade_match, num_cores) %>%
      mutate(hs_version = HS_year_rep,
             year = analysis_year)

    s_net_max <- create_snet(baci_data_analysis_year, export_source_weights_max, reweight_W_long,
                             reweight_X_long, V1_long, hs_clade_match, num_cores) %>%
      mutate(hs_version = HS_year_rep,
             year = analysis_year)

    s_net_min <- create_snet(baci_data_analysis_year, export_source_weights_min, reweight_W_long,
                             reweight_X_long, V1_long, hs_clade_match, num_cores) %>%
      mutate(hs_version = HS_year_rep,
             year = analysis_year)

    # Save full s_net
    write.csv(
      s_net_midpoint,
      file = file.path(hs_analysis_year_dir, paste(file.date, "_S-net_raw_midpoint_", analysis_year, "_HS", HS_year_rep, ".csv", sep = "")),
      row.names = FALSE
    )
    write.csv(
      s_net_max,
      file = file.path(hs_analysis_year_dir, paste(file.date, "_S-net_raw_max_", analysis_year, "_HS", HS_year_rep, ".csv", sep = "")),
      row.names = FALSE
    )
    write.csv(
      s_net_min,
      file = file.path(hs_analysis_year_dir, paste(file.date, "_S-net_raw_min_", analysis_year, "_HS", HS_year_rep, ".csv", sep = "")),
      row.names = FALSE
    )

    write.csv(consumption_source_weights_mid,
              file = file.path(
                hs_analysis_year_dir,
                paste(file.date, "consumption_raw_midpoint_", analysis_year, "_HS", HS_year_rep, ".csv", sep = "")),
              row.names = FALSE)
    
    write.csv(consumption_source_weights_max,
              file = file.path(
                hs_analysis_year_dir,
                paste(file.date, "consumption_raw_max_", analysis_year, "_HS", HS_year_rep, ".csv", sep = "")),
              row.names = FALSE)
    
    write.csv(consumption_source_weights_min,
              file = file.path(
                hs_analysis_year_dir,
                paste(file.date, "consumption_raw_min_", analysis_year, "_HS", HS_year_rep, ".csv", sep = "")),
              row.names = FALSE)
    
    # Calculate consumption
    calculate_consumption(consumption_source_weights_mid, s_net_midpoint,
                          reweight_X_long, reweight_W_long, V1_long,
                          fao_pop %>%
                            filter(year == analysis_year),
                          hs_analysis_year_dir,
                          estimate_type = "midpoint")
    
    calculate_consumption(consumption_source_weights_min, s_net_min,
                          reweight_X_long, reweight_W_long, V1_long,
                          fao_pop %>%
                            filter(year == analysis_year),
                          hs_analysis_year_dir,
                          estimate_type = "min")
    
    calculate_consumption(consumption_source_weights_max, s_net_max,
                          reweight_X_long, reweight_W_long, V1_long,
                          fao_pop %>%
                            filter(year == analysis_year),
                          hs_analysis_year_dir,
                          estimate_type = "max")
    
    
    # ###############################################################################
    # Step 7: Clean S_net: add attributes for taxa and production information

    # NOTE: Move all snet cleaning options into a separate script that includes data checks, etc.

    # Other Snet cleaning options:
    # Add columns for: common name, isscaap group, classification info
    # S_net_clean <- add_taxa_info(S_net_clean,
    #                              prod_data = prod_data,
    #                              prod_taxa_classification = read.csv(file.path(datadir, "clean_fao_taxa.csv")))

    # Split quantities by production source (i.e., multiply quantity by the proportion of of that species produced wild vs aquaculture and marine vs inland)
    # May no longer need this section / function if prod_source can be included within the analysis of Snet
    # NOTE: This creates tiny quantities again filter again to be > 1
    # S_net_clean <- add_prod_source(S_net_clean, prod_data = prod_data, analysis_year) %>%
    #   filter(quantity > 1) %>%
    #   arrange(desc(quantity))

    # write.csv(S_net_clean, file = file.path(hs_analysis_year_dir, paste(file.date, "_S-net_", analysis_year, "_HS", HS_year_rep, ".csv", sep = "")), row.names = FALSE)

    rm(list=ls()[!(ls() %in% c("analysis_setup", analysis_setup, "coproduct_codes", "V1_long",
                               "analysis_info", analysis_info, "fao_pop", "hs_dir"))])

    # Clear current analysis year and output directory before looping to the next analysis year
    rm(analysis_year)
    rm(hs_analysis_year_dir)
  }

  #-------------------------------------------------------------------------------
  # Step 8: Combining all snet tables for a given in HS version into a single timeseries
  hs_out_folder <- file.path(outdir, hs_dir)
  year_folders <- list.dirs(hs_out_folder, recursive = FALSE)

  combine_snet(year_folders, hs_dir, file.date, snet_type = "max", hs_out_folder)
  combine_snet(year_folders, hs_dir, file.date, snet_type = "midpoint", hs_out_folder)
  combine_snet(year_folders, hs_dir, file.date, snet_type = "min", hs_out_folder)
}


