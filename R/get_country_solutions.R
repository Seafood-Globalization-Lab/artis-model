#' @export
get_country_solutions <- function(datadir, outdir, hs_version = NA, test_year = NA, prod_type = "FAO", solver_type = "quadprog", no_solve_countries = NA) {
  
  #-----------------------------------------------------------------------------
  # Step 0: Setup
  full_analysis_start<-Sys.time()
  
  # Use file.date for all filenames
  # If scripts runs overnight, setting this up top prevents having to deal with two dates before and after midnight
  file.date <- Sys.Date()
  
  # List of variables to retain in memory when environment is cleared
  analysis_info <- c("outdir", "datadir", "file.date", "full_analysis_start", "HS_year_rep", "hs_dir", "df_years", "analysis_year", "hs_analysis_year_dir", "solver_type")
  analysis_setup <- c("prod_data", "V1", "V2", "sc_n", "cc_m", "X_cols", "X_rows", "W_cols", "W_rows", "Xq", "analysis_years_rep", "HS_year_rep", "no_solve_countries")
  
  #-----------------------------------------------------------------------------
  # Step 1: Load production data used for all years
  prod_filename <- NA
  prod_taxa_filename <- NA
  
  if (prod_type == "FAO") {
    prod_filename <- "standardized_fao_prod.csv"
    prod_taxa_filename <- "clean_fao_taxa.csv"
  } else {
    prod_filename <- "standardized_combined_prod.csv"
    prod_taxa_filename <- "clean_taxa_combined.csv"
  }
  
  prod_data <- read.csv(file.path(datadir, prod_filename))
  prod_taxa_classification <- read.csv(file.path(datadir, prod_taxa_filename))
  
  #-----------------------------------------------------------------------------
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
  
  ###############################################################################
  # Choose single HS (this will change for each file submitted to Zorro - 
  # run one HS code per Zorro submission)
  
  HS_year_rep <- hs_version
  
  ###############################################################################
  
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
  
  # Get all analysis years for a given HS version
  analysis_years_rep <- df_years %>%
    filter(HS_year == HS_year_rep)
  
  # Check if there is a specific test year to create an snet for
  if (!is.na(test_year)) {
    analysis_years_rep <- analysis_years_rep %>%
      filter(analysis_year == test_year)
  }
  
  # Create all output directories for HS version and each year
  hs_dir <- paste("HS", HS_year_rep, sep = "")
  dir.create(file.path(outdir, hs_dir))
  for (analysis_year in analysis_years_rep$analysis_year){
    dir.create(file.path(outdir, hs_dir, analysis_year))
  }
  
  # Analysis documentation: Only need this to be outputted once for entire time series so save outside of analysis_year_loop
  # Output transform_to_qp_with_python function for documentation of the optimization parameters used to make country_est
  sink(file.path(outdir, hs_dir, paste(file.date, "_analysis-documentation_transform-to-qp-with-python.txt", sep = "")))
  writeLines(readLines(con = file.path("R/transform_to_qp_with_python.R"), warn = FALSE))
  sink()
  
  
  # Loop through all analysis years for a given HS version
  for (j in 1:nrow(analysis_years_rep)){
    analysis_year <- analysis_years_rep$analysis_year[j]

    hs_analysis_year_dir <- file.path(outdir, hs_dir, analysis_year)
    
    ##############################################################################
    # Step 4: Load trade (BACI) data and standardize countries between production and trade data
    # baci_data_analysis_year <- read.csv(file = file.path(datadir, paste("baci_seafood_hs", HS_year_rep, "_y", analysis_year, ".csv", sep = ""))) %>%
    baci_data_analysis_year <- read.csv(file = file.path(datadir, paste("standardized_baci_seafood_hs", HS_year_rep, "_y", analysis_year, ".csv", sep = ""))) %>%
      # pad hs6 with 0s
      mutate(hs6 = as.character(hs6)) %>%
      mutate(hs6 = if_else(str_detect(hs6, "^30"), true = str_replace(hs6, pattern = "^30", replacement = "030"),
                           if_else(str_detect(hs6, "^511"), true = str_replace(hs6, pattern = "^511", replacement = "0511"),
                                   false = hs6)))
    
    # Filter production data to analysis_year
    prod_data_analysis_year <- prod_data %>%
      filter(year == analysis_year)
    
    # Run "standardize_countries" - i.e., aggregate countries or assign to NEI as necessary
    # FIX IT: This will get moved to clean inputs, ONLY Need to read in correct data file after this is in clean inputs
    # clean_countries_output <- standardize_countries(prod_data = prod_data_analysis_year, baci_data = baci_data_analysis_year)
    # # First two countrycode warning messages are generated prior to cleaning country codes within the function (one for getting iso3_alpha and one for iso3_numeric)
    # # Only need to pay attention to the second two countrycode warning messages which are generated after cleaning country codes within the function - which country names aren"t recognized by the countrycode package?
    # # Note: iso3_alpha and iso3_numeric for Other nei (assigned to Taiwan), Netherlands Antilles, So. African Customs Union, and Serbia and Montenegro are adjusted manually (not with countrycode package)
    # prod_data_analysis_year <- clean_countries_output[[1]]
    # baci_data_analysis_year <- clean_countries_output[[2]]
    
    # Print message for non-matching countries between baci and production data
    baci_country_list <- unique(c(as.character(baci_data_analysis_year$importer_iso3c), as.character(baci_data_analysis_year$exporter_iso3c)))
    prod_country_list <- unique(prod_data_analysis_year$country_iso3_alpha)
    
    # Output list of countries that mismatch between prod and baci data
    sink(file.path(hs_analysis_year_dir, paste(file.date, "_analysis-documentation_prod-v-baci-mismatch-countries_", analysis_year, "_HS", HS_year_rep, ".txt", sep = "")))
    print(paste("Countries in baci, but not in production data: ", paste(setdiff(baci_country_list, prod_country_list), collapse = ", "), sep = ""))
    print(paste("Countries in production, but not in baci data: ", paste(setdiff(prod_country_list, baci_country_list), collapse = ", "), sep = ""))
    sink()
    
    # Remove countries that don"t match between baci and production data
    prod_data_analysis_year <- prod_data_analysis_year %>%
      # filter(country_iso3_alpha %in% baci_country_list) %>%
      select(country_iso3_alpha, taxa_source, quantity)
    
    baci_data_analysis_year <- baci_data_analysis_year %>%
      # filter(importer_iso3c %in% unique(prod_data_analysis_year$country_iso3_alpha)) %>%
      # filter(exporter_iso3c %in% unique(prod_data_analysis_year$country_iso3_alpha)) %>%
      select(importer_iso3c, exporter_iso3c, hs6, total_q)
    
    
    ###############################################################################
    # Step 5: Estimate X, W, c, and error for each country (solve mass balance problem using solve_qp in python)
    
    # SAVE FULL WORKSPACE
    save.image(file.path(hs_analysis_year_dir, paste(file.date, "_all-data-prior-to-solve-country_", analysis_year, "_HS", HS_year_rep, ".RData", sep = "")))
    # RESTARTING POINT for optimization section with qp_solvers: If loading old data, might also want to reset file.date and directories
    # load("qa/snet_20221208/cvxopt_snet/HS96/1996/2022-12-08_all-data-prior-to-solve-country_1996_HS96.RData")
    # Reset time and output directory as needed:
    # file.date <- Sys.Date()
    # hs_analysis_year_dir <- "/Users/rahulab/Documents/Outputs/HS12/2012"
    
    # Clear workspace other than what"s needed for solve_qp
    # Keep in memory both the object "analysis_info", and the list of objects found in analysis_info
    rm(list=ls()[!(ls() %in% c("prod_data_analysis_year", "baci_data_analysis_year", "coproduct_codes", "no_solve_countries",
                               "solver_type",
                               "analysis_setup", analysis_setup,
                               "analysis_info", analysis_info))])
    # Reticulate can create memory leakage, run gc()
    gc()
    
    # Time how long optimization takes
    solve_country_start <- Sys.time()
    
    # Get list of countries
    countries_to_analyze <- NA
    if (is.na(no_solve_countries)) {
      countries_to_analyze <- sort(unique(prod_data_analysis_year$country_iso3_alpha))
    } else {
      
      countries_to_analyze <- no_solve_countries %>%
        filter(hs_version == paste("HS", HS_year_rep, sep="")) %>%
        filter(year == analysis_year)
      
      countries_to_analyze <- countries_to_analyze$country_iso3
    }
    
    countries_to_analyze <- countries_to_analyze[!is.na(countries_to_analyze)]
    countries_to_analyze <- sort(countries_to_analyze, decreasing = TRUE)

    # Skip finding countries for this year if
    # there are no countries left to solve for this HS version and year
    if (length(countries_to_analyze) == 0) {
      next
    }
    
    # Sum production across countries and taxa_source
    prod_data_analysis_year <- prod_data_analysis_year %>%
      group_by(country_iso3_alpha, taxa_source) %>%
      summarize(quantity = sum(quantity))
    
    # Starting file for reporting conditional number of A matrix
    sink(file = file.path(hs_analysis_year_dir, "condition_number.csv"),
         append = FALSE)
    cat(paste("country", "condition_number\n", sep=","))
    sink()
    # Create function to mass balance an individual country, then use mclapply to parallelize the function
    solve_country <- function(i, solver_to_use){
      qp_inputs <- transform_to_qp_with_python(country_j = i, V1 = V1, V2 = V2, 
                                               baci_data_clean = baci_data_analysis_year, 
                                               prod_data_clean = prod_data_analysis_year, 
                                               sc_n = sc_n, cc_m = cc_m, Xq = Xq)
      
      # Assign objects from qp_inputs
      # P = qp_inputs$P
      # q = qp_inputs$q
      # G = qp_inputs$G
      # A = qp_inputs$A
      # b = qp_inputs$b
      # u = qp_inputs$u
      
      # Python code within an R function will not recognize R objects unless they are assigned to the global environment
      assign("P", qp_inputs$P, envir = globalenv())
      assign("q", qp_inputs$q, envir = globalenv())
      assign("A", qp_inputs$A, envir = globalenv())
      assign("b", qp_inputs$b, envir = globalenv())
      assign("u", qp_inputs$u, envir = globalenv())
      
      # Format inputs and run solve_qp as python code
      if (solver_to_use == "quadprog") {
        py_run_string('
from numpy import array, zeros
from numpy import linalg
import qpsolvers

P = array(r.P,dtype=float)
q = array(r.q,dtype=float)
G = array(zeros(P.shape))
h = array(zeros(P.shape[0]))
A = array(r.A,dtype=float)
b = array(r.b,dtype=float).reshape((A.shape[0],))
lb = array(zeros(P.shape[0]))
ub = array(r.u,dtype=float).reshape((P.shape[0],))

cond_num = linalg.cond(A)

x = qpsolvers.solve_qp(P,q,G,h,A,b,lb,ub, solver=\"quadprog\", verbose = True)', convert = TRUE)
      } else {
        py_run_string('
from numpy import array, zeros
from numpy import linalg
import qpsolvers

P = array(r.P,dtype=float)
q = array(r.q,dtype=float)
G = array(zeros(P.shape))
h = array(zeros(P.shape[0]))
A = array(r.A,dtype=float)
b = array(r.b,dtype=float).reshape((A.shape[0],))
lb = array(zeros(P.shape[0]))
ub = array(r.u,dtype=float).reshape((P.shape[0],))

cond_num = linalg.cond(A)

x = qpsolvers.solve_qp(P,q,G,h,A,b,lb,ub, solver=\"cvxopt\", verbose = True)', convert = TRUE)
      }
      
      # , max_iters=int(1e6)
      # Convert to r object with as.numeric()
      qp_sol <- as.numeric(py$x)
      
        # Write out raw output from solver for comparison
      cond_num <- as.numeric(py$cond_num)
      write.csv(qp_sol, file.path(hs_analysis_year_dir, paste(i, "_sol.csv", sep="")), row.names = FALSE)
      sink(file = file.path(hs_analysis_year_dir, "condition_number.csv"),
           append = TRUE)
      cat(paste(i, ",", cond_num, "\n", sep=""))
      sink()
      
      # Unstack solution
      country_est_i <- unstack_qp_sol(qp_sol, qp_inputs)
      country_est_file <- paste(file.date, "_country-est_", i, "_", analysis_year, "_HS", HS_year_rep, ".RDS", sep = "")
      # SAVE country_output one country at a time to avoid memory issues
      saveRDS(country_est_i, file.path(hs_analysis_year_dir, country_est_file))
      
      # Since all the outputs were assigned to the global environment, clear workspace before starting next country
      rm(list=ls()[!(ls() %in% c("prod_data_analysis_year", "baci_data_analysis_year", "coproduct_codes", "no_solve_countries",
                                 "countries_to_analyze", "solver_type",
                                 "analysis_setup", analysis_setup,
                                 "analysis_info", analysis_info))])
      # Reticulate can create memory leakage, run gc()
      gc()
    }
    
    # Test single country
    # solve_country(i = "ABW")
    # Test lapply
    # lapply(countries_to_analyze[1:20], function(i){solve_country(i)})
    
    # Parallelize:
    # Maybe decrease number of mc.cores to a lower amount
    mclapply(countries_to_analyze, solve_country, solver_to_use = solver_type, mc.cores = 10, mc.preschedule = FALSE)
    # mclapply(countries_to_analyze, FUN = function(i){solve_country(i, "quadprog")}, mc.cores = 10, mc.preschedule = FALSE)
    
    # This needs to contain ALL files across quadprog and cvxopt solutions
    # Read in individual country solutions and combine into a list
    output_files <- list.files(hs_analysis_year_dir)
    solve_country_files <- output_files[grepl(pattern = "_country-est_", output_files) & grepl(pattern = analysis_year, output_files) & grepl(pattern = HS_year_rep, output_files)]
    country_est <- vector(mode = "list", length = length(solve_country_files))
    for (i in 1:length(solve_country_files)){
      country_est[[i]] <- readRDS(file.path(hs_analysis_year_dir, solve_country_files[i]))
    }
    
    # Add country names to country_est
    get_country_names <- function(solve_country_files){
      country_rds <- unlist(strsplit(solve_country_files, split = "_"))[3]
      file_countries <- unlist(strsplit(country_rds, split = "\\."))[1]
      return(file_countries)
    }
    file_countries <- lapply(solve_country_files, function(i){get_country_names(i)})
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
    
    # End time 
    solve_country_end <- Sys.time()
    solve_country_time <- solve_country_end - solve_country_start
    
    # Output solve_country_time
    sink(file.path(hs_analysis_year_dir, paste(file.date, "_time-file-solve-country_", analysis_year, "_HS", HS_year_rep, ".txt", sep = "")))
    print(solve_country_start)
    print(solve_country_end)
    print(solve_country_time)
    sink()
    
    # Output list of countries that did not pass solve_qp
    no_sol_countries <- setdiff(unlist(countries_to_analyze), names(country_est))
    sink(file.path(hs_analysis_year_dir, paste(file.date, "_analysis-documentation_countries-with-no-solve-qp-solution_", analysis_year, "_HS", HS_year_rep, ".txt", sep = "")))
    print(no_sol_countries)
    sink()
    
    rm(list=ls()[!(ls() %in% c("analysis_setup", analysis_setup, "coproduct_codes", "no_solve_countries",
                               "countries_to_analyze", "solver_type",
                               "analysis_info", analysis_info))])
    
    # Clear current analysis year and output directory before looping to the next analysis year
    rm(analysis_year)
    rm(hs_analysis_year_dir)
    
  }
}

