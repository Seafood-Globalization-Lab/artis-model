#' @export
get_country_solutions <- function(datadir, outdir, hs_version = NA, test_year = c(),
                                  prod_type = "FAO", solver_type = "quadprog",
                                  no_solve_countries = data.frame(), num_cores = 10,
                                  run_env = "aws", s3_bucket_name = "", s3_region = "") {
  
  setup_values <- initial_variable_setup(datadir, outdir, hs_version, test_year, prod_type, run_env,
                                         s3_bucket_name = s3_bucket_name, s3_region = s3_region)
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
  
  # Analysis documentation: Only need this to be outputted once for entire
  # time series so save outside of analysis_year_loop.
  # Output transform_to_qp_with_python function for documentation of the
  # optimization parameters used to make country_est
  sink(
    file.path(
      outdir, hs_dir,
      paste(
        file.date, "_analysis-documentation_transform-to-qp-with-python.txt",
        sep = ""
      ))
  )
  writeLines(readLines(con = file.path("R/transform_to_qp_with_python.R"),
                       warn = FALSE))
  sink()
  
  
  # Loop through all analysis years for a given HS version
  for (j in 1:nrow(analysis_years_rep)){
    analysis_year <- analysis_years_rep$analysis_year[j]

    hs_analysis_year_dir <- file.path(outdir, hs_dir, analysis_year)
    
    #-----------------------------------------------------------------------------  
    # Step 4: Load trade (BACI) data and standardize countries between production and trade data
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
        if_else(
          str_detect(hs6, "^511"),
          true = str_replace(hs6, pattern = "^511", replacement = "0511"),
          false = hs6
        )))
    
    # Filter production data to analysis_year
    prod_data_analysis_year <- prod_data %>%
      filter(year == analysis_year) %>%
      select(country_iso3_alpha, taxa_source, quantity)

    baci_data_analysis_year <- baci_data_analysis_year %>%
      select(importer_iso3c, exporter_iso3c, hs6, total_q)
    
    #-----------------------------------------------------------------------------  
    # Step 5: Estimate X, W, c, and error for each country
    # (solve mass balance problem using solve_qp in python)
    
    # SAVE FULL WORKSPACE
    workspace_image_fp <- file.path(
      hs_analysis_year_dir,
      paste(file.date,
            "_all-data-prior-to-solve-country_",
            analysis_year, "_HS", HS_year_rep, ".RData", sep = ""))
    
    save.image(workspace_image_fp)
    
    if (run_env == "aws") {
      put_object(
        file = workspace_image_fp,
        object = workspace_image_fp,
        bucket = s3_bucket_name
      )
    }
    
    # Clear workspace other than what"s needed for solve_qp
    rm(list=ls()[!(ls() %in% c("prod_data_analysis_year", "baci_data_analysis_year",
                               "coproduct_codes", "no_solve_countries",
                               "solver_type", "analysis_setup", analysis_setup,
                               "analysis_info", analysis_info))])
    
    # Reticulate can create memory leakage, run gc() to correct for this
    gc()
    
    # Time how long optimization takes
    solve_country_start <- Sys.time()
    
    # Get list of countries
    countries_to_analyze <- NA
    if (nrow(no_solve_countries) == 0) {
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
    
    print(ls())
    
    # Create function to mass balance an individual country,
    # then use mclapply to parallelize the function
    solve_country <- function(i, solver_to_use, run_env = "aws", s3_bucket_name = ""){
      print(paste("start of ", i, " solution"), sep = "")
      qp_inputs <- transform_to_qp_with_python(country_j = i, V1 = V1, V2 = V2, 
                                               baci_data_clean = baci_data_analysis_year, 
                                               prod_data_clean = prod_data_analysis_year, 
                                               sc_n = sc_n, cc_m = cc_m, Xq = Xq)
      
      # Python code within an R function will not recognize R objects unless
      # they are assigned to the global environment
      assign("P", qp_inputs$P, envir = globalenv())
      assign("q", qp_inputs$q, envir = globalenv())
      assign("A", qp_inputs$A, envir = globalenv())
      assign("b", qp_inputs$b, envir = globalenv())
      assign("u", qp_inputs$u, envir = globalenv())
      
      # Format inputs and run solve_qp as python code
      if (solver_to_use == "quadprog") {
        py_run_string(
'from numpy import array, zeros
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

x = qpsolvers.solve_qp(P,q,G,h,A,b,lb,ub, solver=\"quadprog\", verbose = True)',
                      convert = TRUE)
      } else {
        py_run_string(
'from numpy import array, zeros
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

x = qpsolvers.solve_qp(P,q,G,h,A,b,lb,ub, solver=\"cvxopt\", verbose = True)',
convert = TRUE)
      }
      
      # Convert to r object with as.numeric()
      qp_sol <- as.numeric(py$x)
      
      # Write out raw output from solver for comparison
      cond_num <- as.numeric(py$cond_num)
      write.csv(qp_sol, file.path(hs_analysis_year_dir, paste(i, "_sol.csv", sep="")),
                row.names = FALSE)
      sink(file = file.path(hs_analysis_year_dir, "condition_number.csv"),
           append = TRUE)
      cat(paste(i, ",", cond_num, "\n", sep=""))
      sink()
      
      # Unstack solution
      country_est_i <- unstack_qp_sol(qp_sol, qp_inputs)
      country_est_file <- paste(file.date, "_country-est_", i, "_",
                                analysis_year, "_HS", HS_year_rep, ".RDS", sep = "")
      
      # Save country_output one country at a time to avoid memory issues
      country_est_fp <- file.path(hs_analysis_year_dir, country_est_file)
      saveRDS(country_est_i, country_est_fp)
      
      if (run_env == "aws") {
        put_object(
          file = country_est_fp,
          object = country_est_fp,
          bucket = s3_bucket_name
        )
      }
      
      print(paste("end of ", i, " solution"))
      
      # Since all the outputs were assigned to the global environment,
      # clear workspace before starting next country
      rm(list=ls()[!(ls() %in% c("prod_data_analysis_year", "baci_data_analysis_year",
                                 "coproduct_codes", "no_solve_countries",
                                 "countries_to_analyze", "solver_type",
                                 "analysis_setup", analysis_setup,
                                 "analysis_info", analysis_info))])
      
      # Reticulate can create memory leakage, run gc()
      gc()
    }
    
    # Parallelize solution to country mass balance problems:
    mclapply(countries_to_analyze,
             solve_country,
             solver_to_use = solver_type,
             run_env = run_env,
             s3_bucket_name = s3_bucket_name,
             mc.cores = num_cores,
             mc.preschedule = FALSE)

    # This needs to contain ALL files across quadprog and cvxopt solutions
    # Read in individual country solutions and combine into a list
    output_files <- list.files(hs_analysis_year_dir)
    solve_country_files <- output_files[grepl(pattern = "_country-est_", output_files) &
                                          grepl(pattern = analysis_year, output_files) &
                                          grepl(pattern = HS_year_rep, output_files)]
    country_est <- vector(mode = "list", length = length(solve_country_files))
    for (i in 1:length(solve_country_files)){
      country_est[[i]] <- readRDS(
        file.path(hs_analysis_year_dir, solve_country_files[i])
      )
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
        rownames(country_est[[names(country_est)[i]]]$X) <- paste(names(country_est)[i], X_rows,sep="_")
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
    
    all_country_est_fp <- file.path(
      hs_analysis_year_dir,
      paste(file.date, "_all-country-est_", analysis_year, "_HS",
            HS_year_rep, ".RDS", sep = "")
      )
    saveRDS(country_est, all_country_est_fp)
    
    if (run_env == "aws") {
      put_object(
        file = all_country_est_fp,
        object = all_country_est_fp,
        bucket = s3_bucket_name
      )
    }
    
    # End time 
    solve_country_end <- Sys.time()
    solve_country_time <- solve_country_end - solve_country_start
    
    # Output solve_country_time
    sink(file.path(
      hs_analysis_year_dir,
      paste(file.date, "_time-file-solve-country_", analysis_year, "_HS",
            HS_year_rep, ".txt", sep = "")))
    print(solve_country_start)
    print(solve_country_end)
    print(solve_country_time)
    sink()
    
    # Output list of countries that did not pass solve_qp
    no_sol_countries <- setdiff(unlist(countries_to_analyze), names(country_est))
    no_solve_qp_fp <- file.path(hs_analysis_year_dir,
                                paste(file.date,
                                      "_analysis-documentation_countries-with-no-solve-qp-solution_",
                                      analysis_year, "_HS", HS_year_rep, ".txt", sep = ""))
    sink(no_solve_qp_fp)
    print(no_sol_countries)
    sink()
    
    if (run_env == "aws") {
      put_object(
        file = no_solve_qp_fp,
        object = no_solve_qp_fp,
        bucket = s3_bucket_name
      )
    }
    
    # Delete all files created in the AWS worker node if on AWS to free up storage space
    # Note: All these files have been placed in the S3 bucket at this point.
    #       We are just deleting the local copies on the AWS server
    if (run_env == "aws") { unlink(hs_analysis_year_dir) }
    
    rm(list=ls()[!(ls() %in% c("analysis_setup", analysis_setup, "coproduct_codes",
                               "no_solve_countries", "countries_to_analyze",
                               "solver_type", "analysis_info", analysis_info))])
    
    # Clear current analysis year and output directory before looping to the next analysis year
    rm(analysis_year)
    rm(hs_analysis_year_dir)
  }
  
  # Delete all model data from AWS server to free up storage space
  # Delete all output data generated from AWS server to free up storage space
  # Note: this will be downloaded in other functions if needed
  if (run_env == "aws") { unlink(datadir) }
}

