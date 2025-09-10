#' Solve country mass balance problems in parallel
#'
#' @param num_cores Integer. Controls parallel worker allocation for solving
#'   country-level mass balance problems within each year.
#'
#'   - `num_cores = 1` → **sequential mode** (no parallelism; useful for debugging).
#'   - `num_cores = 0` or `NULL` → **auto mode**: use all available cores minus one
#'     (to leave one free for the OS), then cap by the number of countries
#'     to analyze for that year.
#'   - `num_cores >= 2` → **explicit cap**: request that many workers, but will
#'     still be capped at the number of countries for that year.
#'
#'   In all cases, the number of workers is
#'   `min(requested_cores, length(countries_to_analyze))`.
#'
#'   Parallelization is implemented via [future.apply::future_lapply()] with a
#'   `multisession` backend (safe with reticulate).
#' 
#' @importFrom dplyr filter select mutate if_else group_by summarize
#' @importFrom stringr str_detect str_replace
#' @importFrom future plan
#' @importFrom future.apply future_lapply
#' @importFrom reticulate py_run_string
#' @importFrom aws.s3 save_object put_object
#' @importFrom utils read.csv write.csv
#' @export
#' 

############# BEGIN PATCH: S3 retry helpers (no new packages required) #############
is_retryable_s3 <- function(msg) {
  grepl("HTTP (500|502|503|504)", msg, ignore.case = TRUE) ||
    grepl("SlowDown|InternalError|RequestTimeout|temporarily unavailable|timeout|timed out",
          msg, ignore.case = TRUE)
}

s3_put_retry <- function(file, object, bucket, region = NULL,
                         multipart = FALSE, max_attempts = 8, base_sleep = 0.5) {
  stopifnot(is.character(file), length(file) == 1L, file.exists(file))
  delay <- base_sleep
  for (i in seq_len(max_attempts)) {
    ok <- tryCatch(
      aws.s3::put_object(file = file, object = object, bucket = bucket,
                         region = region, multipart = multipart),
      error = identity
    )
    if (isTRUE(ok)) return(TRUE)

    msg <- if (inherits(ok, "error")) conditionMessage(ok) else as.character(ok)
    if (!is_retryable_s3(msg)) {
      stop("S3 PUT failed (non-retryable): ", msg, "\nkey: ", object)
    }
    if (i < max_attempts) {
      Sys.sleep(delay + runif(1, 0, 0.5))  # backoff + jitter
      delay <- min(delay * 2, 8)
    } else {
      stop("S3 PUT failed after ", max_attempts, " attempts: ", object, "\nLast error: ", msg)
    }
  }
}
############# END PATCH: S3 retry helpers #############

############# BEGIN PATCH: S3 list/clear helpers (TOP-LEVEL; not affected by rm()) #############
s3_list_keys <- function(bucket, prefix, region) {
  objs <- aws.s3::get_bucket(bucket = bucket, prefix = prefix, max = 10000L, region = region)
  if (!length(objs)) return(character(0))
  vapply(objs, function(x) if (!is.null(x$Key)) x$Key else NA_character_, character(1))
}

s3_clear_prefix <- function(bucket, prefix, region) {
  # Clear exact prefix; also clear variant without leading slash if present
  p1 <- prefix
  p2 <- sub("^/+", "", prefix)
  keys <- unique(c(s3_list_keys(bucket, p1, region), s3_list_keys(bucket, p2, region)))
  if (!length(keys)) {
    message("[clean] No existing objects under prefix: ", prefix)
    return(invisible(0L))
  }
  message("[clean] Deleting ", length(keys), " objects under prefix: ", prefix)
  # Intentionally no try/catch: fail fast on deletion error
  for (k in keys) {
    aws.s3::delete_object(object = k, bucket = bucket, region = region)
  }
  invisible(length(keys))
}
############# END PATCH: S3 list/clear helpers #############

get_country_solutions <- function(datadir, 
                                  outdir, 
                                  hs_version = NA, 
                                  test_year = c(),
                                  prod_type = "FAO", 
                                  solver_type = "quadprog",
                                  no_solve_countries = data.frame(), 
                                  num_cores = 10,
                                  run_env = "aws", 
                                  s3_bucket_name = "", 
                                  s3_region = "",
                                  dev_mode = FALSE) {
  
  setup_values <- initial_variable_setup(datadir, outdir, hs_version, test_year, 
                                         prod_type, run_env,
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

  # parallel plan (safe with reticulate)
  on.exit(future::plan("sequential"), add = TRUE)

  # Analysis documentation...
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

    # ------------------------------------------------------------------------------------
    # S3 RESUME CHECK: if combined file exists for this year, skip work for this year
    # ------------------------------------------------------------------------------------
    skip_this_year <- FALSE
    if (run_env == "aws") {
      prefix1 <- hs_analysis_year_dir
      objs <- tryCatch(
        aws.s3::get_bucket(bucket = s3_bucket_name,
                           prefix = prefix1,
                           max    = 10000L,
                           region = s3_region),
        error = function(e) NULL
      )

      if (is.null(objs) || !length(objs)) {
        # Fallback without leading slash
        prefix2 <- sub("^/+", "", hs_analysis_year_dir)
        objs <- tryCatch(
          aws.s3::get_bucket(bucket = s3_bucket_name,
                             prefix = prefix2,
                             max    = 10000L,
                             region = s3_region),
          error = function(e) NULL
        )
      }

      if (!is.null(objs) && length(objs)) {
        keys <- vapply(objs, function(x) if (!is.null(x$Key)) x$Key else NA_character_, character(1))
        pat  <- paste0("_all-country-est_", analysis_year, "_HS", HS_year_rep, "\\.RDS$")
        if (any(grepl(pat, keys))) {
          message(sprintf("[resume] Skipping year %d (combined country estimate file already in S3)", analysis_year))
          skip_this_year <- TRUE
        }
      }
    }

    if (skip_this_year) next

    # ------------------------------------------------------------------------------------
    # CLEAN SLATE ON S3: clear the entire analysis-year "folder" before solving
    # ------------------------------------------------------------------------------------
    if (run_env == "aws") {
      s3_clear_prefix(bucket = s3_bucket_name, prefix = hs_analysis_year_dir, region = s3_region)
    }
    
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
    
    # not directly used in the model - activate output with arguement dev_mode
    if(dev_mode == TRUE) {
      workspace_image_fp <- file.path(
        hs_analysis_year_dir,
        paste(file.date,
              "_all-data-prior-to-solve-country_",
              analysis_year, "_HS", HS_year_rep, ".RData", sep = ""))
      save.image(workspace_image_fp)
    }

    # Clear workspace other than what"s needed for solve_qp
    rm(list = ls()[!(ls() %in% c(
      "prod_data_analysis_year", "baci_data_analysis_year",
      "coproduct_codes", "no_solve_countries",
      "solver_type", "analysis_setup", analysis_setup,
      "analysis_info",  analysis_info,
      # Keep S3 helpers so they survive to the next year iteration
      "s3_clear_prefix", "s3_list_keys"
    ))])
    
    gc()  # reticulate can leak, run gc()
    
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

    if (length(countries_to_analyze) == 0) next
    
    # Sum production across countries and taxa_source
    prod_data_analysis_year <- prod_data_analysis_year %>%
      group_by(country_iso3_alpha, taxa_source) %>%
      summarize(quantity = sum(quantity))
    
    # Starting file for reporting conditional number of A matrix
    sink(file = file.path(hs_analysis_year_dir, "condition_number.csv"),
         append = FALSE)
    cat(paste("country", "condition_number\n", sep=","))
    sink()
    
    # Create function to mass balance an individual country,
    # then use future_lapply to parallelize the function
    solve_country <- function(i, 
                              solver_to_use, 
                              run_env = "aws", 
                              s3_bucket_name = "", 
                              s3_region = "",
                              dev_mode_logic = FALSE){
      print(paste0("start of ", i, " solution (HS", HS_year_rep, " ", analysis_year, ")"))
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

x = qpsolvers.solve_qp(P,q,G,h,A,b,lb,ub, solver=\"quadprog\")',
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
ub = array(r.u,dtype=float).reshape((P.shape[0]),)

cond_num = linalg.cond(A)

x = qpsolvers.solve_qp(P,q,G,h,A,b,lb,ub, solver=\"cvxopt\")',
convert = TRUE)
      }
      
      # Convert to r object with as.numeric()
      qp_sol <- as.numeric(py$x)

      ret <- list(country = i, ok = FALSE, fp = NA_character_, err = NULL)

      if(length(qp_sol) > 0 )  {
        if(dev_mode_logic == TRUE) {
          # Write out raw output from solver for comparison
          cond_num <- as.numeric(py$cond_num)
          write.csv(qp_sol, file.path(hs_analysis_year_dir, paste(i, "_sol.csv", sep="")),
                    row.names = FALSE)
          sink(file = file.path(hs_analysis_year_dir, "condition_number.csv"),
               append = TRUE)
          cat(paste(i, ",", cond_num, "\n", sep=""))
          sink()
        }
      
        # Unstack solution
        country_est_i <- unstack_qp_sol(qp_sol, qp_inputs)
        country_est_file <- paste(file.date, "_country-est_", i, "_",
                                  analysis_year, "_HS", HS_year_rep, ".RDS", sep = "")
      
        # Save country_output one country at a time to avoid memory issues
        country_est_fp <- file.path(hs_analysis_year_dir, country_est_file)
        saveRDS(country_est_i, country_est_fp)
      
        if (run_env == "aws") {
          Sys.sleep(runif(1, 0, 0.4))  # tiny de-sync of workers
          upload_ok <- tryCatch({
            s3_put_retry(
              file   = country_est_fp,
              object = country_est_fp,  # mirrored key
              bucket = s3_bucket_name,
              region = s3_region
            )
            TRUE
          }, error = function(e) {
            message("[upload-fail] ", i, " -> ", conditionMessage(e))
            FALSE
          })
        } else {
          upload_ok <- TRUE
        }
        ret <- list(country = i, ok = upload_ok, fp = country_est_fp, err = NULL)
      } # end of qp_sol

      print(paste("end of ", i, " solution"))

      # Clear local env in this worker, but keep 'ret' so we can return status
      rm(list=ls()[!(ls() %in% c(
        "prod_data_analysis_year", "baci_data_analysis_year",
        "coproduct_codes", "no_solve_countries",
        "countries_to_analyze", "solver_type",
        "analysis_setup", analysis_setup,
        "analysis_info",  analysis_info, "ret"))])
      gc()

      ret
    } # end of solve_country fun definition
    
    # explicitly set inside parent environment
    dev_mode_logic <- dev_mode

    #### Setup for for running `solve_country()` in parallel
    reserve_for_os <- 1L
    auto_max <- max(1L, as.integer(future::availableCores()) - reserve_for_os)

    # Normalize user input
    if (is.null(num_cores)) num_cores <- 0L
    num_cores <- as.integer(num_cores)

    if (num_cores == 1L) {
      # Force sequential
      if (!inherits(future::plan(), "sequential")) {
        on.exit(future::plan("sequential"), add = TRUE)
      }
      future::plan("sequential")
      workers_to_use <- 1L
      message(sprintf(
        "[parallel] Running sequentially (num_cores=%d, countries=%d)",
        num_cores, length(countries_to_analyze)
      ))
    } else {
      # Auto (0 or <0) or explicit (>=2)
      requested <- if (num_cores <= 0L) auto_max else num_cores
      workers_to_use <- min(requested, length(countries_to_analyze))
      if (!inherits(future::plan(), "multisession")) {
        on.exit(future::plan("sequential"), add = TRUE)
      }
      future::plan("multisession", workers = workers_to_use)
      message(sprintf(
        "[parallel] Running multisession with %d workers (requested=%d, auto_max=%d, countries=%d)",
        workers_to_use, num_cores, auto_max, length(countries_to_analyze)
      ))
    }

    # Parallelize solution to country mass balance problems:
    country_results <- future.apply::future_lapply(
      countries_to_analyze,
      solve_country,
      solver_to_use  = solver_type,
      run_env        = run_env,
      s3_bucket_name = s3_bucket_name,
      s3_region      = s3_region,
      dev_mode_logic = dev_mode_logic,
      future.seed    = TRUE
    )

    # Sequential re-upload of any failed items
    if (run_env == "aws") {
      failed <- vapply(country_results, function(x) is.list(x) && !isTRUE(x$ok), logical(1))
      if (any(failed)) {
        message("[upload-retry] Retrying ", sum(failed), " country uploads sequentially...")
        for (x in country_results[failed]) {
          fp <- x$fp
          if (is.character(fp) && !is.na(fp) && file.exists(fp)) {
            try(s3_put_retry(file = fp, object = fp, bucket = s3_bucket_name, region = s3_region), silent = TRUE)
          }
        }
      }
    }

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
      } else {
        country_est[[i]]$X <- matrix(0, ncol = length(X_cols), nrow = length(X_rows))
        colnames(country_est[[names(country_est)[i]]]$X) <- X_cols
        rownames(country_est[[names(country_est)[i]]]$X) <- paste(names(country_est)[i],X_rows,sep="_")
      }
      if (is.matrix(country_est[[i]]$W)){
        colnames(country_est[[names(country_est)[i]]]$W) <- paste(names(country_est)[i],W_cols,sep="_")
        rownames(country_est[[names(country_est)[i]]]$W) <- paste(names(country_est)[i],W_rows,sep="_")
      } else {
        country_est[[i]]$W <- matrix(0, ncol = length(W_cols), nrow = length(W_rows))
        colnames(country_est[[names(country_est)[i]]]$W) <- paste(names(country_est)[i],W_cols,sep="_")
        rownames(country_est[[names(country_est)[i]]]$W) <- paste(names(country_est)[i],W_rows,sep="_")
      }
    }
    
    # save all country estimate file (after no country solutions in order for AWS S3 completion detection
    # to pick up at the appropriate analysis year. Not all years will have a no-solve-countries write out 
    # that so that file needs to be writen out before all-country-est. 

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
      s3_put_retry(
        file   = no_solve_qp_fp,
        object = no_solve_qp_fp,
        bucket = s3_bucket_name,
        region = s3_region
      )
    }

    all_country_est_fp <- file.path(
      hs_analysis_year_dir,
      paste(file.date, "_all-country-est_", analysis_year, "_HS",
            HS_year_rep, ".RDS", sep = "")
      )
    saveRDS(country_est, all_country_est_fp)
    
    if (run_env == "aws") {
      s3_put_retry(
        file   = all_country_est_fp,
        object = all_country_est_fp,
        bucket = s3_bucket_name,
        region = s3_region,
        multipart = TRUE
      )
    }
    
    # Delete all files created in the AWS worker node if on AWS to free up storage space
    if (run_env == "aws") { unlink(hs_analysis_year_dir) }
    
    rm(list = ls()[!(ls() %in% c(
      "analysis_setup", analysis_setup, "coproduct_codes",
      "no_solve_countries", "countries_to_analyze",
      "solver_type", "analysis_info", analysis_info,
      # Keep S3 helpers available for the next loop iteration
      "s3_clear_prefix", "s3_list_keys"
    ))])
    
    rm(analysis_year)
    rm(hs_analysis_year_dir)
  }
  

  # Delete all model data from AWS server to free up storage space
  if (run_env == "aws") { unlink(datadir) }
}
