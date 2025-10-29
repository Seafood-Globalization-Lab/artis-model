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
#' @param datadir Character. Path to input data directory
#' @param outdir Character. Path to output directory
#' @param hs_version Character. HS version code
#' @param test_year Numeric vector. Years to test
#' @param prod_type Character. Production data type ("FAO" or "SAU")
#' @param solver_type Character. Type of solver to use
#' @param no_solve_countries Data frame. Countries to exclude
#' @param run_env Character. Running environment
#' @param s3_bucket_name Character. S3 bucket name if using AWS
#' @param s3_region Character. AWS region if using AWS
#' @param dev_mode Logical. Whether to run in development mode
#'
#' @return NULL invisibly
#' 
#' @importFrom dplyr filter select mutate if_else group_by summarize
#' @importFrom stringr str_detect str_replace
#' @importFrom future plan
#' @importFrom future.apply future_lapply
#' @importFrom reticulate py_run_string
#' @importFrom aws.s3 save_object put_object
#' @importFrom utils read.csv write.csv

# Helper Functions for S3 ------------------------------------------------

#' Helper function to check if S3 error is retryable
#' @param msg Character string of error message
#' @return Logical indicating if error is retryable
#' @noRd
is_retryable_s3 <- function(msg) {
  grepl("HTTP (500|502|503|504)", msg, ignore.case = TRUE) ||
    grepl("SlowDown|InternalError|RequestTimeout|temporarily unavailable|timeout|timed out",
          msg, ignore.case = TRUE)
}

#' Helper function to retry S3 put operations
#' @param file Character. File path to upload
#' @param object Character. S3 object name
#' @param bucket Character. S3 bucket name
#' @param region Character. AWS region
#' @param multipart Logical. Use multipart upload
#' @param max_attempts Integer. Maximum retry attempts
#' @param base_sleep Numeric. Base sleep time between retries
#' @return Logical indicating success
#' @noRd
s3_put_retry <- function(
  file,
  object,
  bucket,
  region = NULL,
  multipart = FALSE,
  max_attempts = 8,
  base_sleep = 0.5
) {
  # Too verbose - would print for every single country solution upload
  # cli::cli_alert_info("{.fn get_country_solutions} {.fn s3_put_retry} upload helper - Try S3 upload: {.file {basename(file)}}")

  # Input validation
  stopifnot(is.character(file), length(file) == 1L, file.exists(file))

  delay <- base_sleep
  # Retry loop
  for (i in seq_len(max_attempts)) {
    # Attempt s3 upload with tryCatch - capture error
    ok <- tryCatch(
      aws.s3::put_object(
        file = file,
        object = object,
        bucket = bucket,
        region = region,
        multipart = multipart
      ),
      error = identity
    )
    # Check for success - if so, return TRUE
    if (isTRUE(ok)) {
      # Too verbose - would print for every single country solution upload
      # cli::cli_alert_success("Upload successful: {.file {basename(file)}}")
      return(TRUE)
    }
    # Extract error message
    msg <- if (inherits(ok, "error")) conditionMessage(ok) else as.character(ok)
    # Check if error is retryable - if not, stop immediately
    if (!is_retryable_s3(msg)) {
      cli::cli_h2(
        "S3 Upload Error {hs_version} {test_year} - Non-retryable failure"
      )
      cli::cli_alert_danger(c(
        "!" = "Non-retryable S3 upload failure detected",
        "i" = "error tripped in calling {.fn s3_put_retry} in {.fn get_country_solutions}",
        "i" = "Retryable errors are 500-series HTTP server-side errors and timeouts",
        "x" = "Failed file: {.file {basename(file)}}",
        "x" = "Full path: {.path {file}}",
        "i" = "S3 destination: {.val s3://{bucket}/{object}}",
        "i" = "AWS region: {.val {region}}",
        "i" = "Error details: {.val {msg}}"
      ))
      stop("S3 PUT failed (non-retryable): ", msg, "\nkey: ", object)
    } # end of non-retryable check
    # Wait before retrying
    if (i < max_attempts) {
      Sys.sleep(delay + runif(1, 0, 0.5)) # backoff + jitter
      delay <- min(delay * 2, 8)
    } else {
      stop(
        "S3 PUT failed after ",
        max_attempts,
        " attempts: ",
        object,
        "\nLast error: ",
        msg
      )
    } # end of if-else
  } # end of for loop
} # end of s3_put_retry function

#' List S3 keys with given prefix
#' @param bucket Character. S3 bucket name
#' @param prefix Character. Key prefix to list
#' @param region Character. AWS region
#' @return Character vector of S3 keys
#' @noRd
s3_list_keys <- function(bucket, prefix, region) {
  objs <- aws.s3::get_bucket(bucket = bucket, prefix = prefix, max = 10000L, region = region)
  if (!length(objs)) return(character(0))
  vapply(objs, function(x) if (!is.null(x$Key)) x$Key else NA_character_, character(1))
}

#' Clear S3 objects with given prefix
#' @param bucket Character. S3 bucket name
#' @param prefix Character. Key prefix to clear
#' @param region Character. AWS region
#' @return Invisible integer count of deleted objects
#' @noRd
s3_clear_prefix <- function(bucket, prefix, region) {
  # Clear exact prefix; also clear variant without leading slash if present
  p1 <- prefix
  p2 <- sub("^/+", "", prefix)

  keys <- unique(c(s3_list_keys(bucket, p1, region), s3_list_keys(bucket, p2, region)))
  if (!length(keys)) {
    return(invisible(0L))
  }

  cli::cli_h3("Deleting {length(keys)} object{?s} from S3")
  cli::cli_alert_info(c(
    "i" = "S3 location: {.val s3://{bucket}}",
    "i" = "Directory: {.path {prefix}}",
    "i" = c(
      "Incomplete country solutions for analysis year; ",
      "Did not find .*all-country-est RDS file"),
    "x" = "Objects marked for deletion:"
  ))
  cli::cli_ul(keys)


  # Intentionally no try/catch: fail fast on deletion error
  for (k in keys) {
    aws.s3::delete_object(object = k, bucket = bucket, region = region)
  }
  invisible(length(keys))
}

#' Solve country mass balance problems in parallel
#'
#' @param num_cores Integer. Controls parallel worker allocation...
#' ...existing documentation...
#'
#' @return NULL invisibly
#' 
#' @importFrom dplyr filter select mutate if_else group_by summarize
#' @importFrom stringr str_detect str_replace
#' @importFrom future plan
#' @importFrom future.apply future_lapply
#' @importFrom reticulate py_run_string
#' @importFrom aws.s3 save_object put_object
#' @importFrom utils read.csv write.csv
#' @export
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
  
  
  setup_values <- initial_variable_setup(
    datadir = datadir, 
    outdir = outdir, 
    hs_version = hs_version, 
    test_years = test_year,
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

  cli::cli_h2("Starting HS{HS_year_rep} country solutions with {solver_type} solver")
  start_time <- Sys.time()
  cli::cli_alert_info(c(
    "i" = "Start time: {.val {as.character(start_time)}}",
    "i" = "Analysis years: {.val {paste(analysis_years_rep$analysis_year, collapse = ', ')}}",
    "i" = "Production data type: {.val {prod_type}}",
    "i" = "Development mode: {.val {dev_mode}}",
    "i" = "Output directory: {.file {outdir}/{hs_dir}/}",
    "i" = "Input data directory: {.file {datadir}/}",
    "i" = "Model run environment: {.val {run_env}}"
  ))

  # Begin analysis year loop ------------------------------------------------
  # Loop through all analysis years for a given HS version
  for (j in 1:nrow(analysis_years_rep)) {
    analysis_year <- analysis_years_rep$analysis_year[j]
    #cli::cli_h3("Processing year {analysis_year}")

    hs_analysis_year_dir <- file.path(outdir, hs_dir, analysis_year)

    ## AWS Restart - Check if analysis year completed ----------------------------------
    # if all-country-est file exists for this year, skip work for this year
    # This allows resuming interrupted runs without re-doing completed years
    skip_this_year <- FALSE
    if (run_env == "aws") {
      # get list of objects in S3 prefix
      objs <- tryCatch(
        aws.s3::get_bucket(
          bucket = s3_bucket_name,
          prefix = hs_analysis_year_dir,
          max = 10000L,
          region = s3_region
        ),
        error = function(e) NULL
      )
      # Fallback: try without leading slash
      if (is.null(objs) || !length(objs)) {
        # Fallback without leading slash
        prefix2 <- sub("^/+", "", hs_analysis_year_dir)
        objs <- tryCatch(
          aws.s3::get_bucket(
            bucket = s3_bucket_name,
            prefix = prefix2,
            max = 10000L,
            region = s3_region
          ),
          error = function(e) NULL
        )
      }

      # Check for combined country estimate file
      if (!is.null(objs) && length(objs)) {
        # Extract keys into a character vector
        keys <- vapply(
          objs,
          function(x) if (!is.null(x$Key)) x$Key else NA_character_,
          character(1)
        )
        # Construct pattern to match combined country estimate file
        pat <- glue::glue(
          ".*_all-country-est_{analysis_year}_HS{HS_year_rep}\\.RDS$"
        )
        # Check if any keys match the pattern - change skip_this_year to TRUE if found
        if (any(grepl(pat, keys))) {
          skip_this_year <- TRUE
        } else {
          (cli::cli_alert_info(""))
        }
      }
    } # end of Check existing all-country-est file in S3

    #  Skip to next year if already completed (all-country-est file exists)
    if (skip_this_year) {
      cli::cli_alert_info(c(
        "[restart country solutions] Skipping {analysis_year}:",
        "Detected existing combined country estimate file in S3",
        "{.file <yyyy-mm-dd>_all-country-est_{analysis_year}_HS{HS_year_rep}}",
        "indicating this analysis year was previously completed."
      ))
      next
    }

    ## AWS Restart - Delete incomplete year solutions ----------------------------------
    # clear the entire analysis-year "folder" before solving
    # This ensures that any incomplete or partial results from previous runs don't end up in the final output
    # Allows re-running country solutions when the process exited with incomplete years
    if (run_env == "aws") {
      # messaging inside of function
      s3_clear_prefix(
        bucket = s3_bucket_name,
        prefix = hs_analysis_year_dir,
        region = s3_region
      )
    }

    # Load BACI ----------------------------------------------------------------------
    # get partitioned BACI file for current HS version and year
    # Load trade (BACI) data and standardize countries between production and trade data
    baci_fp <- file.path(
      datadir,
      paste(
        "standardized_baci_seafood_hs",
        HS_year_rep,
        "_y",
        analysis_year,
        ".csv",
        sep = ""
      )
    )

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
      mutate(
        hs6 = if_else(
          str_detect(hs6, "^30"),
          true = str_replace(hs6, pattern = "^30", replacement = "030"),
          if_else(
            str_detect(hs6, "^511"),
            true = str_replace(hs6, pattern = "^511", replacement = "0511"),
            false = hs6
          )
        )
      )

    # Filter production data to analysis_year
    prod_data_analysis_year <- prod_data %>%
      filter(year == analysis_year) %>%
      select(country_iso3_alpha, taxa_source, quantity)

    baci_data_analysis_year <- baci_data_analysis_year %>%
      select(importer_iso3c, exporter_iso3c, hs6, total_q)

    #-----------------------------------------------------------------------------
    # Step 5: Estimate X, W, c, and error for each country
    # (solve mass balance problem using solve_qp in python)

    # dev_mode write out environment ---------------------------------------------
    # not directly used in the model - activate output with arguement dev_mode
    if (dev_mode == TRUE) {
      workspace_image_fp <- file.path(
        hs_analysis_year_dir,
        paste(
          file.date,
          "_all-data-prior-to-solve-country_",
          analysis_year,
          "_HS",
          HS_year_rep,
          ".RData",
          sep = ""
        )
      )
      save.image(workspace_image_fp)
      cli::cli_alert_info(c(
        "[dev-mode TRUE] Writing workspace image / R environment ",
        "{.file {basename(workspace_image_fp)}}"
      ))
    }

    # Clear workspace other than what"s needed for solve_qp
    rm(
      list = ls()[
        !(ls() %in%
          c(
            "prod_data_analysis_year",
            "baci_data_analysis_year",
            "coproduct_codes",
            "no_solve_countries",
            "solver_type",
            "analysis_setup",
            analysis_setup,
            "analysis_info",
            analysis_info,
            # Keep S3 helpers so they survive to the next year iteration
            "s3_clear_prefix",
            "s3_list_keys"
          ))
      ]
    )

    # Could add environmental object message here listing all avialable objects
    #cli::cli_alert_info()

    # reticulate can leak, run gc()
    gc()

    # Time how long optimization takes
    solve_country_start <- Sys.time()

    # Countries to solve -----------------------------------------------------
    countries_to_analyze <- NA
    # no_solve_countries dataframe is passed to get_country_solutions in 02-artis-pipeline
    # when get_country_solutions is run a second time with the cvxopt solver.
    # no_solve_countries dataframe is not passed the first time for quadprog solver

    if (nrow(no_solve_countries) == 0) {
      # get all unique country iso3c values from produciton data
      # AM 2025-10-28 Does the unique country list change among HS_versions? Only filtered by year
      countries_to_analyze <- sort(unique(
        prod_data_analysis_year$country_iso3_alpha
      ))
    } else {
      # get country iso3c values from no_solve_country dataframe
      countries_to_analyze <- no_solve_countries %>%
        filter(hs_version == paste("HS", HS_year_rep, sep = "")) %>%
        filter(year == analysis_year)
      countries_to_analyze <- countries_to_analyze$country_iso3
    }
    # remove NAs and sort
    countries_to_analyze <- countries_to_analyze[!is.na(countries_to_analyze)]
    countries_to_analyze <- sort(countries_to_analyze, decreasing = TRUE)

    # Failsafe: skip year if there are no countries to analyze
    if (length(countries_to_analyze) == 0) {
      cli::cli_alert_warning(
        "Skipping year {analysis_year}: No countries to analyze"
      )
      next
    }

    # Sum production across countries and taxa_source
    prod_data_analysis_year <- prod_data_analysis_year %>%
      group_by(country_iso3_alpha, taxa_source) %>%
      summarize(quantity = sum(quantity))

    # Starting file for reporting conditional number of A matrix
    sink(
      file = file.path(hs_analysis_year_dir, "condition_number.csv"),
      append = FALSE
    )
    cat(paste("country", "condition_number\n", sep = ","))
    sink()

    # Create helper function to solve mass balance for a single country
    # This function will be parallelized across all countries for the current analysis year
    # No roxygen2 documentation for this nested function - keep inside get_country_solutions()
    # definition - not indended for external use.
    #
    # Inputs:
    #   - i: country ISO3 code
    #   - solver_to_use: "quadprog" or "cvxopt"
    #   - run_env: execution environment ("aws" or local)
    #   - s3_bucket_name, s3_region: AWS S3 configuration
    #   - dev_mode_logic: whether to write debug outputs
    #
    # Returns: list with upload status (ok), file path (fp), and any errors
    solve_country <- function(
      i,
      solver_to_use,
      run_env = "aws",
      s3_bucket_name = "",
      s3_region = "",
      dev_mode_logic = FALSE
    ) {
      qp_inputs <- transform_to_qp_with_python(
        country_j = i,
        V1 = V1,
        V2 = V2,
        baci_data_clean = baci_data_analysis_year,
        prod_data_clean = prod_data_analysis_year,
        sc_n = sc_n,
        cc_m = cc_m,
        Xq = Xq
      )

      # Python code within an R function will not recognize R objects unless
      # they are assigned to the global environment
      assign("P", qp_inputs$P, envir = globalenv())
      assign("q", qp_inputs$q, envir = globalenv())
      assign("A", qp_inputs$A, envir = globalenv())
      assign("b", qp_inputs$b, envir = globalenv())
      assign("u", qp_inputs$u, envir = globalenv())

      # Conditional solver setup: choose between quadprog and cvxopt
      # Both solvers use the same mathematical formulation (P, q, G, h, A, b, bounds)
      # but have different algorithmic implementations and numerical behavior.
      #
      # - quadprog: Generally faster, uses active-set method, may fail on poorly conditioned problems
      # - cvxopt: More robust for ill-conditioned problems, uses interior-point method, slightly slower
      #
      # The Python code:
      # 1. Converts R matrices (P, q, A, b, u) to NumPy arrays with explicit float64 dtype
      # 2. Creates constraint matrices G (inequality) and h (set to zeros - unused but required by API)
      # 3. Sets lower bounds (lb) to zeros and upper bounds (ub) from R's 'u' vector
      # 4. Calculates condition number of A matrix for numerical stability diagnostics
      # 5. Calls qpsolvers.solve_qp() with the selected solver backend
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
          convert = TRUE
        )
      } else {
        # cvxopt solver - same setup, different backend
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
          convert = TRUE
          )
        } # end of if solver_to_use

      # Convert to r object with as.numeric()
      qp_sol <- as.numeric(py$x)

      ret <- list(country = i, ok = FALSE, fp = NA_character_, err = NULL)

      # Check if solution was found
      if (length(qp_sol) > 0) {
        # Development mode outputs
        if (dev_mode_logic == TRUE) {
          cli::cli_alert_info(c(
            "[dev-mode TRUE] Writing raw {solver_to_use} outputs {.file *_sol.csv} and ",
            "condition numbers {.file condition_number.csv}"
          ))
          # Write out raw output from solver for comparison
          cond_num <- as.numeric(py$cond_num)
          write.csv(
            qp_sol,
            file.path(hs_analysis_year_dir, paste(i, "_sol.csv", sep = "")),
            row.names = FALSE
          )
          sink(
            file = file.path(hs_analysis_year_dir, "condition_number.csv"),
            append = TRUE
          )
          cat(paste(i, ",", cond_num, "\n", sep = ""))
          sink()
        } # end dev_mode_logic TRUE

        # Unstack solution
        country_est_i <- unstack_qp_sol(qp_sol, qp_inputs)
        country_est_file <- paste(
          file.date,
          "_country-est_",
          i,
          "_",
          analysis_year,
          "_HS",
          HS_year_rep,
          ".RDS",
          sep = ""
        )

        # Save individual country estimate RDS file
        country_est_fp <- file.path(hs_analysis_year_dir, country_est_file)
        saveRDS(country_est_i, country_est_fp)

        # Upload to S3 if in AWS environment
        if (run_env == "aws") {
          Sys.sleep(runif(1, 0, 0.4)) # tiny de-sync of workers
          upload_ok <- tryCatch(
            {
              # Upload to S3 with retry logic
              s3_put_retry(
                file = country_est_fp,
                object = country_est_fp, # mirrored key
                bucket = s3_bucket_name,
                region = s3_region
              )
              TRUE
            },
            error = function(e) {
              message("[upload-fail] ", i, " -> ", conditionMessage(e))
              FALSE
            }
          )
        } else {
          upload_ok <- TRUE
        }
        ret <- list(
          country = i,
          ok = upload_ok,
          fp = country_est_fp,
          err = NULL
        )
      } # end of qp_sol - if solutions were found

      # Clear local env in this worker, but keep 'ret' so we can return status
      rm(
        list = ls()[
          !(ls() %in%
            c(
              "prod_data_analysis_year",
              "baci_data_analysis_year",
              "coproduct_codes",
              "no_solve_countries",
              "countries_to_analyze",
              "solver_type",
              "analysis_setup",
              analysis_setup,
              "analysis_info",
              analysis_info,
              "ret"
            ))
        ]
      )
      gc()

      ret
    } # end of solve_country fun definition

    # Parallel setup --------------------------------------
    # Determine optimal worker allocation for country-level mass balance problems
    # Strategy: Balance performance (more workers = faster) with system stability
    
    # Reserve one core for OS operations to prevent system slowdown
    reserve_for_os <- 1L
    # Calculate maximum workers: all available cores minus OS reserve
    auto_max <- max(1L, as.integer(future::availableCores()) - reserve_for_os)

    # Normalize user input for num_cores parameter
    # NULL is treated as 0 (auto mode)
    if (is.null(num_cores)) {
      num_cores <- 0L
    }
    # Ensure num_cores is integer type for consistent comparisons
    num_cores <- as.integer(num_cores)

    # Branch 1: Sequential mode (num_cores = 1)
    # Used for debugging or when parallelism is explicitly disabled
    if (num_cores == 1L) {
      # Ensure we're starting from a clean sequential plan
      if (!inherits(future::plan(), "sequential")) {
        # Register cleanup: restore sequential plan when function exits
        on.exit(future::plan("sequential"), add = TRUE)
      }
      # Set execution plan to sequential (no parallel workers)
      future::plan("sequential")
      workers_to_use <- 1L

      cli::cli_h3("Parallel Processing Settings - country solutions")
      cli::cli_alert_info(c(
        "i" = "Running {.emph sequentially} not parallel processing",
        "i" = "Workers allocated: {.strong {workers_to_use}}",
        "i" = "Requested cores: {.val {num_cores}}",
        "i" = "Auto-detected max: {.val {auto_max}}",
        "i" = "Countries to process: {.val {length(countries_to_analyze)}}",
        "i" = "Solver type: {.val {solver_type}}"
      ))

    } else {
      # Branch 2: Parallel mode (num_cores = 0 for auto, or >= 2 for explicit)
    
      # Determine requested workers:
      # - If num_cores <= 0: use auto_max (all cores minus OS reserve)
      # - If num_cores >= 2: use that explicit value
      requested <- if (num_cores <= 0L) auto_max else num_cores
      
      # Cap workers at number of countries to avoid idle workers
      # Example: If only 5 countries but 10 cores requested, use 5 workers
      workers_to_use <- min(requested, length(countries_to_analyze))
      
      # Ensure we're starting from a clean multisession plan
      if (!inherits(future::plan(), "multisession")) {
        # Register cleanup: restore sequential plan when function exits
        # This prevents leaking parallel workers across function calls
        on.exit(future::plan("sequential"), add = TRUE)
      }
      
      # Set execution plan to multisession with calculated workers
      # multisession = fork-safe, works with reticulate/Python
      future::plan("multisession", workers = workers_to_use)
      
      cli::cli_h3("Parallel Processing Settings - country solutions")
      cli::cli_alert_info(c(
        "i" = "Running {.emph multisession} parallel processing",
        "i" = "Workers allocated: {.strong {workers_to_use}}",
        "i" = "Requested cores: {.val {num_cores}}",
        "i" = "Auto-detected max: {.val {auto_max}}",
        "i" = "Countries to process: {.val {length(countries_to_analyze)}}",
        "i" = "Solver type: {.val {solver_type}}"
      ))
    } # end of conditional parallel setup

    # Run Parallel country solutions -------------------------------------------
    # Distribute country mass balance problems across workers using future.apply
    # 
    # Workflow:
    # 1. Each worker receives a country ISO3 code from countries_to_analyze
    # 2. Worker calls solve_country() function with that country
    # 3. solve_country() runs transform_to_qp_with_python() to set up the QP problem
    # 4. Python qpsolvers.solve_qp() solves the mass balance optimization
    # 5. Solution is unstacked and saved as individual country RDS file
    # 6. If run_env="aws", file is uploaded to S3 with retry logic
    # 7. Worker returns status list: {country, ok, fp, err}
    #
    # future.seed = TRUE ensures reproducible random number generation across workers
    # This is important for:
    # - Random jitter in S3 upload timing (desynchronizes workers to avoid rate limits)
    # - Any stochastic elements in the QP solver
    #
    # country_results structure:
    # List with one element per country, each containing:
    # - country: ISO3 code
    # - ok: TRUE if S3 upload succeeded (or TRUE if local run)
    # - fp: file path to saved RDS
    # - err: NULL if successful, error message if failed
    country_results <- future.apply::future_lapply(
      X = countries_to_analyze,           # Vector of country ISO3 codes to process
      FUN = solve_country,                # Function to apply to each country
      solver_to_use = solver_type,        # Passed to solve_country(): "quadprog" or "cvxopt"
      run_env = run_env,                  # Passed to solve_country(): "aws" or local
      s3_bucket_name = s3_bucket_name,    # Passed to solve_country(): S3 bucket for uploads
      s3_region = s3_region,              # Passed to solve_country(): AWS region
      dev_mode_logic = dev_mode,          # Passed to solve_country(): write debug outputs?
      future.seed = TRUE,                 # Enable reproducible RNG across workers
      future.scheduling = Inf,            # Dynamic load balancing (default)
      future.lazy = FALSE,                # Start futures immediately (default)
      future.globals = TRUE,              # Auto-detect global variables (default)
      future.packages = NULL              # Auto-detect required packages (default)
    )


    # AWS upload reporting ---------------------------------------------------
    # Report any countries with failed uploads after retry logic
    if (run_env == "aws") {
      failed <- vapply(
        country_results,
        function(x) is.list(x) && !isTRUE(x$ok),
        logical(1)
      )
      
      if (any(failed)) {
        failed_countries <- vapply(
          country_results[failed],
          function(x) x$country,
          character(1)
        )
        
        cli::cli_h3("S3 Upload Failures Detected")
        cli::cli_alert_danger(c(
          "!" = "{sum(failed)} countr{?y/ies} failed S3 upload after 8 retry attempts",
          "x" = "Failed countries: {.val {paste(failed_countries, collapse = ', ')}}",
          "i" = "These failures persisted through exponential backoff retry logic",
          "i" = "Check S3 permissions, network connectivity, rate limits, or disk space"
        ))
      } else {
        cli::cli_alert_success(
          "All {length(country_results)} country solutions uploaded successfully to S3"
        )
      }
    }

    
    # Build all-country-est ------------------------------------------
    # File contains:
    # - Named list with one element per successfully solved country
    # - Each element contains X (consumption matrix), W (waste matrix), c (error), e (error)
    # - Matrices have row/column names for traceability
    # - Countries without solutions are excluded (see no_solve_qp file for list)

    # Read in individual country solutions and combine into a list
    output_files <- list.files(hs_analysis_year_dir)
    solve_country_files <- output_files[
      grepl(pattern = "_country-est_", output_files) &
        grepl(pattern = analysis_year, output_files) &
        grepl(pattern = HS_year_rep, output_files)
    ]
    country_est <- vector(mode = "list", length = length(solve_country_files))
    for (i in 1:length(solve_country_files)) {
      country_est[[i]] <- readRDS(
        file.path(hs_analysis_year_dir, solve_country_files[i])
      )
    }

    # Add country names to country_est
    get_country_names <- function(solve_country_files) {
      country_rds <- unlist(strsplit(solve_country_files, split = "_"))[3]
      file_countries <- unlist(strsplit(country_rds, split = "\\."))[1]
      return(file_countries)
    }
    file_countries <- lapply(solve_country_files, function(i) {
      get_country_names(i)
    })
    names(country_est) <- file_countries

    # Add row and column names to X and W
    for (i in 1:length(country_est)) {
      if (is.matrix(country_est[[i]]$X)) {
        colnames(country_est[[names(country_est)[i]]]$X) <- X_cols
        rownames(country_est[[names(country_est)[i]]]$X) <- paste(
          names(country_est)[i],
          X_rows,
          sep = "_"
        )
      } else {
        country_est[[i]]$X <- matrix(
          0,
          ncol = length(X_cols),
          nrow = length(X_rows)
        )
        colnames(country_est[[names(country_est)[i]]]$X) <- X_cols
        rownames(country_est[[names(country_est)[i]]]$X) <- paste(
          names(country_est)[i],
          X_rows,
          sep = "_"
        )
      }
      if (is.matrix(country_est[[i]]$W)) {
        colnames(country_est[[names(country_est)[i]]]$W) <- paste(
          names(country_est)[i],
          W_cols,
          sep = "_"
        )
        rownames(country_est[[names(country_est)[i]]]$W) <- paste(
          names(country_est)[i],
          W_rows,
          sep = "_"
        )
      } else {
        country_est[[i]]$W <- matrix(
          0,
          ncol = length(W_cols),
          nrow = length(W_rows)
        )
        colnames(country_est[[names(country_est)[i]]]$W) <- paste(
          names(country_est)[i],
          W_cols,
          sep = "_"
        )
        rownames(country_est[[names(country_est)[i]]]$W) <- paste(
          names(country_est)[i],
          W_rows,
          sep = "_"
        )
      }
    }

    # Write Out Files ----------------------------------------------------
    # Write files in order: diagnostics first, then completion marker
    # Order matters for AWS restart logic:
    # 1. Write no-solve-qp file first (diagnostic info - may not exist for all years)
    # 2. Write all-country-est file second (completion marker - exists for every year)
    # 
    # The restart logic (check for combined country estimate file in "AWS Restart - Check if 
    # analysis year completed" section) looks for all-country-est to determine if a year
    # is complete. Writing it last ensures diagnostic files are captured before marking
    # the year as done.

    # Output list of countries that did not pass solve_qp
    # Compare the list of countries we attempted to solve against the countries
    # for which we have successful solutions (names in country_est list)
    no_sol_countries <- setdiff(
      unlist(countries_to_analyze),   # All countries we attempted
      names(country_est)              # Countries with successful solutions
    )

    # Diagnostic file - countries without QP solutions
    no_solve_qp_fp <- file.path(
      hs_analysis_year_dir,
      paste(
        file.date,
        "_analysis-documentation_countries-with-no-solve-qp-solution_",
        analysis_year,
        "_HS",
        HS_year_rep,
        ".txt",
        sep = ""
      )
    )
    # Write list of failed countries to text file
    sink(no_solve_qp_fp)
    print(no_sol_countries)
    sink()

    # Upload diagnostic file to S3 if in AWS environment
    if (run_env == "aws") {
      s3_put_retry(
        file = no_solve_qp_fp,
        object = no_solve_qp_fp,
        bucket = s3_bucket_name,
        region = s3_region
      )
    }

    # This is the FINAL file written for each analysis year
    # Its presence in S3 signals that the year is complete and can be skipped on restart
    all_country_est_fp <- file.path(
      hs_analysis_year_dir,
      paste(
        file.date,
        "_all-country-est_",
        analysis_year,
        "_HS",
        HS_year_rep,
        ".RDS",
        sep = ""
      )
    )
    saveRDS(country_est, all_country_est_fp)

    if (run_env == "aws") {
      s3_put_retry(
        file = all_country_est_fp,
        object = all_country_est_fp,
        bucket = s3_bucket_name,
        region = s3_region,
        multipart = TRUE
      )
      cli::cli_alert_success(
        "Year {analysis_year} processing complete and uploaded to S3"
      )
    }

    # Delete all files created in the AWS worker node if on AWS to free up storage space
    if (run_env == "aws") {
      cli::cli_alert_info(
        "Cleaning up local year directory: {.path {hs_analysis_year_dir}}"
      )
      unlink(hs_analysis_year_dir)
    }

    # Cleanup: analysis year  ------------------
    # Clear workspace for next iteration of the year loop
    # Keep only:
    # - Configuration variables (analysis_setup, analysis_info, solver_type, etc.)
    # - Static data that doesn't change between years (coproduct_codes, no_solve_countries)
    # - S3 helper functions (needed for next year's S3 operations)
    # 
    # This prevents memory bloat during multi-year processing

    rm(
      list = ls()[
        !(ls() %in%
          c(
            "analysis_setup",
            analysis_setup,
            "coproduct_codes",
            "no_solve_countries",
            "countries_to_analyze",
            "solver_type",
            "analysis_info",
            analysis_info,
            # Keep S3 helpers available for the next loop iteration
            "s3_clear_prefix",
            "s3_list_keys"
          ))
      ]
    )
    # Remove specific large objects that are no longer needed on local machine
    rm(analysis_year)
    rm(hs_analysis_year_dir)
  } # end of analysis year loop
  
  # Calculate elapsed time
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  
  # Display completion message and perform environment-specific cleanup
  cli::cli_h2("{solver_type} Country Solutions Complete")
  cli::cli_alert_success("All analysis years completed")
  cli::cli_alert_info(c(
    "i" = "HS version: {.strong HS{HS_year_rep}}",
    "i" = "Solver: {.strong {solver_type}}",
    "i" = "Years processed: {.val {paste(analysis_years_rep$analysis_year, collapse = ', ')}}",
    "i" = "Total elapsed time: {.strong {format(elapsed_time, digits = 2)}}"
  ))

  # Cleanup - Final all years complete ----------------------------------------------------------
  # After all analysis years are complete, remove the entire model data directory
  # on AWS to free up storage space on docker instance
  # 
  # This includes:
  # - All input CSV files downloaded from S3 at the start
  # - Intermediate processing files
  # - Any temporary files created during the run
  # 
  # Safe because all outputs are in S3 and all inputs can be re-downloaded if needed
  if (run_env == "aws") {
    cli::cli_alert_info(c(
      "i" = "Results location: {.path s3://{s3_bucket_name}/{outdir}/{hs_dir}/}",
      "i" = "Cleaning up local (docker instance) model data: {.path {datadir}}"
    ))
    unlink(datadir, recursive = TRUE)
    cli::cli_alert_success("Docker instance cleanup complete - storage freed")
  } else {
    cli::cli_alert_info(c(
      "i" = "Results location: {.path {file.path(outdir, hs_dir)}}"
    ))
    cli::cli_alert_success("Local run complete - files saved to disk")
  }

  # Return invisibly (function is called for side effects, not return value)
  invisible(NULL)

} # end of get_country_solutions fun definition
