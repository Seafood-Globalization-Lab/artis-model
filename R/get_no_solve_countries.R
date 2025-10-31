#' Get countries with unsolved QP problems for a specific HS version
#'
#' Retrieves countries where the quadprog solver failed for a specific HS version
#' and set of analysis years. This list is passed to a second run of 
#' \code{get_country_solutions()} using the cvxopt solver.
#'
#' @param quadprog_HS_dir Character. Path to the **HS-version-specific** quadprog 
#'   output directory. This should be constructed as:
#'   \code{file.path(outdir_quadprog, paste0("HS", hs_version))}
#'   
#'   Example: "outputs_2.1.1_SAU/quadprog_snet/HS96"
#'   
#'   The function expects year subdirectories within this path:
#'   \preformatted{
#'   quadprog_HS_dir/                 # HS-specific directory (e.g., .../HS96/)
#'     ├── 1996/                      # Year subdirectory
#'     │   └── YYYY-MM-DD_analysis-documentation_..._1996_HS96.txt
#'     ├── 1997/
#'     │   └── YYYY-MM-DD_analysis-documentation_..._1997_HS96.txt
#'     └── 1998/
#'         └── YYYY-MM-DD_analysis-documentation_..._1998_HS96.txt
#'   }
#'   
#'   For AWS: S3 prefix to HS-specific directory 
#'   (e.g., "outputs/quadprog_snet/HS96")
#'
#' @param artis_run_date Character. ARTIS run date in format "YYYY-MM-DD"
#' @param run_env Character. "aws" for S3 storage or other for local. Default: "aws"
#' @param s3_bucket_name Character. S3 bucket name (AWS only). Default: ""
#' @param s3_region Character. AWS region (AWS only). Default: ""
#'
#' @return Data frame with three columns:
#'   \describe{
#'     \item{country_iso3}{Character. Three-letter ISO3 country code}
#'     \item{year}{Numeric. Analysis year when solution failed}
#'     \item{hs_version}{Character. HS version code (e.g., "HS96")}
#'   }
#'   Returns empty data frame if all countries solved successfully.
#'
#' @details
#' \strong{Pipeline Workflow (per HS version):}
#' \enumerate{
#'   \item Run \code{get_country_solutions()} with quadprog for one HS version
#'   \item Call this function to identify failed countries for that HS version
#'   \item Re-run \code{get_country_solutions()} with cvxopt on failures only
#'   \item Repeat for next HS version
#' }
#'
#' The function searches only within the specified HS directory for year 
#' subdirectories containing files matching the pattern:
#' "YYYY-MM-DD_analysis-documentation_countries-with-no-solve-qp-solution_YYYY_HS##.txt"
#'
#' Files contain space-separated ISO3 country codes that failed quadprog solver.
#' Countries are validated using regex "^[A-Z][A-Z][A-Z]$" to ensure valid ISO3 codes.
#'
#' @seealso \code{\link{get_country_solutions}}
#'
#' @importFrom aws.s3 get_bucket_df save_object
#' @importFrom dplyr filter select mutate bind_rows pull
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_detect str_extract str_length
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success
#' @export
get_no_solve_countries <- function(quadprog_HS_dir, 
                                   artis_run_date,
                                   run_env = "aws", 
                                   s3_bucket_name = "", 
                                   s3_region = "") {
  
  cli::cli_alert_info("Retrieving countries that failed quadprog solver")
  cli::cli_alert_info("Searching in: {.path {quadprog_HS_dir}}")
  cli::cli_alert_info("Expected run date prefix: {.val {artis_run_date}}")
  
  # Initialize empty data frame to collect results across all files
  no_solve_countries <- data.frame()
  
  if (run_env == "aws") {
    # AWS workflow: Download files from S3
    
    # List all objects in S3 bucket matching the no-solve pattern
    no_solve_files <- aws.s3::get_bucket_df(
      bucket = s3_bucket_name,
      region = s3_region,
      prefix = quadprog_HS_dir,
      max = Inf
    ) %>%
      filter(str_detect(Key, pattern = "countries-with-no-solve-qp-solution")) %>%
      pull(Key) %>%
      unique()
    
    if (length(no_solve_files) == 0) {
      cli::cli_alert_success("All countries solved with quadprog - no cvxopt retry needed")
      return(no_solve_countries)
    }
    
    cli::cli_alert_success("Found {length(no_solve_files)} file{?s} with unsolved countries")

    # Process each file
    for (i in 1:length(no_solve_files)) {
      
      curr_no_solve_aws_fp <- no_solve_files[i]

      # Extract date from filename to check for match
      file_date <- str_extract(basename(curr_no_solve_aws_fp), pattern = "^\\d{4}-\\d{2}-\\d{2}")
      
      if (!is.na(file_date) && file_date != artis_run_date) {
        cli::cli_alert_warning(c(
          "!" = "Date mismatch in file: {.file {basename(curr_no_solve_aws_fp)}}",
          "i" = "Expected: {.val {artis_run_date}}, Found: {.val {file_date}}",
          "i" = "This may be OK if the run spanned multiple days"
        ))
      }

      local_no_solve_fp <- file.path(quadprog_HS_dir, curr_no_solve_aws_fp)
      
      # Download file from S3 to local temporary storage
      aws.s3::save_object(
        object = curr_no_solve_aws_fp,
        bucket = s3_bucket_name,
        region = s3_region,
        file = local_no_solve_fp
      )
      
      # Read space-separated file (country codes printed in rows)
      curr_no_solve <- read.csv(local_no_solve_fp, header = FALSE, sep = " ")
      
      if (nrow(curr_no_solve) > 0) {
        # Reshape from wide to long format (handles multiple countries per row)
        curr_no_solve <- curr_no_solve %>%
          pivot_longer(colnames(curr_no_solve), names_to = "col1", values_to = "country_iso3") %>%
          select(country_iso3) %>%
          filter(!is.na(country_iso3)) %>%
          filter(str_detect(country_iso3, "^[A-Z][A-Z][A-Z]$")) # Validate ISO3 format
        
        # Extract year from S3 key pattern: *_YYYY_*
        curr_year <- str_extract(curr_no_solve_aws_fp, pattern = "_[0-9][0-9][0-9][0-9]_")
        curr_year <- as.numeric(substr(curr_year, 2, str_length(curr_year) - 1))
        
        # Extract HS version from S3 key pattern: HS##
        curr_hs_version <- str_extract(curr_no_solve_aws_fp, pattern = "HS[0-9][0-9]")
        
        # Add metadata columns
        curr_no_solve <- curr_no_solve %>%
          mutate(
            year = curr_year,
            hs_version = curr_hs_version
          )
        
        # Append to results
        no_solve_countries <- no_solve_countries %>%
          bind_rows(curr_no_solve)
      }
    }
    
  } else {
    # Local workflow: Read files from local file system
    # quadprog_HS_dir already points to HS-specific directory (e.g., .../HS96/)
    
    # Get year subdirectories within the HS directory
    year_dirs <- list.dirs(quadprog_HS_dir, recursive = FALSE)
    
    if (length(year_dirs) == 0) {
      cli::cli_alert_success("No year directories found - all countries solved with quadprog")
      return(no_solve_countries)
    }
    
    # Extract HS version from quadprog_HS_dir path (last 4 characters)
    curr_hs_version <- substring(quadprog_HS_dir, nchar(quadprog_HS_dir) - 3, nchar(quadprog_HS_dir))
    
    for (j in 1:length(year_dirs)) {
      curr_year_dir <- year_dirs[j]
      # Extract year from directory name (last 4 characters)
      curr_year <- as.numeric(substring(curr_year_dir, nchar(curr_year_dir) - 3, nchar(curr_year_dir)))
      
      # Construct expected filename
      filename <- paste(
        artis_run_date,
        "analysis-documentation",
        "countries-with-no-solve-qp-solution",
        curr_year,
        curr_hs_version,
        sep = "_"
      )
      filename <- paste(filename, ".txt", sep = "")
      curr_fp <- file.path(curr_year_dir, filename)
      
      # Only process if file exists
      if (file.exists(curr_fp)) {
        curr_no_solve <- read.csv(curr_fp, header = FALSE, sep = " ")
        
        if (nrow(curr_no_solve) > 0) {
          # Reshape and validate country codes
          curr_no_solve <- curr_no_solve %>%
            pivot_longer(colnames(curr_no_solve), names_to = "col1", values_to = "country_iso3") %>%
            select(country_iso3) %>%
            filter(!is.na(country_iso3)) %>%
            filter(str_detect(country_iso3, "^[A-Z][A-Z][A-Z]$")) # Validate ISO3 format
          
          # Add metadata columns
          curr_no_solve <- curr_no_solve %>%
            mutate(
              year = curr_year,
              hs_version = curr_hs_version
            )
          
          # Append to results
          no_solve_countries <- no_solve_countries %>%
            bind_rows(curr_no_solve)
        }
      }
    }
  }
  
  if (nrow(no_solve_countries) > 0) {
    cli::cli_alert_info("Found {nrow(no_solve_countries)} countr{?y/ies} to retry with cvxopt solver")
  } else {
    cli::cli_alert_success("All countries solved successfully with quadprog")
  }
  
  return(no_solve_countries)
}