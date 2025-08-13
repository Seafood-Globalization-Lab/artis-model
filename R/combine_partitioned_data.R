#' Combine partitioned ARTIS data files into a single Parquet file
#'
#' @param search_dir Directory where partitioned files are stored.
#' @param outdir Output directory for the combined parquet file.
#' @param data_type Data type: "trade" (snet), "consumption", etc.
#' @param estimate_data_type Which estimate: e.g., "midpoint", "max", "min".
#' @param artis_version Model version string, e.g. "v1.2.0".
#' @param prod_data_type Production model configuration used for file naming only.
#' @param file_type Input file format. Options: "qs" (default), "csv", "parquet".
#' @param date Date string for output file naming (default: today's date).
#' @param search_pattern Regex to match the desired files.
#' @param custom_timeseries Logical. If TRUE, adds "custom_ts" to output filename. FIXIT: add this functionality.
#' @param verbose Logical. Print file names as they are added.
#'
#' @importFrom duckdb duckdb
#' @importFrom DBI dbConnect dbExecute dbDisconnect dbExistsTable dbWriteTable
#' @import glue
#' @import qs2
#' @import data.table
#' @export
combine_partitioned_data <- function(
    search_dir,
    outdir,
    data_type = c("trade", "consumption"),
    estimate_data_type = c("midpoint", "max", "min"),
    artis_version = "v1.0.0",
    prod_data_type = prod_data_type,
    file_type = c("qs2", "qdata", "csv", "parquet"),
    date = format(Sys.Date(), "%Y-%m-%d"),
    search_pattern,
    custom_timeseries = TRUE,
    verbose = TRUE
) {
  data_type <- match.arg(data_type)
  estimate_data_type <- match.arg(estimate_data_type)
  file_type <- match.arg(file_type)
  
  # Map estimate_data_type to short label for filenames
  estimate_short <- c(midpoint = "mid", max = "max", min = "min")[estimate_data_type]
  
  df_files <- list.files(
    path = search_dir,
    pattern = search_pattern,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = TRUE
  )
  
  # Output file naming logic (write to outdir)
  if (custom_timeseries) {
    out_file <- file.path(
      outdir,
      glue("ARTIS_{artis_version}_{data_type}_{prod_data_type}_{estimate_short}_custom_ts_{date}.parquet")
    )
  } else {
    out_file <- file.path(
      outdir,
      glue("ARTIS_{artis_version}_{data_type}_{prod_data_type}_{estimate_short}_all_HS_yrs_{date}.parquet")
    )
  }
  
  con <- dbConnect(duckdb())
  dbExecute(con, "PRAGMA max_temp_directory_size='500GiB'")  # increase spill-to-disk limit
  on.exit(dbDisconnect(con), add = TRUE)
  
  for (f in df_files) {
    if (verbose) message(glue("Appending {which(f == df_files)}/{length(df_files)}: {f}"))
    if (!verbose) message(glue("Appending {which(f == df_files)}/{length(df_files)}"))
    chunk <- switch(
      file_type,
      qs2 = qs2::qs_read(f),
      qdata = qs2::qd_read(f),
      csv = data.table::fread(f, stringsAsFactors = FALSE),
      parquet = arrow::read_parquet(f)
    )
    if (!dbExistsTable(con, "combined")) {
      dbWriteTable(con, "combined", chunk, overwrite = TRUE)
    } else {
      dbWriteTable(con, "combined", chunk, append = TRUE)
    }
    rm(chunk); gc()
  }
  
  sql <- glue("COPY combined TO '{out_file}' (FORMAT 'parquet')")
  dbExecute(con, sql)
  if (verbose) message(glue("Finished writing out combined {data_type} file: {out_file}"))
  if (!verbose) message(glue("Finished writing out combined {data_type}"))
}
