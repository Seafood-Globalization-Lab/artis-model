#' Combine partitioned ARTIS data files into a single Parquet file
#'
#' @param search_dir Directory where partitioned files are stored.
#' @param outdir Output directory for the combined parquet file.
#' @param data_type Data type: "artis" (trade), "consumption", etc.
#' @param estimate_data_type Which estimate: e.g., "midpoint", "max", "min".
#' @param artis_version Model version string, e.g. "v1.2.0".
#' @param file_type Input file format. Options: "qs" (default), "csv", "parquet".
#' @param date Date string for output file naming (default: today's date).
#' @param search_pattern Regex to match the desired files.
#' @param custom_timeseries Logical. If TRUE, adds "custom_ts" to output filename.
#' @param verbose Logical. Print file names as they are added.
#'
#' @import duckdb
#' @import DBI
#' @import glue
#' @import qs2
#' @import datatable
#' @export
combine_partitioned_data <- function(
    search_dir,
    outdir,
    data_type = c("artis", "consumption"),
    estimate_data_type = c("midpoint", "max", "min"),
    artis_version = "v1.0.0",
    file_type = c("qs", "csv", "parquet"),
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
      glue("{data_type}_{estimate_short}_custom_ts_{artis_version}_{date}.parquet")
    )
  } else {
    out_file <- file.path(
      outdir,
      glue("{data_type}_{estimate_short}_all_HS_yrs_{artis_version}_{date}.parquet")
    )
  }
  
  con <- dbConnect(duckdb())
  on.exit(dbDisconnect(con), add = TRUE)
  
  for (f in df_files) {
    if (verbose) message(glue("Adding {f}"))
    chunk <- switch(
      file_type,
      qs = qs2::qd_read(f),
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
  if (verbose) message(glue("Wrote combined data to {out_file}"))
}
