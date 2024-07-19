#' @export
build_artis_data <- function(artis_dir, outdir,
                             run_env = "aws", s3_bucket_name = "", s3_region = "") {
  
  # Collect all trade data------------------------------------------------------
  print("Collecting trade midpoint data")
  snet_midpoint_regexr <- "S-net_raw_midpoint"
  trade_midpoint <- collect_data(artis_dir, snet_midpoint_regexr,
                                 run_env = "aws", s3_bucket_name, s3_region)
  
  trade_mid_fp <- file.path(outdir, "snet_midpoint_all_hs_all_years.csv")
  trade_max_fp <- file.path(outdir, "snet_max_all_hs_all_years.csv")
  trade_min_fp <- file.path(outdir, "snet_min_all_hs_all_years.csv")
  
  print("Writing trade midpoint data")
  write.csv(trade_midpoint, trade_mid_fp, row.names = FALSE)
  print("Done..writing trade midpoint")
  
  print("Collecting trade max data")
  snet_max_regexr <- "S-net_raw_max"
  trade_max <- collect_data(artis_dir, snet_max_regexr,
                            run_env = "aws", s3_bucket_name, s3_region)
  
  print("Writing trade max data")
  write.csv(trade_max, trade_max_fp, row.names = FALSE)
  print("Done..writing trade max")
  
  print("Collecting trade min data")
  snet_min_regexr <- "S-net_raw_min"
  trade_max <- collect_data(artis_dir, snet_min_regexr,
                            run_env = "aws", s3_bucket_name, s3_region)
  
  print("Writing trade min data")
  write.csv(trade_min, trade_min_fp, row.names = FALSE)
  print("Done..writing trade min")
  
  if (run_env == "aws") {
    print("Uploading trade files to AWS S3 bucket")
    put_object(
      file = trade_mid_fp,
      object = trade_mid_fp,
      bucket = s3_bucket_name
    )
    put_object(
      file = trade_max_fp,
      object = trade_max_fp,
      bucket = s3_bucket_name
    )
    put_object(
      file = trade_min_fp,
      object = trade_min_fp,
      bucket = s3_bucket_name
    )
    
    # remove files to preserve space
    file.remove(trade_mid_fp)
    file.remove(trade_max_fp)
    file.remove(trade_min_fp)
  }
  
  
  
  # Collect all consumption data------------------------------------------------
  print("Collecting consumption midpoint")
  consumption_midpoint_regexr <- "consumption_midpoint"
  consumption_midpoint <- collect_data(artis_dir, consumption_midpoint_regexr,
                                       run_env = "aws", s3_bucket_name, s3_region)
  
  consumption_mid_fp <- file.path(outdir, "consumption_midpoint_all_hs_all_years.csv")
  consumption_max_fp <- file.path(outdir, "consumption_max_all_hs_all_years.csv")
  consumption_min_fp <- file.path(outdir, "consumption_min_all_hs_all_years.csv")
  
  print("Writing consumption midpoint")
  write.csv(consumption_midpoint, consumption_mid_fp, row.names = FALSE)
  print("Done...writing consumption midpoint")
  
  print("Collecting consumption max")
  consumption_max_regexr <- "consumption_max"
  consumption_max <- collect_data(artis_dir, consumption_max_regexr,
                                  run_env = "aws", s3_bucket_name, s3_region)
  
  print("Writing consumption max")
  write.csv(consumption_max, consumption_max_fp, row.names = FALSE)
  print("Done...writing consumption max")
  
  print("Collecting consumption min")
  consumption_min_regexr <- "consumption_min"
  consumption_min <- collect_data(artis_dir, consumption_min_regexr,
                                  run_env = "aws", s3_bucket_name, s3_region)
  
  print("Writing consumption min")
  write.csv(consumption_min, consumption_min_fp, row.names = FALSE)
  print("Done...writing consumption min")
  
  if (run_env == "aws") {
    print("Uploading consumption files to AWS S3")
    put_object(
      file = consumption_mid_fp,
      object = consumption_mid_fp,
      bucket = s3_bucket_name
    )
    put_object(
      file = consumption_max_fp,
      object = consumption_max_fp,
      bucket = s3_bucket_name
    )
    put_object(
      file = consumption_min_fp,
      object = consumption_min_fp,
      bucket = s3_bucket_name
    )
    
    # remove files to preserve space
    file.remove(consumption_mid_fp)
    file.remove(consumption_max_fp)
    file.remove(consumption_min_fp)
  }
}
