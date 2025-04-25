#' @export
build_artis_data <- function(artis_dir, 
                             outdir,
                             estimate_data_type = "midpoint", 
                             run_env = "", 
                             s3_bucket_name = "", 
                             s3_region = "") {
  
  # Collect all trade data------------------------------------------------------
  if (estimate_data_type == "midpoint") {
    print("Collecting trade midpoint data")
    snet_midpoint_regexr <- "S-net_raw_midpoint"
    trade_midpoint <- collect_data(artis_dir = artis_dir, 
                                   file_pattern = snet_midpoint_regexr,
                                   run_env = run_env, 
                                   s3_bucket_name = s3_bucket_name, 
                                   s3_region = s3_region)
    
    trade_mid_fp <- file.path(outdir, "snet_midpoint_all_hs_all_years.csv")
    
    print("Writing trade midpoint data")
    write.csv(trade_midpoint, trade_mid_fp, row.names = FALSE)
    print("Done..writing trade midpoint")
    
    if (run_env == "aws") {
      print("Uploading snet (midpoint) to AWS S3 bucket")
      if (file.size(trade_mid_fp) > 5368709120) {
        # Use multipart upload for large files
        put_object(
          file = trade_mid_fp,
          object = trade_mid_fp,
          bucket = s3_bucket_name,
          multipart = TRUE
        )
      } else {
        # Standard upload
        put_object(
          file = trade_mid_fp,
          object = trade_mid_fp,
          bucket = s3_bucket_name
        )
      }
      
      # Remove files to preserve space
      file.remove(trade_mid_fp)
      gc()
    }
  }
  
  if (estimate_data_type == "max") {
    print("Collecting trade max data")
    snet_max_regexr <- "S-net_raw_max"
    trade_max <- collect_data(artis_dir, snet_max_regexr,
                              run_env = "aws", s3_bucket_name, s3_region)
    
    trade_max_fp <- file.path(outdir, "snet_max_all_hs_all_years.csv")
    
    print("Writing trade max data")
    write.csv(trade_max, trade_max_fp, row.names = FALSE)
    print("Done..writing trade max")
    
    if (run_env == "aws") {
      print("Uploading snet (max) to AWS S3 bucket")
      if (file.size(trade_max_fp) > 5368709120) {
        # Use multipart upload for large files
        put_object(
          file = trade_max_fp,
          object = trade_max_fp,
          bucket = s3_bucket_name,
          multipart = TRUE
        )
      } else {
        # Standard upload
        put_object(
          file = trade_max_fp,
          object = trade_max_fp,
          bucket = s3_bucket_name
        )
      }
      
      # Remove files to preserve space
      file.remove(trade_max_fp)
      gc()
    }
  }
  
  if (estimate_data_type == "min") {
    print("Collecting trade min data")
    snet_min_regexr <- "S-net_raw_min"
    trade_min <- collect_data(artis_dir, snet_min_regexr,
                              run_env = "aws", s3_bucket_name, s3_region)
    
    trade_min_fp <- file.path(outdir, "snet_min_all_hs_all_years.csv")
    
    print("Writing trade min data")
    write.csv(trade_min, trade_min_fp, row.names = FALSE)
    print("Done..writing trade min")
    
    if (run_env == "aws") {
      print("Uploading snet (min) to AWS S3 bucket")
      if (file.size(trade_min_fp) > 5368709120) {
        # Use multipart upload for large files
        put_object(
          file = trade_min_fp,
          object = trade_min_fp,
          bucket = s3_bucket_name,
          multipart = TRUE
        )
      } else {
        # Standard upload
        put_object(
          file = trade_min_fp,
          object = trade_min_fp,
          bucket = s3_bucket_name
        )
      }
      
      # Remove files to preserve space
      file.remove(trade_min_fp)
      gc()
    }
  }
  
  # Collect all consumption data------------------------------------------------
  
  if (estimate_data_type == "midpoint") {
    print("Collecting consumption midpoint")
    consumption_midpoint_regexr <- "consumption_midpoint"
    consumption_midpoint <- collect_data(artis_dir, consumption_midpoint_regexr,
                                         run_env = run_env, s3_bucket_name, s3_region)
    
    consumption_mid_fp <- file.path(outdir, "consumption_midpoint_all_hs_all_years.csv")
    
    print("Writing consumption midpoint")
    write.csv(consumption_midpoint, consumption_mid_fp, row.names = FALSE)
    print("Done...writing consumption midpoint")
    
    if (run_env == "aws") {
      print("Uploading consumption (midpoint) to AWS S3")
      if (file.size(consumption_mid_fp) > 5368709120) {
        # Use multipart upload for large files
        put_object(
          file = consumption_mid_fp,
          object = consumption_mid_fp,
          bucket = s3_bucket_name,
          multipart = TRUE
        )
      } else {
        # Standard upload
        put_object(
          file = consumption_mid_fp,
          object = consumption_mid_fp,
          bucket = s3_bucket_name
        )
      }
      
      # Remove files to preserve space
      file.remove(consumption_mid_fp)
      gc()
    }
  }
  
  if (estimate_data_type == "max") {
    print("Collecting consumption max")
    consumption_max_regexr <- "consumption_max"
    consumption_max <- collect_data(artis_dir, consumption_max_regexr,
                                    run_env = run_env, s3_bucket_name, s3_region)
    
    consumption_max_fp <- file.path(outdir, "consumption_max_all_hs_all_years.csv")
    
    print("Writing consumption max")
    write.csv(consumption_max, consumption_max_fp, row.names = FALSE)
    print("Done...writing consumption max")
    
    if (run_env == "aws") {
      print("Uploading consumption (max) to AWS S3")
      if (file.size(consumption_max_fp) > 5368709120) {
        # Use multipart upload for large files
        put_object(
          file = consumption_max_fp,
          object = consumption_max_fp,
          bucket = s3_bucket_name,
          multipart = TRUE
        )
      } else {
        # Standard upload
        put_object(
          file = consumption_max_fp,
          object = consumption_max_fp,
          bucket = s3_bucket_name
        )
      }
      
      # Remove files to preserve space
      file.remove(consumption_max_fp)
      gc()
    }
  }
  
  if (estimate_data_type == "min") {
    print("Collecting consumption min")
    consumption_min_regexr <- "consumption_min"
    consumption_min <- collect_data(artis_dir, consumption_min_regexr,
                                    run_env = run_env, s3_bucket_name, s3_region)
    
    consumption_min_fp <- file.path(outdir, "consumption_min_all_hs_all_years.csv")
    
    print("Writing consumption min")
    write.csv(consumption_min, consumption_min_fp, row.names = FALSE)
    print("Done...writing consumption min")
    
    if (run_env == "aws") {
      print("Uploading consumption (min) to AWS S3")
      if (file.size(consumption_min_fp) > 5368709120) {
        # Use multipart upload for large files
        put_object(
          file = consumption_min_fp,
          object = consumption_min_fp,
          bucket = s3_bucket_name,
          multipart = TRUE
        )
      } else {
        # Standard upload
        put_object(
          file = consumption_min_fp,
          object = consumption_min_fp,
          bucket = s3_bucket_name
        )
      }
      
      # Remove files to preserve space
      file.remove(consumption_min_fp)
      gc()
    }
  }
}
