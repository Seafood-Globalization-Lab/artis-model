#' @export
build_artis_data <- function(artis_dir,
                             outdir) {
  
  # Collect all trade data------------------------------------------------------
  print("Collecting trade midpoint data")
  snet_midpoint_regexr <- "S-net_raw_midpoint"
  trade_midpoint <- collect_data(artis_dir, snet_midpoint_regexr)
  
  print("Writing trade midpoint data")
  write.csv(trade_midpoint,
            file.path(outdir, "snet_midpoint_all_hs_all_years.csv"),
            row.names = FALSE)
  print("Done..writing trade midpoint")
  
  print("Collecting trade max data")
  snet_max_regexr <- "S-net_raw_max"
  trade_max <- collect_data(artis_dir, snet_max_regexr)
  
  print("Writing trade max data")
  write.csv(trade_max,
            file.path(outdir, "snet_max_all_hs_all_years.csv"),
            row.names = FALSE)
  print("Done..writing trade max")
  
  print("Collecting trade min data")
  snet_min_regexr <- "S-net_raw_min"
  trade_max <- collect_data(artis_dir, snet_min_regexr)
  
  print("Writing trade min data")
  write.csv(trade_min,
            file.path(outdir, "snet_min_all_hs_all_years.csv"),
            row.names = FALSE)
  print("Done..writing trade min")
  
  # Collect all consumption data------------------------------------------------
  print("Collecting consumption midpoint")
  consumption_midpoint_regexr <- "consumption_midpoint"
  consumption_midpoint <- collect_data(artis_dir, consumption_midpoint_regexr)
  
  print("Writing consumption midpoint")
  write.csv(consumption_midpoint,
            file.path(outdir, "consumption_midpoint_all_hs_all_years.csv"),
            row.names = FALSE)
  print("Done...writing consumption midpoint")
  
  print("Collecting consumption max")
  consumption_max_regexr <- "consumption_max"
  consumption_max <- collect_data(artis_dir, consumption_max_regexr)
  
  print("Writing consumption max")
  write.csv(consumption_max,
            file.path(outdir, "consumption_max_all_hs_all_years.csv"),
            row.names = FALSE)
  print("Done...writing consumption max")
  
  print("Collecting consumption min")
  consumption_min_regexr <- "consumption_min"
  consumption_min <- collect_data(artis_dir, consumption_min_regexr)
  
  print("Writing consumption min")
  write.csv(consumption_max,
            file.path(outdir, "consumption_min_all_hs_all_years.csv"),
            row.names = FALSE)
  print("Done...writing consumption min")
}
