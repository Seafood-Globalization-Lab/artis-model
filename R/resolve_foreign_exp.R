#' @export
# Resolves foreign exports by 1 stage back
#   finding which foreign exports originated from foreign/domestic/error exports
#   all flows from foreign exports that are less than the resolve threshold (default 1 tonne)
#     will be hard coded as originating from error exports and will not be further resolved
#   all flows that are less than the zero_threshold (default 1e-3 tonnes) will be removed
resolve_foreign_exp <- function(foreign_exports_in, reweight_W_long, import_props,
                                hs_clade_match, resolve_threshold = 1, zero_threshold = 1e-3) {
  
  # find the origins (domestic/foreign/error exports) for all foreign exports
  exp_breakdown <- foreign_exports_in %>%
    # format to follow perspective of re exporter
    rename(re_exporter_iso3c = exporter_iso3c,
           hs6_processed = hs6) %>%
    # Get conversion factors for hs6 processed to hs6 original for re exporter
    left_join(
      reweight_W_long,
      by = c("re_exporter_iso3c" = "exporter_iso3c", "hs6_processed")
    ) %>%
    # disggregate foreign exports hs6 processed into their original hs6 imports
    mutate(product_weight_t = product_weight_t * reweighted_W) %>%
    select(-c(reweighted_W)) %>%
    # find proportion of imports by origin (domestic/foreign/error exports)
    # and source country
    left_join(
      import_props %>%
        select(importer_iso3c, exporter_iso3c, hs6, source_type, import_prop),
      by = c("re_exporter_iso3c"="importer_iso3c", "hs6_original"="hs6")
    ) %>%
    # Now we have a link between importer, re exporter and exporter by source type
    mutate(product_weight_t = product_weight_t * import_prop) %>%
    select(-c(import_prop)) %>%
    # weight threshold eliminating flows that are essentially 0
    filter(product_weight_t > zero_threshold)
  
  # Separate into flows that should be further resolved vs flows that will be
  # categorized as originating from an error export
  unresolved_foreign_exp <- exp_breakdown %>%
    filter(product_weight_t < resolve_threshold)
  
  exp_breakdown <- exp_breakdown %>%
    filter(product_weight_t >= resolve_threshold)
  
  # Note:
  # - Export flows that originate from domestic or error exports
  #   can be broken down further to sciname
  # - Export flows that still originate from foreign exports
  #   have to be processed further
  
  resolved_dom_exp <- exp_breakdown %>%
    filter(source_type == "domestic") %>%
    select(-c(source_type))
  
  resolved_error_exp <- exp_breakdown %>%
    filter(source_type == "error") %>%
    select(-c(source_type))
  
  resolved_foreign_exp <- exp_breakdown %>%
    filter(source_type == "foreign") %>%
    select(-c(source_type))
  
  # clearing data for faster processing
  rm(exp_breakdown)
  gc()
  
  return(list(resolved_dom_exp, resolved_error_exp, resolved_foreign_exp, unresolved_foreign_exp))
}
