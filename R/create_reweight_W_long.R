#' @export
create_reweight_W_long <- function(W_long, baci_data_analysis_year) {
  # reweighted_W = proportion of hs6 processed that came from hs6 original
  # estimated W = proportion of each imported hs6 going into
  #               each processed hs6 which is available for export
  reweight_W_long <- W_long %>%
    left_join(
      # get total imports by country and hs6 code for weighting
      baci_data_analysis_year %>%
        group_by(importer_iso3c, hs6) %>%
        summarize(product_weight_t = sum(product_weight_t)) %>%
        ungroup() %>%
        rename(iso3c = importer_iso3c),
      by = c("exporter_iso3c"="iso3c", "hs6_original"="hs6")
    ) %>%
    # remove flows where country did not have imports of hs6 original products
    filter(!is.na(product_weight_t)) %>%
    # weight estimated W by the total imports of each hs6 original code by country
    mutate(estimated_W = estimated_W * product_weight_t) %>%
    select(-product_weight_t) %>%
    # reweight estimated W such that you calculate the prop of hs6 processed
    # that came from 'x' hs6 originals
    group_by(exporter_iso3c, hs6_processed) %>%
    mutate(row_sum = sum(estimated_W)) %>%
    ungroup() %>%
    mutate(reweighted_W = estimated_W / row_sum) %>%
    # remove columns that are no longer needed
    select(-c(row_sum, estimated_W))
  
  check_reweight_W <- reweight_W_long %>%
    group_by(exporter_iso3c, hs6_processed) %>%
    summarize(reweighted_W = sum(reweighted_W)) %>%
    ungroup() %>%
    mutate(difference = 1 - reweighted_W) %>%
    filter(abs(difference) > 1e-9)
  
  if (nrow(check_reweight_W) > 0) {
    warning("not all reweight W values group back up to 1.")
    return(NULL)
  }
  
  return(reweight_W_long)
}