#' @export
create_W_long <- function(country_est, num_cores) {
  # Creating reweighted W long that finds proportion of hs6 processed codes that
  # come from hs6 original codes outlines how much hs6 original code gets
  # transferred to hs6 processed code
  W_long <- data.frame()
  
  # creating a cluster of cores to parallelize creating a dataframe for W long:
  # hs6 processed, hs6 original, exporter_iso3c
  w_long_cl <- makeCluster(num_cores, type="FORK")
  registerDoParallel(w_long_cl) 
  
  # Parallel approach to building W long
  W_long <- foreach(i = 1:length(names(country_est)), .combine = rbind) %dopar% {
    curr_country <- names(country_est)[i]
    curr_W_long <- as.data.frame(country_est[[curr_country]]$W)
    
    curr_W_long %>%
      # Reformat W as a long data frame
      # Original imported product form is on the columns and
      # processed form is on the rows
      rownames_to_column(var = "hs6_processed") %>%
      pivot_longer(2:(ncol(curr_W_long)+1), 
                   names_to = "hs6_original", 
                   values_to = "estimated_W") %>%
      # Separate hs codes
      mutate(hs6_original = str_extract(hs6_original, "[[:digit:]]+"),
             hs6_processed = str_extract(hs6_processed, "[[:digit:]]+")) %>%
      mutate(exporter_iso3c = curr_country) %>%
      filter(estimated_W > 0)
  }
  # Free up clusters after use
  stopCluster(w_long_cl)
  
  return(W_long)
}