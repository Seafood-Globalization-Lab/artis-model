#' @export
create_X_long <- function(country_est, num_cores) {

  X_long <- data.frame()

  for (i in 1:length(names(country_est))) {
      curr_country <- names(country_est)[i]
      curr_X_long <- as.data.frame(country_est[[curr_country]]$X)

      curr_X_long <- curr_X_long %>%
        rownames_to_column(var = "hs6") %>%
        pivot_longer(2:(ncol(curr_X_long)+1),
                     names_to = "SciName",
                     values_to = "estimated_X") %>%
        filter(estimated_X > 0) %>%
        mutate(iso3c = str_extract(hs6, "[[:alpha:]]+"),
               hs6 = str_extract(hs6, "[[:digit:]]+"))
      
      X_long <- X_long %>%
        bind_rows(curr_X_long)
  }
  
  X_long <- X_long %>%
    separate(SciName, c("sciname", "habitat", "method"), sep = "_") %>%
    mutate(sciname = gsub("\\.", " ", sciname))
  
  return(X_long)
}
