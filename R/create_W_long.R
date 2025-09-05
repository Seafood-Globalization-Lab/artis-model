#' Create long-form W matrix by country in parallel. W is the estimates of 
#' the proportion of a given imported product that converts to each other product
#'
#' Reformat each country's W matrix into a long data frame and bind them
#' together in parallel.
#'
#' @param country_est A named list where each element has a matrix `W`
#'   (rows = processed hs6, cols = original hs6) for that country.
#' @param num_cores Integer number of parallel workers for the cluster.
#'
#' @return A data frame with columns:
#'   `hs6_processed`, `hs6_original`, `exporter_iso3c`, `estimated_W`.
#'
#' @details Uses `foreach` + `doParallel` with `parallel::makeCluster(type = "FORK")`.
#'   On Windows, use `"PSOCK"` instead of `"FORK"`.
#'
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate filter
#' @importFrom stringr str_extract
#' @importFrom magrittr %>%
#'
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