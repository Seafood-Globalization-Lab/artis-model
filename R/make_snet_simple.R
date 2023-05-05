#' @export

make_snet_simple <- function(country_est, C_net, V1, country_combos){
  S_dat_list <- vector(mode = "list", length = nrow(country_combos)) # pre-allocate length of S_dat_list
  X_country <- lapply(country_est, FUN = function(i) i$X)
  X_country[is.na(X_country)] <- 0
  
  # Make C_block for all country_combos and create a conditional statement based on sum() == 0?
  C_block_sum <- vector(mode = "list", length = nrow(country_combos))
  for(i in 1:nrow(country_combos)){
    importer = country_combos$importer[i]
    exporter = country_combos$exporter[i]
    C_block <- C_net[(substr(rownames(C_net), 1, 3) == importer) , (substr(colnames(C_net), 1, 3) == exporter)]
    C_block_sum[[i]] <- sum(C_block)
  } # takes 4 minutes
  C_test <- C_block_sum != 0
  # Note: unable to calcualte X_p outside of the loop: vectory memory exhausted
  
  # Change V1 to convert from commodity to live weight
  V1[V1>0] <- 1/V1[V1>0]
  
  # Now create S_net, but only run the loop for when C_test == TRUE
  for(i in (1:nrow(country_combos))[C_test]){
    importer = country_combos$importer[i]
    exporter = country_combos$exporter[i]
    
    # Select block: C_block is processing + production of PRODUCTS between AND within countries: e.g., product A in USA becomes product B, C, D in CHN
    C_block <- C_net[(substr(rownames(C_net), 1, 3) == importer) , (substr(colnames(C_net), 1, 3) == exporter)]
    C_block <- as.matrix(C_block)
    
    # FIX IT: This likely needs to be added to regular conversion to species net
    p <- country_est[[exporter]]$p
    X_country[[exporter]] <- sweep(X_country[[exporter]], MARGIN = 2, FUN = "*", STATS = p)
    X_country[[exporter]] <- sweep(X_country[[exporter]], MARGIN = 1, FUN = "/", STATS = rowSums(X_country[[exporter]], na.rm = TRUE))
    X_country[[exporter]][is.na(X_country[[exporter]])] <- 0

    # NOTES: X_country is SPECIES to PRODUCT conversion matrix (Species 1, 2, 3 becomes Product A)
    S_block <- diag(rowSums(t(V1 * X_country[[exporter]]) %*% (C_block)))
    colnames(S_block) <- paste(exporter, "_", colnames(X_country[[exporter]]), sep = "") 
    rownames(S_block) <- paste(importer, "_", colnames(X_country[[exporter]]), sep = "") # colnames for X_country (species names) is the same for exporter and importer
    
    # instead of: S_net[rownames(S_net) == rownames(S_block), colnames(S_net) == colnames(S_block)] <- S_block
    # make into three column data frame and omit NAs
    
    # Replace 0s with NAs so they are automatically dropped in melt.data.table
    S_block[(S_block==0)]<-NA
    S_as_dt <- as.data.table(S_block, keep.rownames = TRUE) # doesn't automatically drop NAs because rownames are retained for now
    S_melt_i <- melt.data.table(data = S_as_dt, id.vars = "rn", measure.vars = names(S_as_dt)[-1], na.rm = TRUE)
    names(S_melt_i) <- c("importer", "exporter", "quantity")
    #S_melt_i$i <- i # in case we want to keep track of which iteration produced it (for data checks)
    S_dat_list[[i]] <- S_melt_i
  }
  
  S_net <- data.table::rbindlist(S_dat_list)
  
  return(S_net)
}