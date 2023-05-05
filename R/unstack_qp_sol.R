#' @export
unstack_qp_sol <- function(qp_sol, qp_inputs) {
  
  # Save solution to vector
  x <- qp_sol
  
  # Assign all list elements in qp_inputs to separate objects
  Z <- qp_inputs$Z # Z is matrix with the positions of the unkowns for X, W, consumption, and error
  non_null_consumption <- qp_inputs$non_null_consumption
  null_production <- qp_inputs$null_production
  null_consumption <- qp_inputs$null_consumption
  null_imports <- qp_inputs$null_imports
  dim_V1 <- qp_inputs$dim_V1
  dim_V2 <- qp_inputs$dim_V2
  V_X <- qp_inputs$V_X
  V_W <- qp_inputs$V_W
  scale_factor <- qp_inputs$scale_factor
  p <- qp_inputs$production
  exports <- qp_inputs$exports
  imports <- qp_inputs$imports
  
  # pad solution to length(c(Z)) with zeros where c(Z) is NA
  X <- matrix(0, nrow = length(c(Z)))
  X[which(is.na(c(Z))), 1] <- x
  
  # unstack to dim(Z)
  dim(X) <- dim(Z)
  
  # separate to error, consumption, X, and W
  out_error <- X[,(ncol(X)-non_null_consumption+1):ncol(X)]
  out_error <- out_error * scale_factor # Rescale error terms
  if (is.matrix(out_error)){
    out_error <- diag(out_error)
  } else {}
  out_error[which(null_consumption == FALSE)] <- out_error
  out_error[which(null_consumption == TRUE)] <- 0
  
  out_c <- X[,(ncol(X)-2*non_null_consumption+1):(ncol(X)-non_null_consumption)]
  out_c <- out_c * scale_factor # Rescale consumption
  if (is.matrix(out_c)){
    out_c <- diag(out_c)
  } else {}
  out_c[which(null_consumption == FALSE)] <- out_c
  out_c[which(null_consumption == TRUE)] <- 0
  
  X <- X[,1:(ncol(X)-2*non_null_consumption)]
  if (is.matrix(X)==FALSE){
    X <- as.matrix(X, ncol = 1)
  } else {}
  W_cols <- c(logical(ncol(V_X)), !logical(ncol(V_W)))
  
  if (ncol(V_W)!=0){
    W <- X[ , W_cols]
    # insert columns removed due to zero imports
    out_W <- diag(1, dim_V2)
    out_W[!null_consumption, !null_imports] <- W
  } else {out_W <- NA}
  
  if (ncol(V_X)!=0){
    X <- X[ , !W_cols]
    # insert columns removed due to zero production
    out_X <- array(0, dim_V1)
    out_X[!null_consumption, !null_production] <- X
  } else {out_X <- NA}

  return(list(X = out_X, W = out_W, c = out_c, p = p, error_term = out_error, exports = exports, imports = imports))
  # Note: p needed for make_snet_simple
}