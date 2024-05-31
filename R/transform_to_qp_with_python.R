#' @importFrom data.table data.table
#' @importFrom data.table as.data.table 
#' @importFrom reticulate import
#' @importFrom reticulate py_to_r
#' @export
transform_to_qp_with_python <- function(country_j, V1, V2, baci_data_clean, prod_data_clean, sc_n, cc_m, Xq){
  # Create data table with all hs codes to standardize the order
  cc_dt <- data.table(hs6 = as.character(cc_m)) 
  
  # exports: vector of exports from country j (number of commodities x 1)
  exports <- as.data.table(baci_data_clean)
  exports <- exports[exporter_iso3c==country_j, .(total_q = sum(total_q, na.rm = TRUE)), by = hs6] # filter, summarise by groupings
  exports <- exports[cc_dt, on = "hs6"] # right join
  exports <- exports[is.na(total_q), total_q := 0] # mutate
  exports <- as.matrix(exports$total_q)
  
  # imports: vector of imports to country j (number of commodities x 1)
  imports <- as.data.table(baci_data_clean)
  imports <- imports[importer_iso3c==country_j, .(total_q = sum(total_q, na.rm=TRUE)), by = hs6]
  imports <- imports[cc_dt, on = "hs6"]
  imports <- imports[is.na(total_q), total_q := 0]
  imports <- as.matrix(imports$total_q)
  
  # production: vector of domestic production (number of species x 1)
  sc_n_dt <- as.data.table(sc_n)
  setnames(sc_n_dt, old = "sc_n", new = "taxa_source")
  
  production <- as.data.table(prod_data_clean)
  production <- production[country_iso3_alpha == country_j] # filter
  production <- production[sc_n_dt, on = "taxa_source"]
  production <- production[is.na(quantity), quantity := 0]
  production <- as.matrix(production$quantity)
  
  # Re-scale inputs
  scale_factor <- max(production, imports, exports, na.rm = TRUE)
  # If country has no p, imports, or e, don't scale (otherwise dividing by 0 will create NA)
  if (scale_factor == 0) {
    scale_factor <- 1
  }
  p <- production / scale_factor # Once re-scaled, rename as "p", "i", "e"
  i <- imports / scale_factor
  e <- exports / scale_factor
  
  # Eliminate unknowns that will be assumed zero due to zero exports, production, and imports
  # Identify position of zero production
  null_production <- p == 0
  # Keep only non zero columns from V1 (ie only keep columns associated with produced species)
  V_X <- V1[, !null_production, drop = FALSE]
  # Keep ony non zero elements in production vector
  p <- p[!null_production]
  # Identify position of zero imports
  null_imports <- i == 0
  # Keep only non zero columns from V2 (ie only keep columns associated with imported)
  V_W <- V2[, !null_imports, drop = FALSE]
  # Keep only elements that were imported
  i <- i[!null_imports]
  # Create positions for null consumption (ie did not consume because there is no production or import of this product)
  # We also include e, so as not to eliminate things that could be exported from the error term alone
  null_consumption <- apply(cbind(e, V_X, V_W) == 0, 1, all)
  
  # Remove null consumption from V_X, V_W and e rows
  V_X <- V_X[!null_consumption, , drop = FALSE]
  V_W <- V_W[!null_consumption, , drop = FALSE]
  e <- e[!null_consumption] # use this to create b vector
  non_null_consumption <- sum(!null_consumption) # number of non-null consumption
  
  # Create X: NA for unknowns (where conversion factors are retained in V_X) and 0 otherwise
  X <- array(0, dim(V_X))
  X[V_X != 0] <- NA
  
  X_unknowns <- Xq[!null_consumption, !null_production , drop = FALSE]
  X_unknowns <- c(X_unknowns)
  X_unknowns <- X_unknowns[X_unknowns!=0]
  
  # Create vector of unknown X's
  # Use 1, 2, 3, indicators from Xq (eg explicit match, broad match) to control priority for filling volumes
  # (large value on an element means we favor small values)
  # 0 means you are not pushing it towards this solution at all
  X_unknowns[X_unknowns==1] <- 0
  X_unknowns[X_unknowns==2] <- 0
  X_unknowns[X_unknowns==3] <- 100
  
  # Create W: NA for unknowns (where conversion factors are retained in V_W) and 0 otherwise
  W <- array(0, dim(V_W))
  W[V_W != 0] <- NA
  
  # Identify indices of W associated with consumption of foreign goods
  W_tmp <- V2
  W_tmp[W_tmp > 0] <- 1
  W_tmp <- W_tmp[!null_consumption, !null_imports , drop = FALSE]
  W_unknowns <- c(W_tmp)
  W_unknowns <- W_unknowns[W_unknowns!=0]
  
  # Set number of unknowns in X and W
  n_unknown_X <- sum(is.na(X))
  n_unknown_W <- sum(is.na(W))
  
  ####################################################################################
  # Create P term of the objective function (1/2 x^T P x + q^T x) to be minimized
  # P is structured as unknowns for X, unknowns for W, unknowns for consumption, unknowns for error
  # P controls evenness of solution within each vector of unknowns
  P <- diag(c(rep(0.1, n_unknown_X), rep(0.1, n_unknown_W), rep(0.1, non_null_consumption), rep(0.1, non_null_consumption)))
  
  # Create q term of the objective function
  # q controls how much to favor each of the corresponding vectors of unknowns in the optimization (i.e., large value on errors, means we favor small errors)
  q <- c(X_unknowns, W_unknowns, rep(1, non_null_consumption), rep(10000, non_null_consumption)) 
  ####################################################################################
  # Create G, matrix for the inequality constraints (none in our case)
  G <- matrix(0, nrow = nrow(P), ncol = ncol(P))
  
  ####################################################################################
  # Make A, matrix for the equality constraints
  # Bind conversion factors from V_X, V_W (for which production is > 0), and a diagonal matrix of 1's corresponding with consumption 
  # Remove diag(nrow = non_null_consumption) if not including consumption in the constraints
  V <- cbind(V_X, V_W, diag(1, nrow = non_null_consumption), diag(1, nrow = non_null_consumption))
  # Bind X, W (with NA's in positions of unknowns), and a diagonal matrix of NA's corresponding with consumption unknowns
  # Remove diag(NA, non_null_consumption) if not including consumption in the constraints
  Z <- cbind(X, W, diag(NA, non_null_consumption), diag(NA, non_null_consumption))
  # Vector of non-zero production, non-zero imports and -1's (corresponding to consumption subtracted off)
  # remove rep(-1, non_null_consumption) if not including consumption in the constraints
  y <- c(p, i, rep(-1, non_null_consumption), rep(1, non_null_consumption))
  
  ## PYTHON SECTION
  # Create matrix of knowns using vec(ABC) = (t(C) %x% A)vec(B),
  # where %x% represents the kronecker product. 
  # Multiplying by diag(V) converts production and import data to the equivalent processed quantity
  
  # Create all matrices in R before passing to python
  t_y <- t(y)
  identity_mat <- diag(nrow = non_null_consumption)
  v_for_diag <- c(V)
  
  ## BEGIN PYTHON SECTION
  # Use scipy for sparse matrices
  scipy <- import("scipy", convert = FALSE) 
  # Convert to sparse matrices
  kron_prod_sparse <- scipy$sparse$kron(t_y, identity_mat)
  diag_v_sparse <- scipy$sparse$diags(v_for_diag) 
  Ab_py <- scipy$sparse$csr_matrix$dot(kron_prod_sparse, diag_v_sparse)
  # Pass Ab back to r
  Ab <- py_to_r(Ab_py)
  # Convert to dense matrix
  Ab <- as.matrix(Ab)
  ## END PYTHON SECTION
  
  # Reticulate can create memory leakage, run gc() here to prevent R from crashing
  gc() 
  
  # Create matrix of columns of knowns corresponding to positions of unknowns (satisfy equality constraint, e = X_unknowns + Wi - c + error)
  A <- Ab[ , is.na(c(Z)), drop = FALSE]
  # Format so that rowsums are e for matrix 1 and rowsums are 1 for the rest
  # Similarly create E for the equality constraint: a matrix of 1's and 0's,
  # with 1's in positions that should sum to 1 when multiplied by solution vector
  XW <- cbind(X, W) 
  # Create matrix of constraints for unknowns in X and W that must sum to 1
  E <- diag(nrow = ncol(XW)) %x% t(rep(1, non_null_consumption))
  E <- E[, is.na(c(XW)), drop = FALSE]
  E <- cbind(E, matrix(0, nrow = nrow(E), ncol = 2*non_null_consumption))
  # Bind all equality constraints into a single matrix
  A <- rbind(A, E) # done with A
  ####################################################################################
  
  # uvec are the upper bounds
  uvec <- c(rep(1, n_unknown_X), rep(1, n_unknown_W), rep(9999999999, non_null_consumption), rep(9999999999, non_null_consumption))
  u <- matrix(uvec, nrow = nrow(P))
  
  # Create b, the equality constraints
  b <- matrix(c(e, rep(1, length = nrow(E))), nrow = nrow(A))
  
  dim_V1 <- dim(V1)
  dim_V2 <- dim(V2)
  
  return(list(P = P, q = q, G = G, A = A, b = b, u = u, 
              # Outputs for unstack:
              Z = Z,
              non_null_consumption = non_null_consumption,
              null_consumption = null_consumption,
              null_production = null_production, 
              null_imports = null_imports,
              dim_V1 = dim_V1,
              dim_V2 = dim_V2,
              V_X = V_X,
              V_W = V_W,
              scale_factor = scale_factor,
              # Outputs for make_snet_simple 
              production = production,
              # Outputs for eX_unknownsloring results
              imports = imports,
              exports = exports
  ))
  
}