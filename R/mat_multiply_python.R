#' @importFrom reticulate import
#' @importFrom reticulate py_to_r
#' @export
mat_multiply_python <- function(W_r, C_net_r){

# Multiplying W (block diagonal matrix of each country's estimated W) times C_net (matrix of trade flows with importers as the rows)
  
# Run python using R code (reticulate code)
scipy <- import("scipy", convert = FALSE) 
W_csr_py <- scipy$sparse$csr_matrix(W_r) 
C_csr_py <- scipy$sparse$csr_matrix(C_net_r)
A_py <- scipy$sparse$csr_matrix$dot(W_csr_py, C_csr_py) # Equivalent to A_csr_py <- W_csr_py @ C_csr_py in python code

#dim(W_csr_py)
#dim(C_csr_py)
#dim(A_py)

# Pass A_py back to r
A_tmp <- py_to_r(A_py)
# NOTE: Matrix package converts between different object classes as needed
#class(A_tmp) # dgRMatrix - row-oriented sparse Matrix

return(A_tmp)
}