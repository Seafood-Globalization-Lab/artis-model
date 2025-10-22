#' Build product attributes table
#'
#' Creates a product metadata table containing HS codes, descriptions, FMFO status,
#' and product form information. Combines raw HS codes with presentation and state 
#' information from hs-hs-match files.
#'
#' @param datadir_raw Character. Path to directory containing All_HS_Codes.csv
#' @param datadir Character. Path to directory containing hs-hs-match files
#' @param outdir_attribute Character. Path to directory where products.csv will be written
#'
#' @return Invisibly returns the products data frame
#' @importFrom dplyr mutate select distinct left_join rename bind_rows
#' @importFrom stringr str_length
#' @importFrom data.table fread
#' @export
#'
#' @examples
#' \dontrun{
#' build_attr_products(
#'   datadir_raw = "path/to/raw/data",
#'   datadir = "path/to/data",
#'   outdir_attribute = "path/to/output",
#'   hs_raw_file = "All_HS_Codes.csv"
#' )
#' }
build_attr_products <- function(datadir_raw, datadir, outdir_attribute, hs_raw_file) {
  
  if (!dir.exists(datadir_raw)) stop("datadir_raw does not exist: ", datadir_raw)
  if (!dir.exists(datadir)) stop("datadir does not exist: ", datadir)
  if (!dir.exists(outdir_attribute)) stop("outdir_attribute does not exist: ", outdir_attribute)
  
  # Read raw HS codes
  products <- fread(
    file.path(datadir_raw, hs_raw_file),
    colClasses = "character",
    data.table = FALSE
  ) %>%
    mutate(Code = case_when(
      str_length(Code) < 6 ~ paste0("0", Code),
      TRUE ~ Code
    ))
  
  # Get list of all hs-hs-match files
  prep_state_files <- list.files(
    path = datadir,
    pattern = "hs-hs-match",
    include.dirs = FALSE
  )
  
  if (length(prep_state_files) == 0) {
    warning("No hs-hs-match files found in: ", datadir)
    prep_state <- data.frame()
  } else {
    # Process each hs-hs-match file
    prep_state <- data.frame()
    
    for (file in prep_state_files) {
      curr_file <- file.path(datadir, file)
      curr_prep_state <- read.csv(curr_file)
      
      curr_prep_state <- curr_prep_state %>%
        select(Code_pre, Code_post,
               Presentation_pre, Presentation_post,
               State_pre, State_post)
      
      curr_prep_state <- data.frame(
        hs6 = c(curr_prep_state$Code_pre, curr_prep_state$Code_post),
        presentation = c(curr_prep_state$Presentation_pre,
                        curr_prep_state$Presentation_post),
        state = c(curr_prep_state$State_pre, curr_prep_state$State_post)
      ) %>%
        distinct() %>%
        mutate(hs6 = as.character(hs6)) %>%
        mutate(hs6 = case_when(
          str_length(hs6) < 6 ~ paste0("0", hs6),
          TRUE ~ hs6
        ))
      
      prep_state <- prep_state %>%
        bind_rows(curr_prep_state)
    }
  }
  
  # Combine products with presentation and state info
  products <- products %>%
    left_join(prep_state, by = c("Code" = "hs6")) %>%
    rename(hs6 = Code)
  
  # Standardize column names to lowercase
  names(products) <- tolower(names(products))
  
  # Write output
  out_path <- file.path(outdir_attribute, "products.csv")
  write.csv(products, out_path, row.names = FALSE)
  
  message("Products metadata written to: ", out_path)
  invisible(products)
}