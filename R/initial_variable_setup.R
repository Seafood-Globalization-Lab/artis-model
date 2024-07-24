#' @export
initial_variable_setup <- function(datadir, outdir, hs_version = NA, test_years = c(),
                                   prod_type = "FAO",
                                   run_env = "aws", s3_bucket_name = "", s3_region = "") {
  
  
  #-----------------------------------------------------------------------------
  # Step 0: Setup
  full_analysis_start <- Sys.time()
  
  # Use file.date for all file names
  # If scripts runs overnight, setting this up top prevents having to deal with
  # two dates before and after midnight
  file.date <- Sys.Date()
  
  # List of variables to retain in memory when environment is cleared
  analysis_info <- c("outdir", "datadir", "file.date", "full_analysis_start",
                     "hs_version", "HS_year_rep", "hs_dir", "df_years", "analysis_year",
                     "hs_analysis_year_dir", "solver_type", "num_cores",
                     "quadprog_dir", "cvxopt_dir", "test_years",
                     "run_env", "s3_bucket_name", "s3_region", "estimate_type")
  
  analysis_setup <- c("prod_data", "V1", "V2", "sc_n", "cc_m", "X_cols",
                      "X_rows", "W_cols", "W_rows", "Xq", "analysis_years_rep",
                      "HS_year_rep", "no_solve_countries", "non_human_codes",
                      "code_max_resolved")
  
  #-----------------------------------------------------------------------------
  # Step 1: Load production data used for all years
  prod_filename <- NA
  prod_taxa_filename <- NA
  
  if (prod_type == "FAO") {
    prod_filename <- "standardized_fao_prod.csv"
    prod_taxa_filename <- "clean_fao_taxa.csv"
  } else {
    prod_filename <- "standardized_combined_prod.csv"
    prod_taxa_filename <- "clean_taxa_combined.csv"
  }
  
  if (run_env == "aws") {
    # bring data into local environment first
    
    save_object(
      object = file.path(datadir, prod_filename),
      bucket = s3_bucket_name,
      region = s3_region,
      file = file.path(datadir, prod_filename)
    )
    
    save_object(
      object = file.path(datadir, prod_taxa_filename),
      bucket = s3_bucket_name,
      region = s3_region,
      file = file.path(datadir, prod_taxa_filename)
    )
  }
  
  prod_data <- read.csv(file.path(datadir, prod_filename))
  prod_taxa_classification <- read.csv(file.path(datadir, prod_taxa_filename))
  
  #-----------------------------------------------------------------------------
  # Step 2: Loop through all years for each HS code year
  
  # List of possible HS versions: HS92, HS96, HS02, HS12, HS17
  # No need to do HS92 when using BACI though as that data starts in 1996
  df_years <- data.frame(HS_year = c(rep("96", length(1996:2020)),
                                     rep("02", length(2002:2020)),
                                     rep("07", length(2007:2020)),
                                     rep("12", length(2012:2020)),
                                     rep("17", length(2017:2020))),
                         analysis_year = c(1996:2020, 2002:2020, 2007:2020,
                                           2012:2020, 2017:2020))
  
  #-----------------------------------------------------------------------------
  # Choose single HS (this will change for each file submitted to Zorro - 
  # run one HS code per Zorro submission)
  
  HS_year_rep <- hs_version
  
  #-----------------------------------------------------------------------------  
  # Load hs_taxa_match for appropriate HS year
  hs_taxa_match_fp <- file.path(datadir, paste("hs-taxa-match_HS", HS_year_rep, ".csv", sep = ""))
  hs_taxa_cf_fp <- file.path(datadir,
                             paste("hs-taxa-CF_strict-match_HS", HS_year_rep, ".csv", sep = ""))
  hs_hs_match_fp <- file.path(datadir,
                              paste("hs-hs-match_HS", HS_year_rep, ".csv", sep=""))
  
  if (run_env == "aws") {
    # bring into local file system from s3 bucket
    save_object(
      object = hs_taxa_match_fp,
      bucket = s3_bucket_name,
      region = s3_region,
      file = hs_taxa_match_fp
    )
    
    save_object(
      object = hs_taxa_cf_fp,
      bucket = s3_bucket_name,
      region = s3_region,
      file = hs_taxa_cf_fp
    )
    
    save_object(
      object = hs_hs_match_fp,
      bucket = s3_bucket_name,
      region = s3_region,
      file = hs_hs_match_fp
    )
  }
  
  
  hs_taxa_match <- read.csv(hs_taxa_match_fp) %>%
    # pad HS codes with zeroes
    mutate(Code = as.character(Code)) %>%
    mutate(Code = if_else(
      str_detect(Code, "^30"),
      true = str_replace(Code, pattern = "^30", replacement = "030"),
      if_else(str_detect(Code, "^511"),
              true = str_replace(Code, pattern = "^511", replacement = "0511"),
              false = Code)))
  
  # Make new version of hs_taxa_match that includes SciName +
  # taxa_source for the full snet estimation
  hs_taxa_match <- hs_taxa_match %>%
    left_join(
      prod_data %>%
        select(SciName, taxa_source) %>%
        distinct(),
      by = "SciName"
    ) %>%
    select(-SciName) %>%
    rename("SciName" = "taxa_source") %>%
    filter(!is.na(SciName)) %>%
    filter(
      str_detect(code_habitat, "marine|diadromous") & str_detect(SciName, "marine") |
        str_detect(code_habitat, "inland|diadromous") & str_detect(SciName, "inland")
    ) %>%
    select(-c(sciname_habitat, code_habitat))
  
  # Load hs_taxa_CF_match for appropriate HS year
  hs_taxa_CF_match <- read.csv(hs_taxa_cf_fp) %>%
    # pad HS codes with zeroes
    mutate(Code = as.character(Code)) %>%
    mutate(
      Code = if_else(
        str_detect(Code, "^30"),
        true = str_replace(Code, pattern = "^30", replacement = "030"),
        if_else(
          str_detect(Code, "^511"),
          true = str_replace(Code, pattern = "^511", replacement = "0511"),
          false = Code
        )
      )
    )
  
  # Make new version of hs_taxa_CF_match that includes SciName +
  # taxa_source for the full snet estimation
  hs_taxa_CF_match <- hs_taxa_CF_match %>%
    left_join(
      prod_data %>%
        select(SciName, taxa_source) %>%
        distinct(),
      by = c("Taxa" = "SciName")
    ) %>%
    select(-Taxa) %>%
    rename("Taxa" = "taxa_source") %>%
    filter(!is.na(Taxa)) %>%
    right_join(
      hs_taxa_match %>%
        select("Code", "SciName"), 
      by = c("Taxa" = "SciName", "Code")
    )
  
  # Load hs-hs match for appropriate HS year
  hs_hs_match <- read.csv(hs_hs_match_fp) %>% 
    # pad HS codes with zeroes
    mutate(Code_pre = as.character(Code_pre)) %>%
    mutate(
      Code_pre = if_else(
        str_detect(Code_pre, "^30"),
        true = str_replace(Code_pre, pattern = "^30", replacement = "030"),
        if_else(
          str_detect(Code_pre, "^511"),
          true = str_replace(Code_pre, pattern = "^511", replacement = "0511"),
          false = Code_pre
        )
      )
    ) %>%
    mutate(Code_post = as.character(Code_post)) %>%
    mutate(
      Code_post = if_else(
        str_detect(Code_post, "^30"),
        true = str_replace(Code_post, pattern = "^30", replacement = "030"),
        if_else(
          str_detect(Code_post, "^511"),
          true = str_replace(Code_post, pattern = "^511", replacement = "0511"),
          false = Code_post
        )
      )
    )
  
  #-----------------------------------------------------------------------------  
  # Step 3: Make V1 and V2
  
  # V1: sparse matrix (products x species) of conversion factors corresponding 
  # to the entries of X, coproduct_codes are products with CF == 0
  coproduct_codes <- hs_taxa_CF_match %>%
    select(Code, CF_calc) %>%
    filter(CF_calc == 0) %>%
    distinct() %>%
    pull(Code)
  
  X_all <- make_v1(hs_taxa_CF_match, coproduct_codes)
  V1 <- X_all[[1]]
  X_rows <- X_all[[2]]
  X_cols <- X_all[[3]]
  
  # Make Xq, matrix for controlling strength of species to product estimates in
  # optimization problem (see transform_to_qp_with_python.R)
  Xq <- categorize_hs_to_taxa(hs_taxa_match, coproduct_codes)
  
  # V2: sparse matrix of (products x products) conversion factors corresponding
  # to the entries of W, columns are the original products,
  # Rows are thefinal product state
  W_all <- make_v2(hs_hs_match = hs_hs_match, hs_taxa_CF_match = hs_taxa_CF_match, coproduct_codes)
  V2 <- W_all[[1]]
  W_rows <- W_all[[2]]
  W_cols <-W_all[[3]]
  
  # Set a max for V2 to prevent gain of mass
  V2[V2>1] <- 1
  
  # Insert -1 on diagonal if all imports are represented as consumption of foreign goods
  # Insert 1 on diagonal if imports can be exported under the same product code
  diag(V2) <- 1
  
  # Save list of species for analysis from hs_taxa_match
  # Note: hs_taxa_match is what is used to make hs_hs_match, V_1, V_2,
  # so all of these lists should match
  sc_n <- X_cols
  
  # Save list of HS codes for analysis from V_1 matrix,
  # since some codes had to be filtered out
  cc_m <- X_rows
  
  # Get all analysis years for a given HS version
  analysis_years_rep <- df_years %>%
    filter(HS_year == HS_year_rep)
  
  # Check if there is a specific test year to create an snet for
  if (length(test_years) > 0) {
    analysis_years_rep <- analysis_years_rep %>%
      filter(analysis_year %in% test_years)
  }
  
  # Create all output directories for HS version and each year
  hs_dir <- paste("HS", HS_year_rep, sep = "")
  
  if (run_env == "aws") {
    put_folder(
      file.path(outdir, hs_dir),
      bucket = s3_bucket_name
    )
    
    for (analysis_year in analysis_years_rep$analysis_year){
      put_folder(
        file.path(outdir, hs_dir, analysis_year),
        bucket = s3_bucket_name
      )
    }
  }
  
  dir.create(file.path(outdir, hs_dir))
  for (analysis_year in analysis_years_rep$analysis_year){
    dir.create(file.path(outdir, hs_dir, analysis_year))
  }
  
  return(list(
    full_analysis_start,
    file.date,
    analysis_info,
    analysis_setup,
    df_years,
    prod_data,
    prod_taxa_classification,
    hs_taxa_match,
    hs_taxa_CF_match,
    hs_hs_match,
    coproduct_codes,
    Xq,
    X_rows,
    X_cols,
    V1,
    V2,
    W_rows,
    W_cols,
    sc_n,
    cc_m,
    HS_year_rep,
    analysis_years_rep,
    hs_dir
  ))
}
