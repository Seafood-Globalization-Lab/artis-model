# 00-raw-data-download.R

# Run Local Machine Configuration (directory paths, parameters)
source("00-local-machine-setup.R")

# Comtrade ---------------------------------------------------------------
# Get Comtrade commodity descriptions for each HS version used in ARTIS
# could be transfered into it own function later.

pak::pak("comtradr")
library(comtradr)
library(dplyr)
library(arrow)

# create character vector of "H0", "H1", "H2", "H3", "H4", "H5", "H6" etc based on provided hs_version_lookup set in 00-local-machine-sestup.R
# Lookup vector mapping Comtrade H-codes to standard HS year labels
hs_version_lookup <- setNames(
  hs_version_range, 
  paste0("H", seq_along(1:length(hs_version_range)), sep = "")
)

# Fetch commodity descriptions for each HS version and bind into one data frame
descriptions_raw <- purrr::map(names(hs_version_lookup), \(hs) {
  ct_get_ref_table(hs) %>% 
    dplyr::mutate(
      classification = hs,
      hs_version = hs_version_lookup[hs]
    )
}) %>% 
  dplyr::bind_rows() 

# write out raw data
file_name <- glue("un_comtrade_codes_descr_{first(hs_version_lookup)}_{last(hs_version_lookup)}_raw")

arrow::write_csv_arrow(descriptions_raw, file.path(path_hs_codes_raw, glue("{file_name}.csv")))
arrow::write_parquet(descriptions_raw, file.path(path_hs_codes_raw, glue("{file_name}.parquet")))

# clean raw data
descriptions_clean <- descriptions_raw %>% 
  # remove header rows in each HS version
  dplyr::filter(id != "TOTAL") %>% 
  # trim leading "{id} - " prefix from text column where present
  dplyr::mutate(text = stringr::str_remove(text, "^\\d+ - "))

file_name <- glue("un_comtrade_codes_descr_{first(hs_version_lookup)}_{last(hs_version_lookup)}_clean")
arrow::write_csv_arrow(descriptions_clean, file.path(path_hs_codes_raw, glue("{file_name}.csv")))
arrow::write_parquet(descriptions_clean, file.path(path_hs_codes_raw, glue("{file_name}.parquet")))

# Regex to filter to ARTIS-relevant HS codes (chapter 03, 05/0511, 16/1604/1605, 23/2301)
artis_hs_regex <- "^03$|^03[0-9]{2}$|^16$|^160[45]$|^23$|^03[0-9]{4}$|^160[45][0-9]{2}$|^2301$|^230120$|^051191$|^05$|^0511$"

desc_aquatic <- descriptions_clean %>% 
  dplyr::filter(stringr::str_detect(id, artis_hs_regex)) %>% 
  select(
    Code = id,
    Description = text,
    Parent = parent,
    Classification = classification
  )

file_name <- glue("un_comtrade_codes_descr_{first(hs_version_lookup)}_{last(hs_version_lookup)}_clean_ARTIS")
arrow::write_csv_arrow(desc_aquatic, file.path(path_hs_codes_raw, glue("{file_name}.csv")))
arrow::write_parquet(desc_aquatic, file.path(path_hs_codes_raw, glue("{file_name}.parquet")))


