# Pipeline step to collect raw data ingested by the ARTIS model

# Setup -----------------------------------------------------------

# make Posit package manager primary install repo (uses faster binaries)
# @ snapshot date - hold all package versions to the "latest" version at this date
# Think about updating this date, but a few months delayed from "today" to avoid release bugs
pak::repo_add(CRAN = "PPM@2026-06-21")

# install artis from the package root
pak::local_install(".")

# Run Local Machine Configuration (directory paths, parameters)
source(system.file("00-local-machine-setup.R", package = "artis"))

# define data collection functions
source(system.file("collect_raw_data_functions.R", package = "artis"))

# Collect FAO Production -------------------------------------------------

path_fao_prod_zip <- collect_fao_prod_data(
  outdir = path_fao_prod_raw,
  version = fao_prod_version, 
  overwrite = FALSE
)

# Collect BACI Trade -----------------------------------------------------

collect_baci_data(
  outdir = path_baci_raw,
  hs_versions = hs_versions
)

# Collect FAO Population -------------------------------------------------

path_fao_pop_zip <- collect_fao_pop_data(
  outdir    = path_fao_pop_raw,
  overwrite = FALSE
)

# Collect FishBase & Sealife Base ----------------------------------------

artis::collect_fb_slb_data(parent_outdir = path_fb_slb_raw)

# Collect EUMOFA processing ----------------------------------------------

## website - https://fishery-aquaculture-market-observatory.ec.europa.eu/en/data/bulk-download
## Download link "yearly_processing" - 

# Collect WCO HS Nomenclature --------------------------------------------

collect_wco_hs_chapters(
  outdir      = path_wco_hs,
  hs_editions = hs_editions,
  hs_chapters = hs_chapters,
  overwrite   = FALSE
)
