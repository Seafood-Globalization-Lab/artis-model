# Changelog

All notable changes to **artis-model** are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [1.1.0] – 2025-06-02

### Added
- **Apache License** (2025-05-08)  
- **`inst/CITATION` file** for package citation (2025-05-08)  
- **Per-capita and `diff_large` outputs** in `calculate_consumption()` to write out large-difference dataframes and retain per-capita columns (2025-03-03 to 2025-03-05)  
- **`V2_long` argument support**: Updated `calculate_consumption()`, `create_reweight_W_long.R`, and `get_snet.R` to accept and preserve `V2_long` data (2025-03-04)  
- **`dev_mode` and `test_year` parameters** in setup scripts and `get_country_solutions()` for controlled local runs (Resolves #73) (2025-04-28 to 2025-05-01)  

### Changed
- **`calculate_consumption.R` refactor**:  
  - Expanded function signature to include `reweight_W_long`, `V1_long`, `V2_long` and new `dev_mode` flag for debugging CSV output.  
  - Standardized column-formatting (converted `hs6` → numeric; renamed `SciName`→`sciname`, `prod_method`→`method`, `quantity`→`live_weight_t`).  
  - Revised domestic-consumption logic:  
    - Separated “live” vs. “product” export volumes (`domestic_export_live_t`, `domestic_export_product_t`).  
    - Added data-check comparing calculated exports against ARTIS-recorded exports.  
  - Overhauled foreign-consumption pipeline into three stages (`unprocessed_consumption`, `consumption_export_1`, `consumption_export_2`) to properly allocate retained vs. re-exported volumes across intermediaries and final consumers.  
  - Consolidated all consumption sources (domestic & foreign) into `complete_consumption`, grouping by `(year, hs_version, source_country_iso3c, exporter_iso3c, consumer_iso3c, consumption_source, sciname, habitat, method, end_use)`.  
  - Added per-capita capping logic: computed `consumption_percap_live_kg`, capped by `max_percap_consumption`, and returned `complete_consumption_capped` when `dev_mode = TRUE`.  

- **S-net & consumption outputs** now write in `.qs` format to reduce file size (fixes #80) (2025-05-28)  
- **Removed redundant “all-country-est” compilation**: `get_snet.R` now reads combined country-solve outputs from both solvers directly (2025-05-29)  
- **Production-file refactoring**:  
  - Moved `group_by()/summarise()` logic into `01-clean-input-data.R` and removed unused `code_max_resolved` dependencies (2025-04-29)  
  - Unified column ordering; retained `country_name_en` in SAU output (2025-05-07)  
  - Reduced redundant columns (e.g., `isscaap_group`, `Species01`, etc.) to avoid multiple rows per record (2025-05-07)  
- **Setup script consolidation**:  
  - Moved configuration values (`hs_version_run`, model parameters) into `00-local-machine-setup.R` and `00-aws-hpc-setup.R` (2025-04-28 to 2025-04-29)  
  - Parameterized all HS versions and years (SAU & FAO); removed hard-coded `clean_prod` writes (2025-05-08)  
- **Fishmeal-priority feature**:  
  - Adjusted thresholds and grouping in `get_fmfo_species.R`, `match_hs_to_taxa.R`, and `01-clean-input-data.R` to support fishmeal priority and zero-threshold species (2025-04-02 to 2025-04-28)  
- **Logging & benchmarking**:  
  - Turned off verbose `qpsolvers` output to reduce log noise (Resolves #69) (2025-04-21)  
  - Added benchmark messages in `02-artis-pipeline.R` (2025-04-29)  
- **Documentation updates**:  
  - Updated roxygen comments in `calculate_consumption.R` and related scripts (2025-02-28 to 2025-03-07)  
  - Added environmental files and `man/` roxygen2 build documentation to `.
