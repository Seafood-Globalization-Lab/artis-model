# Changelog

All notable changes to **artis-model** are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/). Add most recent changes at the top, and retain all older text, this is a running/living document.

## \[2.0.0\] – 2025-09-24

### Added

-   **New FAO Global Fishery Production Data 1996-2023**
    - Ingested new data from 2021 - 2023. This expands the ARTIS `trade` and `consumption` data table timeseries to 2023. 
    - Version "2025.1.0" pulled 2025-08-08
-   **New Fishbase and Sealifebase Data**
    - Pulled new stable release of the data with `rfishbase` R package 
    - Version `"24.07"` This is approximately the fishbase and sealifebase snapshot at 2025-07
-   **New FAO Annual Population Data**
    - Pulled 2025-08-25 (no obvious versioning)
-   **New BACI International Bilateral Trade Data**
    - Version: "v202501" pulled 2025-08-22
-   **New EUMOFA Yearly Processing Data**
    - Pulled 2025-09-02 (no obvious versioning)
-   **Scripted Data Validation**
    - Created `00-raw-data-assessment.qmd` to evaluate differences in raw data versions
    - Created `07-post-processing-validation.Rmd` to interigate numerous ARTIS assumptions across the `trade` and `consumption` data tables using all data years. 
    - Primarily used after a full ARTIS model run on AWS across all HS versions and years
    - Created `08-validation-single-HS-year.Rmd` interigate numerous ARTIS assumptions across the `trade` and `consumption` data tables using a single HS version and year pair. 
    - Primarily used after a local test run of ARTIS for a single HS version and year pair.


### Changed

-   **Fishmeal Bug Fix**
    - A portion of of fishmeal was being quietly allocated to `"direct human consumption"` in line 317, 324, and 339 `calculate_consumption.R`
-   **Fishbase "perciformes/*" bug** 
    - Fix bug in reweight_X_long where "perciformes/*" sciname was not joining and introduced NAs
-   **Parallelalize `get_country_solutions.R`** 
    - No need to run in series, when you can run in parallel using the `future` and `future.apply` R packages 
    - Speeds things up quite a bit. 
    - `num_cores` arguement controls parallel worker allocation for solving country-level mass balance problems within each year `sequential mode`, `auto mode`, and `explicit cap`. 
    - Required tryCatch around AWS s3 write out since 3 workers were accessing very similar file prefix on S3. S# has high read concurrencey, but much less for write concurencey. 
-   **Python package versions**
    - Use a `requirements.lock` file instead of a more flexible `requirements.txt` file to build the local `venv` environment.
-   **Write out R global env when running locally**
    - Write out `.qs2` files right before `create_snet()` and `calculate_consumption()` in `get_snet()` function called within `02-artis-pipeline.R`.
    - Pinpoint access into the model for improved local troubleshooting and development. 
    - Easy to share. 
-   **Additional country standardization cases**
    - New `BACIv202501` data changed the handeling/grouping of country iso3c `"ZA1"` and `"ZAF"`.
        - Previous BACI versions luped South Africa and Southern African Customs Union into `"ZAF"` (South Africa).
        - `"ZA1"` includes: Botswana, Eswatini, Lesotho, Nambia, and South Africa.
        - In `BACIv202501` data `"ZA1"` is used before 2000 only. 
    - New FAO Population data introduced `NA`s because of extended timeseries values. 
        - Filter out countries with population values of `NA` 
        - Belgium and Luxemburg combined production under Belgium. 




## \[2.0.0\] – 2025-09-08

### Added

-   **New FAO Global Fishery Production Data 1996-2023**
    - Ingested new data from 2021 - 2023. This expands the ARTIS `trade` and `consumption` data table timeseries to 2023. 
    - Version "2025.1.0" pulled 2025-08-08
-   **New Fishbase and Sealifebase Data**
    - Pulled new stable release of the data with `rfishbase` R package 
    - Version `"24.07"` This is approximately the fishbase and sealifebase snapshot at 2025-07
-   **New FAO Annual Population Data**
    - Pulled 2025-08-25 (no obvious versioning)
-   **New BACI International Bilateral Trade Data**
    - Version: "v202501" pulled 2025-08-22
-   **New EUMOFA Yearly Processing Data**
    - Pulled 2025-09-02 (no obvious versioning)
-   **Scripted Data Validation**
    - Created `00-raw-data-assessment.qmd` to evaluate differences in raw data versions
    - Created `07-post-processing-validation.Rmd` to interigate numerous ARTIS assumptions across the `trade` and `consumption` data tables using all data years. 
    - Primarily used after a full ARTIS model run on AWS across all HS versions and years
    - Created `08-validation-single-HS-year.Rmd` interigate numerous ARTIS assumptions across the `trade` and `consumption` data tables using a single HS version and year pair. 
    - Primarily used after a local test run of ARTIS for a single HS version and year pair.


### Changed

-   **Fishmeal Bug Fix**
    - A portion of of fishmeal was being quietly allocated to `"direct human consumption"` in line 317, 324, and 339 `calculate_consumption.R`
-   **Fishbase "perciformes/*" bug** 
    - Fix bug in reweight_X_long where "perciformes/*" sciname was not joining and introduced NAs
-   **Parallelalize `get_country_solutions.R`** 
    - No need to run in series, when you can run in parallel using the `future` and `future.apply` R packages 
    - Speeds things up quite a bit. 
    - `num_cores` arguement controls parallel worker allocation for solving country-level mass balance problems within each year `sequential mode`, `auto mode`, and `explicit cap`. 
-   **Python package versions**
    - Use a `requirements.lock` file instead of a more flexible `requirements.txt` file to build the local `venv` environment.
-   **Write out R global env when running locally**
    - Write out `.qs2` files right before `create_snet()` and `calculate_consumption()` in `get_snet()` function called within `02-artis-pipeline.R`.
    - Pinpoint access into the model for improved local troubleshooting and development. 
    - Easy to share. 




## \[1.1.0\] – 2025-08-13

### Added

-   **Apache License** (2025-05-08)
-   **`inst/CITATION` file** for package citation (2025-05-08)
-   **Per-capita and `diff_large` outputs** in `calculate_consumption()` to write out large-difference dataframes and retain per-capita columns (2025-03-03 to 2025-03-05)
-   **`V2_long` argument support**: Updated `calculate_consumption()`, `create_reweight_W_long.R`, and `get_snet.R` to accept and preserve `V2_long` data (2025-03-04)
-   **`dev_mode` and `test_year` parameters** in setup scripts and `get_country_solutions()` for controlled local runs (Resolves #73) (2025-04-28 to 2025-05-01)
-   **Fishmeal-priority feature**:
    -   Adjusted thresholds and grouping in `get_fmfo_species.R`, `match_hs_to_taxa.R`, and `01-clean-input-data.R` to support fishmeal priority and zero-threshold species (2025-04-02 to 2025-04-28)

### Changed

-   **`calculate_consumption.R` refactor**:
    -   Expanded function arguements to include `reweight_W_long`, `V1_long`, `V2_long` and new `dev_mode` flag for debugging CSV output.
    -   Revised domestic-consumption logic:
        -   Separated “live” vs. “product” export volumes (`domestic_export_live_t`, `domestic_export_product_t`).
        -   Added data-check comparing calculated exports against ARTIS-recorded exports.
    -   Overhauled foreign-consumption pipeline into three stages (`unprocessed_consumption`, `consumption_export_1`, `consumption_export_2`) to properly allocate retained vs. re-exported volumes across intermediaries and final consumers.
    -   Consolidated all consumption sources (domestic & foreign) into `complete_consumption`, grouping by `(year, hs_version, source_country_iso3c, exporter_iso3c, consumer_iso3c, consumption_source, sciname, habitat, method, end_use)`.
    -   Added per-capita capping logic: computed `consumption_percap_live_kg`, capped by `max_percap_consumption`, and returned `complete_consumption_capped` when `dev_mode = TRUE`.
    - `code_max_resolved` retained to get `sciname_hs_modified`
    - Moved assumption test to run for `complete_consumption` regardless if `max_percap_consumption` arguement is set to `NA` (off) or includes a per capita threshold value. 
    - Explicit per capita consumption outliers (above set threshold value) for only direct human consumption (exclude fishmeal)
-   **S-net & consumption outputs** now write in `.qs2` format to reduce file size (fixes #80) (2025-05-28)
-   **Removed redundant “all-country-est” compilation**: `get_snet.R` now reads combined country-solve outputs from both solvers directly (2025-05-29)
-   **Production-file refactoring**:
    -   Moved `group_by()/summarise()` logic into `01-clean-input-data.R` and removed unused `code_max_resolved` dependencies (2025-04-29)
    -   Unified column ordering; retained `country_name_en` in SAU output (2025-05-07)
    -   Reduced redundant columns (e.g., `isscaap_group`, `Species01`, etc.) to avoid multiple rows per record (2025-05-07)
-   **Setup script consolidation**:
    -   Moved configuration values (`hs_version_run`, model parameters) into `00-local-machine-setup.R` and `00-aws-hpc-setup.R` (2025-04-28 to 2025-04-29)
    -   Parameterized all HS versions and years (SAU & FAO); removed hard-coded `clean_prod` writes (2025-05-08)
-   **Logging & benchmarking**:
    -   Turned off verbose `qpsolvers` output to reduce log noise (Resolves #69) (2025-04-21)
    -   Added benchmark messages in `02-artis-pipeline.R` (2025-04-29)
    - Created global environment wite out files in `get_snet.R` just before `create_snet()` and `calculate_consumption()` for local runs. Not developed for AWS yet.
-   **Documentation updates**:
    -   Updated roxygen comments in `calculate_consumption.R` and related scripts (2025-02-28 to 2025-03-07)
    -   Added environmental files and `man/` roxygen2 build documentation to \`.
-   **Post-processing updates**:
    -   Updated `03-combine-tables.R` script to use new function `combine_partitioned_data.R` and remove `build_artis_data.R` and `collect_data.R`. Appends .qs2 with duckdb and writes out parquet file (2025-06-05) resloves #82 and #25
    - Created validation report `07-post-processing-validation.Rmd` to standardize assumption checks and provide documentation for datasets. 