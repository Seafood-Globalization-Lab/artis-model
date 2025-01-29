# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased 

FIXIT: example changelog has Unrealeased header linked to "Comparing changes" page on github. Which ones are relevant for this project?

### Added

- New `rebuild_fao_[yyyy]_dat.R` function
- xxx

### Changed

- FAO Production data `CL_FI_SPECIES_GROUPS.csv` file added a new column `Major_Group` and removed `Major_Group_En, Major_Group_Fr, Major_Group_Es, Major_Group_Ar, Major_Group_Cn, Major_Group_Ru` columns. New unique values in this column. 

```
> unique(prod_ts$species_major_group)
[1] "PISCES"               
[2] "CRUSTACEA"            
[3] "MOLLUSCA"             
[4] "INVERTEBRATA AQUATICA"
[5] "PLANTAE AQUATICAE"    
[6] "AMPHIBIA, REPTILIA"   
[7] "MAMMALIA"  
```

- BACI new columns in country_codes_V202401b.csv that requires updating the `load_baci.R` script. Old columns were `country_code`, `country_name_abvbreviation`,`country_name_full`, `iso2digit_alpha`, and `iso3digit_alpha`. The new version columns are `country_code`, `country_name`, `country_iso2`, and `country_iso3`. 


### Removed

- xxx
- xxx

## [1.0.1] - [yyyy-mm-dd] (retroactive change log if there is time)

### Added

- xxx (#commithash)?
- xxx

### Fixed

- xxx (#commithash)

### Changed

- xxx (#commithash)
- xxx

### Removed

- xxx (#commithash)
- xxx

## [1.0.0] - [yyyy-mm-dd] (retroactive change log if there is time)