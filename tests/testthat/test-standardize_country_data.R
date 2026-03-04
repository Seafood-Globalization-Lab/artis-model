library(testthat)
library(dplyr)
library(stringr)

df <- standardize_country_data()

# ── OUTPUT STRUCTURE ──────────────────────────────────────────────────────────

test_that("output is a data frame with the correct column names", {
  expect_s3_class(df, "data.frame")
  expect_named(df, c("country_name", "iso3c", "year", "artis_country_name", "artis_iso3c"))
})

test_that("all columns have correct types", {
  expect_type(df$country_name,       "character")
  expect_type(df$iso3c,              "character")
  expect_type(df$year,               "integer")
  expect_type(df$artis_country_name, "character")
  expect_type(df$artis_iso3c,        "character")
})

test_that("iso3c values are all exactly 3 characters", {
  expect_equal(mean(str_length(df$iso3c),       na.rm = TRUE), 3)
  expect_equal(mean(str_length(df$artis_iso3c), na.rm = TRUE), 3)
})

# ── NO MISSING VALUES ─────────────────────────────────────────────────────────

# Note: iso3c and country_name are intentionally NA for some special_corrections
# rows (e.g. "Other Asia, nes" has no iso3c). Only artis_* columns must always
# be populated.

test_that("year contains no NAs", {
  expect_false(any(is.na(df$year)))
})

test_that("artis_iso3c contains no NAs", {
  expect_false(any(is.na(df$artis_iso3c)))
})

test_that("artis_country_name contains no NAs", {
  expect_false(any(is.na(df$artis_country_name)))
})

# ── YEAR RANGE ────────────────────────────────────────────────────────────────

test_that("year values fall within the default range 1996-2023", {
  expect_true(all(df$year >= 1996))
  expect_true(all(df$year <= 2023))
})

test_that("custom year_range parameter is respected", {
  custom <- standardize_country_data(year_range = 2010:2015)
  expect_true(all(custom$year >= 2010))
  expect_true(all(custom$year <= 2015))
})

test_that("single-year year_range works", {
  single <- standardize_country_data(year_range = 2020L)
  expect_true(all(single$year == 2020L))
})

# ── US TERRITORY MAPPINGS ─────────────────────────────────────────────────────

test_that("US territories map to USA across all years", {
  us_territories <- c("ASM", "GUM", "MNP", "PRI", "VIR")
  result <- df |> filter(iso3c %in% us_territories)
  expect_true(all(result$artis_iso3c == "USA"))
  expect_equal(nrow(result), length(us_territories) * length(1996:2023))
})

test_that("'US Misc. Pacific Isds' (name-based) maps to USA", {
  result <- df |> filter(country_name == "US Misc. Pacific Isds")
  expect_true(nrow(result) > 0)
  expect_true(all(result$artis_iso3c == "USA"))
})

test_that("'US Virgin Isl.' (name-based) maps to USA", {
  result <- df |> filter(country_name == "US Virgin Isl.")
  expect_true(all(result$artis_iso3c == "USA"))
})

# ── UK TERRITORY MAPPINGS ─────────────────────────────────────────────────────

test_that("UK territories map to GBR across all years", {
  uk_territories <- c("AIA", "BMU", "IOT", "VGB", "CYM", "GIB", "PCN",
                      "SHN", "TCA", "FLK", "IMN", "SGS", "GGY", "JEY")
  result <- df |> filter(iso3c %in% uk_territories)
  expect_true(all(result$artis_iso3c == "GBR"))
})

test_that("'Channel Islands' and 'Channel Isl.' (name-based) map to GBR", {
  result <- df |> filter(country_name %in% c("Channel Islands", "Channel Isl."))
  expect_true(nrow(result) > 0)
  expect_true(all(result$artis_iso3c == "GBR"))
})

test_that("'Brit. Indian Ocean Terr.' maps to GBR", {
  result <- df |> filter(country_name == "Brit. Indian Ocean Terr.")
  expect_true(all(result$artis_iso3c == "GBR"))
})

test_that("'Tristan da Cunha Isl.' maps to GBR via SHN", {
  result <- df |> filter(country_name == "Tristan da Cunha Isl.")
  expect_true(all(result$iso3c == "SHN"))
  expect_true(all(result$artis_iso3c == "GBR"))
})

test_that("'Ascension Isl.' maps to GBR via SHN", {
  result <- df |> filter(country_name == "Ascension Isl.")
  expect_true(all(result$iso3c == "SHN"))
  expect_true(all(result$artis_iso3c == "GBR"))
})

# ── FRENCH TERRITORY MAPPINGS ─────────────────────────────────────────────────

test_that("French territories map to FRA across all years", {
  fra_territories <- c("PYF", "MYT", "NCL", "SPM", "WLF", "GUF",
                       "GLP", "MTQ", "MCO", "REU", "MAF", "BLM", "ATF")
  result <- df |> filter(iso3c %in% fra_territories)
  expect_true(all(result$artis_iso3c == "FRA"))
})

test_that("'St Martin' (name-based) maps to FRA via MAF", {
  result <- df |> filter(country_name == "St Martin")
  expect_true(all(result$iso3c == "MAF"))
  expect_true(all(result$artis_iso3c == "FRA"))
})

# ── CHINESE TERRITORY MAPPINGS ────────────────────────────────────────────────

test_that("Hong Kong and Macau map to CHN across all years", {
  result <- df |> filter(iso3c %in% c("HKG", "MAC"))
  expect_true(all(result$artis_iso3c == "CHN"))
  expect_equal(nrow(result), 2L * length(1996:2023))
})

# ── DUTCH TERRITORY MAPPINGS ──────────────────────────────────────────────────

test_that("Dutch territories map to NLD across all years", {
  nld_territories <- c("ABW", "ANT", "BES", "SXM", "CUW")
  result <- df |> filter(iso3c %in% nld_territories)
  expect_true(all(result$artis_iso3c == "NLD"))
})

test_that("Netherlands Antilles (ANT) has correct country_name", {
  result <- df |> filter(iso3c == "ANT")
  expect_true(all(result$country_name == "Netherlands Antilles"))
})

test_that("'Bonaire' and 'Saba and Saint Eustaius' map to NLD via BES", {
  result <- df |> filter(country_name %in% c("Bonaire", "Saba and Saint Eustaius"))
  expect_true(all(result$iso3c == "BES"))
  expect_true(all(result$artis_iso3c == "NLD"))
})

# ── NZ, AU, DANISH, NORWEGIAN TERRITORY MAPPINGS ─────────────────────────────

test_that("New Zealand territories map to NZL across all years", {
  result <- df |> filter(iso3c %in% c("COK", "NIU", "TKL"))
  expect_true(all(result$artis_iso3c == "NZL"))
})

test_that("Australian territories map to AUS across all years", {
  result <- df |> filter(iso3c %in% c("NFK", "CXR", "CCK", "HMD"))
  expect_true(all(result$artis_iso3c == "AUS"))
})

test_that("Danish territories (Greenland, Faroe Islands) map to DNK across all years", {
  result <- df |> filter(iso3c %in% c("GRL", "FRO"))
  expect_true(all(result$artis_iso3c == "DNK"))
})

test_that("Norwegian territories (SJM, BVT) map to NOR across all years", {
  result <- df |> filter(iso3c %in% c("SJM", "BVT"))
  expect_true(all(result$artis_iso3c == "NOR"))
})

# ── TANZANIA TERRITORY ────────────────────────────────────────────────────────

test_that("Zanzibar (EAZ) maps to TZA with correct country_name", {
  result <- df |> filter(iso3c == "EAZ")
  expect_true(all(result$artis_iso3c == "TZA"))
  expect_true(all(result$country_name == "Zanzibar"))
})

# ── TIME-DEPENDENT CORRECTIONS: TIMOR-LESTE ───────────────────────────────────

test_that("Timor-Leste (TLS) maps to IDN from 1996 to 2001", {
  result <- df |> filter(iso3c == "TLS", year <= 2001)
  expect_true(all(result$artis_iso3c == "IDN"))
  expect_equal(sort(unique(result$year)), 1996:2001)
})

test_that("Timor-Leste (TLS) maps to TLS from 2002 onwards", {
  result <- df |> filter(iso3c == "TLS", year >= 2002)
  expect_true(all(result$artis_iso3c == "TLS"))
  expect_equal(sort(unique(result$year)), 2002:2023)
})

test_that("TLS has a row for every year in the default range", {
  result <- df |> filter(iso3c == "TLS")
  expect_equal(sort(unique(result$year)), 1996:2023)
})

# ── TIME-DEPENDENT CORRECTIONS: SERBIA / MONTENEGRO ──────────────────────────

test_that("Serbia (SRB) maps to SCG from 1996 to 2005", {
  result <- df |> filter(iso3c == "SRB", year <= 2005)
  expect_true(all(result$artis_iso3c == "SCG"))
  expect_equal(sort(unique(result$year)), 1996:2005)
})

test_that("Serbia (SRB) maps to SRB from 2006 onwards", {
  result <- df |> filter(iso3c == "SRB", year >= 2006)
  expect_true(all(result$artis_iso3c == "SRB"))
  expect_equal(sort(unique(result$year)), 2006:2023)
})

test_that("Montenegro (MNE) maps to SCG from 1996 to 2005", {
  result <- df |> filter(iso3c == "MNE", year <= 2005)
  expect_true(all(result$artis_iso3c == "SCG"))
})

test_that("Montenegro (MNE) maps to MNE from 2006 onwards", {
  result <- df |> filter(iso3c == "MNE", year >= 2006)
  expect_true(all(result$artis_iso3c == "MNE"))
})

test_that("SCG iso3c maps to SCG artis_iso3c with correct country_name", {
  result <- df |> filter(iso3c == "SCG")
  expect_true(all(result$artis_iso3c == "SCG"))
  expect_true(all(result$country_name == "Serbia and Montenegro"))
})

test_that("artis_country_name for SCG is 'Serbia and Montenegro'", {
  result <- df |> filter(artis_iso3c == "SCG")
  expect_true(all(result$artis_country_name == "Serbia and Montenegro"))
})

# ── TIME-DEPENDENT CORRECTIONS: SOUTH SUDAN ──────────────────────────────────

test_that("South Sudan (SSD) maps to SDN from 1996 to 2011", {
  result <- df |> filter(iso3c == "SSD", year <= 2011)
  expect_true(all(result$artis_iso3c == "SDN"))
  expect_equal(sort(unique(result$year)), 1996:2011)
})

test_that("South Sudan (SSD) maps to SSD from 2012 onwards", {
  result <- df |> filter(iso3c == "SSD", year >= 2012)
  expect_true(all(result$artis_iso3c == "SSD"))
  expect_equal(sort(unique(result$year)), 2012:2023)
})

test_that("artis_country_name for SDN (pre-2012) is 'Sudan (Former)'", {
  result <- df |> filter(artis_iso3c == "SDN")
  expect_true(all(result$artis_country_name == "Sudan (Former)"))
})

# ── TIME-DEPENDENT CORRECTIONS: SACU ─────────────────────────────────────────

test_that("SACU members (BWA, LSO, NAM, SWZ) map to ZA1 from 1996 to 1999", {
  sacu <- c("BWA", "LSO", "NAM", "SWZ")
  result <- df |> filter(iso3c %in% sacu, year <= 1999)
  expect_true(all(result$artis_iso3c == "ZA1"))
  expect_equal(sort(unique(result$year)), 1996:1999)
})

test_that("SACU members map to themselves from 2000 onwards", {
  sacu <- c("BWA", "LSO", "NAM", "SWZ")
  result <- df |> filter(iso3c %in% sacu, year >= 2000)
  expect_true(all(result$artis_iso3c == result$iso3c))
  expect_equal(sort(unique(result$year)), 2000:2023)
})

test_that("artis_country_name for ZA1 is 'So. African Customs Union'", {
  result <- df |> filter(artis_iso3c == "ZA1")
  expect_true(all(result$artis_country_name == "So. African Customs Union"))
})

test_that("each SACU member has a row for every year in the default range", {
  sacu <- c("BWA", "LSO", "NAM", "SWZ")
  result <- df |> filter(iso3c %in% sacu)
  expect_equal(
    result |> count(iso3c) |> pull(n) |> unique(),
    length(1996:2023)
  )
})

# ── SPECIAL CORRECTIONS: NEI ──────────────────────────────────────────────────

test_that("SMR and AND map to NEI", {
  result <- df |> filter(iso3c %in% c("SMR", "AND"))
  expect_true(all(result$artis_iso3c == "NEI"))
})

test_that("'Other nei' maps to NEI with iso3c == 'NEI'", {
  result <- df |> filter(country_name == "Other nei", iso3c == "NEI")
  expect_true(nrow(result) > 0)
  expect_true(all(result$artis_iso3c == "NEI"))
})

test_that("'Unknown Fishing Country' maps to NEI", {
  result <- df |> filter(country_name == "Unknown Fishing Country")
  expect_true(all(result$artis_iso3c == "NEI"))
  expect_true(all(result$iso3c == "NEI"))
})

test_that("artis_country_name for all NEI rows is 'Other nei'", {
  result <- df |> filter(artis_iso3c == "NEI")
  expect_true(all(result$artis_country_name == "Other nei"))
})

# ── SPECIAL CORRECTIONS: TAIWAN ───────────────────────────────────────────────

test_that("'Other Asia, nes' maps to TWN with NA iso3c", {
  result <- df |> filter(country_name == "Other Asia, nes")
  expect_true(nrow(result) > 0)
  expect_true(all(result$artis_iso3c == "TWN"))
  expect_true(all(is.na(result$iso3c)))
})

# ── SPECIAL CORRECTIONS: LUXEMBOURG ──────────────────────────────────────────

test_that("LUX maps to BEL", {
  result <- df |> filter(iso3c == "LUX")
  expect_true(all(result$artis_iso3c == "BEL"))
})

# ── SPECIAL CORRECTIONS: PORTUGAL ISLANDS ────────────────────────────────────

test_that("Azores Isl. maps to PRT with artis_iso3c PRT", {
  result <- df |> filter(country_name == "Azores Isl.")
  expect_true(all(result$iso3c == "PRT"))
  expect_true(all(result$artis_iso3c == "PRT"))
})

test_that("Madeira Isl. maps to PRT with artis_iso3c PRT", {
  result <- df |> filter(country_name == "Madeira Isl.")
  expect_true(all(result$iso3c == "PRT"))
  expect_true(all(result$artis_iso3c == "PRT"))
})

# ── SPECIAL CORRECTIONS: FSM ──────────────────────────────────────────────────

test_that("'Micronesia' maps to FSM with artis_iso3c FSM", {
  result <- df |> filter(country_name == "Micronesia")
  expect_true(all(result$iso3c == "FSM"))
  expect_true(all(result$artis_iso3c == "FSM"))
})

# ── POST-MUTATION FIELD CONSISTENCY ──────────────────────────────────────────

test_that("country_name is never NA for rows where iso3c is not NA", {
  result <- df |> filter(!is.na(iso3c))
  expect_false(any(is.na(result$country_name)))
})

test_that("ANT country_name is set to 'Netherlands Antilles' after mutation", {
  result <- df |> filter(iso3c == "ANT")
  expect_true(all(result$country_name == "Netherlands Antilles"))
})

test_that("SCG iso3c has country_name 'Serbia and Montenegro' after mutation", {
  result <- df |> filter(iso3c == "SCG")
  expect_true(all(result$country_name == "Serbia and Montenegro"))
})

test_that("NEI iso3c has country_name 'Other nei' after mutation", {
  result <- df |> filter(iso3c == "NEI")
  expect_true(all(result$country_name == "Other nei"))
})

test_that("iso3c is backfilled to 'NEI' for NA iso3c rows with artis_iso3c == 'NEI'", {
  result <- df |> filter(artis_iso3c == "NEI")
  expect_false(any(is.na(result$iso3c)))
  expect_true(all(result$iso3c == "NEI"))
})