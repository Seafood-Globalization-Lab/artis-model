df <- standardize_country_data()

# ── Column types ──────────────────────────────────────────────────────────────

test_that("all columns have correct types", {
  expect_type(df$country_name,       "character")
  expect_type(df$iso3c,              "character")
  expect_type(df$year,               "integer")
  expect_type(df$artis_country_name, "character")
  expect_type(df$artis_iso3c,        "character")
})

# ── Missing values ────────────────────────────────────────────────────────────

test_that("country_name, artis_country_name and artis_iso3c have no NAs or empty strings", {
  expect_true(all(!is.na(df$country_name)))
  expect_true(all(!is.na(df$artis_country_name)))
  expect_true(all(!is.na(df$artis_iso3c)))

  expect_true(all(df$country_name       != ""))
  expect_true(all(df$artis_country_name != ""))
  expect_true(all(df$artis_iso3c        != ""))
})

test_that("iso3c contains some NAs (expected behaviour for unmatched countries)", {
  expect_true(any(is.na(df$iso3c)))
})

test_that("year has no missing values", {
  expect_true(all(!is.na(df$year)))
})

# ── iso3c format ──────────────────────────────────────────────────────────────

test_that("iso3c and artis_iso3c non-NA values are all exactly 3 characters", {
  bad_iso3c      <- df$iso3c[!is.na(df$iso3c)           & stringr::str_length(df$iso3c)       != 3]
  bad_artis_iso3c <- df$artis_iso3c[!is.na(df$artis_iso3c) & stringr::str_length(df$artis_iso3c) != 3]

  expect_equal(length(bad_iso3c),       0, info = paste("Bad iso3c values:",       paste(bad_iso3c,       collapse = ", ")))
  expect_equal(length(bad_artis_iso3c), 0, info = paste("Bad artis_iso3c values:", paste(bad_artis_iso3c, collapse = ", ")))
})

# ── Year range ────────────────────────────────────────────────────────────────

test_that("year values are within the expected range and all years are present", {
  # FIXIT: source min/max year dynamically from ARTIS config file
  expect_true(all(df$year >= 1996 & df$year <= 2023), info = "Some year values are out of range")

  missing_years <- setdiff(1996:2023, unique(df$year))
  expect_equal(length(missing_years), 0,
               info = paste("Missing years:", paste(missing_years, collapse = ", ")))
})

# ── Duplicates ────────────────────────────────────────────────────────────────

test_that("no duplicate rows for country_name, iso3c and year", {
  duplicates <- df |>
    dplyr::group_by(country_name, iso3c, year) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  fail_msg <- paste(
    "Found", nrow(duplicates), "duplicate entries:\n",
    paste(capture.output(print(duplicates)), collapse = "\n")
  )

  expect_true(nrow(duplicates) == 0, info = fail_msg)
})

# ── Standardization mapping consistency ──────────────────────────────────────

test_that("each artis_iso3c maps to exactly one artis_country_name", {
  mapping_counts <- df |>
    dplyr::distinct(artis_iso3c, artis_country_name) |>
    dplyr::count(artis_iso3c) |>
    dplyr::filter(n > 1)

  ambiguous <- df |>
    dplyr::distinct(artis_iso3c, artis_country_name) |>
    dplyr::filter(artis_iso3c %in% mapping_counts$artis_iso3c) |>
    dplyr::arrange(artis_iso3c)

  fail_msg <- paste(
    "These iso3c codes map to more than one country name:\n",
    paste(ambiguous$artis_iso3c, "->", ambiguous$artis_country_name, collapse = "\n")
  )

  expect_equal(nrow(mapping_counts), 0, info = fail_msg)
})
