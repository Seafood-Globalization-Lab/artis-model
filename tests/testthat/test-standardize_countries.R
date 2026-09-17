# Tests for standardize_countries()


# Return structure -------------------------------------------------------

test_that("returns a data frame with artis_country_name and artis_iso3c columns", {
  # Minimal two-row input using the name_en path. The main goal here is to
  # confirm the function returns a data frame (not a tibble-only class or list)
  # and that both standardized output columns are present regardless of whether
  # the lookup succeeded.
  df <- data.frame(
    country = c("United States", "France"),
    year    = c(2015L, 2015L),
    stringsAsFactors = FALSE
  )
  
  result <- standardize_countries(
    data             = df,
    country_id_format = "name_en",
    country_col       = "country",
    year_col          = "year"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("artis_country_name" %in% names(result))
  expect_true("artis_iso3c"        %in% names(result))
})


test_that("name_en output does not contain an intermediate iso3c column", {
  # The name_en branch introduces an iso3c column via the corrections join and
  # must drop it before returning — otherwise the output schema changes
  # unexpectedly for callers who don't expect that column.
  df <- data.frame(
    country = "France",
    year    = 2015L,
    stringsAsFactors = FALSE
  )

  result <- standardize_countries(
    data              = df,
    country_id_format = "name_en",
    country_col       = "country",
    year_col          = "year"
  )

  expect_false("iso3c" %in% names(result))
})


test_that("all original columns are preserved", {
  # The function should append the two standardized columns without dropping
  # any column that was already in the input. This test uses an extra column
  # (`extra_col`) as a canary — if it disappears or its value changes, the
  # function has mutated the data unexpectedly.
  df <- data.frame(
    country   = "Germany",
    year      = 2010L,
    extra_col = "keep_me",
    stringsAsFactors = FALSE
  )
  
  result <- standardize_countries(
    data             = df,
    country_id_format = "name_en",
    country_col       = "country",
    year_col          = "year"
  )
  
  expect_true("extra_col" %in% names(result))
  expect_equal(result$extra_col, "keep_me")
})



# Happy paths ------------------------------------------------------------

test_that("standardizes well-known country names (name_en)", {
  # Exercises the name_en branch with three unambiguous English country names.
  # All three should resolve to non-NA ISO3c codes via either the ARTIS
  # corrections table or the countrycode fallback.
  df <- data.frame(
    country = c("United States", "France", "Germany"),
    year    = c(2015L, 2015L, 2015L),
    stringsAsFactors = FALSE
  )
  
  result <- standardize_countries(
    data             = df,
    country_id_format = "name_en",
    country_col       = "country",
    year_col          = "year"
  )
  
  # No NA codes expected for these well-known countries
  expect_false(any(is.na(result$artis_iso3c)))
  expect_true("USA" %in% result$artis_iso3c)
  expect_true("FRA" %in% result$artis_iso3c)
  expect_true("DEU" %in% result$artis_iso3c)
})


test_that("standardizes ISO3c codes (iso3c)", {
  # Exercises the iso3c branch. Note the deliberately non-default column names
  # (`iso` and `yr`) to confirm the function respects country_col /
  # year_col rather than hard-coding column names.
  df <- data.frame(
    iso = c("USA", "FRA", "DEU"),
    yr  = c(2015L, 2015L, 2015L),
    stringsAsFactors = FALSE
  )
  
  result <- standardize_countries(
    data             = df,
    country_id_format = "iso3c",
    country_col       = "iso",
    year_col          = "yr"
  )
  
  # Both the code and the name columns should resolve cleanly
  expect_false(any(is.na(result$artis_iso3c)))
  expect_true("USA" %in% result$artis_iso3c)
  expect_false(any(is.na(result$artis_country_name)))
})



# ARTIS-specific corrections ---------------------------------------------

test_that("maps territory ISO3c codes to their sovereign country (iso3c)", {
  # Territory codes like ASM (American Samoa) must be corrected to their
  # sovereign country (USA) via the ARTIS corrections table, not left as-is.
  df <- data.frame(
    iso  = "ASM",
    year = 2015L,
    stringsAsFactors = FALSE
  )

  result <- standardize_countries(
    data              = df,
    country_id_format = "iso3c",
    country_col       = "iso",
    year_col          = "year"
  )

  expect_equal(result$artis_iso3c, "USA")
})


test_that("maps territory names to their sovereign country via two-pass logic (name_en)", {
  # "Puerto Rico" won't match a country_name in the corrections table directly.
  # The function should flag it, use countrycode to get "PRI", then re-join
  # the corrections table to resolve PRI → USA (the sovereign country).
  df <- data.frame(
    country = "Puerto Rico",
    year    = 2015L,
    stringsAsFactors = FALSE
  )

  result <- standardize_countries(
    data              = df,
    country_id_format = "name_en",
    country_col       = "country",
    year_col          = "year"
  )

  expect_equal(result$artis_iso3c, "USA")
})


test_that("applies time-dependent sovereign corrections by year", {
  # Timor-Leste (TLS) was part of Indonesia (IDN) before independence in 2002.
  # The same iso3c code must resolve to different sovereigns depending on year.
  df <- data.frame(
    iso  = c("TLS", "TLS"),
    year = c(1999L, 2003L),
    stringsAsFactors = FALSE
  )

  result <- standardize_countries(
    data              = df,
    country_id_format = "iso3c",
    country_col       = "iso",
    year_col          = "year"
  )

  expect_equal(result$artis_iso3c[result$year == 1999L], "IDN")
  expect_equal(result$artis_iso3c[result$year == 2003L], "TLS")
})



# String cleaning — parenthetical stripping   ----------------------------

test_that("strips trailing parenthetical phrases from country names", {
  # Raw data from some sources appends qualifiers in parentheses, e.g.
  # "Korea (Republic of)" or "Bolivia (Plurinational State of)". The function
  # should strip the parenthetical before the lookup so the match succeeds.
  df <- data.frame(
    country = "Korea (Republic of)",
    year    = 2015L,
    stringsAsFactors = FALSE
  )
  
  result <- standardize_countries(
    data             = df,
    country_id_format = "name_en",
    country_col       = "country",
    year_col          = "year"
  )
  
  # The parenthetical must have been stripped for "Korea" to resolve — a NA
  # artis_iso3c would indicate the strip didn't happen or the lookup failed.
  expect_equal(result$artis_iso3c, "KOR")
})



# Unresolvable inputs ----------------------------------------------------

test_that("unresolvable country names produce NA artis_country_name and a message", {
  # "Atlantis" is not in the ARTIS corrections table and will not be recognized
  # by countrycode. The function should degrade gracefully: emit a cli message
  # and leave artis_country_name as NA for that row.
  df <- data.frame(
    country = "Atlantis",
    year    = 2015L,
    stringsAsFactors = FALSE
  )
  
  result <- 
    standardize_countries(
      data              = df,
      country_id_format = "name_en",
      country_col       = "country",
      year_col          = "year"
    )
  

  expect_true(is.na(result$artis_country_name))
  expect_true(is.na(result$artis_iso3c))
  expect_message(
    standardize_countries(
      data              = df,
      country_id_format = "name_en",
      country_col       = "country",
      year_col          = "year"
    ))
})


test_that("unresolvable ISO3c codes produce NA artis_iso3c and a message", {
  # "ZZZ" is not a valid ISO3c code. Same graceful-degradation expectation as
  # the name_en case above: emit a cli message and leave artis_iso3c as NA.
  df <- data.frame(
    iso  = "ZZZ",
    year = 2015L,
    stringsAsFactors = FALSE
  )
  
  result <- 
    standardize_countries(
      data              = df,
      country_id_format = "iso3c",
      country_col       = "iso",
      year_col          = "year"
    )
  

  expect_true(is.na(result$artis_iso3c))
  expect_true(is.na(result$artis_country_name))
  expect_message( 
    standardize_countries(
      data              = df,
      country_id_format = "iso3c",
      country_col       = "iso",
      year_col          = "year"
    ))
})



# Input validation -------------------------------------------------------

test_that("errors when country column is not character type", {
  # Passing integers as country identifiers is a clear user mistake. The
  # function should detect this early (before any join) and abort with a
  # descriptive message rather than producing silently wrong output.
  df <- data.frame(
    country = 1:3,
    year    = c(2010L, 2011L, 2012L)
  )
  
  expect_error(
    standardize_countries(
      data             = df,
      country_id_format = "name_en",
      country_col       = "country",
      year_col          = "year"
    ),
    regexp = "does not appear to be a character type"
  )
})


test_that("errors when year column is not numeric", {
  # Years supplied as character strings (e.g. imported from a CSV without type
  # coercion) must be caught before the join — a character year will silently
  # fail to match any numeric year in the corrections table.
  df <- data.frame(
    country = c("France", "Germany"),
    year    = c("2010", "2011"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    standardize_countries(
      data             = df,
      country_id_format = "name_en",
      country_col       = "country",
      year_col          = "year"
    ),
    regexp = "does not appear to be numeric"
  )
})


test_that("errors on invalid country_id_format", {
  # Only "name_en" and "iso3c" are accepted. Any other string should abort
  # with the "Invalid value supplied" message so users know exactly what went
  # wrong and what the valid options are.
  df <- data.frame(
    country = "France",
    year    = 2015L,
    stringsAsFactors = FALSE
  )
  
  expect_error(
    standardize_countries(
      data             = df,
      country_id_format = "foobar",
      country_col       = "country",
      year_col          = "year"
    ),
    regexp = "Invalid value supplied"
  )
})


# Missing / empty data ---------------------------------------------------

test_that("issues a message (not an error) when NA values are present in country column", {
  # NA rows are common in real data. The function should emit a cli message
  # alerting the user rather than halting execution entirely.
  df <- data.frame(
    country = c("France", NA_character_),
    year    = c(2015L, 2015L),
    stringsAsFactors = FALSE
  )

  expect_no_error(
    expect_message(
      standardize_countries(
        data              = df,
        country_id_format = "name_en",
        country_col       = "country",
        year_col          = "year"
      )
    )
  )
})


test_that("issues a message when empty string values are present in country column", {
  # Empty strings are distinct from NA but equally unresolvable. The pre-flight
  # check counts both and should emit a cli message for either.
  df <- data.frame(
    country = c("France", ""),
    year    = c(2015L, 2015L),
    stringsAsFactors = FALSE
  )
  
  expect_message(
    standardize_countries(
      data             = df,
      country_id_format = "name_en",
      country_col       = "country",
      year_col          = "year"
    )
  )
})


# Invariants -------------------------------------------------------------

test_that("output has the same number of rows as input", {
  # The left_join to the corrections table could theoretically expand rows if
  # there are multiple matches for a country-year pair. This test guards against
  # that by asserting a strict 1-to-1 row correspondence.
  df <- data.frame(
    country = c("United States", "France", "Germany", "Japan"),
    year    = c(2010L, 2011L, 2012L, 2013L),
    stringsAsFactors = FALSE
  )
  
  result <- standardize_countries(
    data             = df,
    country_id_format = "name_en",
    country_col       = "country",
    year_col          = "year"
  )
  
  expect_equal(nrow(result), nrow(df))
})


test_that("works with non-default column names", {
  # country_col and year_col are user-supplied strings, so the
  # function must use tidy-eval / .data[[]] indirection rather than assuming
  # fixed column names. This test uses deliberately odd names to catch any
  # hard-coded "country" or "year" references.
  df <- data.frame(
    my_country_col = c("Japan", "Brazil"),
    my_year_col    = c(2018L, 2019L),
    stringsAsFactors = FALSE
  )
  
  result <- standardize_countries(
    data             = df,
    country_id_format = "name_en",
    country_col       = "my_country_col",
    year_col          = "my_year_col"
  )
  
  expect_true("artis_country_name" %in% names(result))
  expect_true("artis_iso3c"        %in% names(result))
  expect_false(any(is.na(result$artis_iso3c)))
})

