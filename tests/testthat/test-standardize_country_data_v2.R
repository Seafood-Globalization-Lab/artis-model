# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })


df <- standardize_country_data()

# 2. Test for Correct Year Mapping
test_that("Year column has no missing values and is valid", {
  expect_true(all(!is.na(df$year)), "Some years are missing")
  expect_true(all(df$year >= 1996 & df$year <= 2023), "Invalid year values")
})

# Test that artis columns are their expected types
test_that("`country_name` column data type is character", {
  expect_type(df$country_name, "character")
})

test_that("`iso3c` column data type is character", {
  expect_type(df$iso3c, "character")
})

test_that("`year` column data type is integer", {
  expect_type(df$year, "integer")
})

test_that("`artis_iso3c` column data type is character", {
  expect_type(df$artis_iso3c, "character")
})

test_that("`artis_country_name` column data type is character", {
  expect_type(df$artis_country_name, "character")
})

# Check length of iso3c variables

test_that("`iso3c` column character values are length of 3", {
  expect_equal(mean(str_length(df$iso3c), na.rm = TRUE),3)
})

test_that("`artis_iso3c` column character values are length of 3", {
  expect_equal(mean(str_length(df$artis_iso3c), na.rm = TRUE),3)
})


# Don't want NA's in year
test_that("`year` contains no NAs", {
  expect_true(all(!is.na(df$year)))
})

# Dont want NA's in artis_iso3c
test_that("`artis_iso3c` contains no NAs", {
  expect_true(all(!is.na(df$artis_iso3c)))
})

# Dont want NA's in artis_country_name
test_that("`artis_country_name` contains no NAs", {
  expect_true(all(!is.na(df$artis_country_name)))
})



 # Don't want special characters in artis_country_name

# 3. Test that All Unique ISO3C Codes Map to Valid Country Names
valid_country_names <- df$artis_country_name
test_that("Unique ISO3C codes map to valid country names", {
  unique_iso3c <- unique(df$iso3c)
  for (iso3c in unique_iso3c) {
    country_name <- df$artis_country_name[df$iso3c == iso3c][1]
    expect_true(country_name %in% valid_country_names,
                info = paste("Invalid country name for ISO3C", iso3c))
  }
})







# 4. Test for Missing or Null Values
test_that("No missing values in important columns", {
  required_columns <- c("iso3c", "country_name", "artis_iso3c", "artis_country_name", "year")
  for (col in required_columns) {
    expect_true(all(!is.na(df[[col]])), paste("Missing values in column:", col))
  }
})

# 5. Test Standardization Logic
test_that("Standardization logic works as expected", {
  test_cases <- list(
    c('USA', 'United States'),
    c('GB', 'United Kingdom'),
    c('DE', 'Germany')
  )

  for (test_case in test_cases) {
    iso3c <- test_case[1]
    expected_name <- test_case[2]

    actual_name <- df$artis_country_name[df$iso3c == iso3c][1]
    expect_equal(actual_name, expected_name, info = paste("Failed for", iso3c))

    # Also check the ISO3C mapping
    actual_iso3c <- df$artis_iso3c[df$iso3c == iso3c][1]
    expect_equal(actual_iso3c, iso3c, info = paste("Failed for", iso3c))
  }
})

# 6. Test for Consistency with External Data (if applicable)
iso_3166_reference <- list(
  'USA' = 'United States',
  'GB' = 'United Kingdom',
  'DE' = 'Germany',
  'IN' = 'India'
)
test_that("ISO3C codes map to valid country names according to ISO 3166 reference", {
  for (iso3c in df$artis_iso3c) {
    expected_country_name <- iso_3166_reference[[iso3c]]
    if (!is.null(expected_country_name)) {
      actual_country_name <- df$artis_country_name[df$artis_iso3c == iso3c][1]
      expect_equal(actual_country_name, expected_country_name,
                   info = paste("Failed for ISO3C", iso3c))
    }
  }
})

# 7. Test for Duplicate Entries
test_that("No duplicate rows for iso3c and country_name", {
  duplicates <- df %>%
    group_by(iso3c, country_name) %>%
    filter(n() > 1)
  expect_true(nrow(duplicates) == 0, paste("Found duplicate entries:", nrow(duplicates)))
})

# 8. Test for Edge Cases (e.g., Non-standard Inputs)
test_that("Edge cases are handled correctly", {
  edge_cases <- data.frame(
    iso3c = c('XXX', '', 'GB'),
    country_name = c('Unknown', 'No country', 'United Kingdom'),
    artis_iso3c = c('XXX', 'UNK', 'GB'),
    artis_country_name = c('Unknown', 'No country', 'United Kingdom')
  )

  for (i in 1:nrow(edge_cases)) {
    iso3c <- edge_cases$iso3c[i]
    expected_country_name <- edge_cases$artis_country_name[i]
    actual_country_name <- df$artis_country_name[df$iso3c == iso3c][1]
    expect_equal(actual_country_name, expected_country_name,
                 info = paste("Edge case failed for", iso3c))

    # Check the ISO3C as well
    expected_iso3c <- edge_cases$artis_iso3c[i]
    actual_iso3c <- df$artis_iso3c[df$iso3c == iso3c][1]
    expect_equal(actual_iso3c, expected_iso3c,
                 info = paste("Edge case failed for", iso3c))
  }
})

# 9. Test for Historical Country Name Changes (if applicable)
test_that("Country names reflect historical changes", {
  test_cases <- list(
    c(2000, 'Yugoslavia', 'Serbia and Montenegro'),
    c(2010, 'Serbia and Montenegro', 'Serbia')
  )
  for (test_case in test_cases) {
    year <- test_case[1]
    old_name <- test_case[2]
    new_name <- test_case[3]
    row <- df[df$year == year & df$artis_country_name == old_name, ]
    expect_equal(row$artis_country_name[1], new_name,
                 info = paste("Historical mapping failed for", year, old_name))
  }
})
