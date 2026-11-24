# ── Track 1: Higher-rank taxa ─────────────────────────────────────────────────

test_that("extracts single family name in parens", {
  df <- data.frame(desc = "Rays and skates (Rajidae)", stringsAsFactors = FALSE)
  result <- extract_scinames(df, "desc")
  expect_equal(result$desc_scinames[[1]], "Rajidae")
})

test_that("extracts multiple family names in parens", {
  df <- data.frame(
    desc = "Flat fish (Pleuronectidae, Bothidae, Cynoglossidae, Soleidae, Scophthalmidae, Citharidae)",
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_setequal(
    result$desc_scinames[[1]],
    c("Pleuronectidae", "Bothidae", "Cynoglossidae", "Soleidae", "Scophthalmidae", "Citharidae")
  )
})

test_that("extracts single family name from raw text (no parens)", {
  df <- data.frame(desc = "Salmonidae", stringsAsFactors = FALSE)
  result <- extract_scinames(df, "desc")
  expect_equal(result$desc_scinames[[1]], "Salmonidae")
})

test_that("extracts multiple family names from raw text", {
  df <- data.frame(
    desc = paste0(
      "Fish of the families Bregmacerotidae, Euclichthyidae, Gadidae, Macrouridae, ",
      "Melanonidae, Merlucciidae, Moridae and Muraenolepididae"
    ),
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_setequal(
    result$desc_scinames[[1]],
    c(
      "Bregmacerotidae", "Euclichthyidae", "Gadidae", "Macrouridae",
      "Melanonidae", "Merlucciidae", "Moridae", "Muraenolepididae"
    )
  )
})

test_that("extracts subfamily name (-inae suffix) from raw text", {
  df <- data.frame(desc = "Pacific salmonids (Salmoninae)", stringsAsFactors = FALSE)
  result <- extract_scinames(df, "desc")
  expect_true("Salmoninae" %in% result$desc_scinames[[1]])
})

test_that("extracts order-level name (-formes suffix) from raw text", {
  df <- data.frame(desc = "Perch-like fishes (Perciformes)", stringsAsFactors = FALSE)
  result <- extract_scinames(df, "desc")
  expect_true("Perciformes" %in% result$desc_scinames[[1]])
})

test_that("extracts order-level name (oidea suffix) alongside binomial in paren list", {
  df <- data.frame(
    desc = "Sea cucumbers (Stichopus japonicus, Holothurioidea)",
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_true("Holothurioidea" %in% result$desc_scinames[[1]])
  expect_true("Stichopus japonicus" %in% result$desc_scinames[[1]])
})


# ── Track 2: Binomials and genus+spp. ────────────────────────────────────────

test_that("extracts binomial in parens", {
  df <- data.frame(
    desc = "Albacore or longfinned tunas (Thunnus alalunga)",
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_equal(result$desc_scinames[[1]], "Thunnus alalunga")
})

test_that("extracts genus+spp. and retains trailing period", {
  df <- data.frame(desc = "Anchovies (Engraulis spp.)", stringsAsFactors = FALSE)
  result <- extract_scinames(df, "desc")
  expect_equal(result$desc_scinames[[1]], "Engraulis spp.")
})

test_that("extracts comma-separated binomial list in parens", {
  df <- data.frame(
    desc = "Cod (Gadus morhua, Gadus ogac, Gadus macrocephalus)",
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_setequal(
    result$desc_scinames[[1]],
    c("Gadus morhua", "Gadus ogac", "Gadus macrocephalus")
  )
})

test_that("extracts and-joined binomial list in parens", {
  df <- data.frame(
    desc = paste0(
      "Trout (Salmo trutta, Oncorhynchus mykiss, Oncorhynchus clarki, ",
      "Oncorhynchus aguabonita, Oncorhynchus gilae, Oncorhynchus apache ",
      "and Oncorhynchus chrysogaster)"
    ),
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_setequal(
    result$desc_scinames[[1]],
    c(
      "Salmo trutta", "Oncorhynchus mykiss", "Oncorhynchus clarki",
      "Oncorhynchus aguabonita", "Oncorhynchus gilae", "Oncorhynchus apache",
      "Oncorhynchus chrysogaster"
    )
  )
})

test_that("extracts binomials from multiple paren groups in one description", {
  df <- data.frame(
    desc = "Atlantic salmon (Salmo salar) and Danube salmon (Hucho hucho)",
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_setequal(
    result$desc_scinames[[1]],
    c("Salmo salar", "Hucho hucho")
  )
})

test_that("normalizes subgenus notation: returns inner genus + species, not outer genus", {
  df <- data.frame(
    desc = "Skipjack tuna (Euthynnus (Katsuwonus) pelamis)",
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_true("Katsuwonus pelamis" %in% result$desc_scinames[[1]])
  expect_false("Euthynnus pelamis" %in% result$desc_scinames[[1]])
  expect_false("Euthynnus" %in% result$desc_scinames[[1]])
})

test_that("post-processes 'of the genus X' to 'X spp.'", {
  df <- data.frame(
    desc = "Tunas (of the genus Thunnus)",
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_true("Thunnus spp." %in% result$desc_scinames[[1]])
})


# ── Combined and edge cases ───────────────────────────────────────────────────

test_that("returns Track 2 names only when no higher-rank suffixes are present", {
  df <- data.frame(
    desc = "Chinese carp (Hypophthalmichthys spp.)",
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_equal(result$desc_scinames[[1]], "Hypophthalmichthys spp.")
})

test_that("returns character(0) when no scientific names are present", {
  df <- data.frame(desc = "Carp", stringsAsFactors = FALSE)
  result <- extract_scinames(df, "desc")
  expect_equal(result$desc_scinames[[1]], character(0))
})

test_that("returns character(0) silently for NA input row", {
  df <- data.frame(desc = NA_character_, stringsAsFactors = FALSE)
  expect_silent(result <- extract_scinames(df, "desc"))
  expect_equal(result$desc_scinames[[1]], character(0))
})

test_that("handles multiple rows, including mixed content and NA", {
  df <- data.frame(
    desc = c(
      "Albacore or longfinned tunas (Thunnus alalunga)",
      "Carp",
      NA_character_,
      "Rays and skates (Rajidae)"
    ),
    stringsAsFactors = FALSE
  )
  result <- extract_scinames(df, "desc")
  expect_equal(result$desc_scinames[[1]], "Thunnus alalunga")
  expect_equal(result$desc_scinames[[2]], character(0))
  expect_equal(result$desc_scinames[[3]], character(0))
  expect_equal(result$desc_scinames[[4]], "Rajidae")
})

test_that("output column name follows the <col>_scinames convention", {
  df <- data.frame(hs6_desc = "Anchovies (Engraulis spp.)", stringsAsFactors = FALSE)
  result <- extract_scinames(df, "hs6_desc")
  expect_true("hs6_desc_scinames" %in% names(result))
})

test_that("input df is unchanged except for the new list-column", {
  df <- data.frame(hs6_desc = "Carp", value = 1L, stringsAsFactors = FALSE)
  result <- extract_scinames(df, "hs6_desc")
  expect_equal(result$hs6_desc, df$hs6_desc)
  expect_equal(result$value, df$value)
  expect_equal(ncol(result), ncol(df) + 1L)
})


# ── Input validation ──────────────────────────────────────────────────────────

test_that("cli_abort is triggered for non-data-frame df", {
  expect_error(extract_scinames(list(x = 1), "x"), class = "rlang_error")
})

test_that("cli_abort is triggered for non-character col", {
  df <- data.frame(x = "foo", stringsAsFactors = FALSE)
  expect_error(extract_scinames(df, 1L), class = "rlang_error")
})

test_that("cli_abort is triggered for col not found in df", {
  df <- data.frame(x = "foo", stringsAsFactors = FALSE)
  expect_error(extract_scinames(df, "not_a_col"), class = "rlang_error")
})
