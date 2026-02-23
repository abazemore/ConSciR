# # parse_datalogger
# test_that("Central parsing function has expected number of columns and rows for Meaco", {
#   expect_equal(nrow(parse_brand(test_path("fixtures", "meaco"), brand = "meaco")), 20)
#   expect_equal(ncol(parse_brand(test_path("fixtures", "meaco"), brand = "meaco")), 5)
#   expect_true(sum(is.na(parse_brand(test_path("fixtures", "meaco"), brand = ))) == 0)
# })

# parse_meaco
test_that("Pre-Gingerbread Meaco file has correct number of columns and rows with no NA values",
          {
            expect_equal(nrow(parse_meaco(
              test_path("fixtures", "meaco", "Meaco_1.csv")
            )), 10)
            expect_equal(ncol(parse_meaco(
              test_path("fixtures", "meaco", "Meaco_1.csv")
            )), 5)
          })
test_that("Pre-Gingerbread Meaco file has no NA values", {
  expect_true(sum(is.na(parse_meaco(
    test_path("fixtures", "meaco", "Meaco_1.csv")
  ))) == 0)
})
test_that("Gingerbread Meaco file has correct number of columns and rows with no NA values",
          {
            expect_equal(nrow(parse_meaco(
              test_path("fixtures", "meaco", "Meaco_3.csv")
            )), 10)
            expect_equal(ncol(parse_meaco(
              test_path("fixtures", "meaco", "Meaco_3.csv")
            )), 5)
          })
test_that("Gingerbread Meaco file has no NA values", {
  expect_true(sum(is.na(parse_meaco(
    test_path("fixtures", "meaco", "Meaco_3.csv")
  ))) == 0)
})

# parse_miniclima
# No miniClima sample data

# parse_rotronic
test_that("Rotronic csv file has correct number of columns and rows with no NA values",
          {
            expect_equal(nrow(parse_rotronic(
              test_path("fixtures", "rotronic", "Rotronic_1.csv")
            )), 10)
            expect_equal(ncol(parse_rotronic(
              test_path("fixtures", "rotronic", "Rotronic_1.csv")
            )), 5)
            expect_equal(sum(is.na(parse_rotronic(
              test_path("fixtures", "rotronic", "Rotronic_1.csv")
            ))), 0)
          })
test_that("Rotronic tsv file has correct number of columns and rows with no NA values",
          {
            expect_equal(nrow(parse_rotronic(
              test_path("fixtures", "rotronic", "Rotronic_2.xls")
            )), 10)
            expect_equal(ncol(parse_rotronic(
              test_path("fixtures", "rotronic", "Rotronic_2.xls")
            )), 5)
            expect_equal(sum(is.na(parse_rotronic(
              test_path("fixtures", "rotronic", "Rotronic_2.xls")
            ))), 0)
          })
# parse_tandd
test_that("T&D file has correct number of columns and rows with no NA values", {
  expect_equal(nrow(parse_TandD(
    test_path("fixtures", "tandd", "TandD_1.csv")
  )), 10)
  expect_equal(ncol(parse_TandD(
    test_path("fixtures", "tandd", "TandD_1.csv")
  )), 5)
  expect_equal(sum(is.na(parse_trendBMS(
    test_path("fixtures", "trend", "Trend_1.csv")
  ))), 0)
})

# parse_tinytag
test_that("TinyTag file has correct number of columns and rows with no NA values",
          {
            expect_equal(nrow(parse_tinytag(
              test_path("fixtures", "tinytag", "TinyTag_1.csv")
            )), 10)
            expect_equal(ncol(parse_tinytag(
              test_path("fixtures", "tinytag", "TinyTag_1.csv")
            )), 5)
          })
test_that("TinyTag file has no NA values", {
  expect_equal(sum(is.na(parse_tinytag(
    test_path("fixtures", "tinytag", "TinyTag_1.csv")
  ))), 0)
})

# parse_trend
test_that("TrendBMS file has correct number of columns and rows (T or RH only)",
          {
            expect_equal(nrow(parse_trendBMS(
              test_path("fixtures", "trend", "Trend_1.csv")
            )), 10)
            expect_equal(ncol(parse_trendBMS(
              test_path("fixtures", "trend", "Trend_1.csv")
            )), 4)
          })
test_that("TrendBMS file has no NA values", {
  expect_equal(sum(is.na((
    parse_trendBMS(test_path("fixtures", "trend", "Trend_1.csv"))
  ))), 0)
})
# combine_data
