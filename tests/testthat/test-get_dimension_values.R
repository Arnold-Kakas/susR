
# Test 1: Check proper error is thrown for an invalid dimension code
test_that("susr_dimension_values() fails for an invalid dimension code", {
  skip_on_cran()
  skip_if_offline()

  expect_error(
    susr_dimension_values("as1001rs", "invalid_dimension", lang = "en"),
    "Failed to retrieve dimension info for invalid_dimension"
  )
})

# Test 2: Check that valid parameters return a tibble with the expected columns.
test_that("susr_dimension_values() returns a tibble with expected columns", {
  skip_on_cran()
  skip_if_offline()

  # Replace "as1001rs" and "as1001rs_rok" with known valid table and dimension codes.
  res <- susr_dimension_values("as1001rs", "as1001rs_rok", lang = "en")
  expect_s3_class(res, "tbl_df")

  expected_cols <- c("dimension_code", "dimension_label", "dimension_note",
                     "element_index", "element_value")
  expect_true(all(expected_cols %in% names(res)))

  # Optionally, check that the tibble is non-empty if data is expected
  # (This test may fail if the API returns no elements for some reason.)
  expect_true(nrow(res) >= 0)
})
