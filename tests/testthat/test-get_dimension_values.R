test_that("get_dimension_values works as expected", {
  # Skip on CRAN to avoid potential network issues or rate limiting
  skip_on_cran()

  # Skip if offline (e.g. no internet)
  skip_if_offline()

  # Known table code and dimension code that exist at the time of writing
  # Replace these with stable codes as needed
  table_code    <- "as1001rs"
  dimension_code <- "as1001rs_rok"

  # Call the function
  res <- susr_dimension_values(
    table_code     = table_code,
    dimension_code = dimension_code,
    lang           = "en"
  )

  # Check that we got a tibble
  expect_s3_class(res, "tbl_df")

  # Check that essential columns are present, element_label is skipped as it is not available for all tables
  expected_cols <- c(
    "dimension_code",
    "dimension_label",
    "dimension_note",
    "element_index",
    "element_value"
  )
  expect_true(all(expected_cols %in% names(res)))

  # Optionally, check for some data rows (assuming at least 1 row)
  expect_true(nrow(res) > 0)

  # We can also verify that dimension_code in the result matches the input
  if (nrow(res) > 0) {
    expect_true(all(res$dimension_code == dimension_code))
  }
})
