# tests/testthat/test-manual-table-list.R
test_that("Manual table list covers all codes in get_table_list()", {

  # Possibly skip on CRAN or offline environments
  skip_on_cran()
  skip_if_offline()

  manual_df <- get_domains()
  api_df    <- get_table_list()

  # Basic check: Ensure the columns exist
  expect_true("table_code" %in% names(manual_df))
  expect_true("table_code" %in% names(api_df))

  # Are all manual table codes present in the official list?
  missing_in_api <- setdiff(manual_df$table_code, api_df$table_code)
  expect_true(
    length(missing_in_api) == 0,
    info = paste("Some manual codes not found in API:", paste(missing_in_api, collapse = ", "))
  )

  # Optional: Are there any new codes in the API that aren't in the manual CSV?
  # (Depending on your requirement, you can warn or fail the test.)
  extra_in_api <- setdiff(api_df$table_code, manual_df$table_code)
  if (length(extra_in_api) > 0) {
    warning("There are new codes in the API that are not in the manual list: ",
            paste(extra_in_api, collapse = ", "))
  }
})
