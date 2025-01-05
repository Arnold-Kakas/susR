test_that("get_table_list returns a dataframe", {
  # Skip on CRAN to avoid potential network issues or rate limiting
  skip_on_cran()

  # Skip if offline (e.g. no internet)
  skip_if_offline()

  res <- get_table_list()
  expect_s3_class(res, "tbl_df")
})
