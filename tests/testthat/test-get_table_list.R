test_that("get_table_list returns a dataframe", {
  expect_equal(any(class(get_table_list()) == "data.frame"), TRUE)
})
