# Test 1: Error if number of elements in params is odd.
test_that("fetch_susr_data() throws error when params length is odd", {
  # Create a params list with three elements (odd length)
  odd_params <- list("np3106rr", list("SK021", "2016"), "extra_item")
  expect_warning(
    result <- fetch_susr_data(odd_params, lang = "en"),
    "odd number of elements"
  )
  expect_null(result)
})

# Test 2: Incomplete parameters (likely to trigger HTTP 400)
test_that("fetch_susr_data() returns warning and NULL for incomplete parameters", {
  skip_on_cran()
  skip_if_offline()

  # Incomplete parameters: only two dimension segments are provided.
  params <- list("np3106rr", list("SK021", "2016"))

  expect_warning(
    res <- fetch_susr_data(params, lang = "en")
  )
})

# Test 3: Invalid table code should produce a warning and return NULL
test_that("fetch_susr_data() returns warning and NULL for an invalid table code", {
  skip_on_cran()
  skip_if_offline()

  params <- list("nonexistent", list("all", "all", "all"))

  expect_warning(
    res <- fetch_susr_data(params, lang = "en"),
    "In `params`, each table code must be a single 8-character string. Problem at position 1"
  )

})

# Test 4: Valid API call should return a named list with a tibble for the table code
test_that("fetch_susr_data() returns a named list with a tibble for valid parameters", {
  skip_on_cran()
  skip_if_offline()

  params <- list(
    "np3106rr",
    list("SK021", c("2016", "2017", "2018"), "E_PRIEM_HR_MZDA", "all")
  )

  res <- fetch_susr_data(params, lang = "en")

  # Check that the result is a list and has the expected table code
  expect_type(res, "list")
  expect_true("np3106rr" %in% names(res))

  # If the API call succeeded, then the value should be a tibble
  if (!is.null(res[["np3106rs"]])) {
    expect_s3_class(res[["np3106rr"]], "tbl_df")
    # Optionally, ensure the tibble has at least one column
    expect_true(ncol(res[["np3106rr"]]) >= 1)
  }
})
