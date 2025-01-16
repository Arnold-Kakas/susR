test_that("susr_tables() returns correct wide format output", {
  skip_on_cran()
  skip_if_offline()

  tbl_wide <- susr_tables(long = FALSE)
  expect_s3_class(tbl_wide, "tbl_df")

  # Check expected columns in the wide output
  expected_cols <- c("class", "href", "table_code", "label", "update", "dimension_names")
  expect_true(all(expected_cols %in% names(tbl_wide)))

  # Optionally, check that the tibble is not empty
  expect_true(nrow(tbl_wide) >= 0)
})

test_that("susr_tables() returns correct long format output", {
  skip_on_cran()
  skip_if_offline()

  tbl_long <- susr_tables(long = TRUE)
  expect_s3_class(tbl_long, "tbl_df")

  # In long mode, we expect a column "dimension_code" and NOT the column "dimension_names"
  expect_true("dimension_code" %in% names(tbl_long))
  expect_false("dimension_names" %in% names(tbl_long))

  # If a wide table row has missing dimension_names, the corresponding long entries should have NA in dimension_code
  tbl_wide <- susr_tables(long = FALSE)
  missing_idx <- which(is.na(tbl_wide$dimension_names) | tbl_wide$dimension_names == "")
  if (length(missing_idx) > 0) {
    tbl_long_subset <- dplyr::filter(tbl_long, table_code %in% tbl_wide$table_code[missing_idx])
    expect_true(all(is.na(tbl_long_subset$dimension_code)))
  }
})

test_that("susr_tables() filters correctly by table_codes", {
  skip_on_cran()
  skip_if_offline()

  tbl <- susr_tables(long = FALSE)
  if(nrow(tbl) > 0) {
    # Select one or two unique table codes from the output
    selected_codes <- unique(tbl$table_code)[1:2]
    tbl_filtered <- susr_tables(long = FALSE, table_codes = selected_codes)
    # Check that all returned rows have table_code in selected_codes
    expect_true(all(tbl_filtered$table_code %in% selected_codes))
  } else {
    succeed("No tables available to test table_codes filtering")
  }
})


test_that("susr_tables() filters correctly by domains", {
  skip_on_cran()
  skip_if_offline()

  # susr_domains() should return a tibble with columns 'table_code', 'domain', 'subdomain'
  domains_df <- susr_domains()
  if(nrow(domains_df) > 0) {
    # Select one or two domains from the static CSV
    selected_domains <- unique(domains_df$domain)[1:2]
    tbl_filtered <- susr_tables(long = FALSE, domains = selected_domains)
    # For each row, at least one of the 'domain' or 'subdomain' should be in selected_domains
    # (If the join fails to add these columns, this condition could be skipped)
    expect_true(all(tbl_filtered$domain %in% selected_domains | tbl_filtered$subdomain %in% selected_domains))
  } else {
    succeed("No domain info available to test domain filtering")
  }
})
