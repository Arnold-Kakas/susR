
<!-- README.md is generated from README.Rmd. Please edit that file -->

# susR

<!-- badges: start -->
<!-- badges: end -->

The susR package provides a simple and consistent R interface to the
Slovak Statistical Office (SUSR) open data API. It streamlines tasks
like:

Listing available datasets (tables). Retrieving dimension codes and
values for a selected table. Fetching the data itself in a
straightforward manner. This allows you to explore official Slovak
statistical data directly within R, ensuring a reproducible and
efficient workflow.

## Installation

You can install the development version of susR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")   # if you do not have pak installed
pak::pak("Arnold-Kakas/susR")
```

## Example

This is a basic example illustrating how you might use susR to list
available tables and fetch dimension values for a specific one.

``` r


library(susR)

# 1) Retrieve a list of all available tables
tables <- get_table_list()
head(tables)
#> # A tibble: ... 
#> #   class   href                                            table_code ...
#> #   <chr>   <chr>                                           <chr>      ...
#> #   ...

# 2) Suppose we want the dimension values for the first table's first dimension:
#    We'll pick a table code and a dimension code from the table info

table_code <- tables$table_code[1]
# E.g., "as1001rs" or something similar
dimension_code <- "as1001rs_rok" # Hypothetical dimension code

dim_values <- get_dimension_values(table_code, dimension_code)
head(dim_values)
#> # A tibble: ...
#> #   dimension_code  dimension_label dimension_note element_code element_label
#> #   ...
```

For more details on additional functions, usage examples, and how to
retrieve the actual data, check out the package documentation and
vignettes once susR is installed.
