## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("Arnold-Kakas/susR")

## -----------------------------------------------------------------------------
library(susR)

## -----------------------------------------------------------------------------
susr_domains_table <- susr_domains()
head(susr_domains_table)

## -----------------------------------------------------------------------------
susr_tables <- susr_tables()
head(susr_tables)

## -----------------------------------------------------------------------------
# Example: filter by domain = "Demographic and social statistics"
domain_tables <- susr_tables(domain = "Demographic and social statistics")

dim(domain_tables)

## -----------------------------------------------------------------------------
subdomain_tables <- susr_tables(domain = "Housing")

dim(subdomain_tables)

## -----------------------------------------------------------------------------
multiple_subdomain_tables <- susr_tables(domain = c("Housing", "Energy"))

dim(multiple_subdomain_tables)

## -----------------------------------------------------------------------------
subdomain_tables_long <- susr_tables(domain = "Housing",
                                     long = TRUE)

dim(subdomain_tables_long)

## -----------------------------------------------------------------------------
dims <- susr_dimension_values(table_code = "bv3001rr",
                              dimension_code = "bv3001rr_voda")

dims

## -----------------------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)

# 1) Get all tables in the "Energy" domain, in long format
energy_long <- susr_tables(domain = "Energy", long = TRUE)

# Inspect the columns:
# - table_code
# - dimension_code
# - plus other metadata (domain, subdomain, label, href, etc.)

# 2) Map susr_dimension_values() across each row:
#    We'll create a new list-column 'dim_info' that stores the dimension values
energy_long_dim <- energy_long |> 
  mutate(
    dim_info = map2(table_code, dimension_code, ~ susr_dimension_values(.x, .y))
  )

# Now `energy_long_dim$dim_info` is a list of tibbles/data frames (one for each dimension).
# Each entry holds columns like:
#   dimension_code, dimension_label, dimension_note, element_code, element_label, etc.

# 3) (Optional) Unnest into a single combined data frame
#    If you'd like a tall data frame where each row is a unique dimension element:
full_dim_info <- energy_long_dim |> 
  unnest(cols = dim_info, names_sep = "dim_")

# `full_dim_info` now contains columns:
#   class, href, table_code, label, update, dimension_code, 
#   (optionally) domain/subdomain, 
#   and the un-nested dimension columns from susr_dimension_values().

head(full_dim_info)

## -----------------------------------------------------------------------------
params <- list(
  "np3106rr",
  list("SK021", c("2016","2017","2018"), "E_PRIEM_HR_MZDA", "all"),
  "as1001rs",
  list("all", "all", "all")
)

multi_res <- fetch_susr_data(params)

names(multi_res)

class(multi_res$np3106rr)

## ----out.width="100%", fig.width=12, fig.height=6-----------------------------
library(ggplot2)

p <- multi_res$as1001rs |>
  filter(as1001rs_ukaz == "Pre-productive population (aged 0 -14 years)",
         as1001rs_poh == "Total") |>
  ggplot() +
  geom_line(aes(x = as1001rs_rok, y = value, group = 1)) +
  labs(title = "Pre-productive population (aged 0 -14 years)",
       x = "Year",
       y = NULL) +
  theme_minimal()


p

