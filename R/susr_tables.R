#' Get the list of all available datasets (tables) from the Slovak Statistical Office
#'
#' This function calls the SUSR open data API endpoint
#' \code{https://data.statistics.sk/api/v2/collection?lang=en} to obtain a list
#' of all datasets, optionally filters them by domain/subdomain and/or
#' table code, and returns the results in a tibble.
#'
#' @param long Logical (default \code{FALSE}). If \code{TRUE}, the result is
#'   "long" with one row per dimension code (see Details).
#' @param domains Character vector or NULL (default). If provided, we filter
#'   tables to only those whose \code{domain} OR \code{subdomain} matches
#'   any of these values. Domain info is taken from the static CSV loaded via
#'   [susr_domains()].
#' @param table_codes Character vector or NULL (default). If provided, we filter
#'   to only these table codes.
#'
#' @details
#' If \code{long = FALSE} (default), each row corresponds to a single dataset
#' and the dimension codes are concatenated in the \code{dimension_names} column,
#' separated by \code{":"}.
#'
#' If \code{long = TRUE}, the function pivots such that each dimension code is
#' in its own row (under \code{dimension_code}).
#'
#' If \code{domains} is not NULL, this function calls [susr_domains()] to retrieve
#' your static CSV with domain and subdomain data, joins it on \code{table_code},
#' and filters accordingly.
#'
#' If \code{table_codes} is not NULL, we filter the final list to only those codes.
#'
#' @return A tibble. The columns differ slightly depending on \code{long} (wide vs. long).
#'   - Always contains: \code{class}, \code{href}, \code{table_code}, \code{label}, \code{update}
#'   - If \code{long = FALSE}: also \code{dimension_names}
#'   - If \code{long = TRUE}: also \code{dimension_code}
#'   - If \code{domain} was used, columns \code{domain} / \code{subdomain} from \code{susr_domains()} are also included.
#'
#' @examples
#' \dontrun{
#' # 1) Get all tables (wide format, no filtering)
#' tables_all <- susr_tables()
#'
#' # 2) Filter to just certain table codes:
#' tables_some <- susr_tables(table_code = c("as1001rs", "as1002rs"))
#'
#' # 3) Filter by domain or subdomain:
#' #    e.g., if your CSV has domain = "Environment", "Macroeconomic statistics", etc.
#' env_tables <- susr_tables(domain = "Environment")
#'
#' # 4) Combined: filter by domain + get long format
#' macroeconomic_tables <- susr_tables(long = TRUE, domain = "Macroeconomic statistics")
#' }
#' @export
susr_tables <- function(long = FALSE,
                           domains = NULL,
                           table_codes = NULL) {
  base_url <- "https://data.statistics.sk/api/v2/collection"

  # 1) Perform the request to the API
  resp <- httr2::request(base_url) |>
    httr2::req_url_query(lang = "en") |>
    httr2::req_perform()

  if (httr2::resp_is_error(resp)) {
    stop("Failed to retrieve data from SUSR: ", httr2::resp_status_desc(resp))
  }

  # 2) Parse JSON
  parsed <- httr2::resp_body_json(resp)

  # 3) Safety check: if no items returned
  if (is.null(parsed$link$item) || length(parsed$link$item) == 0) {
    out_empty <- dplyr::tibble(
      class = character(0),
      href = character(0),
      table_code = character(0),
      label = character(0),
      update = character(0),
      dimension_names = character(0)
    )
    if (long) {
      out_empty$dimension_code <- character(0)
      out_empty$dimension_names <- NULL
    }
    # if we plan to add domain info columns in the future, we'd do so here
    return(out_empty)
  }

  items <- parsed$link$item

  # 4) Build the 'wide' tibble first
  tbl_wide <- purrr::map_dfr(items, function(x) {
    dims <- x$dimension

    # Combine dimension codes with ':'
    dim_names <- if (!is.null(dims) && length(dims) > 0) {
      paste(names(dims), collapse = ":")
    } else {
      NA_character_
    }

    # Extract table_code from x$href
    href_no_leading <- sub("^.*?/dataset/", "", x$href)
    code <- sub("/.*$", "", href_no_leading)
    code <- sub("\\?.*$", "", code)

    dplyr::tibble(
      class = x$class,
      href = x$href,
      table_code = code,
      label = x$label,
      update = x$update,
      dimension_names = dim_names
    )
  })

  # 5) If user provided domain(s), we join with the static domain data
  if (!is.null(domains)) {
    # susr_domains() must exist in your package, returning e.g.:
    #   table_code, domain, subdomain
    domain_df <- susr_domains()

    # Left join so we add domain/subdomain columns
    tbl_wide <- dplyr::left_join(tbl_wide, domain_df, by = "table_code")

    # Filter rows: keep only if domain or subdomain matches any of user param
    # ( domain can be a vector, e.g. c("Education","Population") )
    # If there's no match for domain/subdomain, row is dropped.
    tbl_wide <- dplyr::filter(
      tbl_wide,
      .data$domain %in% domains | .data$subdomain %in% domains
    )
  }

  # 6) If user provided table_code(s), filter
  if (!is.null(table_codes)) {
    tbl_wide <- dplyr::filter(tbl_wide, .data$table_code %in% table_codes)
  }

  # 7) If user wants 'long' format, pivot dimension_names
  if (!long) {
    return(tbl_wide)
  }

  # Pivot to "long": one dimension_code per row
  tbl_long <- dplyr::bind_rows(
    lapply(seq_len(nrow(tbl_wide)), function(i) {
      dnames <- tbl_wide$dimension_names[i]
      if (is.na(dnames) || !nzchar(dnames)) {
        # No dimension codes
        return(dplyr::tibble(
          class = tbl_wide$class[i],
          href = tbl_wide$href[i],
          table_code = tbl_wide$table_code[i],
          label = tbl_wide$label[i],
          update = tbl_wide$update[i],
          dimension_code = NA_character_,
          # If domain/subdomain columns exist, carry them over
          domain = tbl_wide$domain[i],
          subdomain = tbl_wide$subdomain[i]
        ))
      }
      # Split the dimension names by ':'
      dim_vec <- strsplit(dnames, ":", fixed = TRUE)[[1]]

      # Build multiple rows
      out_i <- dplyr::tibble(
        class = tbl_wide$class[i],
        href = tbl_wide$href[i],
        table_code = tbl_wide$table_code[i],
        label = tbl_wide$label[i],
        update = tbl_wide$update[i],
        dimension_code = dim_vec
      )
      # If domain columns exist, add them
      if ("domain" %in% names(tbl_wide)) {
        out_i$domain <- tbl_wide$domain[i]
      }
      if ("subdomain" %in% names(tbl_wide)) {
        out_i$subdomain <- tbl_wide$subdomain[i]
      }
      out_i
    })
  )

  tbl_long
}
