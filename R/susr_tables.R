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
#' @param lang The language code. Defaults to \code{"en"}. Can also be \code{"sk"}.

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
#'   - If \code{domains} was used, columns \code{domain} / \code{subdomain} from [susr_domains()] are also included.
#'
#' @examples
#' \dontrun{
#' # 1) Get all tables (wide format, no filtering)
#' tables_all <- susr_tables()
#'
#' # 2) Filter to just certain table codes:
#' tables_some <- susr_tables(table_codes = c("as1001rs", "as1002rs"))
#'
#' # 3) Filter by domain or subdomain:
#' env_tables <- susr_tables(domains = "Environment")
#'
#' # 4) Combined: filter by domain + get long format
#' macroeconomic_tables <- susr_tables(long = TRUE, domains = "Macroeconomic statistics")
#' }
#'
#' @export
susr_tables <- function(long = FALSE,
                        domains = NULL,
                        table_codes = NULL,
                        lang = "en") {
  base_url <- "https://data.statistics.sk/api/v2/collection"

  # 1) Perform the API request with error handling
  resp <- httr2::request(base_url) |>
      httr2::req_url_query(lang = lang) |>
      httr2::req_perform()

  if (httr2::resp_is_error(resp)) {
    warning("Failed to retrieve data from SUSR: ", httr2::resp_status_desc(resp))
    return(NULL)
  }

  # 2) Extract raw JSON text and parse with jsonlite::fromJSON() to preserve JSON-stat structure
  raw_text <- httr2::resp_body_string(resp)
  parsed <- tryCatch({
    jsonlite::fromJSON(raw_text, simplifyVector = FALSE)
  }, error = function(e) {
    warning("Failed to parse JSON response from SUSR API: ", e$message)
    return(NULL)
  })

  # 3) Identify items: try using "item" first; if not available, try "items"
  items <- if (!is.null(parsed$link$item)) {
    parsed$link$item
  } else if (!is.null(parsed$link$items)) {
    parsed$link$items
  } else {
    warning("No datasets found in the SUSR API response.")
    items <- list()
  }

  # 5) Build the wide tibble from the items
  tbl_wide <- purrr::map_dfr(items, function(x) {
    dims <- x$dimension

    # Combine dimension codes with ':' if present
    dim_names <- tryCatch({
      if (!is.null(dims) && length(dims) > 0) {
        paste(names(dims), collapse = ":")
      } else {
        NA_character_
      }
    }, error = function(e) {
      NA_character_
    })

    # Extract table code from x$href with robust error handling
    code <- tryCatch({
      href_no_leading <- sub("^.*?/dataset/", "", x$href)
      code <- sub("/.*$", "", href_no_leading)
      sub("\\?.*$", "", code)
    }, error = function(e) {
      NA_character_
    })

    dplyr::tibble(
      class = if (!is.null(x$class)) x$class else NA_character_,
      href = if (!is.null(x$href)) x$href else NA_character_,
      table_code = code,
      label = if (!is.null(x$label)) x$label else NA_character_,
      update = if (!is.null(x$update)) x$update else NA_character_,
      dimension_names = dim_names
    )
  })

  # 6) If user provided domains, join with susr_domains() and filter rows accordingly.
  if (!is.null(domains)) {
    domain_df <- susr_domains()

    tbl_wide <- dplyr::left_join(tbl_wide, domain_df, by = "table_code")
    tbl_wide <- dplyr::filter(
      tbl_wide,
      (.data$domain %in% domains) | (.data$subdomain %in% domains)
    )
  }

  # 7) If user provided table_codes, filter accordingly.
  if (!is.null(table_codes)) {
    tbl_wide <- dplyr::filter(tbl_wide, .data$table_code %in% table_codes)
  }

  if (nrow(tbl_wide) == 0) {
    warning("Incorrect table code provided")
    return(NULL)
  }


  # 8) If long format is not requested, return the wide tibble.
  if (!long) {
    return(tbl_wide)
  }

  # 9) Pivot to "long" format: one row per dimension_code.
  tbl_long <- dplyr::bind_rows(
    lapply(seq_len(nrow(tbl_wide)), function(i) {
      dnames <- tbl_wide$dimension_names[i]

      dim_vec <- strsplit(dnames, ":", fixed = TRUE)[[1]]
      out_i <- dplyr::tibble(
        class = tbl_wide$class[i],
        href = tbl_wide$href[i],
        table_code = tbl_wide$table_code[i],
        label = tbl_wide$label[i],
        update = tbl_wide$update[i],
        dimension_code = dim_vec
      )
      if ("domain" %in% names(tbl_wide)) {
        out_i$domain <- tbl_wide$domain[i]
      }
      if ("subdomain" %in% names(tbl_wide)) {
        out_i$subdomain <- tbl_wide$subdomain[i]
      }
      out_i
    })
  )

  return(tbl_long)
}
