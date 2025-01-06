#' Get the list of all available datasets (tables) from the Slovak Statistical Office
#'
#' This function calls the SUSR open data API endpoint
#' \code{https://data.statistics.sk/api/v2/collection?lang=en} and returns
#' either a "wide" or "long" data frame (depending on \code{long}).
#'
#' If \code{long = FALSE} (default), each row corresponds to a single dataset
#' and the dimension codes are concatenated in the \code{dimension_names} column.
#'
#' If \code{long = TRUE}, the output "pivots" these dimension codes so each
#' dimension code appears in its own row (under the \code{dimension_code} column).
#'
#' @param long Logical (default \code{FALSE}). If \code{TRUE}, the result is
#'   "long" with one row per dimension code.
#'
#' @return A tibble. If \code{long = FALSE}, columns include:
#' \describe{
#'   \item{class}{The “class” of the item (usually "dataset")}
#'   \item{href}{The URL to the dataset endpoint}
#'   \item{table_code}{The 8-character table code extracted from the URL}
#'   \item{label}{A descriptive label of the dataset}
#'   \item{update}{The date of the most recent update (if present)}
#'   \item{dimension_names}{A single string with dimension codes, separated by ":"}
#' }
#'
#' If \code{long = TRUE}, columns include:
#' \describe{
#'   \item{class}{As above}
#'   \item{href}{As above}
#'   \item{table_code}{As above}
#'   \item{label}{As above}
#'   \item{update}{As above}
#'   \item{dimension_code}{One dimension code per row}
#' }
#'
#' @examples
#' \dontrun{
#' # Default (wide) usage:
#' wide_tbls <- get_table_list()
#' head(wide_tbls)
#'
#' # Long usage:
#' long_tbls <- get_table_list(long = TRUE)
#' head(long_tbls)
#' }
#'
#' @export
get_table_list <- function(long = FALSE) {

  base_url <- "https://data.statistics.sk/api/v2/collection"

  # 1) Build and perform the request
  resp <- httr2::request(base_url) |>
    httr2::req_url_query(lang = "en") |>
    httr2::req_perform()

  # 2) Handle errors (e.g., HTTP 4xx or 5xx)
  if (httr2::resp_is_error(resp)) {
    stop("Failed to retrieve data from SUSR: ", httr2::resp_status_desc(resp))
  }

  # 3) Parse JSON
  parsed <- httr2::resp_body_json(resp)

  # 4) Safety check: if no items returned
  if (is.null(parsed$link$item) || length(parsed$link$item) == 0) {
    # Return an empty tibble with consistent column names
    out_empty <- dplyr::tibble(
      class = character(0),
      href = character(0),
      table_code = character(0),
      label = character(0),
      update = character(0),
      dimension_names = character(0)
    )
    if (long) {
      # For long format, dimension_code instead of dimension_names
      out_empty$dimension_code <- character(0)
      out_empty$dimension_names <- NULL  # remove dimension_names column
    }
    return(out_empty)
  }

  items <- parsed$link$item

  # 5) Flatten the response into a "wide" tibble first
  out <- purrr::map_dfr(items, function(x) {
    dims <- x$dimension

    # Combine the dimension codes into a single string separated by ":"
    dim_names <- if (!is.null(dims) && length(dims) > 0) {
      paste(names(dims), collapse = ":")
    } else {
      NA_character_
    }

    # Extract table_code from x$href
    # Example: "https://data.statistics.sk/api/v2/dataset/as1001rs/as1001rs_rok?lang=en"
    href_no_leading <- sub("^.*?/dataset/", "", x$href)   # remove up to "/dataset/"
    table_code      <- sub("/.*$", "", href_no_leading)  # remove everything after first slash
    table_code      <- sub("\\?.*$", "", table_code)      # remove query parameters

    dplyr::tibble(
      class = x$class,
      href = x$href,
      table_code = table_code,
      label = x$label,
      update = x$update,
      dimension_names = dim_names
    )
  })

  # If user wants "wide" format, return as is
  if (!long) {
    return(out)
  }

  # Otherwise, pivot to "long" format:
  # - Split dimension_names by ":"
  # - Each dimension code gets its own row in a new column dimension_code

  long_out <- dplyr::bind_rows(
    lapply(seq_len(nrow(out)), function(i) {
      dims_i <- out$dimension_names[i]
      # If NA or empty, return a single row with dimension_code = NA
      if (is.na(dims_i) || !nzchar(dims_i)) {
        return(dplyr::tibble(
          class = out$class[i],
          href = out$href[i],
          table_code = out$table_code[i],
          label = out$label[i],
          update = out$update[i],
          dimension_code = NA_character_
        ))
      }
      # Otherwise, split by ":" and create multiple rows
      dim_vec <- unlist(strsplit(dims_i, ":", fixed = TRUE))

      dplyr::tibble(
        class = out$class[i],
        href = out$href[i],
        table_code = out$table_code[i],
        label = out$label[i],
        update = out$update[i],
        dimension_code = dim_vec
      )
    })
  )

  return(long_out)
}
