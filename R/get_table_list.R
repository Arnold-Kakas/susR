#' Get the list of all available datasets (tables) from the Slovak Statistical Office
#'
#' This function calls the SUSR open data API endpoint
#' \code{https://data.statistics.sk/api/v2/collection?lang=en} and returns
#' a flat data frame with essential metadata for each available dataset.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \strong{class}: The “class” of the item (usually "dataset").
#'   \item \strong{href}: A URL to the dataset endpoint.
#'   \item \strong{table_code}: The eight-digit table code extracted from the URL.
#'   \item \strong{label}: A descriptive label of the dataset.
#'   \item \strong{update}: The date of the most recent update (if present).
#'   \item \strong{dimension_names}: A semicolon-separated string of dimension identifiers.
#' }
#'
#' @examples
#' \dontrun{
#'   tbls <- get_table_list()
#'   head(tbls)
#'
#'   # Then pass table_code to get_dimensions():
#'   get_dimensions(tbls$table_code[1])
#' }
#' @export
get_table_list <- function() {

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
    return(dplyr::tibble(
      class = character(0),
      href = character(0),
      table_code = character(0),
      label = character(0),
      update = character(0),
      dimension_names = character(0)
    ))
  }

  # 5) Flatten the response into a tibble/data frame
  items <- parsed$link$item

  out <- purrr::map_dfr(items, function(x) {
    dims <- x$dimension

    # Combine the dimension names into a single string (e.g. "as1001rs_rok; as1001rs_ukaz")
    dim_names <- if (!is.null(dims) && length(dims) > 0) {
      paste(names(dims), collapse = "; ")
    } else {
      NA_character_
    }

    # Parse out the table code from x$href
    # Example href:
    #   "https://data.statistics.sk/api/v2/dataset/as1001rs/as1001rs_rok?lang=en"
    # We'll strip off everything up to and including "/dataset/", then take
    # the substring up to the first slash.
    # That gives us "as1001rs/as1001rs_rok?lang=en" => "as1001rs"

    href_no_leading <- sub("^.*?/dataset/", "", x$href)  # remove up to "/dataset/"
    table_code       <- sub("/.*$", "", href_no_leading) # remove everything after first slash
    table_code       <- sub("\\?.*$", "", table_code)    # if there's a ?lang=..., remove that too

    dplyr::tibble(
      class = x$class,
      href = x$href,
      table_code = table_code,
      label = x$label,
      update = x$update,
      dimension_names = dim_names
    )
  })

  return(out)
}
