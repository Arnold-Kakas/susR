#' Retrieve all possible values for a specific dimension in a given table
#'
#' This function queries the dimension endpoint:
#' \code{https://data.statistics.sk/api/v2/dimension/<table_code>/<dimension_code>?lang=<lang>}
#' for a given table code and dimension code, returning a tibble with all possible
#' values (element codes and labels) for that dimension.
#'
#' @param table_code A string with the table code (e.g. "as1001rs").
#'   This is the 8-character (or similar) code identifying the data cube.
#' @param dimension_code A string with the dimension code (e.g. "as1001rs_rok").
#' @param lang A string specifying the language code, either \code{"en"} or \code{"sk"}.
#'   Defaults to \code{"en"}.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{dimension_code}{The provided dimension code (e.g. "as1001rs_rok").}
#'   \item{dimension_label}{A short label for the dimension (if available).}
#'   \item{dimension_note}{A more descriptive note for the dimension (if available).}
#'   \item{element_index}{Index of each element.}
#'   \item{element_value}{Each possible value code (e.g. "2016", "SK021", etc.).}
#'   \item{element_label}{A descriptive label for each element code (if available).}
#' }
#'
#' @examples
#' \dontrun{
#' # For table code "as1001rs" and dimension "as1001rs_rok":
#' dim_vals <- susr_dimension_values("as1001rs", "as1001rs_rok")
#' head(dim_vals)
#' }
#'
#' @export
susr_dimension_values <- function(table_code, dimension_code, lang = "en") {
  # Build the dimension API URL
  dim_url <- paste0(
    "https://data.statistics.sk/api/v2/dimension/",
    table_code, "/",
    dimension_code
  )

  # Perform the request with a tryCatch wrapper to catch HTTP errors gracefully
  resp <- tryCatch({
    httr2::request(dim_url) |>
      httr2::req_url_query(lang = lang) |>
      httr2::req_perform()
  }, error = function(e) {
    warning("Failed to retrieve dimension info for ", dimension_code, ": ", e$message,
            " Check the provided table and dimension codes.")
    return(NULL)
  })

  # If the request returned NULL, end the function here
  if (is.null(resp)) {
    return(NULL)
  }

  # Parse the JSON response
  parsed <- httr2::resp_body_json(resp)

  # Extract dimension metadata
  dim_label <- if (!is.null(parsed$label)) parsed$label else NA_character_
  dim_note  <- if (!is.null(parsed$note))  parsed$note  else NA_character_

  # Extract element values, indexes, and labels (if any)
  element_idx <- parsed$category$index

  # Process the returned indexes and labels
  element_index <- unlist(element_idx)
  element_values <- names(element_idx)
  element_labels <- unlist(parsed$category$label)

  dplyr::tibble(
    dimension_code  = dimension_code,
    dimension_label = dim_label,
    dimension_note  = dim_note,
    element_index   = element_index,
    element_value   = element_values,
    element_label   = element_labels
  )
}
