#' Retrieve data from SUSR (Slovak Statistical Office) open data API as data frames
#'
#' This function constructs and calls the dataset endpoint for one or more tables,
#' including their dimension selections, and returns each as a data frame (tibble).
#' Internally, it fetches JSON-stat and parses it using the \pkg{rjstat} package.
#'
#' @param params A list structured in **pairs**:
#'   \enumerate{
#'     \item A character string with the table code (e.g. "np3106rr").
#'     \item A list (or vector) of dimension "segments" in the order they should
#'           appear in the URL. Each segment can be:
#'           \itemize{
#'             \item A character scalar (e.g. "SK021"), or
#'             \item A character vector (e.g. \code{c("2016","2017","2018")})
#'                   which we join by commas (\code{"2016,2017,2018"}),
#'             \item Special keywords like \code{"all"}, \code{"last5"}, etc.
#'                   (passed as-is).
#'           }
#'   }
#'   For multiple tables, just keep repeating those pairs in the same list.
#'
#' @param lang The language code. Defaults to \code{"en"}. Can also be \code{"sk"}.
#' @param base_url The base SUSR dataset endpoint. Defaults to
#'   \code{"https://data.statistics.sk/api/v2/dataset"}.
#'
#' @details
#' The **order** of dimension segments is crucial. For example, if the
#' official URL pattern is:
#' \preformatted{
#'   https://data.statistics.sk/api/v2/dataset/<table_code>/<param1>/<param2>/<param3>?lang=en&type=json
#' }
#' then pass \code{list("param1", "param2", "param3")} in that *exact* order.
#'
#' Internally, each JSON-stat response is converted to a data frame (tibble)
#' using \code{rjstat::fromJSONstat()}. If the JSON structure is invalid or
#' the API call fails, we store \code{NULL} and emit a warning for that table.
#'
#' @return A *named list of data frames*, keyed by the table codes. If you request
#'   data for two tables (e.g. "np3106rr" and "as1001rs"), you get a list of two
#'   tibbles. For example:
#'   \preformatted{
#'     $np3106rr
#'       # A tibble with columns for the dimension categories and the measured value
#'
#'     $as1001rs
#'       # Another tibble for that table
#'   }
#'
#' @examples
#' \dontrun{
#' # Example: retrieve data from two tables
#' params <- list(
#'   "np3106rr",
#'   list("SK021", c("2016","2017","2018"), "E_PRIEM_HR_MZDA", "7"),
#'   "as1001rs",
#'   list("all", "all", "all")  # e.g. get all years, all other dims
#' )
#'
#' res <- fetch_susr_data(params, lang = "en")
#' names(res)
#' #> [1] "np3106rr" "as1001rs"
#'
#' # Each element is a data frame. For example:
#' head(res[["np3106rr"]])
#' }
#'
#' @importFrom rjstat fromJSONstat
#' @export
fetch_susr_data <- function(
    params,
    lang     = "en",
    base_url = "https://data.statistics.sk/api/v2/dataset"
) {
  # We expect `params` in pairs: (table_code, dimension_list)
  n_par <- length(params)
  if (n_par %% 2 != 0) {
    stop(
      "`params` must be in pairs: (table_code, dimension_list) repeated.\n",
      "You provided an odd number of elements."
    )
  }

  results <- list()    # Will hold one data frame per table code
  idx <- seq(1, n_par, by = 2)

  for (i in idx) {
    table_code <- params[[i]]
    dim_specs  <- params[[i + 1]]  # the dimension segments

    # Check table_code is a single character
    if (!is.character(table_code) || length(table_code) != 1) {
      stop(
        "In `params`, each table code must be a single character string.\n",
        "Problem at position ", i
      )
    }

    # Build the dimension path, joining multiple values with commas
    dimension_parts <- sapply(dim_specs, function(x) {
      if (length(x) > 1) {
        paste(x, collapse = ",")  # e.g. c("2016","2017","2018") -> "2016,2017,2018"
      } else {
        as.character(x)
      }
    })
    dimension_path <- paste(dimension_parts, collapse = "/")

    # Final URL
    # e.g. https://data.statistics.sk/api/v2/dataset/np3106rr/SK021/2016,2017,2018/E_PRIEM_HR_MZDA/7?lang=en&type=json
    full_url <- paste0(
      base_url, "/", table_code, "/", dimension_path,
      "?lang=", lang,
      "&type=json"
    )

    # Perform the request
    resp <- httr2::request(full_url) |>
      httr2::req_perform()

    # Check if the response is an error
    if (httr2::resp_is_error(resp)) {
      warning(
        "Failed to retrieve data for table_code='", table_code, "'.\n",
        "URL: ", full_url, "\n",
        "Status: ", httr2::resp_status_desc(resp)
      )
      results[[table_code]] <- NULL
      next
    }

    # Extract raw JSON text
    raw_json_text <- httr2::resp_body_string(resp)

    # Parse using rjstat::fromJSONstat()
    # This typically returns a list of data frames if there's more than one dataset
    df_list <- try(rjstat::fromJSONstat(raw_json_text), silent = TRUE)

    if (inherits(df_list, "try-error") || !is.list(df_list)) {
      # If parsing fails, store NULL
      warning(
        "rjstat::fromJSONstat() failed for table_code='", table_code, "'\n",
        "with the given JSON. Storing NULL."
      )
      results[[table_code]] <- NULL
      next
    }

    df <- df_list

    # Ensure it's a tibble
    df <- tibble::as_tibble(df)

    # Store in results
    results[[table_code]] <- df
  }

  results
}
