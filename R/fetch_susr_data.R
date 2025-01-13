#' Retrieve data from SUSR (Slovak Statistical Office) open data API as data frames
#'
#' This function constructs and calls the dataset endpoint for one or more tables,
#' including their dimension selections, and returns each as a data frame (tibble).
#' Internally, it fetches JSON-stat and parses it using the \pkg{rjstat} package.
#'
#' @param params A list structured in **pairs**:
#'   \enumerate{
#'     \item A character string with the table code (e.g. "np3106rs").
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
#' the API call fails, the corresponding table is stored as \code{NULL} (with a warning).
#'
#' @return A *named list of data frames*, keyed by the table codes.
#'
#' @examples
#' \dontrun{
#' # Example: retrieve data from two tables
#' params <- list(
#'   "np3106rs",
#'   list("SK021", c("2016","2017","2018"), "E_PRIEM_HR_MZDA", "7"),
#'   "as1001rs",
#'   list("all", "all", "all")
#' )
#'
#' res <- fetch_susr_data(params, lang = "en")
#' names(res)
#' #> [1] "np3106rs" "as1001rs"
#'
#' # Each element is a data frame. For example:
#' head(res[["np3106rs"]])
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
    stop("`params` must be in pairs: (table_code, dimension_list) repeated.\n",
         "You provided an odd number of elements.")
  }

  results <- list()    # Will hold one data frame per table code
  idx <- seq(1, n_par, by = 2)

  for (i in idx) {
    # Wrap the table_code check in tryCatch
    valid_table <- tryCatch({
      table_code <- params[[i]]
      if (!is.character(table_code) || length(table_code) != 1) {
        stop("In `params`, each table code must be a single character string. Problem at position ", i)
      }
      TRUE
    }, error = function(e) {
      warning(e$message)
      return(FALSE)
    })
    if (!valid_table) next

    # Retrieve dimension segments
    dim_specs <- params[[i + 1]]

    # Build the dimension path: join multiple values with commas
    dimension_parts <- sapply(dim_specs, function(x) {
      if (length(x) > 1) {
        paste(x, collapse = ",")
      } else {
        as.character(x)
      }
    })
    dimension_path <- paste(dimension_parts, collapse = "/")

    # Construct the full URL
    full_url <- paste0(
      base_url, "/", params[[i]], "/", dimension_path,
      "?lang=", lang,
      "&type=json"
    )

    # Perform the API request using tryCatch to catch HTTP errors
    resp <- tryCatch({
      httr2::request(full_url) |> httr2::req_perform()
    }, error = function(e) {
      warning("Failed to retrieve data for table_code='", params[[i]],
              "'.\nURL: ", full_url, "\nError: ", e$message)
      return(NULL)
    })
    if (is.null(resp)) {
      results[[params[[i]]]] <- NULL
      next
    }

    # Extract raw JSON text
    raw_json_text <- httr2::resp_body_string(resp)

    # Parse the JSON using rjstat::fromJSONstat()
    df_list <- tryCatch({
      rjstat::fromJSONstat(raw_json_text)
    }, error = function(e) {
      warning("rjstat::fromJSONstat() failed for table_code='", params[[i]],
              "' with the given JSON. Storing NULL.")
      results[[params[[i]]]] <- NULL
      return(NULL)  # Return NULL to signal error
    })
    if (is.null(df_list)) {
      next  # skip to the next iteration if parsing failed
    }


    df <- df_list
    # Ensure the result is a tibble
    df <- tibble::as_tibble(df)

    # Store the result in our list keyed by the table code
    results[[params[[i]]]] <- df
  }

  results
}
