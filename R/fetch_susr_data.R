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
#'             \item Special keywords like \code{"all"}, \code{"last5"}, \code{"LAU1"}, etc.
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
#'   list("districts", "last5", "all")
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
#' **Error & Warning Handling**
#' We use `tryCatch()` blocks around both network calls and JSON parsing.
#' While we know some specific issues that could occur (like an invalid URL, or
#' invalid JSON structure), there may be other rare/unexpected problems.
#' If an error occurs, we issue a warning and store `NULL` for that table.
#'
#' **Dimension Count Check**
#' Before constructing the URL, we call [susr_tables()] (in "long" format) to retrieve
#' a list of the dimension codes for the table. We then check if the user-supplied
#' dimension specs have the same length as the **number of distinct dimension_code**.
#' If not, we warn the user. (This ensures at least the total dimension count is valid.)
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
    warning("`params` must be in pairs: (table_code, dimension_list) repeated.\n",
            "You provided an odd number of elements.")
    return(NULL)
  }

  # Prepare our output as an empty list that we'll fill
  results <- list()

  # We'll loop over pairs: 1,3,5,... in 'params'
  idx <- seq(1, n_par, by = 2)

  for (i in idx) {
    #----------------------------------------------------------------
    # Safely retrieve the table_code and check it's a valid string
    #----------------------------------------------------------------
    valid_table <- tryCatch({
      table_code <- params[[i]]
      if (!is.character(table_code) || length(table_code) != 1 || nchar(table_code) != 8) {
        warning("In `params`, each table code must be a single 8-character string. Problem at position ", i)
        return(NULL)  # We'll return NULL here, so this table is skipped
      }
      TRUE
    }, error = function(e) {
      # If anything goes unexpectedly wrong, we warn and skip
      warning("Error while checking table_code at position ", i, ": ", e$message)
      return(FALSE)
    })
    if (!isTRUE(valid_table)) {
      # If the check failed, skip this table and move on
      next
    }

    # Extract the table code and dimension specs
    table_code <- params[[i]]
    dim_specs  <- params[[i + 1]]

    #----------------------------------------------------------------
    #  Check the dimension COUNT by calling susr_tables(long=TRUE)
    #----------------------------------------------------------------
    # Retrieve the metadata for this table and count how many
    # distinct dimensions it has. This helps ensure the user didn't pass
    # the wrong number of segments for the table in question.
    table_info_long <- tryCatch({
      susr_tables(long = TRUE, table_codes = table_code)
    }, error = function(e) {
      warning("Failed to retrieve or parse table metadata for table_code='", table_code,
              "'. Skipping. Error was: ", e$message)
      return(NULL)
    })
    # The distinct dimension codes in the table's metadata:
    tbl_dim_codes <- unique(table_info_long$dimension_code)
    # Check if user-supplied dimension specs have the same length
    # For instance, if the table has 4 dimension codes, we expect 4 dimension segments.
    if (length(dim_specs) != length(tbl_dim_codes)) {
      warning("You provided ", length(dim_specs), " dimension segments for table_code='", table_code,
              "', but the table metadata shows ", length(tbl_dim_codes), " dimension(s).",
              "\nDimension names in metadata: ", paste(tbl_dim_codes, collapse=", "))
      return(NULL)
    }

    #----------------------------------------------------------------
    #  Resolve each dimension segment with helper function(s)
    #----------------------------------------------------------------
    dim_path <- resolve_href(
      table_code     = table_code,
      param_list     = dim_specs,
      table_info_long = table_info_long,
      lang           = lang
    )

    #----------------------------------------------------------------
    # Construct the full URL for the dataset call
    #----------------------------------------------------------------
    full_url <- paste0(base_url, "/",
                       table_code, "/",
                       dim_path, "?lang=",
                       lang, "&type=json")

    #----------------------------------------------------------------
    # Perform the API request via httr2 within tryCatch
    #----------------------------------------------------------------
    resp <- tryCatch({
      httr2::request(full_url) |> httr2::req_perform()
    }, error = function(e) {
      # If there's a network issue, DNS error, etc., we catch it here
      warning("Failed to retrieve data for table_code='", table_code,
              "'. URL: ", full_url, "\nError: ", e$message)
      return(NULL)
    })
    if (is.null(resp)) {
      # If we can't get a response, store NULL for this table
      results[[table_code]] <- NULL
      next
    }

    #----------------------------------------------------------------
    # Extract the response body (raw JSON) and parse with rjstat
    #----------------------------------------------------------------
    raw_json_text <- httr2::resp_body_string(resp)

    # Try to parse via rjstat. If there's something invalid in the JSON,
    # or some unexpected structure, this catch will handle it.
    df_list <- tryCatch({
      rjstat::fromJSONstat(raw_json_text)
    }, error = function(e) {
      warning("rjstat::fromJSONstat() failed for table_code='", table_code,
              "' with the given JSON. Storing NULL.")
      return(NULL)
    })
    if (is.null(df_list)) {
      # If parsing failed, skip
      results[[table_code]] <- NULL
      next
    }

    # Convert the parsed object to a tibble
    df <- tibble::as_tibble(df_list)

    # Store the tibble in our results list, keyed by table_code
    results[[table_code]] <- df

  }

  # Return the final list of data frames
  results
}
