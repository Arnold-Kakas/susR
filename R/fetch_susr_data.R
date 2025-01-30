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
#' @param geocode Binary TRUE/FALSE. If TRUE, result will contain geometry. Default is FALSE.
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
#' @importFrom rjstat fromJSONstat
#' @export
fetch_susr_data <- function(
    params,
    geocode = FALSE,
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
    table_code <- params[[i]]
      if (!is.character(table_code) || length(table_code) != 1 || nchar(table_code) != 8) {
        warning("In `params`, each table code must be a single 8-character string. Problem at position ", i)
        return(NULL)  # We'll return NULL here, so this table is skipped
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
    table_info_long <- if (is.null(susr_tables(long = TRUE, table_codes = table_code))) {
      warning("Failed to retrieve or parse table metadata for table_code='", table_code,"'. Skipping.")
      return(NULL)
    } else {
      susr_tables(long = TRUE, table_codes = table_code)
    }



    # The distinct dimension codes in the table's metadata:
    tbl_dim_codes <- unique(table_info_long$dimension_code)
    # Check if user-supplied dimension specs have the same length
    # For instance, if the table has 4 dimension codes, we expect 4 dimension segments.
    if (length(dim_specs) != length(tbl_dim_codes)) {
      warning("You provided ", length(dim_specs), " dimension segments for table_code='", table_code,
              "', but the table metadata shows ", length(tbl_dim_codes), " dimension(s).",
              "\nDimension names in metadata: ", paste(tbl_dim_codes, collapse = ", "))
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
    # Perform the API request via httr2
    #----------------------------------------------------------------
    resp <- tryCatch({
      httr2::request(full_url) |> httr2::req_perform()
    }, error = function(e) {
      # If there's a network issue, DNS error, etc., we catch it here
      warning("Failed to retrieve data for table_code='", table_code,
              "'. URL: ", full_url)
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

    if (geocode) {
      reg_col <- names(df)[grep("vuc|obc|nuts|kraj|vuc|obec|kra", names(df))]

      regions <- susr_dimension_values(table_code,
                                       reg_col) |>
        dplyr::select(dimension_code, code = element_value, element_label) |>
        dplyr::mutate(code = dplyr::if_else(grepl("obc", dimension_code),
                                                     substr(code, nchar(code) - 5, nchar(code)),
                                                     code)) |>
        dplyr::select(-dimension_code)

      df <- df |>
        dplyr::left_join(regions, by = dplyr::join_by(!!reg_col == element_label), keep = FALSE) |>
        dplyr::left_join(regions_geometry |> dplyr::select(-name) |> sf::st_as_sf(), by = "code", keep = FALSE)
    }

    # Store the tibble in our results list, keyed by table_code
    results[[table_code]] <- df

  }

  # Return the final list of data frames
  results
}

