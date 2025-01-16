#' Load manual table list (with domain & subdomain) from CSV
#'
#' This function reads the static CSV file containing the manual list
#' of tables along with domain and subdomain information.
#'
#' @return A tibble (or data frame) with columns like:
#'   \itemize{
#'     \item \code{table_code}
#'     \item \code{domain}
#'     \item \code{subdomain}
#'     \item ... (others you may have)
#'   }
#' @examples
#' \dontrun{
#'   df <- susr_domains()
#'   head(df)
#' }
#' @export
susr_domains <- function() {

  result <- tryCatch({
    # Build path to CSV
    csv_path <- system.file("extdata", "domains.csv", package = "susR")

    # Use readr to read the CSV and dplyr to drop the unnecessary column
    df <- readr::read_delim(csv_path, show_col_types = FALSE, delim = ";") |>
      dplyr::select(-domain_subdomain)

    df
  }, error = function(e) {
    # Print a message and return NULL if an error occurs
    message("CSV file not found. Please re-install or contact package authors.")
  })

  result
}
