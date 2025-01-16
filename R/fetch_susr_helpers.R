#' Resolve HREF path for a given table and user-supplied dimension parameters
#'
#' This function takes:
#' - A table code (e.g. "np3106rr"),
#' - A list of parameters (one for each dimension),
#' - The `table_info_long` metadata (with dimension codes in the needed order),
#' - (Optionally) a language code (default "en").
#'
#' It then:
#' 1) Expands special keywords, such as "last5" or "districts",
#' 2) Checks them against the actual available dimension values via `susr_dimension_values()`,
#' 3) Discards any invalid codes, warns the user,
#' 4) If no valid codes remain, attempts to use "all" if available, otherwise
#'    the first available value for that dimension, and warns the user.
#'
#' Finally, it returns a single string of the form:
#'   "VAL11,VAL12/.../VALN1,VALN2"
#' for use in the dataset endpoint call.
#'
#' @param table_code Character: an 8-character code for the table, e.g. "np3106rr"
#' @param param_list List: user-supplied dimension selections, in the order of the table's dimensions
#' @param table_info_long A data frame of metadata (long format) for the given table
#' @param lang Character: the language code ("en" or "sk"), default "en"
#'
#' @importFrom utils tail
#'
#' @return Character string suitable for appending after the table code in the SUSR dataset endpoint.
#' @export

# nocov start
resolve_href <- function(
    table_code,
    param_list,
    table_info_long,
    lang = "en"
) {
  # -----------------------------------------------------------------------
  # 1) Get the dimension codes in the correct order
  #    (table_info_long might have repeated rows per dimension -> use unique)
  # -----------------------------------------------------------------------
  # Ensure we preserve the order they appear in table_info_long
  # One way is to do something like this:
  dimension_codes <- unique(table_info_long$dimension_code)

  n_dims <- length(dimension_codes)

  # -----------------------------------------------------------------------
  # Build up a character vector of dimension parts (one per dimension).
  # -----------------------------------------------------------------------
  dimension_parts <- vector("character", n_dims)

  # -----------------------------------------------------------------------
  # Helper function(s) to handle expansions
  # -----------------------------------------------------------------------

  expand_last_x <- function(segment_str, dimension_code) {
    # segment_str like "last5"
    x <- as.integer(tolower(gsub("last", "", segment_str)))

    # e.g. call susr_dimension_values to get all time codes
    all_vals <- susr_dimension_values(table_code, dimension_code, lang = lang)
    # Suppose these are sorted ascending. If not, you may want to sort them.
    # Take the tail
    if (nrow(all_vals) > 0) {
      last_codes <- tail(sort(all_vals$element_value), x)
      return(last_codes)
    } else {
      return(character(0))
    }
  }

  is_region_keyword <- function(segment_str) {
    # vector of supported shortcuts
    region_keywords <- c(
      "district", "districts", "nuts4", "nuts 4", "lau1", "lau 1", "okres", "okresy",
      "nuts2", "nuts 2",
      "country", "sr", "slovakia", "slovak republic", "slovensko", "slovenska republika",
      "city", "cities", "town", "towns", "mesto", "mesta",
      "bratislava", "capital", "cap", ".cap", "capital city", "cap city", "cap. city", "hlavne mesto", "hlm", "hl.m.", "hl.m", "hlm."
    )
    tolower(segment_str) %in% region_keywords
  }

  expand_region_keyword <- function(segment_str, dimension_code) {
    keyword <- tolower(segment_str)
    all_vals <- susr_dimension_values(table_code, dimension_code, lang = lang)
    if (nrow(all_vals) == 0) return(character(0))

    # By default, return all
    selected <- all_vals

    if (keyword %in% c("district", "districts", "nuts4", "nuts 4", "lau1", "lau 1", "okres", "okresy")) {
      selected <- subset(selected, grepl("District of", element_label))
    } else if (keyword == "nuts3") {
      # standard 8 NUTS3 codes
      selected <- subset(selected, element_value %in%
                           c("SK010","SK021","SK022","SK023","SK031","SK032","SK041","SK042"))
    } else if (keyword %in% c("nuts2", "nuts 2")) {
      selected <- subset(selected, element_value %in% c("SK01","SK02","SK03","SK04"))
    } else if (keyword %in% c("country", "sr", "slovakia", "slovak republic", "slovensko", "slovenska republika")) {
      selected <- subset(selected, element_value == "SK0")
    } else if (keyword  %in% c("city", "cities", "town", "towns", "mesto", "mesta")) {
      selected <- subset(selected, element_value == "M")
    } else if (keyword %in% c("bratislava", "capital", "cap", ".cap", "capital city", "cap city", "cap. city", "hlavne mesto", "hlm", "hl.m.", "hl.m", "hlm.")) {
      selected <- subset(selected, element_value == "SK_CAP")
    }

    selected$element_value
  }

  # A small function that expands single segment (like you had in resolve_segment())
  expand_single_segment <- function(segment_str, dimension_code) {
    # If it's "lastX" pattern
    if (grepl("^last\\d+$", segment_str, ignore.case = TRUE)) {
      return(expand_last_x(segment_str, dimension_code))
    }
    # If it's a recognized region keyword
    if (is_region_keyword(segment_str)) {
      return(expand_region_keyword(segment_str, dimension_code))
    }
    # Otherwise, return it literally (one code)
    return(segment_str)
  }

  # -----------------------------------------------------------------------
  # 2) For each dimension, figure out the final set of codes
  # -----------------------------------------------------------------------
  for (i in seq_len(n_dims)) {
    dim_code    <- dimension_codes[i]         # e.g. "np3106rs_rok"
    user_input  <- param_list[[i]]            # could be vector or single

    if (is.null(user_input)) {
      # If user didn't supply anything for this dimension, let's default to "all"
      user_input <- "all"
    }

    # Step A: expand if user_input is multiple codes or single code
    if (length(user_input) > 1) {
      # multiple codes already provided, treat them as final expansions
      expanded_codes <- user_input
    } else {
      # single character -> check expansions
      expanded_codes <- expand_single_segment(as.character(user_input), dim_code)

      # If user typed e.g. "all", that won't expand further, so it's just "all".
      # We'll handle the "all" logic after we see which codes are actually valid.
      #
      # Similarly, "last5" or region keywords become multiple codes here.
      #
      # Or a normal string "SK021" remains as such.
    }

    # Step B: Check which of these expanded codes are actually valid in that dimension
    all_avail <- susr_dimension_values(table_code, dim_code, lang = lang)

    # If user typed "all", just interpret that as "all" the dimension’s codes
    if (length(expanded_codes) == 1 && tolower(expanded_codes) == "all") {
      chosen_codes <- all_avail$element_value
    } else {
      # Keep only the intersection
      chosen_codes <- intersect(expanded_codes, all_avail$element_value)

      # If some user-provided codes did not match, warn
      not_matched <- setdiff(expanded_codes, all_avail$element_value)
      if (length(not_matched) > 0) {
        warning(
          "For dimension '", dim_code, "' in table '", table_code, "', ",
          "the following code(s) are not valid and will be ignored: ",
          paste(not_matched, collapse = ", "),
          " Please check for any typos, use `susr_dimnesion_values()` or visit https://data.statistics.sk/api/
          to get valid values."
        )
      }

      # If after intersection we have nothing, fallback
      if (length(chosen_codes) == 0) {
        # Check if "all" is valid. (We interpret "all" as the entire set.)
        # If the dimension actually has no values, fallback is impossible anyway,
        # but presumably that won't happen in normal usage.
        if ("all" %in% tolower(expanded_codes) ||
            "all" %in% tolower(all_avail$element_value)) {
          # If "all" is a recognized pattern, or we want to allow it explicitly:
          chosen_codes <- all_avail$element_value
          warning(
            "None of the requested codes for dimension '", dim_code, "' were valid. ",
            "Falling back to ALL available values."
          )
        } else if (length(all_avail$element_value) > 0) {
          # use the first available
          chosen_codes <- all_avail$element_value[1]
          warning(
            "None of the requested codes for dimension '", dim_code, "' were valid. ",
            "Falling back to the first available code: '", chosen_codes, "'"
          )
        } else {
          # No dimension values at all -> empty
          chosen_codes <- character(0)
          warning(
            "No valid values found for dimension '", dim_code, "', and the dimension ",
            "has no available values to fall back to. You may get an empty result."
          )
        }
      }
    }

    # Step C: Collapse final codes into a comma-separated string
    # If it's empty, we do a zero-length string (which may lead to a 404 or empty result).
    dimension_parts[i] <- paste(chosen_codes, collapse = ",")
  }

  # -----------------------------------------------------------------------
  # 3) Finally, combine them into the single dimension path
  # -----------------------------------------------------------------------
  dimension_path <- paste(dimension_parts, collapse = "/")

  # Return dimension path
  dimension_path
}
# nocov end
