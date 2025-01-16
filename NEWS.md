# susR 0.0.1

This is the first release version of susR! 🎉

- Added a `NEWS.md` file to track changes to the package.
- Major functions:
    - `susr_domains()`
    - `susr_tables()`
    - `susr_dimension_values()`
    - `fetch_susr_data()`
- New vignettes:
    - [Getting started with susR](https://arnold-kakas.github.io/susR/articles/getting_started.html)
    - [Example Analysis](https://arnold-kakas.github.io/susR/articles/example_analysis.html)

# susR 0.0.2

Improved dimension handling by refactoring into a single function that expands special codes, validates inputs, and gracefully falls back (with warnings) when invalid codes are provided.

- Major changes:
    - resolve_href() internally expands special keywords (e.g., "lastX", region shortcuts like "districs").
    - Added validation and fallback logic: user-supplied codes are now checked against available values (via susr_dimension_values()), and warnings are issued for any invalid codes.
    - If no valid codes remain, a fallback to "all" or the first available code is used with a warning.
    - Updated fetch_susr_data() to use the new resolve_href() function for cleaner dimension path construction.
