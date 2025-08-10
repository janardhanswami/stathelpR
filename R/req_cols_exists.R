#' Check if columns exist in a data frame (multiple, quoted or unquoted)
#'
#' Checks one or more column names in `df`. Arguments may be unquoted names,
#' quoted strings, or a character vector (e.g. `c("a","b")`).
#'
#' @param df A data.frame or tibble.
#' @param ... One or more column names (unquoted or quoted) or a character vector.
#' @return Invisibly returns TRUE if *all* columns exist, FALSE otherwise.
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#' cols_exists(df, a, "b")
#' cols_exists(df, c("a","z"))
#'
#' @importFrom rlang enquos get_expr is_symbol as_name eval_tidy
#' @export
req_cols_exists <- function(df, ...) {
  if (!is.data.frame(df)) stop("`df` must be a data.frame or tibble.")

  quos <- rlang::enquos(...)
  if (length(quos) == 0L) stop("No column names supplied in `...`.")

  # Resolve each argument into one or more column name strings
  resolved <- character(0L)
  for (q in quos) {
    expr <- rlang::get_expr(q)

    if (rlang::is_symbol(expr)) {
      # unquoted name: col_exists(df, a)
      resolved <- c(resolved, rlang::as_name(expr))
    } else {
      # could be "name", a variable containing names, or c("a","b")
      val <- rlang::eval_tidy(q)
      if (is.character(val)) {
        resolved <- c(resolved, val)
      } else {
        stop("Unsupported argument type for column names. Provide unquoted names, quoted strings, or a character vector.")
      }
    }
  }

  resolved <- unique(resolved)
  df_name <- deparse(substitute(df))
  all_exist <- TRUE

  for (nm in resolved) {
    if (nm %in% colnames(df)) {
      message(sprintf("✅ Variable '%s' exists in %s.", nm, df_name))
    } else {
      all_exist <- FALSE
      # fuzzy suggestions: change max.distance to taste (0.2 is conservative)
      suggestions <- colnames(df)[agrep(nm, colnames(df), max.distance = 0.2)]
      if (length(suggestions) > 0L) {
        message(sprintf("❌ Variable '%s' does not exist in %s. Did you mean: %s?", nm, df_name, paste(suggestions, collapse = ", ")))
      } else {
        message(sprintf("❌ Variable '%s' does not exist in %s.", nm, df_name))
      }
    }
  }

  invisible(all_exist)
}




