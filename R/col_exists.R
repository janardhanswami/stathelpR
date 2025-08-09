#' Check if a column exists in a data frame, with helpful suggestions
#'
#' This function checks if a specified column (unquoted or quoted) exists in the
#' provided data frame. If the column does not exist, it provides suggestions for
#' similar column names (fuzzy matching).
#'
#' @param df A data frame to check for the column.
#' @param var The column name to check, provided unquoted (e.g., `name`) or quoted (e.g., `"name"`).
#'
#' @return Returns `TRUE` invisibly if the column exists, `FALSE` otherwise.
#' @examples
#' df <- data.frame(name1 = 1:3, age = c(20, 25, 30))
#' col_exists(df, name1)    # TRUE, prints confirmation
#' col_exists(df, "age")    # TRUE, prints confirmation
#' col_exists(df, height)   # FALSE, suggests similar columns
#'
#' @importFrom rlang enquo get_expr is_symbol as_name eval_tidy
#' @export
col_exists <- function(df, var) {
  # Capture and interpret the column name (unquoted or quoted)
  var_expr <- rlang::enquo(var)
  if (rlang::is_symbol(rlang::get_expr(var_expr))) {
    var1 <- rlang::as_name(var_expr)   # unquoted name
  } else {
    var1 <- rlang::eval_tidy(var_expr) # quoted string or variable holding string
  }

  df_name <- deparse(substitute(df))

  if (var1 %in% colnames(df)) {
    message(sprintf("✅ Variable '%s' exists in %s.", var1, df_name))
    return(invisible(TRUE))
  } else {
    # Find close matches using agrep for suggestions
    suggestions <- colnames(df)[agrep(var1, colnames(df), max.distance = 0.5)]
    if (length(suggestions) > 0) {
      message(sprintf(
        "❌ Variable '%s' does not exist in %s. Did you mean: %s?",
        var1, df_name, paste(suggestions, collapse = ", ")
      ))
    } else {
      message(sprintf("❌ Variable '%s' does not exist in %s.", var1, df_name))
    }
    return(invisible(FALSE))
  }
}
