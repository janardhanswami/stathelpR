#' Used to get the max length of each variable in the dataset
#'
#' Checks the length of each column name in `df`. Arguments will be unquoted names,
#'
#' @param df Name of the dataset
#' @param out_name Name of the output dataset in which max lengths should be stored (optional)
#' @return returns the max length of each variable in a seperate dataframe
#'
#' @examples
#' pdata <- data.frame(name = c("alice", "barbara", "carol", "darwin"), id = c(1014, 1000, 1012, 1100))
#' cols_length(pdata)
#' @export


cols_length <- function(df, out_name = "col_lengths") {
  out <- c()
  for (i in colnames(df)) {
    col_vals <- as.character(df[[i]])
    if (all(is.na(col_vals))) {
      out[i] <- NA_integer_   # no length if all missing
    } else {
      out[i] <- max(nchar(col_vals), na.rm = TRUE)
    }
  }

  collengths <- data.frame(
    variable   = names(out),
    max_length = as.integer(out),
    row.names  = NULL
  )

  assign(out_name, collengths, envir = .GlobalEnv)
  invisible(collengths)
}


