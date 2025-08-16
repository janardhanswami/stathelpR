#' Check and handle duplicate rows in a dataset
#'
#' This function checks if your dataset has any duplicate rows.
#' If duplicates exist, it can remove them and store them in a separate dataset.
#'
#' @param df A dataframe or tibble.
#' @param dupout A character string. The name of the dataset in which duplicates will be stored.
#'   The default is `"duplicates"`.
#' @param cleandup A character string. The name of the dataset in which only non-duplicate rows will be stored.
#'   The default is `"nodup_df"`.
#'
#' @return Creates two datasets in the global environment:
#'   - `dupout`: containing duplicate rows.
#'   - `cleandup`: containing the dataset without duplicates.
#'   Also returns the cleaned dataset invisibly.
#'
#' @examples
#' dataframe <- data.frame(
#'   apha = c("a", "a", "b", "c"),
#'   num = c(1, 1, 2, 3),
#'   symbols = c("$", "$", "8", "#")
#' )
#' dup_out(dataframe)
#' dup_out(dataframe, "dup", "nodup")
#'
#' @note
#' Messages will be shown such as:
#' - "Your dataset 'dataframe' has 1 duplicate row."
#' - "Removing them and storing the duplicates in a dataset called 'duplicates'."
#'
#' @export




find_dup_out <- function(df, dupout="duplicates", cleandup="nodup_df") {
  df_name <- deparse(substitute(df))
  clean_dup_name <- deparse(substitute(dupout))
  if(!is.data.frame(df)) {
    stop(message(paste("df argument requires class dataframe but supplied ", "class:", class(df))))
  } else {
      duplicates <- df[duplicated(df), ]
      assign(dupout, duplicates, envir = .GlobalEnv)
      invisible(duplicates)
      message(sprintf("Note: Your dataset '%s' has %d duplicate rows.",    df_name, nrow(duplicates)))
      message(sprintf("Note: Removing them and storing the duplicates in a dataset called '%s' ", dupout))

      }
  if((cleandup) != "nodup_df") {
    df <- df[!duplicated(df), ]
    assign(cleandup, df, envir=.GlobalEnv)
    invisible(df)
  }
  else{
    df <- df[!duplicated(df), ]
    assign(cleandup, df, envir=.GlobalEnv)
  }
}
