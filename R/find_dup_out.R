#' usally its  bit lengthy to check if your dataset has any duplicates and to reomve them and store them in a seperate dataset
#' There isn't even a single function in whole tidyverse family or BaseR

#' Inorder to to do this we need some code to remeber every single time
#' so the main goal of this function is to fill that gap and make that process short simple
#' by giving some useful messages to the user


#'  @param df A dataframe or a tibble
#'  @param dupout Name of the dataset as a character string in which your duplicates will be stored the default name will be 'duplicates'
#'  @param cleandup Name of the dataset as a charactet string in whuch only non duplicates will be stored the default name will be nodup_df



#' @examples
#' dataframe <- data.frame(apha=c("a", "a", "b", "c"), num=c(1,1,2,3), symbols=c("$", "$","8", "#"))
#' dup_out(dataframe)
#' dup_out(dataframe, "dup", "nodup")
#'
#' Note: Your dataset 'dataframe' has 1 duplicate rows.
#' Note: Removing them and storing the duplicates in a dataset called 'duplicates'
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
