#' Removes NA values from a data column and calculates the mean.
#'
#' @param path The path to the tab-delimited file you to process.
#' @param column The column of data that you wish to calculate a mean for.
#'
#' @return Prints a numerical mean.
#' @export
meanr = function(path, column){
  result =  readr::read.csv(path) %>%
    dplyr::filter(!is.na({{column}})) %>%
    dplyr::pull({{column}}) %>%
    mean()
  if(sum(is.na(result)) == 0){
    return(result)
  } else
    print("There are still remaining NA values. Either the function failed to catch some NA values, the data has some missing data not deliminated by NA, or the column specified contains nonnumeric or nonlogical data. Clean the data and try again.")
}
