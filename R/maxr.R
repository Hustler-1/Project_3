#' Removes NA values from a data column and calculates the numerical maximum.
#'
#' @param path The file path to the comma-separated file.
#' @param column The column that you wish to calculate a maximum for.
#'
#' @return Prints a numerical maximum
#' @export
maxr = function(path, column){
       result =  readr::read_csv(path) %>%
             dplyr::filter(!is.na({{column}})) %>%
             dplyr::pull({{column}}) %>%
             max()
       if(sum(is.na(result)) == 0){
             return(result)
         } else
               print("There are still remaining NA values. Either the function failed to catch some NA values, the data has some missing data not deliminated by NA, or the column specified contains nonnumeric or nonlogical data. Clean the data and try again.")
   }
