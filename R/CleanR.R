#I want this function to clean a dataset of NA values or other common missing data formats. Also I would like it to prompt the user if it detects spaces in between data entries and allow the users to change the spaces into underscores or periods.


#' cleanR
#'
#' @param data the data you wish to clean
#' @param clean_names whether or not you want to remove bad naming conventions like spaces
#'
#' @return saves clean data set with no NA values and clean naming conventions
#' @export
cleanR = function(data, clean_names){
  na.omit(data)
  if(clean_names == TRUE){
directive = readline("Do you want to ajust data names, y or n?")
if(directive == "y")
    names(data) <- make.names(names(data), unique = TRUE)
    return(data)
  }else{
    return(data)
  }
}
