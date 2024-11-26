#For this function I plan for it to allow the user to take a large data set and create smaller chunks of the data for looking at specific things. Similar to what we needed to do when looking at the NOP dataset.


#' chunkR
#'
#' @param data the data in which you want to subset
#' @param group what you want to group the data by
#' @param selection what you want to select from the data
#' @param sum.name name of summarize column
#'
#' @return saves a chunk of a larger dataset as a new smaller dataset
#' @export
chunkR = function(data, group, selection, sum.name){
chunk = data %>%
  select({{selection}}) %>%
  group_by({{group}}) %>%
  summarize(sum.name = n())
return(chunk)
}
