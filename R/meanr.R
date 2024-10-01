meanr = function(path, column){
  result =  dataframe %>%
    filter(!is.na({{column}})) %>%
    pull({{column}}) %>%
    mean()
  if(sum(is.na(result)) == 0){
    return(result)
  } else
    print("There are still remaining NA values. Either the function failed to catch some NA values, the data has some missing data not deliminated by NA, or the column specified contains nonnumeric or nonlogical data. Clean the data and try again.")
}