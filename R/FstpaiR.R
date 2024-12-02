# In this function I want it to run all major pairwise fst analyses


#' FstpaiR
#'
#' @param data the data in which to analyse
#' @param pcoa whether or not you want a pcoa plot of the data
#'
#' @return prints a matrix of Fst values and a pcoa plot if specified
#' @export
FstpaiR = function(data, pcoa){
nei = hierfstat::pairwise.neifst(data[,-2],diploid=TRUE)
weir = hierfstat::pairwise.WCfst(data[,-2],diploid=TRUE)
if(pcoa == TRUE){
  colo<-c("black","red","blue","yellow","orange","green")
pcoa = hierfstat::pcoa(as.matrix(genet.dist(data[,-1])),col=rep(colo,c(5,5,4,5,5,5)))
return(list(nei = nei, weir = weir))
}else{
  return(list(nei = nei, weir = weir))
}
}
