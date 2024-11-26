# for this function I want it to create a PCA plot based off of existing data.



#' PCAplotR
#'
#' @param data the data used to make the plot
#' @param loadings whether or not you want the data displayed with loadings
#' @param clusters whether or not you want data displayed as clusters
#'
#' @return plots a PCA with loadings or clusters
#' @export
PCAplotR = function(data, loadings ,clusters){
  df = data[1:4]
  pca_res <- prcomp(df, scale. = TRUE)
  PCA = ggplot2::autoplot(pca_res)
  if(loadings == TRUE){
    loadings = ggplot2::autoplot(pca_res, data = iris, colour = 'Species',
                        loadings = TRUE, loadings.colour = 'blue',
                        loadings.label = TRUE, loadings.label.size = 3)
    return(loadings)
    }else{
      if(clusters == TRUE){
        clusters = ggplot2::autoplot(cluster::pam(data[-5], 3), frame = TRUE, frame.type = 'norm')
        return(clusters)
      }else{
      return(PCA)
    }
    }
}


