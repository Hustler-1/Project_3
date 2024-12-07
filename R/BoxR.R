
# In this function I plan to all for the creation of boxplots. With the possibility of prompting the user as it generates for different things that can be added to the plot.

#' boxR
#'
#' @param data data that will be plotted
#' @param x x axis variable
#' @param y y axis variable
#' @param title plot title
#' @param xname x axis name
#' @param yname y axis name
#' @param points whether or not you want individual data points on your box plots
#' @param savename what to save the plot as
#'
#' @return plots either a boxplot or dotplot
#' @export
boxR = function(data, x, y, title, xname, yname, points, savename){
  plotdata = data
  plot = ggplot2::ggplot(data = plotdata, mapping = ggplot2::aes(x = {{x}}, y = {{y}})) +
    ggplot2::geom_boxplot() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45)) + ggplot2::labs(x = xname, y = yname, title = title)
  dotplot = ggplot2::ggplot(data = plotdata, mapping = ggplot2::aes(x = {{x}}, y = {{y}})) +
    ggplot2::geom_boxplot() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45)) + ggplot2::labs(x = xname, y = yname, title = title) + ggplot2::geom_jitter(alpha = 0.3)
  if (points == TRUE){
    if (!is.null(plot)){
      directive = readline("Do you want to save plot, y or n?")
      if(directive == "y"){
        ggplot2::ggsave(savename, dpi = 600, height = 80, width = 80, units = "mm")

      }else{
        message("Not saving plot.")
      }
    }
    return(dotplot)
  }else{
    if (!is.null(plot)){
    directive = readline("Do you want to save plot, y or n?")
      if(directive == "y"){
      ggplot2::ggsave(savename, dpi = 600, height = 80, width = 80, units = "mm")

    }else{
      message("Not saving plot.")
    }
    }
  }
  return(plot)
}
