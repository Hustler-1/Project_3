# In this function I plan to all for the creation of boxplots. With the possibility of prompting the user as it generates for different things that can be added to the plot.

boxR = function(data, x, y, title, xname, yname, median, savename){
  plotdata = data
  plot = ggplot(data = plotdata, mapping = aes(x = {{x}}, y = {{y}})) +
    geom_boxplot() + theme(axis.text.x = element_text(angle = 45)) + labs(x = xname, y = yname, title = title)
  if (!is.null(plot)){
    directive = readline("Do you want to save plot, y or n?")
    if(directive == "y"){
      ggsave(savename, dpi = 600, height = 80, width = 80, units = "mm")
    }else{
      message("Not saving plot.")
    }
  }
  return(plot)
}
