#I would like this function to be an entire suite of normality tests and plots like the Q-Q plot. the function will keep going through each one until it either finsihes or detects nonnormal data. IF it finishes I want the function to print/save a full report of the tests and if it finds non normal data I want it to switch to a non normal set of tests.
#' normalitycheckR
#'
#' @param data the dataset or individual data column in which you want to run normality tests on
#'
#' @return prints multiple statistical test reports for shaprio.wilk, kolmogorov.smirnov, and anderson.darling tests
#' @export
normalitycheckR = function(data){
  mean = mean(data)
  mx_norm = rnorm(5000, mean = mean)
  shapiro = shapiro.test(data)
  if(shapiro$p.value > 0.05){
    kolmogorov = ks.test(data, mx_norm)
  }else{
    print("data is non-normal")
    return(shapiro)
  }
  if(kolmogorov$p.value > 0.05){
    anderson = DescTools::AndersonDarlingTest(data, null = "pnorm")
  }else{
    print("data is non-normal")
    return(list(shapiro = shapiro, kolmogorov = kolmogorov))
  }
  if(anderson$p.value > 0.05){
    return(list(shapiro = shapiro, kolmogorov = kolmogorov, anderson = anderson))
  }else{
    print("data is non-normal")
    return(list(shapiro = shapiro, kolmogorov = kolmogorov, anderson = anderson))
  }
  }

