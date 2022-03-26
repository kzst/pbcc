#-----------------------------------------------------------------------------#
#                                                                             #
#  RISK-BASED MULTIVARIATE CONTROL CHARTS                                     #
#                                                                             #
#  Written by: Aamir Saghir, Zsolt T. Kosztyan                                #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kzst@gtk.uni-pannon.hu                                         #
#                                                                             #
# Last modified: April 2022                                                   #
#-----------------------------------------------------------------------------#

#' @export

summary.pbcc <- function(object, digits =  getOption("digits"), ...) {
  if (!requireNamespace("rgenoud", quietly = TRUE)) {
    stop(
      "Package \"rgenoud\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (class(object)=="pbxbar"){
    k <- object["k"]
    n <- object["n"]
    h <- object["h"]
    Fval <- object["Fval"]
    cat("\nSummary of the Optimum Parameters:\n")
    cat("\nSample Size: ",n)
    cat("\nSampling Interval: ",h)
    cat("\n\nSummary of Control Charting Constant:\n")
    cat("\nCharting Constant: ",k)
    cat("\n\nOptimum value of function:\n")
    cat("\nOptimum function value: ",Fval)

    if (!is.null(k)){
      cat("\n\nControl Charting Constant",k)
    }
  }else{
    if (class(object)=="pbs2"){
      l <- object["l"]
      n <- object["n"]
      h <- object["h"]
      Fval <- object["Fval"]
      cat("\nSummary of the Optimum Parameters:\n")
      cat("\nSample Size: ",n)
      cat("\nSampling Interval: ",h)
      cat("\n\nSummary of Control Charting Constant:\n")
      cat("\nCharting Constant: ",l)
      cat("\n\nOptimum value of function:\n")
      cat("\nOptimum function value: ",Fval)

      if (!is.null(l)){
        cat("\n\nControl Charting Constant",l)
      }
    }else{
      if (class(object)=="pbxbars2"){
        k <- object["k"]
        l <- object["l"]
        n <- object["n"]
        h <- object["h"]
        Fval <- object["Fval"]
        cat("\nSummary of the Optimum Parameters:\n")
        cat("\nSample Size: ",n)
        cat("\nSampling Interval: ",h)
        cat("\n\nSummary of Control Charting Constants:\n")
        cat("\nCharting Constant: ",k)
        cat("\nCharting Constant:",l)
        cat("\n\nOptimum value of function:\n")
        cat("\nOptimum function value:",Fval)

        if (!is.null(k)){
          cat("\n\nControl Charting Constant",k)
        }
      }else{
        summary(object,...)
      }
    }
  }
}


