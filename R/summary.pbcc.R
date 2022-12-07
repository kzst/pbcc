#-----------------------------------------------------------------------------#
#                                                                             #
#  PERCENTILE-BASED CONTROL CHARTS                                            #
#                                                                             #
#  Written by: Aamir Saghir, Zsolt T. Kosztyan                                #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kzst@gtk.uni-pannon.hu                                         #
#                                                                             #
# Last modified: October 2022                                                 #
#-----------------------------------------------------------------------------#

#' @export
summary.pbcc <- function(object, digits =  getOption("digits"),...) {

  if ("pbcc" %in% class(object)){

    data.name <- object$data.name
    type <- object$type
    sizes <- object$sizes
    std.dev <- object$std.dev
    std.dev1 <- object$std.dev1
    Statistics <- object$statistics
    Statistics1 <- object$statistics1
    LCL <- object$LCL
    center <- object$CL
    UCL<- object$UCL
    limits <- matrix(c(LCL, UCL), ncol = 2)
    rownames(limits) <- rep("", length = nrow(limits))
    colnames(limits) <- c("LCL", "UCL")
    LCL1 <- object$LCL1
    center1 <- object$CL1
    UCL1<- object$UCL1

    .printShortMatrix <- function(x, head = 2, tail = 1, chead = 5, ctail = 1, ...)
    {
      # print a short version of a matrix by allowing to select
      # the number of head/tail rows and columns to display
      x <- as.matrix(x)
      nr <- nrow(x)
      nc <- ncol(x)
      rnames <- rownames(x)
      cnames <- colnames(x)
      dnames <- names(dimnames(x))

      if(is.na(head <- as.numeric(head))) head <- 2
      if(is.na(tail <- as.numeric(tail))) tail <- 1
      if(is.na(chead <- as.numeric(chead))) chead <- 5
      if(is.na(ctail <- as.numeric(ctail))) ctail <- 1

      if(nr > (head + tail + 1))
      {
        if(is.null(rnames))
          rnames <- paste("[", 1:nr, ",]", sep ="")
        x <- rbind(x[1:head,,drop=FALSE],
                   rep(NA, nc),
                   x[(nr-tail+1):nr,,drop=FALSE])
        rownames(x) <- c(rnames[1:head], ":", rnames[(nr-tail+1):nr])
      }
      if(nc > (chead + ctail + 1))
      {
        if(is.null(cnames))
          cnames <- paste("[,", 1:nc, "]", sep ="")
        x <- cbind(x[,1:chead,drop=FALSE],
                   rep(NA, nrow(x)),
                   x[,(nc-ctail+1):nc,drop=FALSE])
        colnames(x) <- c(cnames[1:chead], "...", cnames[(nc-ctail+1):nc])
      }
      names(dimnames(x)) <- dnames
      print(x, na.print = "", ...)
    }

    cat("Chart type                 =", type, "\n")
    cat("\nSummary of group statistics:\n", sep="")
    print(summary(Statistics), digits=digits,...)
    cat("\nGroup Sample size             =", sizes, "\n")
    cat("Number of groups                =", length(Statistics), "\n")
    cat("Center of groups statistics     =", center, "\n")
    cat("StdDev of groups statistics     =", std.dev, "\n")
    if(!is.null(limits))
    {
      # cat("Control limits:\n")
      cat("\nControl limits =", "\n")
      # names(dimnames(limits)) <- c("Control limits             =", "")

      .printShortMatrix(limits, digits = digits, ...)
    }

    if(!is.null(Statistics1))
    {
      cat("\nSummary of group statistics:\n", sep="")
      print(summary(Statistics1), digits=digits,...)
      cat("\nGroup Sample size             =", sizes, "\n")
      cat("Number of groups                =", length(Statistics1), "\n")
    }
    if(!is.null(center1))
    {
      cat("Center of groups statistics     =", center1, "\n")
    }

    if(!is.null(std.dev1))
    {
      cat("StdDev of groups statistics     =", std.dev1, "\n")
    }

    if(!is.null(LCL1))
    {
      LCL2=LCL1
    }
    if(!is.null(UCL1))
    {
      UCL2=UCL1
      limits1 <- matrix(c(LCL2, UCL2), ncol = 2)
      rownames(limits1) <- rep("", length = nrow(limits1))
      colnames(limits1) <- c("LCL", "UCL")
      # cat("Control limits:\n")
      cat("\nControl limits =", "\n")
      # names(dimnames(limits)) <- c("Control limits             =", "")

      .printShortMatrix(limits1, digits = digits, ...)
    }

  } else{
      summary(object,...)
    }
  }

#' @export
summary.odpbc <- function(object, digits =  getOption("digits"),...) {

  if ("odpbc" %in% class(object)){

    k <- object$k
    n <- object$n
    h <- object$h
    lp<- object$lp
    up<- object$up
    cat("\nSummary of the Optimum Parameters:\n")
    cat("\nSample Size (n)  = ",n)
    cat("\nSampling Interval (h) = ",h)
    cat("\n\nSummary of Optimal Control Charting Constants:\n")
    if (!is.null(k)){
      cat("\n\nControl Charting Constant (K) =",k)
    }
    if (!is.null(lp)){
      cat("\n\nLower Percentile Point of relative distribution (Lp) =",lp)
    }
    if (!is.null(up)){
      cat("\n\nUpper Percentile Point of relative distribution Point (Up) =",up)
    }

  }else{
    summary(object,...)
  }
}
