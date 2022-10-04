#-----------------------------------------------------------------------------#
#                                                                             #
#         PERECNTILE-BASED CONTROL CHARTS IN R                                #
#                                                                             #
#  Written by: Aamir Saghir, Zsolt T. Kosztyan                                #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kzst@gtk.uni-pannon.hu                                         #
#                                                                             #
# Last modified: October 2022                                                 #
#-----------------------------------------------------------------------------#

#' @export
pbcc<- function(data,T1, p1, type=c("Xbar", "R", "S", "S2","Xbar-R","Xbar-S","Xbar-S2"), sided="two")
{
  if (!requireNamespace("qcc", quietly = TRUE)) {
    stop(
      "Package \"qcc\" must be installed to use this function.",
      call. = FALSE ) }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop(
      "Package \"stats\" must be installed to use this function.",
      call. = FALSE ) }

  call <- match.call()
  if (missing(data))
    stop("'data' argument is not specified")
  if(identical(type, eval(formals(pbcc)$type)))
  { type <- as.character(type)[1]
  warning("chart 'type' not specified, assuming \"", type, "\"",
          immediate. = TRUE) }
  if(missing(p1))
  {p1 <- 0.05}
  if(missing(T1))
  {T1 <- 100}
  if(missing(sided))
  {sided <- "two"}
  data.name <- deparse(substitute(data))
  sizes <- as.numeric(ncol(data))
  alfa <- 1-exp(log(1-p1)/T1)
  alpha <- alfa
if("Xbar" %in% type )
 {

  k <- stats::qnorm(1-alpha/2)
  q1 <- qcc::qcc(data, type="xbar", plot=FALSE)
  Statisticsx <- q1$statistics
  std.dev <- q1$std.dev
  lclx <- (q1$center - k * q1$std.dev/sqrt(sizes))
  clx <- q1$center
  uclx <- (q1$center + k * q1$std.dev/sqrt(sizes))
  #lim <- matrix(c(lcl, ucl), ncol = 2)
  output <- list(data.name=data.name,type=type, sizes=sizes, std.dev=std.dev, statistics=Statisticsx, LCL=lclx, CL=clx, UCL=uclx)
  }
if("R" %in% type )
  {
  if (sided=="one"){
    lu <- stats::qtukey(alpha, sizes, Inf, lower.tail=FALSE)
    ll <- 0
  }else{
    ll <- stats::qtukey(alpha/2, sizes, Inf)
    lu <- stats::qtukey(alpha/2, sizes, Inf, lower.tail=FALSE)
  }
    q1 <- qcc::qcc(data, type="R", plot=FALSE)
    std.dev <- q1$std.dev
    StatisticsR <- q1$statistics
    lclR <- ll*q1$center/qcc::qcc.options("exp.R.unscaled")[sizes]
    clR <-  q1$center
    uclR <- lu*q1$center/qcc::qcc.options("exp.R.unscaled")[sizes]
    output <- list(data.name=data.name,type=type,sizes=sizes, std.dev=std.dev, statistics=StatisticsR, LCL=lclR, CL=clR, UCL=uclR)
}
  if("S" %in% type )
  {
    qcc.c4 <- function(n)
    { sqrt(2/(n - 1)) * exp(lgamma(n/2) - lgamma((n - 1)/2)) }
    if (sided=="one"){
      lu <- sqrt(stats::qchisq(1-alpha, sizes-1))
      ll <- 0
    }else{
      ll <- sqrt(stats::qchisq(alpha/2, sizes-1))
      lu <- sqrt(stats::qchisq(1-alpha/2, sizes-1))
    }
    q1 <- qcc::qcc(data, type="S", plot=FALSE)
    std.dev <- q1$std.dev
    StatisticsS <- q1$statistics
    lclS <- ll*q1$center/qcc.c4(sizes)
    clS <- q1$center
    uclS <- lu*q1$center/qcc.c4(sizes)
    output <- list(data.name=data.name,type=type, sizes=sizes, std.dev=std.dev, statistics=StatisticsS, LCL=lclS, CL=clS, UCL=uclS)
  }
  if("S2" %in% type )
  {

        if (sided=="one"){
      lu <- stats::qchisq(1-alpha, sizes-1)
      ll <- 0
    }else{
      ll <- stats::qchisq(alpha/2, sizes-1)
      lu <- stats::qchisq(1-alpha/2, sizes-1)
    }

    data <- as.matrix(data)
    statistics <- apply(data, 1, stats::var, na.rm=TRUE)
    if (length(sizes == 1))
      sizes1 <- rep(sizes, length(statistics))
    center <- sum(sizes1 * statistics)/sum(sizes1)
    std.dev <- sqrt(2/(sizes-1))* (sum(statistics)/length(sizes1))
    StatisticsS2 <- statistics
    lclS2 <- ll*center/(sizes-1)
    clS2 <- center
    uclS2 <- lu*center/(sizes-1)
    output <- list(data.name=data.name,type=type,sizes=sizes, std.dev=std.dev, statistics=StatisticsS2, LCL=lclS2, CL=clS2, UCL=uclS2)
  }
if("Xbar-R" %in% type )
  {
    k <- stats::qnorm(1-alpha/2)
  if (sided=="one") {
      lu <- stats::qtukey(alpha, sizes, Inf, lower.tail=FALSE)
      ll <- 0
    }else {
      ll <- stats::qtukey(alpha/2, sizes, Inf)
      lu <- stats::qtukey(alpha/2, sizes, Inf, lower.tail=FALSE)
    }
    q1 <- qcc::qcc(data, type="xbar", plot=FALSE)
    std.devx <- q1$std.dev
    Statisticsx <- q1$statistics
    lclx <- (q1$center - k * q1$std.dev/sqrt(sizes))
    clx  <- q1$center
    uclx <- (q1$center + k * q1$std.dev/sqrt(sizes))
    q2 <- qcc::qcc(data, type="R", plot=FALSE)
    std.devR <- q2$std.dev
    StatisticsR <- q2$statistics
    lclR <- ll*q2$center/qcc::qcc.options("exp.R.unscaled")[sizes]
    clR <- q2$center
    uclR <- lu*q2$center/qcc::qcc.options("exp.R.unscaled")[sizes]
    output <- list(data.name=data.name,type=type, sizes=sizes,std.dev=std.devx, statistics=Statisticsx,LCL=lclx, CL=clx, UCL=uclx, std.dev1=std.devR, statistics1=StatisticsR,LCL1=lclR, CL1=clR, UCL1=uclR)
  }

  if("Xbar-S" %in% type )
  {
    k <- stats::qnorm(1-alpha/2)
    qcc.c4 <- function(n)
    { sqrt(2/(n - 1)) * exp(lgamma(n/2) - lgamma((n - 1)/2)) }
    if (sided=="one"){
      lu <- sqrt(stats::qchisq(1-alpha, sizes-1))
      ll <- 0
    }else{
      ll <- sqrt(stats::qchisq(alpha/2, sizes-1))
      lu <- sqrt(stats::qchisq(1-alpha/2, sizes-1))
    }
    q1 <- qcc::qcc(data, type="xbar", plot=FALSE)
    std.devx <- q1$std.dev
    Statisticsx <- q1$statistics
    lclx <- (q1$center - k * q1$std.dev/sqrt(sizes))
    clx <- q1$center
    uclx <- (q1$center + k * q1$std.dev/sqrt(sizes))
    q2 <- qcc::qcc(data, type="S", plot=FALSE)
    std.devS <- q2$std.dev
    StatisticsS <- q2$statistics
    lclS <- ll*q2$center/qcc.c4(sizes)
    clS <- q2$center
    uclS <- lu*q2$center/qcc.c4(sizes)
    output <- list(data.name=data.name,type=type,sizes=sizes,std.dev=std.devx,statistics=Statisticsx,LCL=lclx, CL=clx, UCL=uclx, std.dev1=std.devS, statistics1=StatisticsS,LCL1=lclS, CL1=clS, UCL1=uclS)
  }

  if("Xbar-S2" %in% type )
  {
    k <- stats::qnorm(1-alpha/2)
    if (sided=="one"){
      lu <- stats::qchisq(1-alpha, sizes-1)
      ll <- 0
    }else{
      ll <- stats::qchisq(alpha/2, sizes-1)
      lu <- stats::qchisq(1-alpha/2, sizes-1)
    }
    q1 <- qcc::qcc(data, type="xbar", plot=FALSE)
    std.devx <- q1$std.dev
    Statisticsx <- q1$statistics
    lclx <- (q1$center - k * q1$std.dev/sqrt(sizes))
    clx <- q1$center
    uclx <- (q1$center + k * q1$std.dev/sqrt(sizes))
    data <- as.matrix(data)
    statistics <- apply(data, 1, stats::var, na.rm=TRUE)
    if (length(sizes == 1))
      sizes1 <- rep(sizes, length(statistics))
    centerS2 <- sum(sizes1 * statistics)/sum(sizes1)
    std.devS2 <- sqrt(2/(sizes-1))* (sum(statistics)/length(sizes1))
    StatisticsS2 <- statistics
    lclS2 <- ll*centerS2/(sizes-1)
    clS2 <- centerS2
    uclS2 <- lu*centerS2/(sizes-1)
    output <- list(data.name=data.name,type=type, sizes=sizes,std.dev=std.devx,statistics=Statisticsx,LCL=lclx, CL=clx, UCL=uclx, std.dev1=std.devS2,statistics1=StatisticsS2,LCL1=lclS2, CL1=clS2, UCL1=uclS2)
  }
  class(output) <- "pbcc"
   return (output)
}


