#-----------------------------------------------------------------------------#
#                                                                             #
#         PERCENTILE-BASED CONTROL CHARTS                                     #
#                                                                             #
#  Written by: Aamir Saghir, Zsolt T. Kosztyan                                #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kzst@gtk.uni-pannon.hu.                                        #
#                                                                             #
# Last modified: October 2022                                                 #
#-----------------------------------------------------------------------------#

#' @export
odpbc<- function(nmax,T1, T2, hv, mat, p1=0.05,p2=0.05, d=1.0, delta=1.5,type= c("Xbar", "R", "S", "S2", "Xbar-R", "Xbar-S", "Xbar-S2"),pop.size=1000, sided="two")
{
  if (!requireNamespace("rgenoud", quietly = TRUE)) {
    stop(
      "Package \"rgenoud\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop(
      "Package \"stats\" must be installed to use this function.",
      call. = FALSE ) }
  if(missing(pop.size))
  {pop.size <- 1000}
  if(missing(sided))
  {sided <- "two"}
if ("Xbar" %in%type)
  {
      fitness2 <- function(x) {
        n <- x[1]     # sample size
        h <- hv[x[2]]  # intersample interval
        C0 <- T1/h
        alfa <- 1-exp(log(1-p1)/C0)
        alpha <- alfa
          k <- stats::qnorm(1-alpha/2)
          betaX <- stats::pnorm((d*sqrt(n)+k)/delta)-stats::pnorm((d*sqrt(n)-k)/delta)
          beta <- betaX
        OCTS <- (log(p2)/log(beta))*h
        OBJ <- abs(T2-OCTS)+ 100*(round(OCTS, digits = -1)> T2)
        return((OBJ))
      }
      GA2 <- rgenoud:: genoud(fitness2,nvars = ncol(mat), max = FALSE, pop.size = pop.size, max.generations = 100,
                              wait.generations = 10, Domains = mat, boundary.enforcement = 2, data.type.int = TRUE)
      n <- GA2$par[1]  # sample size
      h <- hv[GA2$par[2]]  # acceptance number
      C0 <- T1/h
      alfa <- 1-exp(log(1-p1)/C0)
      alpha <- alfa
      k <- stats::qnorm(1-alpha/2)
      Fval <- GA2$Value   #format(round(OBJ, 2), nsmall = 2)
      K<- ((Fval <-1)*1)*k
      n1<- ((Fval <-1)*1)*n
      h1<- ((Fval <-1)*1)*h
      output <- list(type=type, n=n1, h=h1, k=K)
    }
if("R" %in% type)
{
  fitness2 <- function(x) {
    n <- x[1]     # sample size
    h <- hv[x[2]]  # intersample interval
    C0 <- T1/h
    alfa <- 1-exp(log(1-p1)/C0)
    alpha <- alfa
    if (sided=="one"){
      lu <- stats::qtukey(alpha, n, Inf, lower.tail=FALSE)
      betaR <-  stats::ptukey(lu / delta, n, Inf, lower.tail=FALSE)
      beta <- betaR
    }
    if (sided=="two"){
      ll <- stats::qtukey(alpha/2, n, Inf)
      lu <- stats::qtukey(alpha/2, n, Inf, lower.tail=FALSE)
      betaR <- stats::ptukey(lu / delta, n, Inf, lower.tail=FALSE) - stats::ptukey(ll / delta, n, Inf)
      beta <- betaR
    }
    OCTS <- (log(p2)/log(beta))*h
    OBJ <- abs(T2-OCTS)+ 100*(round(OCTS, digits = -1)> T2)
    return((OBJ))
  }
  GA2 <- rgenoud:: genoud(fitness2,nvars = ncol(mat), max = FALSE, pop.size = pop.size, max.generations = 100,
                          wait.generations = 10, Domains = mat, boundary.enforcement = 2, data.type.int = TRUE)
  n <- GA2$par[1]  # sample size
  h <- hv[GA2$par[2]]  # acceptance number
  C0 <- T1/h
  alfa <- 1-exp(log(1-p1)/C0)
  alpha <- alfa
  if (sided=="one"){
    lu <- stats::qtukey(alpha, n, Inf, lower.tail=FALSE)
    ll <- 0}
  if (sided=="two")
  {ll <- stats::qtukey(alpha/2, n, Inf)
  lu <- stats::qtukey(alpha/2, n, Inf, lower.tail=FALSE)}
  Fval <- GA2$Value   #format(round(OBJ, 2), nsmall = 2)
  L1<- ((Fval <-1)*1)*ll
  L2<- ((Fval <-1)*1)*lu
  n1<- ((Fval <-1)*1)*n
  h1<- ((Fval <-1)*1)*h
  output <- list( type=type,n=n1, h=h1, lp=L1, up=L2)
}
 if ("S" %in% type)
    {
     fitness2 <- function(x) {
     n <- x[1]     # sample size
     h <- hv[x[2]]  # intersample interval
     C0 <- T1/h
     alfa <- 1-exp(log(1-p1)/C0)
     alpha <- alfa
     if (sided=="one"){
       lu <- sqrt(stats::qchisq(1-alpha, n-1))
       betaS2 <- sqrt(stats::pchisq(lu/(delta^2), n-1))
     }
     if (sided=="two"){
       ll <- sqrt(stats::qchisq(alpha/2, n-1))
       lu <- sqrt(stats::qchisq(1-alpha/2, n-1))
       betaS2 <- sqrt(stats::pchisq(lu / (delta^2), n-1)) - sqrt(stats::pchisq(ll / (delta^2), n-1))
     }
     beta <- betaS2
     OCTS <- (log(p2)/log(beta))*h
     OBJ <- abs(T2-OCTS)+ 100*(round(OCTS, digits = -1)> T2)
     return((OBJ))
   }
   GA2 <- rgenoud::genoud(fitness2,nvars = ncol(mat), max = FALSE, pop.size =pop.size, max.generations = 100,
                          wait.generations = 10, Domains = mat, boundary.enforcement = 2, data.type.int = TRUE)
   n <- GA2$par[1]  # sample size
   h <- hv[GA2$par[2]]  # acceptance number
   C0 <- T1/h
   alfa <- 1-exp(log(1-p1)/C0)
   alpha <- alfa
   if (sided=="one"){
     lu <- sqrt(stats::qchisq(1-alpha, n-1))
     ll <- 0}
   if (sided=="two"){
     ll <- sqrt(stats::qchisq(alpha/2, n-1))
     lu <- sqrt(stats::qchisq(1-alpha/2, n-1))}
   Fval <- GA2$Value  #format(round(OBJ, 2), nsmall = 2)
   L1<-  ((Fval <-1)*1)*ll
   L2<-  ((Fval <-1)*1)*lu
   n1<- ((Fval <-1)*1)*n
   h1<- ((Fval <-1)*1)*h
   output <- list(type=type,n=n1, h=h1, lp=L1, up=L2)
   }
if ("S2" %in% type)
  {
  fitness2 <- function(x) {
    n <- x[1]     # sample size
    h <- hv[x[2]]  # intersample interval
    C0 <- T1/h
    alfa <- 1-exp(log(1-p1)/C0)
    alpha <- alfa
    if (sided=="one"){
      lu <- stats::qchisq(1-alpha, n-1)
      betaS2 <- stats::pchisq(lu/(delta^2), n-1)
    }
    if (sided=="two"){
      ll <- stats::qchisq(alpha/2, n-1)
      lu <- stats::qchisq(1-alpha/2, n-1)
      betaS2 <- stats::pchisq(lu / (delta^2), n-1) - stats::pchisq(ll / (delta^2), n-1)
    }

    beta <- betaS2
    OCTS <- (log(p2)/log(beta))*h
    OBJ <- abs(T2-OCTS)+ 100*(round(OCTS, digits = -1)> T2)
    return((OBJ))
  }

  GA2 <- rgenoud::genoud(fitness2,nvars = ncol(mat), max = FALSE, pop.size =pop.size, max.generations = 100,
                         wait.generations = 10, Domains = mat, boundary.enforcement = 2, data.type.int = TRUE)
  n <- GA2$par[1]  # sample size
  h <- hv[GA2$par[2]]  # acceptance number
  C0 <- T1/h
  alfa <- 1-exp(log(1-p1)/C0)
  alpha <- alfa
  if (sided=="one"){
    lu <- stats::qchisq(1-alpha, n-1)
    ll <- 0}
  if (sided=="two"){
    ll <- stats::qchisq(alpha/2, n-1)
    lu <- stats::qchisq(1-alpha/2, n-1)}
  Fval <- GA2$Value  #format(round(OBJ, 2), nsmall = 2)
  L1<-  ((Fval <-1)*1)*ll
  L2<-  ((Fval <-1)*1)*lu
  n1<- ((Fval <-1)*1)*n
  h1<- ((Fval <-1)*1)*h
  output <- list(type=type,n=n1, h=h1, lp=L1, up=L2)
}
  if ("Xbar-R" %in% type)
  {
    fitness2 <- function(x) {
      n <- x[1]     # sample size
      h <- hv[x[2]]  # intersample interval
      C0 <- T1/h
      alfa <- 1-exp(log(1-p1)/C0)
      alpha <- 1-sqrt(1-alfa)
      k <- stats::qnorm(1-alpha/2)
      betaX <- stats::pnorm((d*sqrt(n)+k)/delta)-stats::pnorm((d*sqrt(n)-k)/delta)
      if (sided=="one"){
        lu <- stats::qtukey(alpha, n, Inf, lower.tail=FALSE)
        betaR <-  stats::ptukey(lu / delta, n, Inf, lower.tail=FALSE)
      }
      if (sided=="two"){
        ll <- stats::qtukey(alpha/2, n, Inf)
        lu <- stats::qtukey(alpha/2, n, Inf, lower.tail=FALSE)
        betaR <- stats::ptukey(lu / delta, n, Inf) - stats::ptukey(ll / delta, n, Inf)
      }
      beta <- betaX*betaR
      OCTS <- (log(p2)/log(beta))*h
      OBJ <- abs(T2-OCTS)+ 100*(round(OCTS, digits = -1)> T2)
      return((OBJ))
    }

    GA2 <- rgenoud:: genoud(fitness2,nvars = ncol(mat), max = FALSE, pop.size = pop.size,max.generations = 100,
                            wait.generations = 10, Domains = mat, boundary.enforcement = 2, data.type.int = TRUE)
    n <- GA2$par[1]  # sample size
    h <- hv[GA2$par[2]]  # acceptance number
    C0 <- T1/h
    alfa <- 1-exp(log(1-p1)/C0)
    alpha <- 1-sqrt(1-alfa)
    k <- stats::qnorm(1-alpha/2)
    if (sided=="one"){
      lu <- stats::qtukey(alpha, n, Inf, lower.tail=FALSE)
      ll <- 0
    }
    if (sided=="two"){
      ll <- stats::qtukey(alpha/2, n, Inf)
      lu <- stats::qtukey(alpha/2, n, Inf, lower.tail=FALSE)
    }
    Fval <- GA2$Value                  #format(round(OBJ, 2), nsmall = 2)
    K<- ((Fval <-1)*1)*k
    L1<-  ((Fval <-1)*1)*ll
    L2<-  ((Fval <-1)*1)*lu
    n1<- ((Fval <-1)*1)*n
    h1<- ((Fval <-1)*1)*h
    output <- list(type=type,n=n1, h=h1, k=K, lp=L1, up=L2)
  }
  if ("Xbar-S" %in% type)
  {
    fitness2 <- function(x) {
      n <- x[1]     # sample size
      h <- hv[x[2]]  # intersample interval
      C0 <- T1/h
      alfa <- 1-exp(log(1-p1)/C0)
      alpha <- 1-sqrt(1-alfa)
       k <- stats::qnorm(1-alpha/2)
       betaX <- stats::pnorm((d*sqrt(n)+k)/delta)-stats::pnorm((d*sqrt(n)-k)/delta)
      if (sided=="one"){
        lu <- sqrt(stats::qchisq(1-alpha, n-1))
        betaS <- sqrt(stats::pchisq(lu/(delta^2), n-1))
      }
      if (sided=="two"){
        ll <- sqrt(stats::qchisq(alpha/2, n-1))
        lu <- sqrt(stats::qchisq(1-alpha/2, n-1))
        betaS <- sqrt(stats::pchisq(lu / (delta^2), n-1)) - sqrt(stats::pchisq(ll / (delta^2), n-1))
      }
      beta <- betaX*betaS
      OCTS <- (log(p2)/log(beta))*h
      OBJ <- abs(T2-OCTS)+ 100*(round(OCTS, digits = -1)> T2)
      return((OBJ))
    }

    GA2 <- rgenoud:: genoud(fitness2,nvars = ncol(mat), max = FALSE, pop.size = pop.size,max.generations = 100,
                            wait.generations = 10, Domains = mat, boundary.enforcement = 2, data.type.int = TRUE)
    n <- GA2$par[1]  # sample size
    h <- hv[GA2$par[2]]  # acceptance number
    C0 <- T1/h
    alfa <- 1-exp(log(1-p1)/C0)
    alpha <- 1-sqrt(1-alfa)
    k <- stats::qnorm(1-alpha/2)
    if (sided=="one"){
      lu <- sqrt(stats::qchisq(1-alpha, n-1))
      ll <- 0
    }
    if (sided=="two"){
      ll <- sqrt(stats::qchisq(alpha/2, n-1))
      lu <- sqrt(stats::qchisq(1-alpha/2, n-1))
    }
    Fval <- GA2$Value  #format(round(OBJ, 2), nsmall = 2)
    K<- ((Fval <-1)*1)*k
    L1<-  ((Fval <-1)*1)*ll
    L2<-  ((Fval <-1)*1)*lu
    n1<- ((Fval <-1)*1)*n
    h1<- ((Fval <-1)*1)*h
    output <- list(type=type,n=n1, h=h1, k=K, lp=L1, up=L2)
  }
  if ("Xbar-S2" %in% type)
  {
    fitness2 <- function(x) {
      n <- x[1]     # sample size
      h <- hv[x[2]]  # intersample interval
      C0 <- T1/h
      alfa <- 1-exp(log(1-p1)/C0)
      alpha <- 1-sqrt(1-alfa)
      k <- stats::qnorm(1-alpha/2)
      betaX <- stats::pnorm((d*sqrt(n)+k)/delta)-stats::pnorm((d*sqrt(n)-k)/delta)
      if (sided=="one"){
       lu <- stats::qchisq(1-alpha, n-1)
        betaS2 <- stats::pchisq(lu/(delta^2), n-1)
      }
      if (sided=="two"){
        ll <- stats::qchisq(alpha/2, n-1)
        lu <- stats::qchisq(1-alpha/2, n-1)
        betaS2 <- stats::pchisq(lu / (delta^2), n-1) - stats::pchisq(ll / (delta^2), n-1)
      }
      beta <- betaX*betaS2
      OCTS <- (log(p2)/log(beta))*h
      OBJ <- abs(T2-OCTS)+ 100*(round(OCTS, digits = -1)> T2)
      return((OBJ))
    }

    GA2 <- rgenoud:: genoud(fitness2,nvars = ncol(mat), max = FALSE, pop.size = pop.size,max.generations = 100,
                            wait.generations = 10, Domains = mat, boundary.enforcement = 2, data.type.int = TRUE)
    n <- GA2$par[1]  # sample size
    h <- hv[GA2$par[2]]  # acceptance number
    C0 <- T1/h
    alfa <- 1-exp(log(1-p1)/C0)
    alpha <- 1-sqrt(1-alfa)
    k <- stats::qnorm(1-alpha/2)
    if (sided=="one"){
      lu <- stats::qchisq(1-alpha, n-1)
      ll <- 0
    }
    if (sided=="two"){
      ll <- stats::qchisq(alpha/2, n-1)
      lu <- stats::qchisq(1-alpha/2, n-1)
    }
    Fval <- GA2$Value  #format(round(OBJ, 2), nsmall = 2)
    K<- ((Fval <-1)*1)*k
    L1<-  ((Fval <-1)*1)*ll
    L2<-  ((Fval <-1)*1)*lu
    n1<- ((Fval <-1)*1)*n
    h1<- ((Fval <-1)*1)*h
    output <- list(type=type,n=n1, h=h1, k=K, lp=L1, up=L2)
  }
  class(output) <- "odpbc"
  return (output)
}
