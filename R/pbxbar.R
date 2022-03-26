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

pbxbar<- function(nmax,T1, T2, hv, mat, p1=0.05,p2=0.05, delta=1.5, d=2.0, pop.size=1000, sided="two"){
  if (!requireNamespace("rgenoud", quietly = TRUE)) {
    stop(
      "Package \"rgenoud\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if(missing(p1))
  {p1 <- 0.05}
  if(missing(p2))
  {p2 <- 0.05}
  if(missing(delta))
  {delta <- 1.5}
  if(missing(d))
  {d <- 2.0}
  if(missing(pop.size))
  {pop.size <- 1000}
    fitness2 <- function(x) {
      n <- x[1]     # sample size
      h <- hv[x[2]]  # intersample interval
      C0 <- T1/h
      alfa <- 1-exp(log(1-p1)/C0)
      alpha <- alfa
      if (sided=="one"){
        k <- qnorm(1-alpha)
        betaX <- pnorm((-d*sqrt(n)+k)/delta)
        beta <- betaX
      }
      if (sided=="two"){
        k <- qnorm(1-alpha/2)
        betaX <- pnorm((d*sqrt(n)+k)/delta)-pnorm((d*sqrt(n)-k)/delta)
        beta <- betaX
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
      k <- qnorm(1-alpha)}
    if (sided=="two")
      {k <- qnorm(1-alpha/2) }
    Fval <- GA2$Value   #format(round(OBJ, 2), nsmall = 2)
    K<- ((Fval <-1)*1)*k
    n1<- ((Fval <-1)*1)*n
    h1<- ((Fval <-1)*1)*h
    output <- structure(c(K, n1, h1, Fval), names= c("k", "n","h", "Fval"))
    class(output) <- "pbxbar"
    return (output)
  }
