#-----------------------------------------------------------------------------#
#                                                                             #
#  PERCENTILE-BASED CONTROL CHARTS                                            #
#                                                                             #
#  Written by: Aamir Saghir, Khan, Zahid, Zsolt T. Kosztyan                   #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kosztyan.zsolt@gtk.uni-pannon.hu                               #
#                                                                             #
# Last modified: July 2024                                                    #
#-----------------------------------------------------------------------------#

#' @export
odpbc_binom <- function(nmax, T1, T2, hv, mat, ip, sp, p1=0.05, p2=0.05, pop.size = 1000) {
  if (!requireNamespace("rgenoud", quietly = TRUE)) {
    stop(
      "Package \"rgenoud\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop(
      "Package \"stats\" must be installed to use this function.",
      call. = FALSE)
  }
  if (missing(pop.size)) {
    pop.size <- 1000}
  fitness2 <- function(x) {
    sample_size <- x[1]     # Renamed 'n' to 'sample_size'
    h <- hv[x[2]]  # intersample interval
    C0 <- T1 / h
    alfa <- 1 - exp(log(1 - p1) / C0)
    alpha <- alfa
    k=stats::qnorm(1-alpha/2)
    ll <-max (0,sample_size * ip - k * sqrt(sample_size * ip * (1 - ip)))
    lu <- sample_size * ip + k * sqrt(sample_size * ip * (1 - ip))
    betaS2 <- stats::pbinom(lu, sample_size, sp) - stats::pbinom(ll, sample_size, sp)

    beta <- betaS2
    OCTS <- (log(p2) / log(beta)) * h
    OBJ <- abs(T2 - OCTS) + 100 * (round(OCTS, digits = -1) > T2)
    return((OBJ))
  }

  GA2 <- rgenoud::genoud(fitness2, nvars = ncol(mat), max = FALSE, pop.size = pop.size, max.generations = 100,
                         wait.generations = 10, Domains = mat, boundary.enforcement = 2, data.type.int = TRUE)
  sample_size <- GA2$par[1]  # sample size
  h <- hv[GA2$par[2]]  # acceptance number
  C0 <- T1 / h
  alfa <- 1 - exp(log(1 - p1) / C0)
  alpha <- alfa


  k=stats::qnorm(1-alpha/2)
  ll <-max (0,sample_size * ip - k * sqrt(sample_size * ip * (1 - ip)))
  lu <- sample_size * ip + k * sqrt(sample_size * ip * (1 - ip))

  Fval <- GA2$Value  #format(round(OBJ, 2), nsmall = 2)

  K<- ((Fval <-1)*1)*k
  n1 <- ((Fval <- 1) * 1) * sample_size
  h1<- ((Fval <-1)*1)*h
  output <- list( n=n1, h=h1, k=K)
}
