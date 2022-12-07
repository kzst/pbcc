\name{summary.pbcc}
\alias{summary.pbcc}
\alias{summary.odpbc}
\title{
Print statistics,summary of statistics,control limits,optimal design parameters.
}
\description{
Print statistics, summary of statistics, control limits, optimal design parameters of Percentile-based control charts with guaranted in-control or/and out-of-control performances.
}
\usage{
\method{summary}{pbcc}(object, digits = getOption("digits"), ...)
\method{summary}{odpbc}(object, digits = getOption("digits"), ...)
}
\arguments{
  \item{object}{an object of class 'pbcc' or 'odpbc'.}
  \item{digits}{the number of significant digits to use when \code{add.stats = TRUE}.}
  \item{...}{additional arguments affecting the summary produced.}
}
\value{No return value, called for side effects}

\references{
Faraz, A., Saniga, E., Montgomery, D. (2019). Perentile-based control chart design with an application to Shewhart Xbar and S2 control charts. Quality and Reliability Engineering International, 35(1),116-126.
}
\author{
Aamir Saghir, Zsolt T. Kosztyan*

e-mail: kzst@gtk.uni-pannon.hu
}

\seealso{\code{\link{pbcc}}, \code{\link{odpbc}}, \code{\link{plot}}.}

\examples{

# Print summary of calculation of two-sided individual percentile-based xbar
# control chart for diameter data with guaranted in-control performance.

library(qcc)
data(pistonrings)
diameter = with(pistonrings, qcc.groups(diameter, sample))
T1=100      # Set the process in-control time to signal is at least 100 samples.
p1=0.05     # Set the probability of guaranteed in-control signals is 5%.

#Calculation of two-sided individual percentile-based Xbar for diameter data.
Q11<- pbcc(diameter,T1, p1, type="Xbar")
summary(Q11) # summarize the results

# Print summary of Calculation of two-sided joint percentile-based Xbar
# and S control chart for diameter data with guaranted in-control performance.
# Calculation of two-sided joint percentile-based Xbar and S for diameter data.
Q13<- pbcc(diameter,T1, p1, type="Xbar-S", sided="two")
summary(Q13) # summarize the results

# Print summary of optimal design parameters of percentile-based Xbar chart
# with guaranted in-control and out-of-control performances.
nmax=10     # Set the maximum possible sample size in each h units is 10.
T1=100      # Set the process in-control time to signal is at least 100 samples.
# Set the control chart time to signal is at most 1 samples when shift occur
#in the process mean or/and std.dev.
T2=3
# Set the sampling intersample intervals to 0.5(0.5) T2 units of time.
hv=seq(0.5, T2, by=0.5)
#Set the lower and upper bounds of parameters(n and h) used in the optimization.
mat=matrix(c(2, nmax, 1, length(hv)), 2,2, byrow=TRUE)
p1=0.05     # Set the probability of guaranteed in-control signals is 5%.
p2=0.05     # Set the probability of guaranteed out-of-control signals is 5%.
d=3             # Set the shift size in the process mean is 3 (large shift).
Q1<- odpbc(nmax, T1, T2, hv, mat, p1, p2, d, delta=1.5, type= "Xbar")
summary(Q1) # summarize the results

# Summary of percentile-based design of joint xbar and s chart with guaranted
#in-control and out-of-control performances.
d=1.0        # Set the shift size in the process mean is 1 (small shift).
delta=2    # Set the shift size in the process dispersion is 2 (moderate shift).
Q3<- odpbc(nmax, T1, T2, hv, mat, p1, p2, d, delta, type= "Xbar-S")
summary(Q3) # summarize the results
}
\keyword{Shewhart}
\concept{control chart}
