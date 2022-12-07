\name{odpbc}
\alias{odpbc}

\title{ Optimal design parameters of the Percentile-based control charts
}
\description{ Determine the optimal statistical design parmeters of either individual or joint Shewharts control charts for percentile-based designs (PL) approach with guaranteed in-control and out-of-control performances.
}
\usage{
odpbc(nmax,T1, T2, hv, mat, p1=0.05,p2=0.05, d=1.0, delta=1.5,
type= c("Xbar", "R", "S", "S2", "Xbar-R", "Xbar-S", "Xbar-S2"),pop.size=1000, sided="two")
}

\arguments{
  \item{nmax}{ The maximum possible sample size in each sampling interval (a numeric value).}
  \item{T1}{ The desired in-control time to signal (a numeric value).}
\item{T2}{ The desired out-of-control time to signal (a numeric value).}
\item{hv}{ The vector of intersample interval upto maximum T2 (a numeric vector of possible sample intervals).}
\item{mat}{ The matrix of minimum and maximum bounds for optimum parameters sample size and sample interval. The minimum values of n and h are (2,0.5) and maximum value are (nmax, T2).}
  \item{p1}{The probability to signal in-control from a specified number (default value is 5\%)}
  \item{p2}{The probability to signal out-of-control from a specified number (default value is 5\%)}
  \item{d}{The expected shift size in the process average in term of standrd deviation, i.e. d= |u1-uo|/delta (default value is 1.0). When the process is in-control w.r.t process average, set d=0.}
\item{delta}{ The expected shift size in the process variance (default value is 1.5). When the process is in-control w.r.t process variation, set delta=1.0}
\item{type}{ A character string specifying the type of Shewhart control chart either individual or joint.Available types are; "Xbar", "R", "S", "S2", "Xbar-R", "Xbar-S" and "Xbar-S2".}
\item{pop.size}{ Population size. This is the number of individuals genoud uses to solve the optimization problem for genetic algorithem (default value is 1000).}
\item{sided}{A character string specifying the calculation of either one-sided or two-sided control limits of Shewhart dispersion charts based on PL approach (default type is two-sided).}
}

\value{ Returns the optimal parameters of PL type control chart:
 \item{n}{The optimal sample size to design the percentile-based either individual or joint control chart.}
 \item{h}{The optimal intersampling interval to design the percentile-based either individual or joint control chart.}
 \item{k}{The optimal control chart constant/multiplier to design the percentile-based xbar control chart.}
 \item{lp}{The optimal lower percentile point of relative sample range/standard deviation/variance distribution to calculate the lower control limits of percentile-based dispersion control chart.} or/and
 \item{up}{ The optimal upper percentile point of relative sample range/standard deviation/variance distribution to calculate the lower control limits of percentile-based dispersion control chart.}
}
\references{Faraz A, Saniga E, Montgomery D. (2019). Percentile-based control charts design with an application to Shewhart Xbar and S2 control charts. Quality and Reliability Engineering International, 35(1); 116-126.
}
\author{ Aamir Saghir, Zsolt T. Kosztyan*

e-mail: kzst@gtk.uni-pannon.hu
}


\seealso{\code{\link{pbcc}},\code{\link{summary.pbcc}},\code{\link{plot.pbcc}}.}
\examples{

# Calculation of optimal parameters of the percentile-based control charts
#using "pbcc"" package.

# Set the maximum possible sample size in each h units is 10.
nmax=10

# Set the process in-control time to signal is at least 100 samples.
T1=100

# Set the control chart time to signal is at most 1 samples
# when shift occur in the process mean or/and std.dev.
T2=3

# Set the sampling intersample intervals to 0.5(0.5) T2 units of time.
hv=seq(0.5, T2, by=0.5)

#Set the lower and upper bounds of parameters(n and h) used in the optimization.
mat=matrix(c(2, nmax, 1, length(hv)), 2,2, byrow=TRUE)
p1=0.05     # Set the probability of guaranteed in-control signals is 5%.
p2=0.05     # Set the probability of guaranteed out-of-control signals is 5%.

# Optimal parameters of two-sided percentile-based Xbar control chart.

d=3             # Set the shift size in the process mean is 3 (large shift).
Q1<- odpbc(nmax, T1, T2, hv, mat, p1, p2, d, delta=1.5, type= "Xbar")

# Optimal parameters of one-sided percentile-based variance control chart.

delta=2   # Set the shift size in the process dispersion is 2 (moderate shift).
Q2<- odpbc(nmax, T1, T2, hv, mat, p1, p2, d=1.0, delta, type= "S2", sided="one")

#Optimal parameters of two-sided percentile-based joint Xbar& S control chart.
d=1.0     # Set the shift size in the process mean is 1 (small shift).
delta=2   # Set the shift size in the process dispersion is 2 (moderate shift).
Q3<- odpbc(nmax, T1, T2, hv, mat, p1, p2, d, delta, type= "Xbar-S")

}

\concept{control chart}
