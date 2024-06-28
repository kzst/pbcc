\name{odpbc_binom}
\alias{odpbc_binom}

\title{ Optimal design parameters of the Percentile-based attribute control charts
}
\description{ Determine the optimal statistical design parmeters of percentile-based p-chart.
}
\usage{
odpbc_binom(nmax, T1, T2, hv, mat, ip, sp, p1=0.05, p2=0.05, pop.size = 1000)
}

\arguments{
  \item{nmax}{ The maximum possible sample size in each sampling interval (a numeric value).}
  \item{T1}{ The desired in-control time to signal (a numeric value).}
\item{T2}{ The desired out-of-control time to signal (a numeric value).}
\item{hv}{ The vector of intersample interval upto maximum T2 (a numeric vector of possible sample intervals).}
\item{mat}{The matrix of minimum and maximum bounds for optimum parameters sample size and sample interval. The minimum values of n and h are (2,0.5) and maximum value are (nmax, T2).However minimun sample size should be selected according to recommended no}
\item{ip}{In-control value of monitoring parameter  (a numeric value between 0 and 1)}
\item{sp}{Shifted-value of monitoring parameter (a numeric value between 0 and 1)}
\item{p1}{The probability to signal in-control from a specified number (default value is 5\%)}
\item{p2}{The probability to signal out-of-control from a specified number (default value is 5\%)}
\item{pop.size}{ Population size. This is the number of individuals genoud uses to solve the optimization problem for genetic algorithem (default value is 1000).}
}

\value{ Returns the optimal parameters of PL type control chart:
 \item{n}{The optimal sample size to design the percentile-based p-chart.}
 \item{h}{The optimal intersampling interval to design the percentile-based p-chart.}
 \item{k}{The optimal control chart constant/multiplier to design the percentile p-chart}
}
\references{Faraz A, Saniga E, Montgomery D. (2019). Percentile-based control charts design with an application to Shewhart Xbar and S2 control charts. Quality and Reliability Engineering International, 35(1); 116-126.
}
\author{ Aamir Saghir, Khan Zahid, Zsolt T. Kosztyan*

e-mail: kosztyan.zsolt@gtk.uni-pannon.hu
}


\seealso{\code{\link{pbcc}},\code{\link{summary.pbcc}},\code{\link{plot.pbcc}},\code{\link{odpbc}}.}
\examples{

# set maximum sample size

nmax=500

# Set the process in-control time to signal is at least 100 samples.

T1= 100

# Set the out-of-control chart time to signal is at most 1 sample
T2=3
# Set the sampling intersample intervals to 0.5(0.5) T2 units of time.
hv=seq(0.5, T2, by=0.5)

# Set the probability of guaranteed in-control signal 5%/10%/15%
p1=0.05

#Set the probability of guaranteed out-of-control signals 5%/10%/15%
p2=0.05

#Set the lower and upper bounds of parameters(n and h) used in the optimization.

# 81 minimum value is taken to hold the normality condition np>=5 or n(1-p)>=5

mat=matrix(c(2, nmax, 1, length(hv)), 2,2, byrow=TRUE)

# In-control value of monitoring parameter p
ip=0.10

# out of control value of monitoring parameter p
sp=0.15

output<-odpbc_binom(nmax, T1, T2, hv, mat, ip, sp, p1, p2, pop.size = 1000)

print(output)

}

\concept{control chart}
