\name{pbs2}
\alias{pbs2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Percentile-based S2 control chart
%%  ~~function to do ... ~~
}
\description{ Calculate the optimal parmeters n (sample size), h (sampling inetrval) and l (percentile of the sample varaince distribution) for Statistcal Design of Percentile-based S2 control chart.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pbs2(nmax, T1, T2, hv, mat, p1=0.05, p2=0.05, delta=1.5, pop.size=1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nmax}{ The maximum possible sample size in each sampling interval (a numeric value).}
  \item{T1}{ The desired in-control time to signal (a numeric value).}
\item{T2}{ The desired out-of-control time to signal (a numeric value).}
\item{hv}{ The vector of intersample interval upto maximum T2 (a numeric vector of possible sample intervals).}
\item{mat}{ The matrix of minimum and maximum bounds for optimum parameters sample size and sample interval. The minimum values of n and h are (2,0.5) and maximum value are (nmax, T2).}
  \item{p1}{The probability to signal in-control from a specified number (default value is 5\%)}
  \item{p2}{The probability to signal out-of-control from a specified number (default value is 5\%)}
\item{delta}{ The expected shift size in the process variance (default value is 1.5). When the process is in-control w.r.t process variation, set delta=1.}
\item{pop.size}{ Population size. This is the number of individuals genoud uses to solve the optimization problem for genetic algorithem (default value is 1000).}
}

\value{
 \item{l}{ The optimal control chart constant to design the percentile-based S2 control chart.}
 \item{n }{The optimal sample size to design the percentile-based S2 control chart.}
 \item{h }{The optimal intersampling interval to design the percentile-based S2 control chart.}
  \item{Fval }{The optimal function value return by genetic algorithem ("rgenoud"" object).}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Faraz A, Saniga E, Montgomery D. (2019). Percentile-based control charts design with an application to Shewhart Xbar and S2 control charts. Quality and Reliability Engineering International, 35(1); 116-126.
%% ~put references to the literature/web site here ~
}
\author{ Aamir Saghir, Zsolt T. Kosztyan*

e-mail: kzst@gtk.uni-pannon.hu
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{pbxbar}}, \code{\link{pbxbars2}}, \code{\link{summary}}.}
\examples{

# Calculation of optimal parameters of the percentile-based S2 control chart
# using pbcc package.


# Set the possible sample size in each h units of time is up to 15.

nmax=15

# Set the process in-control time to signal is at least 100 samples.

T1=100

# Set the control chart time to signal is at most 3 samples when shift occur in the process mean.

T2=3

# Set the sampling intersample intervals to 0.5(0.5) T2 units of time.

hv=seq(0.5, T2, by=0.5)

# Set the lower and upper bounds of parameters (n and h values) which "genoud" will used
# in the optimization.
# The columns contains the parameters and the rows reprents lower and upper bound of each parameter.

mat=matrix(c(2, nmax, 1, length(hv)), 2,2, byrow=TRUE)
p1=0.10 # Set the probability of guaranteed in-control signals is 10%.
p2=0.05 # Set the probability of guaranteed out-of-control signals is 5%.
delta=3 # Set the shift size in the process variance is 3 (large shift).

# Calculation of optimal parameters of the percentile-based S2 control chart using pbcc package.

pbs2(nmax, T1, T2, hv, mat, p1, p2, delta)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{control chart}
% \concept{ ~cpt2 }
% Use only one concept per line.
