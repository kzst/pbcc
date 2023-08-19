\name{pbcc}
\alias{pbcc}
\alias{pbcc}
\title{Percentile-based control charts
}
\description{Create an object of class \code{'pbcc'} to perform statistical quality control based on percentile-based designs (PL) approach. This object may then be used to plot percentile-based Shewhart charts either individual or joint, summary statistics and more with guaranted in-control performace.
}
\usage{
pbcc(data,T1, p1, type=c("Xbar", "R", "S", "S2",
  "Xbar-R","Xbar-S","Xbar-S2"), sided="two",
  k=NULL, lp=NULL, up=NULL)
}
\arguments{
\item{data}{ A data frame, a matrix or a vector containing observed data for
  the variable to chart. Each row of a data frame or a matrix, and each
  value of a vector, refers to a sample or ''rationale group''.}
\item{T1}{ The desired in-control time to signal (a numeric value, defualt value is 100).}
\item{p1}{The probability to signal in-control from a specified number (a numeric value, default value is 5\%)}
\item{type}{ A character string specifying the type of Shewhart control chart either individual or joint.Available types are; "Xbar", "R", "S", "S2", "Xbar-R", "Xbar-S" and "Xbar-S2".}
\item{sided}{A character string specifying the calculation of either one-sided or two-sided control limits of Shewhart dispersion charts based on PL approach (default type is two-sided).}
\item{k}{The optimal control chart constant/multiplier to design the percentile-based X-bar control chart either individually or jointly.}
\item{lp}{The optimal lower percentile point to calculate the lower control limits of the percentile-based dispersion control chart. or/and}
\item{up}{The optimal upper percentile point to calculate the lower control limits of the percentile-based dispersion control chart.}
}

\value{ Returns an object of class \code{'pbcc'} with "data.name", "type" and values of "sizes", "std.dev", "statistics" and "control limits" of individual or joint Shewhart control chart.
}
\references{Faraz A, Saniga E, Montgomery D. (2019). Percentile-based control charts design with an application to Shewhart Xbar and S2 control charts. Quality and Reliability Engineering International, 35(1); 116-126.
}
\author{ Aamir Saghir, Zsolt T. Kosztyan*

e-mail: kzst@gtk.uni-pannon.hu
}

\seealso{\code{\link{odpbc}}, \code{\link{summary}},  \code{\link{plot}}.}
\examples{

#Calculation of the percentile-based control charts using "pbcc"" package for
# "diameter" of pistonrings data.
library(qcc)
data(pistonrings)
diameter = with(pistonrings, qcc.groups(diameter, sample))
T1=100      # Set the process in-control time to signal is at least 100 samples.
p1=0.05     # Set the probability of guaranteed in-control signals is 5%.

#Calculation of two-sided individual percentile-based Xbar control chart
#for diameter data.
Q11<- pbcc(diameter,T1, p1, type="Xbar")

# Calculation of One-sided individual percentile-based S2 control chart
# for diameter data.
Q12<- pbcc(diameter,T1, p1, type="S2", sided="one")

# Calculation of two-sided joint percentile-based Xbar and S control chart
#for diameter data.
Q13<- pbcc(diameter,T1, p1, type="Xbar-S", sided="two")

}
\concept{control chart}
