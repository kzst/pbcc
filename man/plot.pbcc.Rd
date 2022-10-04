\name{plot.pbcc}
\alias{plot.pbcc}

\title{
Plot function for Percentile-based Control Charts
}
\description{
Plot function for the Package of Percentile-based Control Charts (pbcc)
}
\usage{
\method{plot}{pbcc}(x, title,...)}
\arguments{
  \item{x}{an object of class 'pbcc'.}
  \item{title}{title of the plot}
   \item{...}{other graphical parameters.}
}

\value{No return value, called for side effects}

\references{
Faraz, A., Saniga, E., Montgomery, D. (2019). Perentile-based control chart design with an application to Shewhart Xbar and S2 control charts. Quality and Reliability Engineering International, 35(1),116-126.
}
\author{
Aamir Saghir, Zsolt T. Kosztyan*

e-mail: kzst@gtk.uni-pannon.hu
}

\seealso{\code{\link{pbcc}}, \code{\link{odpbc}}, \code{\link{summary}}.}

\examples{

library(qcc)
data(pistonrings)
diameter = with(pistonrings, qcc.groups(diameter, sample))
T1=100      # Set the process in-control time to signal is at least 100 samples.
p1=0.05     # Set the probability of guaranteed in-control signals is 5%.

#Calculation of two-sided individual percentile-based Xbar for diameter data.
Q11<- pbcc(diameter,T1, p1, type="Xbar")
# Plot two-sided individual percentile-based Xbar for diameter data.
plot(Q11)

# Calculation of One-sided individual percentile-based S2  for diameter data.
Q12<- pbcc(diameter,T1, p1, type="S2", sided="one")
# # Plot one-sided individual percentile-based S2 for diameter data.
plot(Q12)

# Calculation of two-sided joint percentile-based Xbar and S  for diameter data.
Q13<- pbcc(diameter,T1, p1, type="Xbar-S", sided="two")
# Plot two-sided joint percentile-based Xbar-S control chart for diameter data.
plot(Q13)

}
\keyword{Shewhart}
\concept{control chart}
