\name{summary.pbcc}
\alias{summary.pbcc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary of Percentile-based Control Chart
}
\description{
Print summary of Percentile-based Control Chart
}
\usage{
\method{summary}{pbcc}(object, digits = getOption("digits"), ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class 'pbcc'.}
  \item{digits}{the number of significant digits to use when \code{add.stats = TRUE}.}
  \item{...}{additional arguments affecting the summary produced.}
}

\references{
Faraz, A., Saniga, E., Montgomery, D. (2019). Perentile-based control chart design with an application to Shewhart Xbar and S2 control charts. Quality and Reliability Engineering International, 35(1),116-126.
}
\author{
Aamir Saghir, Zsolt T. Kosztyan*

e-mail: kzst@gtk.uni-pannon.hu
}

\seealso{\code{\link{pbxbar}}, \code{\link{pbs2}}, \code{\link{pbxbars2}}.}

\examples{
# Summary of Percentile based design of Xbar chart

nmax=10; T1=100; T2=2
hv=seq(0.5, T2, by=0.5)
mat=matrix(c(2, nmax, 1, length(hv)), 2,2, byrow=TRUE)
PBXBAR<-pbxbar(nmax, T1, T2, hv, mat)
summary(PBXBAR) # summarize the results

# Summary of Percentile based design of s2 chart

nmax=10; T1=100; T2=2
hv=seq(0.5, T2, by=0.5)
mat=matrix(c(2, nmax, 1, length(hv)), 2,2, byrow=TRUE)
PBS2<-pbs2(nmax, T1, T2, hv, mat)
summary(PBS2) # summarize the results

# Summary of Percentile based design of joint xbar and s2 chart

nmax=10; T1=100; T2=2
hv=seq(0.5, T2, by=0.5)
mat=matrix(c(2, nmax, 1, length(hv)), 2,2, byrow=TRUE)
PBXBARS2<-pbxbars2(nmax, T1, T2, hv, mat)
summary(PBXBARS2) # summarize the results
}
\keyword{Shewhart}
\concept{control chart}
