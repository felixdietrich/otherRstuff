\name{av}
\alias{av}
What is this?
\title{
AVerage (Crossing)
}
\description{
This function evaluates whether a time-series is above/below its moving average.
}
\usage{
av(x, settings = 21, type='SMA')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data, in xts-format}
  \item{settings}{lookback parameter for moving average}
  \item{type}{options: SMA, EMA and WMA}
  \item{\dots}{...}
}
%\details{sghg}
\value{
Returns purely the signal.
}
%\references{}
\author{
Felix Dietrich
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{afgjfhk}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
