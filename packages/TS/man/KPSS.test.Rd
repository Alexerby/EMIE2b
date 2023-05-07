\name{KPSS.test}
\alias{KPSS.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs KPSS test.
%%  ~~function to do ... ~~
}
\description{
Performs two KPSS test for intercept (mu) and trend (tau) and allows the user to directly compare the results with a more convenient presentation of results than the underlaying test function ur.kpss() from package urca. Allows the user to change the number of lags used. 
}
\usage{
KPSS.test(data, use.lag = NULL, lags = c("short", "long", "nil"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A univariate time series object created by ts().
}
  \item{use.lag}{Maximum number of lags used for error term correction.}
  \item{lags}{User specified number of lags.}
}


\author{
Michael Lundholm
}

\examples{
KPSS.test(JohnsonJohnson)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~ADF.test }% use one of  RShowDoc("KEYWORDS")

