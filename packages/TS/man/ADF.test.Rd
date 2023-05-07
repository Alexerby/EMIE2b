\name{ADF.test}
\alias{ADF.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performs a series Augmented Dickey-Fuller tests.
%%  ~~function to do ... ~~
}
\description{
Pfaff (2008, p. 63) describes a test strategy to discriminate between deterministic and stochastic trends. This function performs the different individual ADF tests performed by ur.df() in the package urca necessary and systematise the results presentation. AIC chooses the number of lags up to n*frequency. 
}
\usage{
ADF.test(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A univariate time series object created by ts().
}
}

\references{
B. Pfaff, 2008, Analysis of integrated and cointegrated times series with R, second edition, Springer Verlag.
}
\author{
Michael Lundholm
}

\examples{
data(nporg, package = "urca")
gnp.r <- na.omit(nporg[, "gnp.r"])
ADF.test(gnp.r)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~ADF.test }% use one of  RShowDoc("KEYWORDS")

