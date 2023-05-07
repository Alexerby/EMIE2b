\name{JJ.test}
\alias{JJ.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performs a series trace or maximal eigenvalue tests.
}
\description{
Johansen (1992, p. 391f)  describes a test strategy described in in order to identify the number of cointegrating vectors in the presence of intercept/trend. This function performs the different tests using  the function ca.jo() in package urca and systematise results presentation in order to facilitate conclusions.
}
\usage{
JJ.test(data, test = c("trace", "eigen"), ic = c("AIC", "BIC"), extdata = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A multivariate time series object created by ts().
}
  \item{test}{
The Johansen trace test or the Johansen and Juselius maximal eigenvalue test.
}
  \item{ic}{
Select information criterion AIC or BIC for choice of number of lags in the VAR model.
}
  \item{extdata}{
Any matrix of control variables of the same length as data, not including intercept or deterministic trend.
}
}

\references{
S. Johansen, 1992, Determination of cointegration rank in the presence of a linear trend, Oxford bulletin of economics and statistics 54(3), pp. 383-397.
}
\author{
Michael Lundholm
}

\examples{
data(denmark)
denmark <- ts(denmark[, 2:5], start = c(1974, 1), frequency = 4)
JJ.test(denmark, test = "trace", ic = "AIC")
JJ.test(denmark, test = "eigen", ic = "AIC")
JJ.test(denmark[, 3:4], test = "eigen", ic = "AIC", extdata = denmark[, 1:2])
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~JJ.test }% use one of  RShowDoc("KEYWORDS")

