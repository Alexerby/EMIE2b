\name{LjungBox}
\alias{LjungBox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ljung and Box Portmanteau Test
}
\description{
The Ljung-Box (1978) modified portmanteau test. In the
     multivariate time series, this test statistic is asymptotically
     equal to `Hosking'.
	 
	 The function, together with functions GetResiduals on which it depends, was originally a part of the R package portes, which was archived on 2022-05-23 as it elsewhere uses archived package `FitAR' unconditionally. The code of both functions was written by Esam Mahdi and A.I. McLeod and is used here with no change. Only the code and help text for the function LjungBox are included here, although the examples are truncated.
}
\usage{
LjungBox(obj,lags=seq(5,30,5),order=0,season=1,squared.residuals=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{A univariate or multivariate series with class `'"numeric"',
          `"matrix"', `"ts"', or `("mts" "ts")'.  It can be also an
          object of fitted time-series model with class `"ar"',
          `"arima0"', `"Arima"', `("ARIMA forecast ARIMA Arima")',
          `"lm"', `("glm" "lm")', or `"varest"'.  `obj' may also an
          object with class `"list"' (see details and following
          examples).

}
  \item{lags}{
Vector of lag auto-cross correlation coefficients used for
          `Hosking' test.
}
  \item{order}{
Default is zero for testing the randomness of a given
          sequence with class `"numeric"', `"matrix"', `"ts"', or
          `("mts" "ts")'.  In general `order' equals to the number of
          estimated parameters in the fitted model.  If `obj' is an
          object with class `"ar"', `"arima0"', `"Arima"', `"varest"',
          `("ARIMA forecast ARIMA Arima")', or `"list"' then no need to
          enter the value of `order' as it will be automatically
          determined.  For `obj' with other classes, the `order' is
          needed for degrees of freedom of asymptotic chi-square
          distribution.
}
  \item{season}{Seasonal periodicity for testing seasonality. Default is 1
          for testing the non seasonality cases.
  }
  \item{squared.residuals}{If `TRUE' then apply the test on the squared values.
          This checks for Autoregressive Conditional Heteroscedastic,
          `ARCH', effects.  When `squared.residuals = FALSE', then
          apply the test on the usual residuals.
  }
}
\details{However the portmanteau test statistic can be applied directly on
     the output objects from the built in `R' functions `ar()',
     `ar.ols()', `ar.burg()', `ar.yw()', `ar.mle()', `arima()',
     `arim0()', `Arima()', `auto.arima()', `lm()', `glm()', and
     `VAR()', it works with output objects from any fitted model.  In
     this case, users should write their own function to fit any model
     they want, where they may use the built in `R' functions
     `FitAR()', `garch()', `garchFit()', `fracdiff()', `tar()', etc.
     The object `obj' reprsesents the output of this function.  This
     output must be a list with at least two outcomes: the fitted
     residual and the order of the fitted model (`list(res = ..., order
     = ...)').  See the following example with the function
     `FitModel()'.

     Note: In `stats' `R', the function `Box.test' was built to compute
     the Box and Pierce (1970) and Ljung and Box (1978) test statistics
     only in the univariate case where we can not use more than one
     single lag value at a time. The functions `BoxPierce' and
     `LjungBox' are more accurate than `Box.test' function and can be
     used in the univariate or multivariate time series at vector of
     different lag values as well as they can be applied on an output
     object from a fitted model described in the description of the
     function `BoxPierce'.
}

\value{The Ljung and Box test statistic with the associated p-values for
     different lags based on the asymptotic chi-square distribution
     with `k^2(lags-order)' degrees of freedom.}
\references{
 Ljung, G.M. and Box, G.E.P (1978). "On a Measure of Lack of Fit in
     Time Series Models".  Biometrika, 65, 297-303.
}
\author{
 Esam Mahdi and A.I. McLeod.
}

\examples{
x <- rnorm(100)
     LjungBox(x)                              ## univariate test
     x <- cbind(rnorm(100),rnorm(100))
     LjungBox(x)                              ## multivariate test      
     ##
     ##
     ## Annual flow of the river Nile at Aswan - 1871 to 1970
     fit <- arima(Nile, c(1, 0, 1))
     lags <- c(5, 10, 20)
     ## Apply the univariate test statistic on the fitted model 
     LjungBox(fit, lags)            ## Correct (no need to specify order) 
     LjungBox(fit, lags, order = 2) ## Correct 
     ## Apply the test statistic on the residuals and set order = 2 
     res <- resid(fit)
     LjungBox(res, lags)             ## Wrong (order is needed!)  
     LjungBox(res, lags, order = 2)  ## Correct 
     ##
     ##

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~JJ.test }% use one of  RShowDoc("KEYWORDS")
