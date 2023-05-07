LjungBox <-
function (obj, lags = seq(5, 30, 5), order = 0, season = 1, squared.residuals = FALSE) 
{
    GetResiduals <- function (obj) 
   {
       class.obj = class(obj)[1]
       if (class.obj != "ar" && class.obj != "arima0" && class.obj != 
           "Arima" && class.obj != "varest" && class.obj != "ARIMA" && 
           class.obj != "lm" && class.obj != "glm" && class.obj != 
           "list") 
           stop("obj must be class ar, arima0, Arima, (ARIMA forecast_ARIMA Arima), varest, lm, (glm lm), or list")
       if (all(class.obj == "ar")) {
           order <- obj$order
           res <- ts(as.matrix(obj$resid)[-(1:order), ])
       }
       else if (all(class.obj == "arima0") || all(class.obj == "Arima") || 
           all(class.obj == "ARIMA")) {
           pdq <- obj$arma
           p <- pdq[1]
           q <- pdq[2]
           ps <- pdq[3]
           qs <- pdq[4]
           order <- p + q + ps + qs
           res <- ts(obj$residuals)
       }
       else if (all(class.obj == "varest")) {
           order <- obj$p
           res <- resid(obj)
       }
       else if (all(class.obj == "list")) {
           order <- obj$order
           if (is.null(order)) 
               order <- 0
           else order <- order
           res <- obj$res
       }
       if (all(class.obj == "lm") || all(class.obj == "glm")) {
           order <- 0
           res <- obj$residuals
       }
       return(list(order = order, res = res))
   }
	
    class.obj = class(obj)[1]
    TestType <- "0"
    if (class.obj == "ts" || class.obj == "numeric" || class.obj == 
        "matrix" || class.obj == "mts") 
        TestType <- "1"
    if (class.obj == "ar" || class.obj == "arima0" || class.obj == 
        "Arima" || class.obj == "ARIMA" || class.obj == "varest" || 
        class.obj == "lm" || class.obj == "glm" || class.obj == 
        "list") 
        TestType <- "2"
    if (TestType == "0") 
        stop("obj must be class ar, arima0, Arima, (ARIMA forecast_ARIMA Arima), varest, lm, (glm lm), ts, numeric, matrix, (mts ts), or list")
    Maxlag <- max(lags)
    if (TestType == "1") 
        res <- as.ts(obj)
    else {
        GetResid <- GetResiduals(obj)
        res <- GetResid$res
        order <- GetResid$order
    }
    if (squared.residuals) 
        res <- res^2
    n <- NROW(res)
    k <- NCOL(res)
    if (Maxlag * season >= n) 
        stop("Maximum value of arguments lags * season can't exceed n!")
    df <- k^2 * (lags - order)
    NegativeDF <- which(df < 0)
    df[NegativeDF] <- 0
    Accmat <- stats::acf(res, lag.max = Maxlag * season, plot = FALSE, 
        type = "correlation")$acf
    inveseR0 <- solve(Accmat[1, , ])
    prodvec <- numeric(Maxlag * season)
    for (l in 1:Maxlag) {
        tvecR <- t(as.vector(Accmat[l * season + 1, , ]))
        prodvec[l] <- 1/(n - l) * crossprod(t(tvecR), crossprod(t(kronecker(inveseR0, 
            inveseR0)), t(tvecR)))
    }
    Q <- n * (n + 2) * cumsum(prodvec)
    STATISTIC <- Q[lags]
    PVAL <- 1 - stats::pchisq(STATISTIC, df)
    PVAL[NegativeDF] <- NA
    summary <- matrix(c(lags, STATISTIC, df, PVAL), ncol = 4)
    dimnames(summary) <- list(rep("", length(STATISTIC)), c("lags", 
        "statistic", "df", "p-value"))
    return(summary)
}
