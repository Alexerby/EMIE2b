 globalVariables(c("ur.df", "Box.test", "jarque.bera.test"))
 ADF.test <- function (data) {
         ADF <- function(type, data) {
         result1 <- ur.df(data, type = type, lags = 3 * frequency(data), 
             selectlags = "AIC")
         DETERM <- ifelse(type == "trend", 2, ifelse(type == "drift", 
             1, 0))
         LAGS <- length(coefficients(result1@testreg)[, 1]) - 
             DETERM - 1
         P.VALUE1 <- result1B <- Box.test(result1@res, 
			            lag = LAGS + DETERM + 1 + floor(0.75*length(result1@res)^(1/3)),
						type = "Ljung-Box", fitdf = LAGS + DETERM + 1)$p.value
         P.VALUE2 <- jarque.bera.test(result1@res)$p.value
         result2  <- cbind(t(result1@teststat), result1@cval, 
 		                   coefficients(result1@testreg)["z.lag.1", 1], LAGS, P.VALUE1, P.VALUE2)
         round(result2, 2)
     }
     types <- c("trend", "drift", "none")
     result3 <- apply(t(types), 2, ADF, data)
     cat(rep("#", 20), "\n")
     cat(rep("#", 20), "\n")
     cat("Augmented Dickey--Fuller test\n")
     cat(rep("#", 20), "\n")
	 result4 <- cbind(result3[[1]][1, 5:8], result3[[2]][1, 5:8], result3[[3]][1, 5:8])
	 result4[1, ] <- result4[1, ] + 1
	 colnames(result4) <- types
	 rownames(result4) <- c("AR1:", "Lags:", "L-B p-value:", "J-B p-value:")
     cat(rep("#", 20), "\n")
     result5 <- rbind(result3[[1]][c(1, 3), 1:4], result3[[2]][1:2, 
         1:4], result3[[3]][1:4])
     rownames(result5)[5] <- "tau1"
     result6 <- list(result4, result5)
	 names(result6) <- c("Analysis", "Test statistics and critical values")
	 result6
 }