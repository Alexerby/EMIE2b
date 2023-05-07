 globalVariables(c("ur.df"))
 KPSS.test <- function(data, use.lag = NULL, lags = c("short", "long", "nil")){
     data.name <- deparse(substitute(data))
     KPSS <- function(type, data, USE.LAG = use.lag, LAGS = lags) {
             result1A <- ur.kpss(data, type, use.lag = USE.LAG, lags = LAGS)
             result2 <- list(result1A@teststat,
                             result1A@cval,
                             result1A@lag)
#             round(result2, 2)
             }
     types   <- c("mu", "tau")
     result3 <- apply(t(types), 2, KPSS, data)
     cat(rep("#", 20),                                 "\n")
     cat(rep("#", 20),                                 "\n")
     cat("KPSS-test. ", "Data: ", paste(data.name),    "\n")
     cat(rep("#", 20),                                 "\n")
     cat("Type: ", "mu ", "tau ",                      "\n")
     cat("Lags: ", result3[[1]][[3]][[1]][1], "  ", 
                   result3[[2]][[3]][[1]][1],     "\n")
     cat(rep("#", 20),'\n')
     result5 <- rbind(result3[[1]][[1]][1],
                      result3[[1]][[2]][4],
                      result3[[1]][[2]][2],
                      result3[[1]][[2]][1]
 #                    ,result3[[1]][[4]]
					 )
     result6 <- rbind(result3[[2]][[1]][1],
                      result3[[2]][[2]][4],
                      result3[[2]][[2]][2],
                      result3[[2]][[2]][1]
#                     ,result3[[2]][[4]]
					  )
     result7 <- cbind(result5, result6)
     result8 <- round(result7, 2)
     rownames(result8) <- c("Test statistic:", "1 %:", "5 %:", "10 %:")
	 colnames(result8) <- c("Type: mu", "Type: tau")
     result8
 }

