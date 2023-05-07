 globalVariables(c("ca.jo", "VARselect"))
 JJ.test <- function(data, test = c("trace", "eigen"), ic = c("AIC", "BIC"), extdata = NULL){
    data.name <- deparse(substitute(data))
	if (is.ts(data) == FALSE) stop("Data is not a ts-object!") else
    VS      <- function(type, data) {
               VARselect(y = data, type = type)
               }
    types   <- t(c("none", "const", "trend"))
    result1 <- apply(types, 2, VS, data)
	select  <- ifelse(ic == "AIC", 1, 3)
    result2 <- sapply(result1, function(x) x$selection[[select]])
    result3 <- list(list(types[1], result2[1]), list(types[2], result2[2]), 
                    list(types[3], result2[3]))
    TT      <- function(Type, data) {
                   if (Type[[2]] < 2) Type[[2]] <- 2  
                   TSTAT  <- rev(ca.jo(data, type = test, ecdet = Type[[1]], K = Type[[2]], dumvar = extdata)@teststat)
                   CVAL   <-     ca.jo(data, type = test, ecdet = Type[[1]], K = Type[[2]], dumvar = extdata)@cval
                   CVAL   <- CVAL[nrow(CVAL):1, 2]
				   LAMBDA <-     ca.jo(data, type = test, ecdet = Type[[1]], K = Type[[2]], dumvar = extdata)@lambda
                   list(TSTAT, CVAL, LAMBDA)
                   }
    result5 <- lapply(result3, TT, data)
    #esult6 <- cbind(result5[[1]][[1]], result5[[1]][[2]], 
    #                result5[[2]][[1]], result5[[2]][[2]],
    #                result5[[3]][[1]], result5[[3]][[2]]
    #                )
	result6none  <- round(cbind(result5[[1]][[1]], result5[[1]][[2]]), 2)
	result6const <- round(cbind(result5[[2]][[1]], result5[[2]][[2]]), 2)
	result6trend <- round(cbind(result5[[3]][[1]], result5[[3]][[2]]), 2)
	lambdanone   <- round(result5[[1]][[3]], 2)
	lambdaconst  <- round(result5[[2]][[3]], 2)
	lambdatrend  <- round(result5[[3]][[3]], 2)
	Roots  <- c( "r  = 0", paste("r <= ", 1 : (length(result5[[1]][[1]]) - 1), sep = ""))
	Rnames <- c("MODEL NONE:",  Roots,
				"MODEL CONST:", Roots,
				"MODEL TREND",  Roots)
	result7 <- rbind(c("", ""), result6none, c("", ""), result6const, c("", ""), result6trend)
	result8 <- data.frame(result7)
	attributes(result8)$row.names <- Rnames	
	attributes(result8)$names     <- c("tstat", "cval")	
    if (test == "trace") TESTTYPE <- "Johansen's tracetest." else TESTTYPE <- "Maximal eigenvalue test."
	if (ic == "AIC") IC <- "AIC." else IC <- "BIC."
	colnames(result6none)  <- c("tstat", "cval")
    colnames(result6const) <- c("tstat", "cval")
    colnames(result6trend) <- c("tstat", "cval")
    cat(rep("#", 35),'\n')
    cat(rep("#", 35),'\n')
    cat("The JOHANSEN PROCEDURE\n")
    cat(rep("#", 35),'\n')
    cat("Test: ", paste(TESTTYPE), "Data: ", paste(data.name), "\n")
    cat("Lags selected by ", paste(IC), "Level of significance: 5 %.\n")
    cat("Number of lags: none = ", result2[1], " const = ", result2[2], " trend = ", result2[3],'\n')
	cat("Eigenvalues  (none):   ",  lambdanone,'\n')
	cat("Eigenvalues (const):   ",  lambdaconst,'\n')
	cat("Eigenvalues (trend):   ",  lambdatrend,'\n')
    cat(rep("#", 35),'\n')
	cat(return(result8))
}