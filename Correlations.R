qDat <- read.table("qDAT2.txt", header = TRUE, fill = TRUE)
attach(qDat)

# check the first few rows of data
head(qDat)

qDat$zone <- NULL
qDat$data <- NULL
qDat$cat <- NULL
qDat$seen <- NULL
qDat$cullinvolve <- NULL
qDat$habmgmt <- NULL
qDat$ euroland<- NULL
qDat$survey <- NULL
qDat$cullland2 <- NULL
qDat$culland1 <- NULL

qDat$cons[qDat$cons==2] <- 1


## Function for pairwise correlations across a data frame

cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}

qDat.corr <- as.data.frame(cor.test.p(qDat))

# write.csv(qDat.corr, file="qDAT2corr.csv")

# Variance Inflation Factors

library(usdm)
vif(qDat)
