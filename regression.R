source("transform.R")

## Create Y-variable
y <- log(1+college[,"ten_yrs_after_entry_median"])

## Create X-variable and X-variable SMM
x <- subset(college,select=c(-ten_yrs_after_entry_median))
xsmm <- sparse.model.matrix(~.,data=x)[,-1]

## Create Principal Components
xpca <- x[,sapply(x,is.numeric)]
pca <- prcomp(xpca,scale=TRUE)
loadings <- pca$rotation
xpca <- predict(pca)

## PCA Analysis

## Plot
plot(xpca[,1:2],pch=21,bg=c(1,3,4,5,1,1,1,1,1,6,1,1,1,1,1)[college$carnegie_undergrad],main="")
# legend("bottomleft",
#       legend = c("FT,4y,incl", "FT,4y,selective,high trans", 
#                   "FT,4y,selective,high trans", "MedFT,4y,inclusive"),
#        fill = c(3, 4, 5, 6),
#        bty="n")
plot.new()
legend("center",
       legend = c("Full-time four-year, inclusive", "Full-time four-year, more selective, higher transfer-in", 
                  "Full-time four-year, more selective, lower transfer-in", "Medium full-time four-year, inclusive"),
       fill = c(3, 4, 5, 6),
       bty="n")
## 3:green (Full-time four-year, inclusive)
## 4:blue (Full-time four-year, more selective, higher transfer-in)
## 5:teal (Full-time four-year, more selective, lower transfer-in)
## 6:purple (Medium full-time four-year, inclusive)

plot(pca,xlab="Principal Components",main="")

## Top 50 factor loadings for PCA #1, #2 & #3
pca1 <- loadings[order(abs(loadings[,1]),decreasing=TRUE)[1:50],1]
pca2 <- loadings[order(abs(loadings[,2]),decreasing=TRUE)[1:50],2]
pca3 <- loadings[order(abs(loadings[,3]),decreasing=TRUE)[1:50],3]
pca4 <- loadings[order(abs(loadings[,4]),decreasing=TRUE)[1:50],4]
pca5 <- loadings[order(abs(loadings[,5]),decreasing=TRUE)[1:50],5]
loadings[order(abs(loadings[,93]),decreasing=TRUE)[1:50],93]

## Export PCA Loadings
## write.table(pca1,"pca1.csv",sep=",")
## write.table(pca2,"pca2.csv",sep=",")

## Top 50 Universities Associated with PCA #1, #2 & #3
college_metadata[order(pca$x[,1],decreasing=TRUE)[1:50],1]
college_metadata[order(pca$x[,2],decreasing=TRUE)[1:50],1]
college_metadata[order(pca$x[,3],decreasing=TRUE)[1:50],1]
college_metadata[order(pca$x[,4],decreasing=TRUE)[1:50],1]
college_metadata[order(pca$x[,5],decreasing=TRUE)[1:50],1]
college_metadata[order(pca$x[,93],decreasing=TRUE)[1:50],1]

## Lasso and CV Lasso Regression
set.seed(41201)
cv.reg <- cv.gamlr(cBind(xsmm,nasmm,xpca),y,lmr=1e-4, nfold = 50)
length(which(cv.reg$gamlr$beta[,cv.reg$seg.1se] != 0))
coeffs <- coef(cv.reg)[which(coef(cv.reg) != 0),]
for (i in 1:length(which(as.matrix(coef(cv.reg)) != 0))) {
  print(paste(names(coeffs)[i],"=", round(coeffs[i], digits = 4)))    
}
## Plot Lasso and write coefficients to file
plot(cv.reg)
sink(file = "CV.gamlr.coeffs.txt")
print(round(coef(cv.reg), digits = 4))
sink()
#Lambda
cv.reg$lambda.1se
#0.02818837
cv.reg$lambda.min
#0.001192155

cv.reg.pc <- cv.gamlr(cBind(xpca,xsmm,nasmm),y,lmr=1e-4,free=1:ncol(xpca) ,verb = TRUE)
print(round(coef(cv.reg.pc), digits = 4))



