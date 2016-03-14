source("transform.R")

## Create Y-variable
y <- college[,"ten_yrs_after_entry_median"]

## Create X-variable and X-variable SMM
x <- subset(college,select=c(-ten_yrs_after_entry_median))
xsmm <- sparse.model.matrix(~.,data=x)[,-1]

## Create Principal Components
xpca <- x[,sapply(x,is.numeric)]
pca <- prcomp(xpca,scale=TRUE)
loadings <- pca$rotation[,1:5]
xpca <- predict(pca)

## PCA Analysis
## Top 50 factor loadings for PCA #1, #2 & #3
loadings[order(abs(loadings[,1]),decreasing=TRUE)[1:50],1]
loadings[order(abs(loadings[,2]),decreasing=TRUE)[1:50],2]
loadings[order(abs(loadings[,3]),decreasing=TRUE)[1:50],3]
loadings[order(abs(loadings[,5]),decreasing=TRUE)[1:50],5]

## Top 50 Universities Associated with PCA #1, #2 & #3
college_metadata[order(pca$x[,1],decreasing=TRUE)[1:50],1]
college_metadata[order(pca$x[,2],decreasing=TRUE)[1:50],1]
college_metadata[order(pca$x[,3],decreasing=TRUE)[1:50],1]
college_metadata[order(pca$x[,5],decreasing=TRUE)[1:50],1]

## Lasso and CV Lasso Regression
cv.reg <- cv.gamlr(cBind(xsmm,nasmm,xpca),y,lmr=1e-4)
reg <- gamlr(cBind(xsmm,nasmm,xpca),y,lmr=1e-4)

## Plot Lasso and view Coefficients
plot(cv.reg)
coef(cv.reg)
