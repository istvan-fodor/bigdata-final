## Create Y-variable
y <- college[,"ten_yrs_after_entry_median"]

## Create X-variable and X-variable SMM
x <- subset(college,select=c(-ten_yrs_after_entry_median))
xsmm <- sparse.model.matrix(~.,data=x)[,-1]

## Lasso and CV Lasso Regression
cv.reg <- cv.gamlr(cBind(xsmm,nasmm),y,lmr=1e-4)
reg <- gamlr(cBind(xsmm,nasmm),y,lmr=1e-4)

## Plot Lasso and view Coefficients
plot(reg)
coef(reg)
