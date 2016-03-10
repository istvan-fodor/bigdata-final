source("transform.R")
library(Matrix)
library(corrplot)

corrplot(college_pred)
college2 <- sparse.model.matrix(~., data = college_pred, na.action='na.omit')

college_pred2 <- college_pred
college_pred2 <- subset(college_pred, select = c(-tuition_program_year))
ncol(college_pred2)

college2 <- college[,5:7]
summary(college[,5:7])
x1 <- sparse.model.matrix(  ~ ., data=college_pred[,c(1:86)])
x2 <- model.matrix(  ~ ., data=college_pred[,83])
nrow(college_pred2[,1:82])
nrow(x1)
ncol(x1)
x1[,c("sat_avg")]

colnames(x1)
x <- x1[,200:206]

x1[79,]
for(i in 1:(ncol(college_pred2)-1)) {
  print(colnames(college_pred2[,c(i,i+1)]))
  x <- sparse.model.matrix( ~ ., data=college_pred2[,c(i, i+1)])
}



summary(college_pred[,"tuition_in_state"])
