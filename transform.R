library(gamlr)
library(RSQLite)

con <- dbConnect(SQLite(), dbname="database.sqlite")

fileName <- 'read_all.sql'
query <- readChar(fileName, file.info(fileName)$size)

college <- dbGetQuery(con, query)
college_metadata <- college[,1:6]
college <- college[,-(1:6)]

college$region <- factor(college$region)
college$control <- as.factor(college$control)
college$locale <- as.factor(college$locale)
college$religious_affil <- as.factor(college$religious_affil)
college$predominant_degree <- as.factor(college$predominant_degree)
college$highest_degree <- as.factor(college$highest_degree)

college$region <- factor(college$region, levels = c(NA, levels(college$region)), exclude = NULL)
college$control <- factor(college$control, levels = c(NA, levels(college$control)), exclude = NULL)
college$locale <- factor(college$locale, levels = c(NA, levels(college$locale)), exclude = NULL)
college$predominant_degree <- factor(college$predominant_degree, levels = c(NA, levels(college$predominant_degree)), exclude = NULL)
college$highest_degree <- factor(college$highest_degree, levels = c(NA, levels(college$highest_degree)), exclude = NULL)


college$logsize <- log(1+college$size)

for (i in 1:ncol(college)) {
  if (class(college[,i]) == "integer") {
    college[,i] <- as.numeric(college[,i])
  }
}

cost_x <- college[,c("tuition_in_state", "tuition_out_of_state", "tuition_program_year", "tuition_revenue_per_fte")]
has_tuition_a_bool <- !(is.na(college$tuition_in_state) & is.na(college$tuition_out_of_state))
has_tuition_b_bool <- !is.na(college$tuition_program_year)
has_tuition_a <- ifelse(has_tuition_a_bool,1,0)
has_tuition_b <- ifelse(has_tuition_b_bool,1,0)
cost_x <- cbind(cost_x,has_tuition_a)
cost_x <- cbind(cost_x,has_tuition_b)
cost_x <- cbind(cost_x, paste(college_metadata$city, college_metadata$stabbr,sep = ","))
colnames(cost_x)[ncol(cost_x)] <- "location"
cost_x[is.na(cost_x)] <- 0 
cost_x <- sparse.model.matrix(~ has_tuition_a * (tuition_in_state + tuition_out_of_state) * location + has_tuition_b * tuition_program_year * location + tuition_revenue_per_fte * location + ., data = cost_x)
cost_y <- college[,c("cost_attendance_academic_year", "cost_attendance_program_year")]
cost_y <- ifelse(is.na(cost_y$cost_attendance_academic_year), cost_y$cost_attendance_program_year, cost_y$cost_attendance_academic_year)
fit <- cv.gamlr(cost_x[which(!is.na(cost_y)),], cost_y[which(!is.na(cost_y))], lambda.min.ratio=1e-4)
1-fit$gamlr$deviance[fit$seg.1se]/fit$gamlr$deviance[1]
#0.8345802
plot(fit)

college$cost <- ifelse(is.na(college$cost_attendance_academic_year), college$cost_attendance_program_year, college$cost_attendance_academic_year)
pred <- as.matrix(predict(fit, newdata = cost_x[which(is.na(college$cost)),]))
###########
pred_check <- predict(fit, newdata = cost_x[which(!is.na(cost_y)),])
pred_check <- as.matrix(cbind(cost_y[which(!is.na(cost_y))], pred_check))
###########
college$is_predicted_cost <- rep(0, nrow(college))
college$is_predicted_cost[which(is.na(college$cost))]=1
college$cost[which(is.na(college$cost))] = pred
which(is.na(college$cost))

college <- subset(college, select = c(-tuition_in_state,-tuition_out_of_state,-tuition_program_year,-tuition_revenue_per_fte,-cost_attendance_academic_year,-cost_attendance_program_year))
##Negative predicitions... look into predicting logs
cmin <- college[(which(college$cost < 0)),]
college$cost[which(college$cost < 0)] <- 0

college$log_cost <- log(1+college$cost)

## NA Code
nam <- college[,sapply(college,is.numeric)]
nam <- as.data.frame(is.na(nam))
nam <- subset(nam,select = c(-ten_yrs_after_entry_median))
nasmm <- sparse.model.matrix(~.,data=nam)[,-1]


length(which(is.na(college$ten_yrs_after_entry_median)))



for(col in 1:ncol(college)) {
  coldata <- college[,col]
  ##print(class(coldata))
  if (class(coldata) == "numeric") {
    ##print(col)
    indexes <- which(is.na(coldata))
    coldata[which(is.na(coldata))] <- rep(mean(coldata, na.rm = TRUE), length(indexes))
    college[,col] <- coldata
  }
}

