set.seed(41201)

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
college$carnegie_basic <- as.factor(college$carnegie_basic)
college$carnegie_undergrad <- as.factor(college$carnegie_undergrad)
college$carnegie_size_setting <-  as.factor(college$carnegie_size_setting)
college$religious_affil <- as.factor(college$religious_affil)
college$religious_affil <- relevel(college$religious_affil, ref = "None")
college$predominant_degree <- as.factor(college$predominant_degree)
college$highest_degree <- as.factor(college$highest_degree)

college$region <- factor(college$region, levels = c(NA, levels(college$region)), exclude = NULL)
college$control <- factor(college$control, levels = c(NA, levels(college$control)), exclude = NULL)
college$locale <- factor(college$locale, levels = c(NA, levels(college$locale)), exclude = NULL)
college$predominant_degree <- factor(college$predominant_degree, levels = c(NA, levels(college$predominant_degree)), exclude = NULL)
college$highest_degree <- factor(college$highest_degree, levels = c(NA, levels(college$highest_degree)), exclude = NULL)
college$carnegie_basic <- factor(college$carnegie_basic,levels=c(NA,levels(college$carnegie_basic)),exclude = NULL)
college$carnegie_undergrad <- factor(college$carnegie_undergrad,levels=c(NA,levels(college$carnegie_undergrad)),exclude = NULL)
college$carnegie_size_setting <- factor(college$carnegie_size_setting,levels=c(NA,levels(college$carnegie_size_setting)),exclude = NULL)

college$log_size <- log(1+college$size)
college$log_instructional_expenditure_per_fte <- log(1+college$instructional_expenditure_per_fte)
college$log_faculty_salary <- log(1+college$faculty_salary)
college$log_median_debt_suppressed_completers_overall <- log(1+college$median_debt_suppressed_completers_overall)

college <- subset(college, select = c(-size, -instructional_expenditure_per_fte, -faculty_salary, -median_debt_suppressed_completers_overall))

for (i in 1:ncol(college)) {
  if (class(college[,i]) == "integer") {
    college[,i] <- as.numeric(college[,i])
  }
}

cost_x <- college[,c("tuition_in_state", "tuition_out_of_state", "tuition_program_year", "tuition_revenue_per_fte")]
cost_x <- data.frame(sapply(cost_x, function (x) log(1+x)))
has_tuition_a_bool <- !(is.na(college$tuition_in_state) & is.na(college$tuition_out_of_state))
has_tuition_b_bool <- !is.na(college$tuition_program_year)
is_academic_year_institution <- ifelse(has_tuition_a_bool,1,0)
is_program_year_institution <- ifelse(has_tuition_b_bool,1,0)
cost_x <- cbind(cost_x,is_academic_year_institution, is_program_year_institution)
cost_x <- cbind(cost_x, paste(college_metadata$city, college_metadata$stabbr,sep = ","))
#cost_x <- cbind(cost_x, college$locale, college$year, college$region)
colnames(cost_x)[ncol(cost_x)] <- "location"
cost_x[is.na(cost_x)] <- 0 
cost_x <- sparse.model.matrix(~ is_academic_year_institution * (tuition_in_state + tuition_out_of_state) * location + is_program_year_institution * tuition_program_year * location + tuition_revenue_per_fte * location + ., data = cost_x)
cost_y <- college[,c("cost_attendance_academic_year", "cost_attendance_program_year")]
cost_y <- ifelse(is.na(cost_y$cost_attendance_academic_year), log(1+cost_y$cost_attendance_program_year), log(1+cost_y$cost_attendance_academic_year))
#Model size
ncol(cost_x)
# 29184
nrow(cost_x[which(!is.na(cost_y)),])
# 11716

fit <- cv.gamlr(cost_x[which(!is.na(cost_y)),], cost_y[which(!is.na(cost_y))], lambda.min.ratio=1e-4, nfold = 10, verb = TRUE)
names(sort(fit$gamlr$beta[,fit$seg.1se], decreasing = TRUE))[1:20]
1-fit$gamlr$deviance[fit$seg.1se]/fit$gamlr$deviance[1]
#seg44 
#0.6851853
1-fit$gamlr$deviance[fit$seg.min]/fit$gamlr$deviance[1]
#seg64 
#0.84871 
plot(fit)
length(which(fit$gamlr$beta[,fit$seg.min] > 0))
#min coefficient count: 2066
length(which(fit$gamlr$beta[,fit$seg.1se] > 0))
#1se coefficient count: 377
college$logcost <- ifelse(is.na(college$cost_attendance_academic_year), log(1+college$cost_attendance_program_year), log(1+college$cost_attendance_academic_year))
pred <- as.matrix(predict(fit, newdata = cost_x[which(is.na(college$logcost)),], select="1se"))
######  Check in-sample predictions ##########
pred_check <- predict(fit, newdata = cost_x[which(!is.na(cost_y)),], select = "1se")
pred_check <- as.matrix(cbind(cost_y[which(!is.na(cost_y))], pred_check))
bad_records <- rownames(pred_check[(which(abs(pred_check[,1] - pred_check[,2]) > 5)),])
print(paste("Number of colleges with residuals greater than 5: ",length(bad_records)))
print(college[bad_records,c("tuition_in_state", "tuition_out_of_state", "tuition_program_year", "tuition_revenue_per_fte", "cost_attendance_academic_year", "cost_attendance_program_year")])
print(college[rownames(pred_check[(which(abs(pred_check[,1] - pred_check[,2]) > 5)),]),c("tuition_in_state", "tuition_out_of_state", "tuition_program_year", "tuition_revenue_per_fte", "cost_attendance_academic_year", "cost_attendance_program_year")])
#Tuition is 10k+ but cost is 0, this is probably bad data, so we remove from our display
pred_check <- pred_check[!rownames(pred_check) %in% bad_records,]
plot(pred_check[,1], pred_check[,2],
     xlab = "In-sample Log(Cost)", ylab = "Log(Cost) Prediction", main = "Predicting Cost",
     xlim = c(7.3,12), ylim = c(7.3,12))
abline(0, 1, col = "red")

#Slight 
print(college_metadata[bad_records,])
###########
college$is_predicted_cost <- rep(0, nrow(college))
college$is_predicted_cost[which(is.na(college$logcost))]=1
college$logcost[which(is.na(college$logcost))] = pred
which(is.na(college$logcost))


college <- subset(college, select = c(-tuition_in_state,-tuition_out_of_state,-tuition_program_year,-tuition_revenue_per_fte,-cost_attendance_academic_year,-cost_attendance_program_year))

## NA Code
nam <- college[,sapply(college,is.numeric)]
nam <- as.data.frame(is.na(nam))
nam <- subset(nam,select = c(-ten_yrs_after_entry_median))
nasmm <- sparse.model.matrix(~.,data=nam)[,-1]

nasmm[,"logcostTRUE"] <- college$is_predicted_cost
college <- subset(x = college, select = c(-is_predicted_cost))
nasmm <- nasmm[,-ncol(nasmm)]

length(which(is.na(college$ten_yrs_after_entry_median)))

college_explore <- cbind(college_metadata, college)

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
