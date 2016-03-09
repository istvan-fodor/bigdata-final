library(RSQLite)
con <- dbConnect(SQLite(), dbname="database.sqlite")

fileName <- 'read_all.sql'
query <- readChar(fileName, file.info(fileName)$size)

college <- dbGetQuery(con, query)
college$region <- factor(college$region, levels = c(NA, levels(college$region)))
college$control <- factor(college$control, levels = c(NA, levels(college$control)))
college$locale <- factor(college$locale, levels = c(NA, levels(college$locale)))
college$carnegie <- factor(college$carnegie, levels = c(NA, levels(college$carnegie)))
college$religious_affil <- factor(college$religious_affil, levels = c(NA, levels(college$religious_affil)))
college$predominant_degree <- factor(college$predominant_degree, levels = c(NA, levels(college$predominant_degree)))
college$highest_degree <- factor(college$highest_degree, levels = c(NA, levels(college$highest_degree)))
college$logsize <- log(1+college$size)
college$log_tuition_in_state <- log(1+college$tuition_in_state) 


for (i in 1:ncol(college)) {
  if (class(college[,i]) == "integer") {
    college[,i] <- as.numeric(college[,i])
  }
}

college_pred <- college[,-1]
