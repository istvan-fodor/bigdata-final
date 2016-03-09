library(RSQLite)
con <- dbConnect(SQLite(), dbname="database.sqlite")

fileName <- 'read_all.sql'
query <- readChar(fileName, file.info(fileName)$size)

college <- dbGetQuery(con, query)
college$region <- as.factor(college$region)
college$control <- as.factor(college$control)
college$locale <- as.factor(college$locale)
college$carnegie <- as.factor(college$carnegie)
college$religious_affil <- as.factor(college$religious_affil)
college$predominant_degree <- as.factor(college$predominant_degree)
college$highest_degree <- as.factor(college$highest_degree)
college$logsize <- log(college$size)
college$log_tuition_in_state <- log(college$tuition_in_state) 
college_pred <- college[,-1]

sum <- summary(college)

fit <- glm( ten_yrs_after_entry_median ~ ., data = college_pred, na.action = "na.omit")

