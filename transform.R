library(RSQLite)
con <- dbConnect(SQLite(), dbname="database.sqlite")

fileName <- 'read_all.sql'
query <- readChar(fileName, file.info(fileName)$size)

college <- dbGetQuery(con, query)
college$region <- factor(college$region)
college$control <- as.factor(college$control)
college$locale <- as.factor(college$locale)
college$carnegie <- as.factor(college$carnegie)
college$religious_affil <- as.factor(college$religious_affil)
college$predominant_degree <- as.factor(college$predominant_degree)
college$highest_degree <- as.factor(college$highest_degree)

college$region <- factor(college$region, levels = c(NA, levels(college$region)), exclude = NULL)
college$control <- factor(college$control, levels = c(NA, levels(college$control)), exclude = NULL)
college$locale <- factor(college$locale, levels = c(NA, levels(college$locale)), exclude = NULL)
college$carnegie <- factor(college$carnegie, levels = c(NA, levels(college$carnegie)), exclude = NULL)
college$religious_affil <- factor(college$religious_affil, levels = c(NA, levels(college$religious_affil)), exclude = NULL)
college$predominant_degree <- factor(college$predominant_degree, levels = c(NA, levels(college$predominant_degree)), exclude = NULL)
college$highest_degree <- factor(college$highest_degree, levels = c(NA, levels(college$highest_degree)), exclude = NULL)

college$logsize <- log(1+college$size)
college$log_tuition_in_state <- log(1+college$tuition_in_state) 


college_pred <- college[,-1]

for (i in 1:ncol(college)) {
  if (class(college[,i]) == "integer") {
    college[,i] <- as.numeric(college[,i])
  }
}


for(col in 1:ncol(college_pred)) {
  coldata <- college_pred[,col]
  print(class(coldata))
  if (class(coldata) == "numeric") {
    indexes <- which(is.na(coldata))
    coldata[which(is.na(coldata))] <- rep(mean(coldata, na.rm = TRUE), length(indexes))
    college_pred[,col] <- coldata
    print(college_pred[,col])
    ""
  }
}

which(is.na(college_pred)[3044420])

