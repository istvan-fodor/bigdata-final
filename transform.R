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


for (i in 1:ncol(college)) {
  if (class(college[,i]) == "integer") {
    college[,i] <- as.numeric(college[,i])
  }
}




for(col in 1:ncol(college)) {
  coldata <- college[,col]
  print(class(coldata))
  if (class(coldata) == "numeric") {
    print(col)
    indexes <- which(is.na(coldata))
    coldata[which(is.na(coldata))] <- rep(mean(coldata, na.rm = TRUE), length(indexes))
    college[,col] <- coldata
  }
}


is_na <- ifelse(is.na(college_pred), 1,0)
colnames(is_na) <- paste(colnames(is_na), "_missing", sep = "")

x_na <- Matrix(is_na, sparse = TRUE)  #sparse.model.matrix( ~ . , data = is_na)[,-1]
x <- sparse.model.matrix(  ~ ., data=college[,-1])[,-1]



