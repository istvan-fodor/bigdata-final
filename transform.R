library(RSQLite)
con <- dbConnect(SQLite(), dbname="database.sqlite")
results <- data.frame(colnames(dbGetQuery(con, "select count(*) from scorecard limit 1")))

fileName <- 'read_all.sql'
query <- readChar(fileName, file.info(fileName)$size)
results <- dbGetQuery(con, query)

