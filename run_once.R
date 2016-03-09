library(RSQLite)
con <- dbConnect(SQLite(), dbname="database.sqlite")

fileName <- 'run_once.sql'
query <- readChar(fileName, file.info(fileName)$size)
results <- dbGetQuery(con, query)

