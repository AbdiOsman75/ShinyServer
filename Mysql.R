library(RMySQL)
library(dbConnect)

#create connection to MySQL
con <- dbConnect(MySQL(),
                  user ='root',
                  password = 'root',
                  host = 'localhost',
                  port = 3306,
                  dbname = 'iowa_db')
#Export my pre-training dataset into MySQL, this is my final cleaned dataset
dbWriteTable(con, name = 'iowa', value = I6,overwrite=TRUE)

#Use dbGetQuery to read the dataset back in
query <- dbGetQuery(con,"SELECT * FROM Iowa LIMIT 1100")


