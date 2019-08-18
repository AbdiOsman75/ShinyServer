library(RMySQL)
library(dbConnect)

#create connection to MySQL
con <- dbConnect(MySQL(),
                  user ='root',
                  password = 'root',
                  host = 'localhost',
                  port = 3306,
                  dbname = 'iowa_db')
agent_id <-as.data.frame(sample(1:10,1008,replace=T))
names(agent_id)<-c("Agent_id")
#Export my pre-training dataset into MySQL, this is my final cleaned dataset
dbWriteTable(con, name = 'iowa', value = cbind(I6,agent_id),overwrite=TRUE)

#Use dbGetQuery to read the dataset back in
query <- dbGetQuery(con,"SELECT * FROM iowa LIMIT 1100")


