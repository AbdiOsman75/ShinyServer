library(dplyr)
library(randomForest)
library(shiny)
library(RMySQL)
library(dbConnect)
logic <- function(input) {
#Import the dataset from MySQL
con <- dbConnect(MySQL(), user ='root', password = 'password', host = 'localhost',port = 3306,dbname = 'iowa_db')
query <- dbGetQuery(con,"SELECT * FROM iowa LIMIT 1100")
I6<-query[,-c(1,31)]
I6[sapply(I6, is.character)] <- lapply(I6[sapply(I6, is.character)],
                                           as.factor)
model <- randomForest(SalePrice~.,data=I6)
input <- list(
      "MSSubClass"=as.factor(input$MSSubClass),
      "LotShape"=input$LotShape,
      "Neighborhood"=input$Neighborhood,
      "LotConfig"=input$LotConfig,
      "OverallCond"=input$OverallCond,
      "OverallQual"=input$OverallQual,
      "Exterior1st"=input$Exterior1st,
      "Exterior2nd"=input$Exterior2nd,
      "MasVnrType"=input$MasVnrType,
      "ExterQual"=input$ExterQual,
      "Foundation"=input$Foundation,
      "BsmtQual"=input$BsmtQual,
      "BsmtExposure"=input$BsmtExposure,
      "BsmtFinType1"=input$BsmtFinType1,
      "HeatingQC"=input$HeatingQC,
      "KitchenQual"=input$KitchenQual,
      "FireplaceQu"=input$FireplaceQu,
      "GarageType"=input$GarageType,
      "GarageFinish"=input$GarageFinish,
      "YearBuilt"=as.factor(input$YearBuilt),
      "YearRemodAdd"=as.factor(input$YearRemodAdd),
      "TotalBsmtSF"=as.numeric(input$TotalBsmtSF),
      "X1stFlrSF"=as.numeric(input$X1stFlrSF),
      "GrLivArea"=as.numeric(input$GrLivArea),
      "FullBath"=as.numeric(input$FullBath),
      "TotRmsAbvGrd"=as.numeric(input$TotRmsAbvGrd),
      "Fireplaces"=as.numeric(input$Fireplaces),
      "GarageCars"=as.numeric(input$GarageCars),
      "SalePrice"=as.numeric(0)
)
test <- as.data.frame(input)
test <- rbind(I6[1, ],test)
test <- test[-1,]
prediction <- predict(model,test)
return(round(prediction))
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$Predicted <- renderText({
      paste("Predicted Value of £", logic(input))
    })
    # output$Table <- renderTable({
    #   logic(input)
    # })
})
