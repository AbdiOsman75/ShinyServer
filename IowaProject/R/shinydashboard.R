#install.packages("shinydashboard")
library(shiny)
library(shinydashboard)
con <- dbConnect(MySQL(), user ='root', password = 'root', host = 'localhost',port = 3306,dbname = 'iowa_db')
query <- dbGetQuery(con,"SELECT * FROM iowa LIMIT 1100")
I6<-query[,-c(1,31)]
I6[sapply(I6, is.character)] <- lapply(I6[sapply(I6, is.character)],
                                       as.factor)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
              box(width = 4,
                  selectInput(inputId = "Foundation", label = "Type of foundation", choices = levels(I6$Foundation)),
                  selectInput(inputId = "BsmtQual", label = "Evaluates the height of the basement", choices = levels(I6$BsmtQual)),
                  selectInput(inputId = "BsmtExposure", label = "Refers to walkout or garden level walls", choices = levels(I6$BsmtExposure)),
                  selectInput(inputId = "BsmtFinType1", label = "Rating of basement finished area", choices = levels(I6$BsmtFinType1)),
                  selectInput(inputId = "HeatingQC", label = "Heating quality and condition", choices = levels(I6$HeatingQC)),
                  selectInput(inputId = "KitchenQual", label = "Kitchen quality", choices = levels(I6$KitchenQual)),
                  selectInput(inputId = "FireplaceQu", label = "Fireplace quality", choices = levels(I6$FireplaceQu)),
                  selectInput(inputId = "GarageType", label = "Garage location", choices = levels(I6$GarageType)),
                  selectInput(inputId = "GarageFinish", label = "Interior finish of the garage", choices = levels(I6$GarageFinish)),
                  selectInput(inputId = "YearBuilt", label = "THe decade the house was built", choices = levels(I6$YearBuilt))),

                box(width = 4,
                  selectInput(inputId = "MSSubClass", label = "Types of Dwelling", choices = levels(I6$MSSubClass)),
                  selectInput(inputId = "LotShape", label = "Shape of the Lot", choices = levels(I6$LotShape)),
                  selectInput(inputId = "Neighborhood", label = "Which neighbourhood", choices = levels(I6$Neighborhood)),
                  selectInput(inputId = "LotConfig", label = "Lot configuration", choices = levels(I6$LotConfig)),
                  selectInput(inputId = "OverallCond", label = "Overall condition", choices = levels(I6$OverallCond)),
                  selectInput(inputId = "OverallQual", label = "Overall Quality", choices = levels(I6$OverallQual)),
                  selectInput(inputId = "Exterior1st", label = "Exterior covering on house", choices = levels(I6$Exterior1st)),
                  selectInput(inputId = "Exterior2nd", label = "Exterior covering on house, if more than one material", choices = levels(I6$Exterior2nd)),
                  selectInput(inputId = "MasVnrType", label = "Masonry veneer type", choices = levels(I6$MasVnrType)),
                  selectInput(inputId = "ExterQual", label = "Evaluates the quality of the material on the exterior", choices = levels(I6$ExterQual))),

                box(width = 4,
                  selectInput(inputId = "YearRemodAdd", label = "The decade the house was remodelled", choices = levels(I6$YearRemodAdd)),
                  numericInput(inputId = "TotalBsmtSF", label = "Number of Beds", value = 0, min = 0, step = 1),
                  numericInput(inputId = "X1stFlrSF", label = "First Floor square feet", min = 0, value = 0, step = 1),
                  numericInput(inputId = "GrLivArea", label = "Above ground living area(Sq. ft)", min = 0, value = 0, step = 1),
                  numericInput(inputId = "FullBath", label = "Number of Full Bathrooms", min = 0, value = 0, step = 1, max = 3),
                  numericInput(inputId = "TotRmsAbvGrd", label = "Total rooms excluding bathrooms", min = 0, value = 0, step = 1,max = 12),
                  numericInput(inputId = "Fireplaces", label = "Number of Fireplaces", min = 0, value = 0, step = 1, max = 3),
                  numericInput(inputId = "GarageCars", label = "Number of cars that garage can hold", min = 0, value = 0, step = 1, max = 4)),
              box(
                textOutput(outputId = "Predicted")
                # tableOutput(outputId = "Table")
              )
                )
              )
      )
    )
  )

logic <- function(input) {
  #Import the dataset from MySQL
  con <- dbConnect(MySQL(), user ='root', password = 'root', host = 'localhost',port = 3306,dbname = 'iowa_db')
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


server <- function(input, output) {
  output$Predicted <- renderText({
    paste("Predicted Value of £", logic(input))
  })

  # output$Table <- renderTable({
  #   logic(input)
  # })

}

shinyApp(ui, server)

