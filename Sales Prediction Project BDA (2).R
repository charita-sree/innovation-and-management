install.packages("tidyverse")
install.packages("ggplot2")
install.packages("caret")
install.packages("Metrics")
install.packages("lubridate")
install.packages("shiny")
install.packages("plotly")


sales_data <- read.csv(file.choose())


library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(caret)
library(Metrics)
library(DT)
library(lubridate)

sales_data <- read.csv("Walmart Dataset.csv")

sales_data$Date <- as.Date(sales_data$Date,"%d-%m-%Y")

sales_data$Month <- month(sales_data$Date)
sales_data$Year <- year(sales_data$Date)

set.seed(123)

train_index <- createDataPartition(sales_data$Weekly_Sales,p=0.8,list=FALSE)

train_data <- sales_data[train_index,]
test_data <- sales_data[-train_index,]


model <- lm(Weekly_Sales ~ Store + Holiday_Flag + Temperature + Fuel_Price + CPI + Unemployment + Month,
            data=train_data)

predictions <- predict(model,test_data)

mse_val <- mse(test_data$Weekly_Sales,predictions)
rmse_val <- rmse(test_data$Weekly_Sales,predictions)
r2_val <- cor(test_data$Weekly_Sales,predictions)^2








ui <- dashboardPage(
  
  dashboardHeader(title="Walmart Sales Prediction Dashboard"),
  
  dashboardSidebar(
    
    selectInput("store","Select Store",
                choices=unique(sales_data$Store)),
    
    selectInput("year","Select Year",
                choices=unique(sales_data$Year)),
    
    sliderInput("temperature",
                "Select Temperature Range",
                min=min(sales_data$Temperature),
                max=max(sales_data$Temperature),
                value=c(min(sales_data$Temperature),max(sales_data$Temperature)))
    
  ),
  
  dashboardBody(
    
    fluidRow(
      
      valueBoxOutput("totalsales"),
      valueBoxOutput("avgsales"),
      valueBoxOutput("maxsales"),
      valueBoxOutput("minsales")
      
    ),
    
    fluidRow(
      
      valueBoxOutput("msebox"),
      valueBoxOutput("rmsebox"),
      valueBoxOutput("r2box")
      
    ),
    
    fluidRow(
      
      box(plotlyOutput("salesTrend"),width=6),
      box(plotlyOutput("monthlySales"),width=6)
      
    ),
    
    fluidRow(
      
      box(plotlyOutput("tempSales"),width=6),
      box(plotlyOutput("fuelSales"),width=6)
      
    ),
    
    fluidRow(
      
      box(plotlyOutput("holidaySales"),width=6),
      box(plotlyOutput("tempTrend"),width=6)
      
    ),
    
    fluidRow(
      
      box(plotlyOutput("predictionPlot"),width=6),
      box(plotlyOutput("predictionTempPlot"),width=6)
      
    ),
    
    fluidRow(
      
      box(plotOutput("featureImportance"),width=6),
      box(DTOutput("datatable"),width=6)
      
    )
    
  )
  
)








server <- function(input,output){
  
  filtered_data <- reactive({
    
    sales_data[
      sales_data$Store==input$store &
        sales_data$Year==input$year &
        sales_data$Temperature>=input$temperature[1] &
        sales_data$Temperature<=input$temperature[2],]
    
  })
  
  output$totalsales <- renderValueBox({
    valueBox(sum(filtered_data()$Weekly_Sales),"Total Sales")
  })
  
  output$avgsales <- renderValueBox({
    valueBox(mean(filtered_data()$Weekly_Sales),"Average Sales")
  })
  
  output$maxsales <- renderValueBox({
    valueBox(max(filtered_data()$Weekly_Sales),"Max Sales")
  })
  
  output$minsales <- renderValueBox({
    valueBox(min(filtered_data()$Weekly_Sales),"Min Sales")
  })
  
  output$msebox <- renderValueBox({
    valueBox(round(mse_val,2),"MSE")
  })
  
  output$rmsebox <- renderValueBox({
    valueBox(round(rmse_val,2),"RMSE")
  })
  
  output$r2box <- renderValueBox({
    valueBox(round(r2_val,3),"R² Score")
  })
  
  output$salesTrend <- renderPlotly({
    
    p <- ggplot(filtered_data(),
                aes(Date,Weekly_Sales))+
      geom_line(color="blue")+
      ggtitle("Sales Trend")
    
    ggplotly(p)
    
  })
  
  output$monthlySales <- renderPlotly({
    
    p <- ggplot(filtered_data(),
                aes(factor(Month),Weekly_Sales))+
      geom_bar(stat="identity",fill="orange")+
      ggtitle("Monthly Sales")
    
    ggplotly(p)
    
  })
  
  output$tempSales <- renderPlotly({
    
    p <- ggplot(filtered_data(),
                aes(Temperature,Weekly_Sales))+
      geom_point(color="red")+
      ggtitle("Temperature vs Sales")
    
    ggplotly(p)
    
  })
  
  output$fuelSales <- renderPlotly({
    
    p <- ggplot(filtered_data(),
                aes(Fuel_Price,Weekly_Sales))+
      geom_point(color="green")+
      ggtitle("Fuel Price vs Sales")
    
    ggplotly(p)
    
  })
  
  output$holidaySales <- renderPlotly({
    
    p <- ggplot(filtered_data(),
                aes(factor(Holiday_Flag),Weekly_Sales))+
      geom_boxplot()+
      ggtitle("Holiday Impact")
    
    ggplotly(p)
    
  })
  
  output$tempTrend <- renderPlotly({
    
    p <- ggplot(filtered_data(),
                aes(Date,Temperature))+
      geom_line(color="purple")+
      ggtitle("Temperature Trend")
    
    ggplotly(p)
    
  })
  
  output$predictionPlot <- renderPlotly({
    
    pred_df <- data.frame(
      Actual=test_data$Weekly_Sales,
      Predicted=predictions)
    
    p <- ggplot(pred_df,
                aes(Actual,Predicted))+
      geom_point(color="blue")+
      geom_abline(slope=1,intercept=0,color="red")+
      ggtitle("Actual vs Predicted Sales")
    
    ggplotly(p)
    
  })
  
  output$predictionTempPlot <- renderPlotly({
    
    pred_df <- data.frame(
      Temperature=test_data$Temperature,
      Predicted=predictions)
    
    p <- ggplot(pred_df,
                aes(Temperature,Predicted))+
      geom_point(color="darkgreen")+
      ggtitle("Predicted Sales vs Temperature")
    
    ggplotly(p)
    
  })
  
  output$featureImportance <- renderPlot({
    
    coeff <- coef(model)[-1]
    
    barplot(coeff,
            main="Feature Importance",
            col="skyblue")
    
  })
  
  output$datatable <- renderDT({
    
    datatable(filtered_data())
    
  })
  
}

shinyApp(ui,server)
