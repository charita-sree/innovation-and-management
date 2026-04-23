
sales_data <- read.csv(file.choose())
sales_data <- read.csv("Walmart Dataset.csv")
head(sales_data)
str(sales_data)
summary(sales_data)


sales_data$Date <- as.Date(sales_data$Date,"%d-%m-%Y")
library(lubridate)

sales_data$Month <- month(sales_data$Date)
sales_data$Year <- year(sales_data$Date)

colSums(is.na(sales_data))
sales_data <- na.omit(sales_data)



hist(sales_data$Weekly_Sales)


plot(sales_data$Temperature,
     sales_data$Weekly_Sales)


boxplot(Weekly_Sales ~ Holiday_Flag,
        data=sales_data)

library(caret)

set.seed(123)

train_index <- createDataPartition(
  sales_data$Weekly_Sales,
  p=0.8,
  list=FALSE)

train_data <- sales_data[train_index,]
test_data <- sales_data[-train_index,]



model <- lm(
  Weekly_Sales ~ Store +
    Holiday_Flag +
    Temperature +
    Fuel_Price +
    CPI +
    Unemployment +
    Month,
  data=train_data)


predictions <- predict(model,test_data)




library(Metrics)

mse_value <- mse(test_data$Weekly_Sales,predictions)
rmse_value <- rmse(test_data$Weekly_Sales, predictions)

r2_value <- cor(test_data$Weekly_Sales, predictions)^2



ggplot(sales_data,
       aes(Date,Weekly_Sales)) +
  geom_line()

ggplot(sales_data,
       aes(Temperature,Weekly_Sales)) +
  geom_point()

ggplot(sales_data,
       aes(factor(Month),Weekly_Sales)) +
  geom_bar(stat="identity")
