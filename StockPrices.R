############## Add Your Name Here.. eg: Tim Smith ############## ############## ############## ############## 
#       
#       Predict the stock index price based on interest rate using Simple Linear Regression 
#       for interest rate = 3.0
#       LINEAR REGRESSION
############### ############## ############## ############## ############## ############## ############## ####
library(caret)
library (ggplot2)
#Read the stock data csv and remember to set the working directory from the Files window on the bottom right
stock = read.csv("stockprices_data.csv")

#Build the model. Use syntax 
stockmodelorginal = train(Stock.Index.Price ~Interest.Rate, data= stock, method="lm")

# use modelName$finalModel on the model that you trained to get the coefficients
stockmodelorginal$finalModel
# predict the stock index prices for the entire dataset using predict(modelName, dataset)
predictstockprices1 = predict(stockmodelorginal, stock)
#predict stock index for InterestRate 3.0
StockIndexPrice = -99.46 + 564.2*InterestRate
InterestRate=3.0

#print the final stock index price value for interestRate =3.0
StockIndexPrice
# calculate R2 and RMSE using sytax R2(predictedValues, actualValues) #actual values are the dataset$columnNames
#print R2
R2 = R2(predictstockprices1, stock$Stock.Index.Price)
R2
#print RMSE
RMSE = RMSE(predictstockprices1, stock$Stock.Index.Price)
RMSE
############################ ############## ############## ############## 
#       
#       Predict the stock index price based on interest rate and unemployment rate using Multiple Linear Regression 
#       for interest rate = 3.0 and unemployment rate = 5.0
#       MULTIPLE LINEAR REGRESSION
############### ############## ############## ############## ############## ############## ############## ####
# Multiple Linear Regression
stockmodel=train(Stock.Index.Price ~ Interest.Rate + Unemployment.Rate, data=stock, method="lm")
predictedstockprices2 = predict(stockmodel, stock)
#everything else remains the same as above!!!
stockmodel$finalModel
#predict stock index for int rate 3.0 and unemployment rate of 5.0
#Stock.Index.Price = intercept + m1*InterestRate + m2*UnemploymentRate
InterestRate = 3.0
UnemploymentRate = 5.0
Stock.Index.Price = 345.5*InterestRate + -250.1*UnemploymentRate + 1798.4
#print the value of Stock Index Price
Stock.Index.Price
#print R2
R2_2 = R2(predictstockprices1, stock$Stock.Index.Price)
R2_2
#print RMSE
RMSE_2 = RMSE(predictstockprices1, stock$Stock.Index.Price)
RMSE_2
##################### EXTRAS
#print validation table with predictedValues for Linear Regression and Multiple LR
valTab= stock[, c("Interest.Rate", "Unemployment.Rate")]
#new column name
valTab$predictedPriceStockIndex = predictedstockprices2
