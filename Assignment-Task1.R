library(readxl)
library(dplyr)

setwd("C:/Users/nramanna/OneDrive - IESEG/Desktop/Forecasting") # Specify the working directory here.

data <- read_excel("DataSets2022.xlsx", sheet="Airpass_BE") # reading the data
airpass_BE <- ts(data[,2], frequency = 12, start = 2003)
plot(airpass_BE)
train <- window(airpass_BE,start = 2003, end=c(2017,12))
test <- window(airpass_BE, start = c(2018,1), end=c(2020,2))
train_test <- window(airpass_BE, end=c(2020,2))
plot(log(airpass_BE))

##Question 1
plot(train_test, col = 'red') #time series plot
lines(test, col="blue")
tsdisplay(train_test)
seasonplot(train_test, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot",
           ylab="Number of passengers",
           col=rainbow(20), 
           pch=19)
monthplot(train_test, main="Seasonal subseries plot", 
          ylab = "Number of passengers",
          xlab="month", type="l")

acf(airpass_BE)

# Finding the optimal lambda value
l <- BoxCox.lambda(train_test)
l <- 0
summary(fit1)
summary(fit2)


h <- length(test)

#Question 3

# Seasonal naive
air_m1 <- snaive(train, h=h, lambda = 0)  # seasonal naive
checkresiduals(air_m1)
plot(air_m1)
accuracy(air_m1,test)[,c(2,3,5,6)]
# Question 4
#Forecasting by decomposition
air_m2 <- stlf(train, method="naive", h=h)
air_m3 <- stlf(train, method="rwdrift", h=h)
air_m4 <- stlf(train, method="ets", h=h)
air_m5 <- stlf(train, method="arima", h=h)
checkresiduals(air_m5)
#Forecasting by decomposition - log transformed
air_m6 <- stlf(train, method="naive", h=h, lambda = 0, biasadj = TRUE, s.window = 'periodic')
air_m7 <- stlf(train, method="rwdrift", h=h, lambda = 0, biasadj = TRUE,s.window = 'periodic')
air_m8 <- stlf(train, method="ets", h=h, lambda = 0, biasadj = TRUE, s.window = 'periodic')
air_m9 <- stlf(train, method="arima", h=h, lambda = 0, biasadj = TRUE, s.window = 'periodic')
checkresiduals(air_m6)
#####################################################################
#### shorter procedure for accuracy and residual diagnostic table ###
#####################################################################

#list of models

models <- c("STL naive lambda", "STL rwdrift lambda", "STL ets lambda", "STL arima lambda" )

n <- length(models); n   #number of models

#naming of models for the given data set
m <- "air_m"

#naming of training and test set
trainset <- train
testset <- test

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

#for loop to collect measures
for(i in 1:4) 
{
  #accuracy measures
  assign(paste0("a_m", i), accuracy(get(paste0(m, i)), testset)[,c(2,3,5,6)])
  a_train <- rbind(a_train, c(i, get(paste0("a_m", i))[1,]))
  a_test <- rbind(a_test, c(i, get(paste0("a_m", i))[2,]))
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0(m, i)), plot = FALSE))
  res_matrix[i,] <- c(i, get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- models
colnames(a_train)[1] <- "nr"

rownames(a_test) <- models
colnames(a_test)[1] <- "nr"

a_train
a_test
round(res_matrix, digits = 4)
#####################################################################
#### end of procedure for accuracy and residual diagnostic table ###
#####################################################################


plot(air_m1)
checkresiduals(air_m1)

## Question 5
# Seasonal exponential smoothing methods
air_m10 <- hw(train, damped = FALSE, seasonal = "additive", h=h)
air_m11 <- hw(train, damped = TRUE, seasonal = "additive", h=h)
air_m12 <- hw(train, damped = FALSE, seasonal = "multiplicative", h=h)
air_m13 <- hw(train, damped = TRUE, seasonal = "multiplicative", h=h)

# Seasonal exponential smoothing methods - log transformed
air_m14 <- hw(train, damped = FALSE, seasonal = "additive", lambda = 0, biasadj = TRUE, h=h)
air_m15 <- hw(train, damped = TRUE, seasonal = "additive", lambda = 0, biasadj = TRUE, h=h)
accuracy(air_m14, test)[,c(2,3,5,6)]
checkresiduals(air_m14)
accuracy(air_m15, test)[,c(2,3,5,6)]
checkresiduals(air_m15)
plot(air_m15)

l<-0
m1 <- ets(train, model = "AAA", damped = TRUE)
air_m1 <- forecast(m1, h=h)     
m2 <- ets(train, model = "MAA", damped = TRUE)
air_m2 <- forecast(m2, h=h)     
m3 <- ets(train, model = "MAM", damped = TRUE)
air_m3 <- forecast(m3, h=h)     
m4 <- ets(train, model = "AAA", damped = FALSE, lambda = l, biasadj = TRUE)
air_m4 <- forecast(m4, h=h)     
m5 <- ets(train, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
air_m5 <- forecast(m5, h=h)     
checkresiduals(air_m5)
plot(air_m5)
# ETS Automated procedure
m6 <- ets(train, lambda = l, biasadj = TRUE)
air_m6 <- forecast(m6, h=h)
checkresiduals(air_m6)

#list of models
models <- c("AAdA", "MAdA", "MAdM", "AAA lambda", "AAdA lambda", 'Auto ETS')



n <- length(models); n   #number of models

#naming of models for the given data set
m <- "air_m"

#naming of training and test set
trainset <- train
testset <- test

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

#for loop to collect measures
for(i in 1:6) 
{
  #accuracy measures
  assign(paste0("a_m", i), accuracy(get(paste0(m, i)), testset)[,c(2,3,5,6)])
  a_train <- rbind(a_train, c(i, get(paste0("a_m", i))[1,]))
  a_test <- rbind(a_test, c(i, get(paste0("a_m", i))[2,]))
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0(m, i)), plot = FALSE))
  res_matrix[i,] <- c(i, get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- models
colnames(a_train)[1] <- "nr"

rownames(a_test) <- models
colnames(a_test)[1] <- "nr"

a_train
a_test
round(res_matrix, digits = 4)
#####################################################################
#### end of procedure for accuracy and residual diagnostic table ###
#####################################################################


# Question Arima
# ARIMA models
nsdiffs(train)
ndiffs(diff(train,12), test = "adf")

m17 <- auto.arima(train, stepwise = FALSE, approximation = FALSE)
air_m17 <- forecast(m17, h=h)
accuracy(air_m17, test)[,c(2,3,5,6)]
m18 <- auto.arima(train, stepwise = FALSE, approximation = FALSE, lambda = 0, biasadj = TRUE)
air_m18 <- forecast(m18, h=h)
accuracy(air_m18, test)[,c(2,3,5,6)]
m19 <- auto.arima(train, stepwise = FALSE, approximation = FALSE, d=1, D=1)
air_m19 <- forecast(m19, h=h)
accuracy(air_m19, test)[,c(2,3,5,6)]
checkresiduals(air_m19)
m20 <- auto.arima(train, stepwise = FALSE, approximation = FALSE, lambda = 0, biasadj = TRUE, d=1, D=1)
air_m20 <- forecast(m20, h=h)
accuracy(air_m20, test)[,c(2,3,5,6)]
checkresiduals(air_m20)
plot(air_m20)
plot(air_m17)
checkresiduals(air_m17)
#####################################################################
# Question 7
air_m1 <- snaive(train, h=h, lambda = 0)
air_m2 <- stlf(train, method="ets", h=h, lambda = 0, biasadj = TRUE, s.window = 'periodic')
m3 <- ets(train, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
air_m3 <- forecast(m3, h=h)
m4 <- ets(train, lambda = l, biasadj = TRUE)
air_m4 <- forecast(m4, h=h)
m5 <- auto.arima(train, stepwise = FALSE, approximation = FALSE)
air_m5 <- forecast(m5, h=h)
summary(m5)
checkresiduals(air_m5)
#####################################################################
#### shorter procedure for accuracy and residual diagnostic table ###
#####################################################################

#list of models
models <- c("Seasonal Naive method", "STL ets lambda",
             "AAdA",'Auto ETS',"ARIMA")



n <- length(models); n   #number of models

#naming of models for the given data set
m <- "air_m"

#naming of training and test set
trainset <- train
testset <- test

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")

#for loop to collect measures
for(i in 1:5) 
{
  #accuracy measures
  assign(paste0("a_m", i), accuracy(get(paste0(m, i)), testset)[,c(2,3,5,6)])
  a_train <- rbind(a_train, c(i, get(paste0("a_m", i))[1,]))
  a_test <- rbind(a_test, c(i, get(paste0("a_m", i))[2,]))
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0(m, i)), plot = FALSE))
  res_matrix[i,] <- c(i, get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- models
colnames(a_train)[1] <- "nr"

rownames(a_test) <- models
colnames(a_test)[1] <- "nr"

a_train
a_test
round(res_matrix, digits = 4)
#####################################################################
#### end of procedure for accuracy and residual diagnostic table ###
#####################################################################

# Question 8-9
m4 <- ets(train_test, lambda = l, biasadj = TRUE)
air_m4 <- forecast(m4, h=34)
plot(airpass_BE, main="Air Passengers Forecast", ylab="Number of passangers",xlab="Year")
lines(air_m4$mean,col=4)
legend("topleft",lty=2,col=c(4,2,3),
       legend=c("ETS(AAdA)"), cex = 0.6)
checkresiduals(air_m4)
