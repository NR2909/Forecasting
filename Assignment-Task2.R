library(readxl)
library(dplyr)

setwd("C:/Users/nramanna/OneDrive - IESEG/Desktop/Forecasting") # Specify the working directory here.

data <- read_csv("IGDSXEEZ18M086NEST.csv") # reading the data
data <- ts(data[,2], frequency = 12, start = c(2000,12))
plot(data)
tsdisplay(data)
ndiffs(data)
nsdiffs(data)
cons1 <- window(data, end=c(2017,12))
cons2 <- window(data, start = c(2018,1), end=c(2019,12))
constt <- window(data, end=c(2019,12))
consco <- window(cons, start=c(2020,1))

plot(constt, col="red")
lines(cons2, col="blue")

seasonplot(constt, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot",
           ylab="Consumer Price Index",
           col=rainbow(20), 
           pch=19)
monthplot(constt, main="Seasonal subseries plot", 
          ylab = "Consumer Price Index",
          xlab="month", type="l")

h <- length(cons2)
h
l <- BoxCox.lambda(constt)
l <- -1

# Seasonal naive
cons_m1 <- snaive(cons1, h=h)            # seasonal naive

#Forecasting by decomposition
cons_m2 <- stlf(cons1, method="naive", h=h)
cons_m3 <- stlf(cons1, method="rwdrift", h=h)
cons_m4 <- stlf(cons1, method="ets", h=h)
cons_m5 <- stlf(cons1, method="arima", h=h)

#Forecasting by decomposition - transformed
cons_m1 <- stlf(cons1, method="naive", h=h, lambda = l, biasadj = TRUE)
cons_m2 <- stlf(cons1, method="rwdrift", h=h, lambda = l, biasadj = TRUE)
cons_m3 <- stlf(cons1, method="ets", h=h, lambda = l, biasadj = TRUE)
cons_m4 <- stlf(cons1, method="arima", h=h, lambda = l, biasadj = TRUE)
checkresiduals(cons_m4)
plot(cons_m4)
models <- c("STL naive lambda", "STL rwdrift lambda", "STL ets lambda", "STL arima lambda")

n <- length(models); n   #number of models

#naming of models for the given data set
m <- "cons_m"

#naming of training and test set
trainset <- cons1
testset <- cons2

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")


#for loop to collect measures
for(i in 1:n) 
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


# Seasonal exponential smoothing methods
cons_m10 <- hw(cons1, damped = FALSE, seasonal = "additive", h=h)
cons_m11 <- hw(cons1, damped = TRUE, seasonal = "additive", h=h)
cons_m12 <- hw(cons1, damped = FALSE, seasonal = "multiplicative", h=h)
cons_m13 <- hw(cons1, damped = TRUE, seasonal = "multiplicative", h=h)

# Seasonal exponential smoothing methods - log transformed
cons_m14 <- hw(cons1, damped = FALSE, seasonal = "additive", lambda = l, biasadj = TRUE, h=h)
cons_m15 <- hw(cons1, damped = TRUE, seasonal = "additive", lambda = l, biasadj = TRUE)
accuracy(cons_m14, cons2)[,c(2,3,5,6)]
checkresiduals(cons_m14)
accuracy(cons_m15, cons2)[,c(2,3,5,6)]
checkresiduals(cons_m15)

# ETS models (some forbidden combinations are deleted)
m16 <- ets(cons1, model = "AAA", damped = FALSE)
cons_m16 <- forecast(m16, h=h)
m17 <- ets(cons1, model = "MAA", damped = FALSE)
cons_m17 <- forecast(m17, h=h)
m18 <- ets(cons1, model = "MAM", damped = FALSE)
cons_m18 <- forecast(m18, h=h)
m19 <- ets(cons1, model = "AAA", damped = TRUE)
cons_m19 <- forecast(m19, h=h)     
m20 <- ets(cons1, model = "MAA", damped = TRUE)
cons_m20 <- forecast(m20, h=h)     
m21 <- ets(cons1, model = "MAM", damped = TRUE)
cons_m21 <- forecast(m21, h=h)     
m22 <- ets(cons1, model = "AAA", damped = FALSE, lambda = l, biasadj = TRUE)
cons_m22 <- forecast(m22, h=h)     
m23 <- ets(cons1, model = "AAA", damped = TRUE, lambda = l, biasadj = TRUE)
cons_m23 <- forecast(m23, h=h)     

# ARIMA models
nsdiffs(cons1)
ndiffs(diff(cons1,12))
tsdisplay(diff(cons1,12))

m24 <- auto.arima(cons1, stepwise = FALSE, approximation = FALSE)
cons_m24 <- forecast(m24, h=h)
m25 <- auto.arima(cons1, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE)
cons_m25 <- forecast(m25, h=h)
summary(cons_m24)
m26 <- auto.arima(cons1, stepwise = FALSE, approximation = FALSE, d=1, D=1)
cons_m26 <- forecast(m26, h=h)
m27 <- auto.arima(cons1, stepwise = FALSE, approximation = FALSE, lambda = l, biasadj = TRUE, d=1, D=1)
cons_m27 <- forecast(m27, h=h)


fcast = Arima(cons1,
              order = c(2,1,0), seasonal = c(2,1,1), include.drift = TRUE, lambda = l, biasadj = TRUE)

fcast_1 <- forecast(fcast, h=h)
accuracy(fcast_1, cons2)[,c(2,3,5,6)]
checkresiduals(fcast_1)
plot(fcast_1)

fcast1 = Arima(constt,
              order = c(2,1,0), seasonal = c(2,1,1), include.drift = TRUE, lambda = l, biasadj = TRUE,
              method = 'CSS')

fcast_1 <- forecast(fcast1, h=36)
accuracy(fcast_1, cons2)[,c(2,3,5,6)]
checkresiduals(cons_m9)

plot(data, main="Air Passengers Forecast", ylab="Number of passangers",xlab="Year")
lines(cons_m4$mean,col=4)
legend("topleft",lty=2,col=c(4,2,3),
       legend=c("Arima"), cex = 0.6)

#####################################################################
#### shorter procedure for accuracy and residual diagnostic table ###
#####################################################################

#list of models
models <- c("Seasonal Naive method", 
            "STL naive", "STL rwdrift", "STL ets", "STL arima",
            "STL naive lambda", "STL rwdrift lambda", "STL ets lambda", "STL arima lambda",
            "HW add", "HW add damped", "HW mult", "HW mult damped", "HW add lambda", "HW add damped lambda", 
            "AAA", "MAA", "MAM", "AAdA", "MAdA", "MAdM", "AAA lambda", "AAdA lambda", 
            "ARIMA", "ARIMA lambda", "ARIMA dD", "ARIMA dD lambda")

models <- c("STL naive lambda", "STL rwdrift lambda", "STL ets lambda", "STL arima lambda")

n <- length(models); n   #number of models

#naming of models for the given data set
m <- "cons_m"

#naming of training and test set
trainset <- cons1
testset <- cons2

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 5)
a_test  <- matrix(nrow = 0, ncol = 5)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = n, ncol = 4)
rownames(res_matrix) <- models
colnames(res_matrix) <- c("nr", "Q*", "df", "p-value")


#for loop to collect measures
for(i in 6:9) 
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

cons_m99 <- stlf(constt, method="arima", h=36, lambda = l, biasadj = TRUE)
plot(data, main="Index of consumer prices", ylab="Consumer price index",xlab="Year")
lines(cons_m99$mean,col=4)
legend("topleft",lty=2,col=c(4,2,3),
       legend=c("STL with Arima"), cex = 0.6)

