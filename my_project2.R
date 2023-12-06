library(forecast)
library(tseries)
library(vars)
library(openxlsx)
library(urca)
library(ggplot2)
library(quantmod)

library(reshape2)
library(zoo)
library (corrplot)
library(stargazer)
library(readxl)
library(lubridate)
library(dint)
library(midasr)
library(forecast)

##### Выгрузим цены акций из yahoo за 2007- 2022

setSymbolLookup(Cola=list(src="yahoo", name="KO",to = '2023-01-01'))
getSymbols("Cola")

df<- data.frame(COLA[,4])

### EPS dataframe
colaEPS <- read_excel("C:/Users/gmaho/Desktop/colaEPS.xlsx", 
                      col_types = c("date", "numeric", "numeric"))

###добавим EPS в конец соотв. квартала
colnames(colaEPS)[1] <- "relesedate"
colaEPS$date<- format(last_of_quarter(as.Date(colaEPS$relesedate) %m-% months(3)), format = "%Y-%m")

### 
df$date<-as.Date(rownames(df),format =" %Y-%m-%d " )
df$Q<-quarters(as.Date(df$date, format = "%d-%m-%Y"))


###среднее по месяцам
output <- aggregate(df$KO, list(format(df$date, "%Y-%m")), mean)
colnames(output) <- c('date', 'X')
output<-merge(x = output, y = colaEPS[c('EPS','date')], by = "date", all.x = TRUE)

x <- mls(output$X, k = 0:2, m = 3)
Y <- na.omit(output$EPS)

##разобьем датасет на тренировочный и тестовый
h <- 3
split<-length(Y) - h

trainY<-Y[0:split]
trainX<-output$X[0:(split*3)]
testX<-output$X[(split*3 +1):((split+h)*3)]
testY<-Y[(split+1):(split+h)]

#MIDAS-almon
m_r <- midas_r(trainY ~ fmls(trainX,2, 3, almonp)+mls(trainY, 1:2, 1, "*"), start = list(trainX = c(0,0,0)))
summary(m_r)

#Прогноз
f<- forecast(m_r,newdata =list(trainX  = testX), method = 'dynamic')
f
summary(f) 

#График
plot(f)
axis(1, tck = 1, lty = 2, col = "gray")
axis(2, tck = 1, lty = 2, col = "gray")
points((split-1):(split+h-2), testY, col ="violet",bg= "violet", cex = 1, pch = 21)






