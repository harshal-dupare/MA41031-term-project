#getting the data in dataframe
tcf <- read.csv("C:/Users/harshal d/Pictures/SP/TCF.csv")
head(tcf)

#getting the useful data
x<-tcf[,5]
head(x)

#installing packages and library

install.packages("rugarch")
library("rugarch")

install.packages("arm")
library("arm")

arima(x,c(1,0,1))

acf(x)
Box.test(x)