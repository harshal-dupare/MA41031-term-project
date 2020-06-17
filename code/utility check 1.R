#getting the data in dataframe
tcf <- read.csv("C:/Users/harshal d/Pictures/SP/TCF.csv")
head(tcf)

#getting the useful data
x<-tcf[,5]
head(x)

#acf test 
acf(x)
Box.test(x)

#plots
plot.ts(x)
hist(x)

## creating the s[t]/s[t-1] array
l=x
size = NROW(x)

for(i in 1:(size-1)){
l[i] = x[i+1]
}

l[size]=0;
r = l/x

#test to see if these are very less coorelated
head(r)
acf(r)
Box.test(r)
plot.ts(r)
hist(r)

for(i in 1:(sizr[e)){
r[i] = (r[i])*100
}
