#loading data in dataframe
tcf <- read.csv("C:/Users/harshal d/Pictures/SP/TCF.csv")
head(tcf)

#extracting the useful data
stock<-tcf[,5]
head(stock)

#plots 
plot.ts(stock)
hist(stock)

#number of rows i.e number of data points
rows = NROW(stock)

#defining bucket size and declaring state array
bucketsize = 0.1
x = stock


#quantizing w.r.t bucket size
for(i in 1:rows){
x[i] = x[i]/bucketsize
}

x=trunc(x)
maxvalue=max(x)
minvalue=min(x)

plot.ts(x)
hist(x)


TPM = matrix(0,maxvalue-minvalue+1,maxvalue-minvalue+1)
TPM

#creating a temporary array to translate in required range
temp = x
for(i in 1:(rows)){
temp[i]=x[i]-minvalue+1
}


#creating the transition counts for each transition
this = 1
nex = 1
for(i in 1:(rows-1)){
this = temp[i]
nex = temp[i+1]
TPM[this,nex]<-TPM[this,nex]+1
}
TPM


rangesize = maxvalue - minvalue+1

#generating the frequency of of each state 
#this will help i generating the random movement 

freq=matrix(0,rangesize,1)

for(i in 1:rangesize){
	for(j in 1:rangesize){
		freq[i]<-freq[i]+TPM[i,j]
	}
}
freq

#cdf matrix stores the values cdf for each row till that point
cdf = TPM

for(i in 1:rangesize){
	for(j in 2:rangesize){
		cdf[i,j]<-cdf[i,j-1]+TPM[i,j]
	}
}

cdf


#here goes the prediction size
presize=100

forcast=matrix(0,presize,1)

#creating the final forcast array as "y"
y=c(x,forcast)
y


for(i in rows:(rows+presize-1)){
j=y[i]-minvalue+1
rn = sample(1:freq[j],1)
idx = 0

#to check where the random number lies
	for(k in 1:rangesize){
		if(rn<=cdf[j,k]){
			idx = k
			break
		}
	}

if(idx==0){
idx=rangesize
}

y[i+1]<-(idx+minvalue-1)

}

y
plot.ts(y)

for(i in 1:(rows+presize)){
y[i] = y[i]*bucketsize +(bucketsize/2)
}

y
plot.ts(y)