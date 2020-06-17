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
bucketsize = 1
x = stock


#quantizing w.r.t bucket size
for(i in 1:rows){
x[i] = x[i]/bucketsize
}

x=trunc(x)
maxvalue=max(x)
minvalue=min(x)
rangesize = maxvalue - minvalue+1

#updated values plots
plot.ts(x)
hist(x)


#TPM matrix  for transition size
TPM = matrix(0,rangesize,2*rangesize-1)
TPM
dimension= 2*rangesize - 1

#creating a temporary array to translate in required range
temp = x
for(i in 1:(rows)){
temp[i]=x[i]-minvalue+1
}


#creating the transition counts for each transition size
this = 1
nex = 1
for(i in 1:(rows-1)){
this = temp[i]
nex = temp[i+1]
step = nex - this
TPM[this,(step+rangesize)]<-TPM[this,(step+rangesize)]+1
}
TPM

#generating the frequency of of each state 
#this will help in generating the random movement 
freq=matrix(0,rangesize,1)

for(i in 1:rangesize){
	for(j in 1:dimension){
		freq[i]<-freq[i]+TPM[i,j]
	}
}
freq


#cdf matrix stores the values cdf for each row till that point of size
cdf = TPM
for(i in 1:rangesize){
	for(j in 2:dimension){
		cdf[i,j]<-cdf[i,j-1]+TPM[i,j]
	}
}
cdf


#transition size counts for out of bound values sum of all that size transition count
nulltpm = matrix(0,dimension,1)

for(i in 1:(rows-1)){
	this = temp[i]
	nex = temp[i+1]
	idx = nex - this + rangesize
	nulltpm[idx] <- ( nulltpm[idx] + 1 )

}
nulltpm

#cdf of transition counts for out of bound values
nullcdf = nulltpm
for(j in 2:dimension){
	nullcdf[j]<-nullcdf[j-1]+nulltpm[j]
}
nullcdf



#here goes the prediction size 
#Note the prediction size must not be too large compared to the rows
#as then errors in each transition aggregate and the forcast diverges
presize=100


#creating the final forcast array as "y"
forcast=matrix(0,presize,1)
y=c(x,forcast)
y


#calculating the forcast
for(i in rows:(rows+presize-1)){

j=y[i]-minvalue+1

#at boundaries we dont have suff info so we choose the NULLTPM 
#so for 1 and rangesize we choose nulltpm
if(j<rangesize&&j>1){
	rn = sample(1:freq[j],1)
	idx = 0

	#to check where the random number lies
		for(k in 1:dimension){
			if(rn<=cdf[j,k]){
				idx = k
				break
			}
		}

	if(idx==0){
	idx=dimension
	}
	step = idx - rangesize

	y[i+1] <- y[i]+step
}
else{

	rn = sample(1:nullcdf[dimension],1)
	idx = 0

	#to check where the random number lies
	for(k in 1:dimension){
		if(rn<=nullcdf[k]){
			idx = k
			break
		}
	}

	if(idx==0){
		idx=dimension
	}
	step = idx - rangesize

	y[i+1] <- y[i]+step

}

}

y
plot.ts(y)

for(i in 1:(rows+presize)){
y[i] = y[i]*bucketsize +(bucketsize/2)
}

y
plot.ts(y)