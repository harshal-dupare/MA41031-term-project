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

#defining the size of past the memory 
past=3

#creating base vector for calculating state base =(2**p-1,2**p-2,.....,2**1,1)
base = 1
for(i in 1:(past-1)){
	base <- c(2**i,base)
}
base

#quantizing w.r.t bucket size
for(i in 1:rows){
x[i] = x[i]/bucketsize
}

x=trunc(x)
maxvalue=max(x)
minvalue=min(x)
rangesize = maxvalue - minvalue+1


#defining two arrrays to store the number of occurances of high and low for each combination
high = matrix(0,2**past,1)
low = matrix(0,2**past,1)
high
low

#it stores the direction in which the quantized value moved from that step to next step, 1 for forward 0 for backward or same
direction = matrix(0,rows-1,1)
for(i in 1:(rows-1)){

	if(x[i]<x[i+1]){
		direction[i]=1
	}

}
direction

#variables to count total number of highs and lows 
highcount = 0
lowcount = 0


#loop for all windows except last one as that is for first forcaste
for(i in 1:(rows-past-1)){

	number=sum(base*direction[i:(i+past-1)])

	if(direction[(i+past)]==1){
		high[number+1] <- (high[number+1]+1)
		highcount <- (highcount+1)
	}
	else{
		low[number+1] <- (low[number+1] + 1)
		lowcount <- (lowcount+1)
	}

}

for(i in 1:(2**past)){

	if((high[i]+low[i])==0){
		high[i]=highcount
		low[i]=lowcount
	}

}

limit = high + low
totalcount = highcount + lowcount


#defining TPM
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
#this will help in generating the random variable
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
	for(j in 2:rangesize){
		cdf[i,j]<-cdf[i,j-1]+TPM[i,j]
	}
	for(j in (rangesize+2):dimension){
		cdf[i,j]<-cdf[i,j-1]+TPM[i,j]
	}
}
cdf


#transition size counts for out of bound values 
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
for(j in 2:rangesize){
	nullcdf[j]<-nullcdf[j-1]+nulltpm[j]
}
for(j in (rangesize+2):dimension){
	nullcdf[j]<-nullcdf[j-1]+nulltpm[j]
}
nullcdf



#here goes the prediction size
#must be relatively small compared to rows
#or the errors will buildup and forcast will diverge
presize=50


#creating the final forcast array as "y"
forcast=matrix(0,presize,1)
y=c(x,forcast)


#forcast
for(i in rows:(rows+presize-1)){

j=y[i]-minvalue+1

if(j>=rangesize||j<=1){

	number=sum(base*direction[(i-past):(i-1)])
	horl_rn = sample(1:limit[(number+1)],1)

	if(horl_rn<=low[number+1]){
		rn = sample(1:nullcdf[rangesize],1)
		
	#to check where the random number lies
		idx = 0

		for(k in 1:rangesize){
			if(rn<=nullcdf[k]){
				idx = k
				break
			}
		}

		if(idx==0){idx=rangesize}

		step = idx - rangesize

		y[i+1] <- y[i]+step

		direction <- c(direction,0)

	}
	else{
		rn = sample(1:nullcdf[dimension],1)

		#to check where the random number lies
		idx = 0

		for(k in (rangesize+1):dimension){
			if(rn<=nullcdf[k]){
				idx = k
				break
			}
		}

		if(idx==0){idx=dimension}

		step = idx - rangesize

		y[i+1] <- y[i]+step

		direction <- c(direction,1)

	}
}
else{

	if(cdf[j,dimension]>0){

		number=sum(base*direction[(i-past):(i-1)])
		horl_rn = sample(1:limit[(number+1)],1)

		if(horl_rn<=low[number+1]){
			rn = sample(1:cdf[j,rangesize],1)
			
		#to check where the random number lies
			idx = 0

			for(k in 1:rangesize){
				if(rn<=cdf[j,k]){
					idx = k
					break
				}
			}

			if(idx==0){idx=rangesize}

			step = idx - rangesize

			y[i+1] <- y[i]+step

			direction <- c(direction,0)

		}
		else{
			rn = sample(1:cdf[j,dimension],1)

			#to check where the random number lies
			idx = 0

			for(k in (rangesize+1):dimension){
				if(rn<=cdf[j,k]){
					idx = k
					break
				}
			}

			if(idx==0){idx=dimension}

			step = idx - rangesize

			y[i+1] <- y[i]+step

			direction <- c(direction,1)

		}
	}
	else{

			number=sum(base*direction[(i-past):(i-1)])
		horl_rn = sample(1:limit[(number+1)],1)

		if(horl_rn<=low[number+1]){
			rn = sample(1:nullcdf[rangesize],1)
			
		#to check where the random number lies
			idx = 0

			for(k in 1:rangesize){
				if(rn<=nullcdf[k]){
					idx = k
					break
				}
			}

			if(idx==0){idx=rangesize}

			step = idx - rangesize

			y[i+1] <- y[i]+step

			direction <- c(direction,0)

		}
		else{
			rn = sample(1:nullcdf[dimension],1)

			#to check where the random number lies
			idx = 0

			for(k in (rangesize+1):dimension){
				if(rn<=nullcdf[k]){
					idx = k
					break
				}
			}

			if(idx==0){idx=dimension}

			step = idx - rangesize

			y[i+1] <- y[i]+step

			direction <- c(direction,1)

		}

	}
	

}
}

for(i in 1:(rows+presize)){
y[i] = y[i]*bucketsize +(bucketsize/2)
}

y
plot.ts(y)