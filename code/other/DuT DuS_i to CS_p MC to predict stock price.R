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

plot.ts(x)
hist(x)


TPM = matrix(0,rangesize,2*rangesize-1)
TPM
dimension= 2*rangesize - 1

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
step = nex - this
TPM[this,(step+rangesize)]<-TPM[this,(step+rangesize)]+1
}
TPM

#generating the frequency of of each state 
#this will help i generating the random movement 

freq=matrix(0,rangesize,1)

for(i in 1:rangesize){
	for(j in 1:dimension){
		freq[i]<-freq[i]+TPM[i,j]
	}
}
freq

#cdf matrix stores the values cdf for each row till that point
cdf = TPM

for(i in 1:rangesize){
	for(j in 2:dimension){
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
for(j in 2:dimension){
	nullcdf[j]<-nullcdf[j-1]+nulltpm[j]
}
nullcdf

#making difference array to store the difference of each tranasition
diffmn=matrix(0,rangesize,1)
diffsd=matrix(0,rangesize,1)

#assuming normal distribution with N(0,1/bucketsize) mean and sd values in case no data is available
mndiff = 0
sddiff = 1/bucketsize

#list to store the differences values
list = 0

#creating the list
for(i in 1:(rows-1)){

	this = temp[i]
	nex = temp[i+1]
	list <- c(list,( ((stock[i+1] - stock[i])/bucketsize) + this - nex ))

}

mndiff = mean(list)

#rejecting the zero value of sd and testing for significant differnce in mean and zero
if(sd(list)!=0){
	sddiff = sd(list)
}

z = (mndiff-0)/(sd(list))
pv = pnorm(-abs(z))

#if significant difference, i.e 2.5% margin from both tails
if(pv >= 0.025){
	mndiff = 0
}


#filling the mean and sd array for each bucket
for(i in 1:rangesize){

	list = 0
	count = 1

	for(j in 1:(rows-1)){

		this = temp[j]
		nex = temp[j+1]

		if(this == i){
			list <- c(list,( ((stock[i+1] - stock[i])/bucketsize) + this - nex ))
			count <- count + 1
		}

	}



	diffmn[i] = mndiff
	diffsd[i] = sddiff

#in case of no data the values are assumed to be mean and sd of the entire data set
#count must be large enough to consider it as proper statistic which by assuming equal for each bucket i.e (rows/rangesize)
if(count>=(rows/rangesize)){

	z = (mean(list)-0)/(sd(list))
	sddiff = sd(list)
	pv = pnorm(-abs(z))

	#if significant difference 2.5% margin from both tails
	if(pv >= 0.025){
		diffmn[i] = 0
	}
	else{
		diffmn[i]=mean(list)
	}

}

}


#here goes the prediction size
presize=100

forcast=matrix(0,presize,1)

#creating the final forcast array as "pred"
pred <- c(stock/bucketsize,forcast)
bct=c(x,forcast)


for(i in rows:(rows+presize-1)){
	j=bct[i]-minvalue+1

	if(j>1&&j<rangesize){

			rn = sample(1:freq[j],1)
			idx = 0

		#to check where the random number lies
		for(k in 1:dimension){
			if(rn<=cdf[j,k]){
				idx = k
				break
			}
		}

		if(idx==0){idx=dimension}
		step = idx - rangesize

		pred[i+1] <-( bct[i] + step + rnorm(1,diffmn[j],diffsd[j]) )
		bct[i+1] = trunc( pred[i+1] )

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

		if(idx==0){idx=dimension}
		step = idx - rangesize

		pred[i+1] <-( bct[i] + step + rnorm(1,mndiff,sddiff) )
		bct[i+1] = trunc( pred[i+1] )

	}

}

y
plot.ts(y)
hist(y)

pred
plot.ts(pred)
hist(pred)

for(i in 1:(rows+presize)){
	pred[i] = pred[i]*bucketsize
}

pred
plot.ts(pred)