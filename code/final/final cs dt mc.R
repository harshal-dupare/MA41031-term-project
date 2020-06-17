#getting the data in dataframe
tcf <- read.csv("C:/Users/harshal d/Pictures/SP/TCF.csv")
head(tcf)

#getting the useful data
stock<-tcf[,5]
head(stock)
x=stock

#plots
plot.ts(x)

#size of data in rows
rows = NROW(x)
bucketsize = 1

#variables to count total number of highs and lows 
highcount = 0
lowcount = 0

maxvalue=trunc(max(x)/bucketsize)
minvalue=trunc(min(x)/bucketsize)
rangesize = maxvalue -  minvalue  + 1

#defining two arrrays to store the number of occurances of high and low for each combination
high = matrix(0,rangesize,1)
low = matrix(0,rangesize,1) 
highsize = matrix(0,rangesize,1)
lowsize = matrix(0,rangesize,1)
hsize = 0
lsize = 0


#direction matrix to store array of direction of movement
direction = matrix(0,rows-1,1)
for(i in 1:(rows-1)){
	if(x[i]<x[i+1]){
		direction[i]=1
	}
}
direction


#filling high low counts and sizes
for(i in 1:(rows-1)){

	j =  trunc(x[i]/bucketsize) - minvalue +1

	if(direction[i]==1){
		highsize[j]=highsize[j]+(x[i+1]-x[i])
		high[j] <- ( high[j] + 1 )
		highcount <- ( highcount + 1 )
		hsize <- c(hsize,(x[i+1]-x[i]))
	}
	if(direction[i]==0){
		lowsize[j]=lowsize[j]+(x[i]-x[i+1])
		low[j]=low[j]+1
		lowcount <- (lowcount+1)
		lsize <- c(lsize,(x[i]-x[i+1]))
	}

}

hsd = sd(hsize)/sqrt(highcount)
lsd = sd(lsize)/sqrt(lowcount)
hmean = mean(hsize)
lmean = -mean(lsize)
totalcount = highcount + lowcount


for(i in 1:rangesize){

	if((high[i]+low[i])==0){
		high[i]=highcount
		low[i]=lowcount
		highsize[i]=sum(hsize)
		lowsize[i]=-sum(lsize)
	}

}

#creating range array
range = low + high
limitsize = lowsize + highsize
lowsize = -lowsize

#here goes the prediction size
predictions=50



#creating the final forcast array as "y"
forcast=matrix(0,predictions,1)
y=c(x,forcast)


for(i in rows:(rows+predictions-1)){

	j =  trunc(y[i]/bucketsize) - minvalue +1

	if(j<rangesize&&j>1){

		horl_rn = sample(1:range[j],1)

		if(horl_rn <= low[j]){

			if(low[j]==0){
				rn = rnorm(1,lmean,lsd)
			}
			else{
				rn =rnorm(1,(lowsize[j]/low[j]),lsd)
			}

			y[i+1] = y[i] + rn

		}
		else{

			if(high[j]==0){
				rn = norm(1,hmean,hsd)
			}
			else{
				rn =rnorm(1,(highsize[j]/high[j]),hsd)
			}

			y[i+1] = y[i] + rn
		}



		if(y[i+1]>y[i]){
			direction <- c(direction,1)
		}
		else{
			direction <- c(direction,0)
		}

	}
	else{

		horl_rn = sample(1:totalcount,1)

		if(horl_rn<=lowcount){

			rn = rnorm(1,lmean,lsd)

			y[i+1] = y[i] + rn

		}
		else{

			rn = rnorm(1,hmean,hsd)

			y[i+1] = y[i] + rn


		}

		if(y[i+1]>y[i]){
			direction <- c(direction,1)
		}
		else{
			direction <- c(direction,0)
		}


	}
}

y
plot.ts(y)