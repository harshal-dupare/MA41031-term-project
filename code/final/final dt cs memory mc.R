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

#defining the gap between the past the memory 
#here there is specific range of past which is just perfect 
#too low values of past leads to lot let consideration of variation
past=9

#creating base vector for calculating state base =(2**p-1,2**p-2,.....,2**1,1)
base = 1
for(i in 1:(past-1)){
	base <- c(2**i,base)
}
base

#defining two arrrays to store the number of occurances of high and low for each combination
high = matrix(0,2**past,1)
low = matrix(0,2**past,1) 
highsize = matrix(0,2**past,1)
lowsize = matrix(0,2**past,1)
hsize = 0
lsize = 0

#variables to count total number of highs and lows 
highcount = 0
lowcount = 0

maxvalue=max(x)
minvalue=min(x)


#direction matrix to store array of direction of movement
direction = matrix(0,rows-1,1)
for(i in 1:(rows-1)){

	if(x[i]<x[i+1]){
		direction[i]=1
	}

}
direction


#filling high low counts and sizes
for(i in 1:(rows-1-past)){

	number=sum(base*direction[(i):(i+past-1)])

	if(direction[i+past]==1){
		highsize[number+1]=highsize[number+1]+(x[i+past+1]-x[i+past])
		high[number+1] <- ( high[number+1] + 1 )
		highcount <- ( highcount + 1 )
		hsize <- c(hsize,(x[i+past+1]-x[i+past]))
	}
	if(direction[i+past]==0){
		lowsize[number+1]=lowsize[number+1]+(x[i+past]-x[i+past+1])
		low[number+1]=low[number+1]+1
		lowcount <- (lowcount+1)
		lsize <- c(lsize,(x[i+past]-x[i+past+1]))
	}

}

hsd = sd(hsize)/sqrt(highcount)
lsd = sd(lsize)/sqrt(lowcount)
hmean = mean(hsize)
lmean = -mean(lsize)

for(i in 1:(2**past)){

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
predictions=5000


#creating the final forcast array as "y"
forcast=matrix(0,predictions,1)
y=c(x,forcast)


for(i in rows:(rows+predictions-1)){

	number=sum(base*direction[(i-past):(i-1)])

	horl_rn = sample(1:range[number+1],1)

	if(horl_rn <= low[number+1]){

		if(low[number+1]==0){
			rn = rnorm(1,lmean,lsd)
		}
		else{
			rn =rnorm(1,(lowsize[number+1]/low[number+1]),lsd)
		}

		y[i+1] = y[i] + rn

	}
	else{

		if(high[number+1]==0){
			rn = norm(1,hmean,hsd)
		}
		else{
			rn =rnorm(1,(highsize[number+1]/high[number+1]),hsd)
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

y
plot.ts(y)