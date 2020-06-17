#getting the data in dataframe
tcf <- read.csv("C:/Users/harshal d/Pictures/SP/TCF.csv")
head(tcf)

#getting the useful data
x<-tcf[,5]
head(x)

#plots
plot.ts(x)
hist(x)

#size of data in rows
rows = NROW(x)

#defining the bucket size
bucketsize = 1
# anaother processs to calculate the bucket size as window sd


bucket = matrix(0,rows,1)

for(i in 1:rows){
	bucket[i] = trunc((x[i]/bucketsize))
}



#defining two arrrays to store the number of occurances of high and low for each combination
avg = matrix(0,2**past,1)
vari = matrix(1,2**past,1) 
avg
vari

#initilizing with 1 both because equal likelihood of going up and down

maxvalue=max(x)
minvalue=min(x)



maxbucket = trunc(maxvalue/bucketsize)
minbucket = trunc(minvalue/bucketsize)



for(i in 1:(maxbucket-minbucket+1)){

mean





}













direction = matrix(0,rows-1,1)

for(i in 1:(rows-1)){

if(x[i]<=x[i+1]){
	direction[i]=1
}

}

direction

list=matrix(0,rows-past+1,1)
k=1

#creating number for first window
number = 0
for(j in 1:past){

	if(direction[j]==1){
		number = number*2 + 1
	}else{
		number = number*2
	}	

}

number
list[k]=number
k=k+1


for(i in past:(rows-1)){

if(x[i+1]>=x[i]){
	high[number]=high[number]+(x[i+1]-x[i])
}else{
	low[number]=low[number]+(x[i]-x[i+1])
}


number = number%%(2**(past-1))

if(x[i+1]>=x[i]){
	nummber=number*2+1
}else{
	number=number*2
}

list[k]=number
k=k+1

}

low
high
#creating range array
range = low + high
range


#here goes the prediction size
predictions=100

forcast=matrix(0,predictions,1)

#creating the final forcast array as "y"
y=c(x,forcast)
y


#creating number for last window
number = 0
for(j in (rows-past):(rows-1)){

	if(direction[j]==1){
		number = number*2 + 1
	}else{
		number = number*2
	}	

}

number


for(i in rows:(rows+predictions-1)){
	
rn = runif(1,0,range[number])


y[i+1]<-(y[i]+(rn-low[number]))

number = number%%(2**(past-1))

if(y[i+1]>=y[i]){
	number=number*2+1
}else{
	number=number*2
}

}

direction
list
plot.ts(y)

y

