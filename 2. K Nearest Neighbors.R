#Install required packages
install.packages("kknn")
install.packages("data.table")

#Load packages
library(kknn)
library(data.table)

#Merge Training and Test Files to create a whole set of rows, which will later be split during K fold cross validation
files=list.files(pattern = ".csv")
temp=lapply(files,fread,sep=",")
data=rbindlist(temp,fill=TRUE)
write.csv(data,file='Wholeset.csv',row.names = FALSE)
AUData=read.csv('Wholeset.csv')

#Scaling Temperature and LongTrip columns to the scale of Long for calculated distances approriately.
AUData$Temp<-scale(AUData$Temp,sd(AUData$Long))
AUData$LongTrip<-scale(AUData$LongTrip,sd(AUData$Long))

# Model 1: Considering (lat,long)
#--------------------------------
#Setup variable required for running model

n=dim(AUData)[1]
set.seed(40)
ind = sample(1:n,1000)
TORunData = AUData[ind,]
Y = AUData$TripCount[ind]

# Creating train and test dataframes
train = data.frame(Y,TORunData$Lat,TORunData$Long)
test = data.frame(Y,TORunData$Lat,TORunData$Long)


#setup parameters for K fold cross validation.
n = dim(TORunData)[1]
kcv = 5
n0 = round(n/kcv,0)

#matrix to capture MSEs for different values of K and for each cross validation
out_MSE = matrix(0,kcv,600)

used = NULL
set = 1:n

#Run k fold cross validation
for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:600){
    
    near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}

#Plot the MSEs for various values of k
mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:600)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="Number of Trips considering (lat,long)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/600)+0.4,sqrt(mMSE[600]),"k=600")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")

#Append best case K and corresponding RMSE for plotting
bestklist<-c(best)
ormse=sqrt(mMSE[best])
best_out_of_sample_RMSE_list=c(ormse)
#-----------------------------------------------------
# Model 2: Considering considering (lat,long,LongTrip)
#-----------------------------------------------------

#Setup variable required for running model

n=dim(AUData)[1]
set.seed(40)
ind = sample(1:n,1000)
TORunData = AUData[ind,]

Y = AUData$TripCount[ind]

# Creating train and test dataframes
train = data.frame(Y,TORunData$Lat,TORunData$Long,TORunData$LongTrip)
test = data.frame(Y,TORunData$Lat,TORunData$Long,TORunData$LongTrip)

#setup parameters for K fold cross validation.
n=dim(TORunData)[1]
kcv = 5
n0 = round(n/kcv,0)

#matrix to capture MSEs for different values of K and for each cross validation
out_MSE = matrix(0,kcv,600)

used = NULL
set = 1:n

#Run k fold cross validation
for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:600){
    
    near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}

#Plot the MSEs for various values of k
mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:600)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="Number of Trips considering (lat,long,longtrip)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/600)+0.4,sqrt(mMSE[600]),"k=600")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")

#Append best case K and corresponding RMSE for plotting
bestklist<-append(bestklist,best)
ormse=sqrt(min(mMSE))
best_out_of_sample_RMSE_list<-append(best_out_of_sample_RMSE_list,ormse)
#-------------------------------------------------------
# Model 3: Considering (lat,long,temp)
#-------------------------------------------------------

#Setup variable required for running model
n=dim(AUData)[1]
set.seed(18)
ind = sample(1:n,1000)
TORunData = AUData[ind,]

Y = AUData$TripCount[ind]

# Creating train and test dataframes
train = data.frame(Y,TORunData$Lat,TORunData$Long,TORunData$Temp)
test = data.frame(Y,TORunData$Lat,TORunData$Long,TORunData$Temp)

#setup parameters for K fold cross validation.
n=dim(TORunData)[1]
kcv = 5
n0 = round(n/kcv,0)

#matrix to capture MSEs for different values of K and for each cross validation
out_MSE = matrix(0,kcv,600)

used = NULL
set = 1:n

#Run k fold cross validation
for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:600){
    
    near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}

#Plot the MSEs for various values of k
mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:600)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="Number of Trips considering (lat,long,temp)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/600)+0.4,sqrt(mMSE[600]),"k=600")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")

#Append best case K and corresponding RMSE for plotting
bestklist<-append(bestklist,best)
ormse=sqrt(min(mMSE))
best_out_of_sample_RMSE_list<-append(best_out_of_sample_RMSE_list,ormse)
#--------------------------------------------------------------------
#Model 4: Considering (lat,long,temp and LongTrip)
#--------------------------------------------------------------------

#Setup variable required for running model
n=dim(AUData)[1]
set.seed(18)
ind = sample(1:n,1000)
TORunData = AUData[ind,]
Y = AUData$TripCount[ind]

# Creating train and test dataframes
train = data.frame(Y,TORunData$Lat,TORunData$Long,TORunData$Temp,TORunData$LongTrip)
test = data.frame(Y,TORunData$Lat,TORunData$Long,TORunData$Temp,TORunData$LongTrip)

#setup parameters for K fold cross validation.
n=dim(TORunData)[1]
kcv = 5
n0 = round(n/kcv,0)

#matrix to capture MSEs for different values of K and for each cross validation
out_MSE = matrix(0,kcv,600)

used = NULL
set = 1:n

#Run k fold cross validation
for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:600){
    
    near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}

#Plot the MSEs for various values of k
mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:600)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="Number of Trips considering (lat,long,temp,longtrip)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/600)+0.4,sqrt(mMSE[600]),"k=600")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")

#Append best case K and corresponding RMSE for plotting
bestklist<-append(bestklist,best)
ormse=sqrt(min(mMSE))
best_out_of_sample_RMSE_list<-append(best_out_of_sample_RMSE_list,ormse)

#Delete the file created for KNN K fold cross validation by merging training and test datasets
unlink('Wholeset.csv', recursive = FALSE, force = FALSE)

#Summarizing all four models in a plot.

plot(best_out_of_sample_RMSE_list,bestklist,xlab='Out of Sample RMSE',ylab="K used for KNN",main="Plot of K versus its best RMSE(Out of Sample)")
