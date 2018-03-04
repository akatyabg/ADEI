#Univariate Descriptive Analysis (to be included for each variable):
##Original numeric variables corresponding to qualitative concepts have to be converted to factors.
##Original numeric variables corresponding to real quantitative concepts are kept as numeric but additional factors should also be created as a discretization of each numeric variable.
##Exploratory Data Analysis for each variables (numeric summary and graphic support).  

setwd("C:/Users/Katya/Desktop")
df <- read.table("green_tripdata_2016-01.csv", header = TRUE, sep = ",")      

#load packages
rm(list=ls())
requiredPackages <- c("effects","FactoMineR","car", "factoextra","RColorBrewer","ggplot2","mvoutlier","missMDA")
missingPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]

if(length(missingPackages)) install.packages(missingPackages)
lapply(requiredPackages, require, character.only = TRUE)

#Load samples

### Use birthday of 1 member of the group
set.seed(28061963)
nrow(df)
sam<-sample(1:nrow(df),5000)
sam<-as.vector(sort(sam))

df<-df[sam,]
save.image("Taxi5000_raw.RData")

load("Taxi5000_raw.RData")
#Converting numeric variables corresponding to qualitative concepts to factors:
# VendorID
sel<-which(df$VendorID==0.0);length(sel) #No missing Data
df$VendorID<-factor(df$VendorID,labels=c("Creative Mobile Technologies, LLC","VeriFone Inc."))
summary(df$VendorID)
table(df$VendorID)
barplot(prop.table(table(df$VendorID)))

# RateCodeID, there whas no group ride
sel<-which(df$RateCodeID==0.0);length(sel) #No missing Data
df$RateCodeID<-factor(df$RateCodeID,labels=c("Standard rate","JFK","Newark","Nassau or Westchester","Negotiated fare"))
summary(df$RateCodeID)
table(df$RateCodeID)
barplot(prop.table(table(df$RateCodeID)))

# Store_and_fwd_flag //first the N and than Y
sel<-which(df$Store_and_fwd_flag==0.0);length(sel) #No missing Data
df$Store_and_fwd_flag<-factor(df$Store_and_fwd_flag,labels=c("not a store and forward trip","store and forward trip"))
summary(df$Store_and_fwd_flag)
table(df$Store_and_fwd_flag)
barplot(prop.table(table(df$Store_and_fwd_flag)))

# Payment_type //only 4 values
sel<-which(df$Payment_type==0.0);length(sel) #No missing Data
df$Payment_type<-factor(df$Payment_type,labels=c("Credit card","Cash", "No charge", "Dispute"))
summary(df$Payment_type)
table(df$Payment_type)
barplot(prop.table(table(df$Payment_type)))

# Trip_type
sel<-which(df$Trip_type==0.0);length(sel) #No missing Data
df$Trip_type<-factor(df$Trip_type,labels=c("Street-hail","Dispatch"))
summary(df$Trip_type)
table(df$Trip_type)
barplot(prop.table(table(df$Trip_type)))

#Creating additional factors as a discretization 
#Factorize function:
factorize<- function(x) {
  quantile(x,seq(0,1,0.1))
  pp<-quantile(x);pp
  breaks<-c(unique(pp))
  f.x<-factor(cut(x,breaks))
  return(f.x);
}

#Passenger_count
df$f.passanger<-factorize(df$Passenger_count)
summary(df$f.passanger)

sel<-which(df$Passenger_count==0.0);length(sel) #2 missings
df[sel,"Passanger_count"]<-NA
boxplot(df$Passenger_count)

#Trip_distance
df$f.distance<-factorize(df$Trip_distance) # NO VA be
summary(df$distance)
sel<-which(df$Trip_distance==0.0);length(sel) #60 missings
df[sel,"Trip_distance"]<-NA 
boxplot(df$Trip_distance)

#Pickup_longitude
df$f.longtitude<-factorize(df$Pickup_longitude)
summary(df$f.longtitude)
#How to detect missing values? 0.0 is a possible value?
#sel<-which(df$Pickup_longitude==0.0);length(sel) #11 missings
#df[sel,"Pickup_longitude"]<-NA 
boxplot(df$Pickup_longitude)


#Pickup_latitude
df$f.latitude<-factorize(df$Dropoff_latitude)
summary(df$f.latitude) #11 NAs
boxplot(df$Pickup_latitude)


#Dropoff_longitude
df$f.longtitudeDrop<-factorize(df$Dropoff_longitude)
summary(df$f.longtitudeDrop) # 1 NAs
boxplot(df$Dropoff_longitude)


#Dropoff_latitude
quantile(df$Dropoff_latitude,seq(0,1,0.1))
pp<-quantile(df$Dropoff_latitude);pp
df$f.latitudeDrop<-factor(cut(df$Dropoff_latitude,pp))  # NO VA be
summary(df$f.latitudeDrop) # 4 NAs ? Outlier
boxplot(df$Pickup_latitude)

#Fare_amount
df$f.fare_amount<-factorize(df$Fare_amount)
summary(df$f.fare_amount)

sel<-which(df$Fare_amount==0.0);length(sel) #10 missings
df[sel,"Fare_amount"]<-NA 
boxplot(df$Fare_amount)

#Extra 
df$f.extra<-factorize(df$Extra)
summary(df$f.extra) #1 NA's
boxplot(df$Extra)

#MTA_tax
df$f.MTA_tax<-factorize(df$MTA_tax)
summary(df$f.MTA_tax) #11 NA's -> values of -0.5 => Outliers?
sel<-which(df$MTA_tax==0.0);length(sel) #103 missings
df[sel,"MTA_tax"]<-NA
boxplot(df$MTA_tax)


#Improvement_surcharge
df$f.Improvement_surcharge<-factorize(df$improvement_surcharge)
summary(df$f.Improvement_surcharge) #11 NA's -> values of -0.3 => Outliers?

sel<-which(df$improvement_surcharge==0.0);length(sel) #107 missings
df[sel,"improvement_surcharge"]<-NA
boxplot(df$improvement_surcharge)


#Tip_amount
df$f.tip_amount<-factorize(df$Tip_amount)
summary(df$f.tip_amount) #2869 NA's
boxplot(df$Tip_amount)


#Tolls_amount
df$f.toll<-factorize(df$Tolls_amount)
summary(df$f.toll) #4907 NA's, not well factorized 

#Total_amount
df$f.total<-factorize(df$Total_amount)  # NO VA be
summary(df$f.total)

sel<-which(df$Total_amount==0.0);length(sel) #9 missings
df[sel,"Total_amount"]<-NA 
boxplot(df$Total_amount)



