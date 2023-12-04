library(ggplot2)
library(dplyr)
library(stringr)
library(caret)
library(mlr)
library(rpart)
library(randomForest)
library(xgboost)

setwd("E:/BB Learning/statistics/R/Car data")
car=read.csv("cardata.csv")
str(car)
dim(car)
summary(car)


#Model column
#string split function
s=str_split_fixed(car$Model," | ",3) #split model name into 3 separated with space bar
s
str(s) #it is as character
s=as.data.frame(s) #create a dataframe for the column
str(s) #converted to dataframe now

#Rename split Model column
d= s %>% rename ("modelname" ="V1","Variant"="V2", "Others"="V3")
d
#Bind the column to dataset
carfull=cbind(car,d) #combining split model name dataset to existing car dataset and saving in carfull dataset

#Delete Model column
carfull<-carfull[, -2] #since it is split, delete full model name column
head(carfull,30)
str(carfull)

dim(car)

#change price column to log function as variations is huge

carfull$Price <- log(carfull$Price)
str(carfull)

# editing alignment of text in graph vertically for make column(x axis) as names are long

ggplot(carfull, aes(x = Make, y = Price)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 3, hjust = 0)) 


#converting certain columns to factors:

carfull$Year=as.factor(carfull$Year)
carfull$Fuel.Type=as.factor(carfull$Fuel.Type)
carfull$Transmission=as.factor(carfull$Transmission)
carfull$Location=as.factor(carfull$Location)
carfull$Color=as.factor(carfull$Color)
carfull$Owner=as.factor(carfull$Owner)
carfull$Seller.Type=as.factor(carfull$Seller.Type)
carfull$Drivetrain=as.factor(carfull$Drivetrain)
str(carfull)

#split and remove cc unit from engine column

Enginesplit=str_split_fixed(car$Engine," ",2) #split engine column into 2 using spacebar given
str(Enginesplit) 
Enginesplit=as.data.frame(Enginesplit) #changing to dataframe so that we include to dataset
y=Enginesplit %>% rename("Engine cc"="V1", "remove"="V2")
carfull=cbind(carfull,y) #binding column to dataset using cbind
str(carfull)

#To remove previous engine column

carfull=carfull[,-c(11,24)] #removing 11th and 24th column 
str(carfull)


#split and remove bhp unit from power column
Powersplit=str_split_fixed(car$Max.Power," ",2)
str(Powersplit)
Powersplit=as.data.frame(Powersplit)
z=Powersplit %>% rename("Power bhp"="V1", "remove"="V2")
carfull=cbind(carfull,z)#binding column to dataset using cbind


#To remove previous power column
str(carfull)
carfull=carfull[,-c(11,24)] #removing 11th and 24th column
str(carfull)


#split and remove rpm unit from torque column
Torquesplit=str_split_fixed(car$Max.Torque," ",2)
str(Torquesplit)
Torquesplit=as.data.frame(Torquesplit)
v=Torquesplit %>% rename("Torque Nm"="V1", "remove"="V2")
carfull=cbind(carfull,v)#binding column to dataset using cbind

#To remove previous torque column
str(carfull)
carfull=carfull[,-c(11,24)] #removing 11th and 24th column
str(carfull)

#To remove unwanted columns
carfull=carfull[-c(11,12,13,14,15,16)]
str(carfull)


Premium=carfull %>%
  filter(Make %in% c("Rolls-Royce","Lamborghini","Porsche","Audi",
                     "Mercedes-Benz", "BMW","Ferrari","Jaguar", "Land Rover","MINI"))
str(Premium)


General=carfull %>%
  filter(!Make %in% c("Rolls-Royce","Lamborghini","Porsche","Audi",
                      "Mercedes-Benz", "BMW","Ferrari","Jaguar", "Land Rover","MINI"))
str(General)


#dplyr function for extracting avg price of scorpio car which ran under 50k km after year 2017


General %>% filter(modelname == "Scorpio", Kilometer<50000, Year==2017) %>% group_by(Location) %>%
  summarise(avg=mean(Price))



#Rename values in a column
renamed_carname <- General %>% 
  mutate(Make = case_when(Make == "Maruti Suzuki"~"Maruti",TRUE ~ Make))
head(renamed_carname)
head(General)

#handling missing values

# Check for missing values in the dataset variablewise and give count of NA in each column

sapply(General, function(x) sum(is.na(x)))

# count number of missing or empty values in each column (NA and empty spaces)

missing_count <- apply(General, 2, function(x) sum(is.na(x) | x == ""))
missing_count

# Subset the dataset to only rows with missing values

missing_values <- General[!complete.cases(General), ]

# Print the missing values

missing_values

# Impute missing count in the engine cc, power bhp, torque Nm columns using median imputation

General$`Engine cc`[is.na(General$`Engine cc`) | General$`Engine cc` == ""] <- median(General$`Engine cc`, na.rm = TRUE)
General$`Power bhp`[is.na(General$`Power bhp`) | General$`Power bhp` == ""] <- median(General$`Power bhp`, na.rm = TRUE)
General$`Torque Nm`[is.na(General$`Torque Nm`) | General$`Torque Nm` == ""] <- median(General$`Torque Nm`, na.rm = TRUE)
head(General,50)

#Update NA for empty values in Others column

General$Others[General$Others == ""] <- NA
head(General,40)
summary(General)

#Model Building

cars=lm(Price~Make+Year+modelname+Kilometer+Owner+Transmission+Fuel.Type,General)
summary(cars)

#predictive modeling
cars1=predict(cars,General)
cars1
summary(cars1)

#rmse
carrmse=sqrt(mean((General$Price-cars1)^2))
carrmse