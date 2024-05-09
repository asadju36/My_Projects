rm(list = ls())
# Working directory to the location where the file is located 
setwd("C:/Users/asadj/OneDrive/Documents/IT/DS & DE Projects")
#setwd("C:/Users/Leah/Documents/!MIS 7621/Project")

library(dplyr)
library(plyr)
library(factoextra)
#Reading  a csv file of Covid-19 data
Covid_19<-read.csv("time_series_covid_19_confirmed_US_missing_vals.csv", sep = ",", header = T)
#Summary of data
summary(Covid_19)

#Finding missing values in two important columns(Lat and Long_ columns)
summary(Covid_19$Lat, Covid_19$Long_)

#Position of missing values in Lat and Long_ columns
which(is.na(Covid_19))

#Total missing values in Lat and Long_ columns
sum(is.na(c(Covid_19$Lat, Covid_19$Long_)))

#Removing rows with NA values
Covid19<-Covid_19[complete.cases(Covid_19), ]

#Verifying whether the rows with NA values have been removed
sum(is.na(c(Covid19$Lat, Covid19$Long_)))

#Counting Total Rows
nrow(Covid_19)
nrow(Covid19)

#Remove non-usable columns
COVID19<- subset(Covid19,select = -c(2,3,4,5))
names(COVID19)
View(COVID19)
names(COVID19)
COVID19<-as.data.frame(COVID19)
#Rename function to rename ambiguous column names
colnames(COVID19)[colnames(COVID19) %in% c("Admin2", "Province_State","Country_Region","Lat", "Long_","Combined_Key")] <- c("City", "State","Country", "Latitude","Longitude","City_State")
View(COVID19)

d1<-COVID19



#filtering out Islands and Ships which are not states
d1<- subset(d1, State !="American Samoa"& 
              State !="Diamond Princess"& 
              State !="Grand Princess"& 
              State !="Guam"& 
              State !="Northern Mariana Islands"& 
              State !="Virgin Islands")
d1

library('skimr')
skim(d1)

# View data 
View(d1)

d1<-as.data.frame(d1)



#Converting all date rows into months columns to make a new table


new_data <- cbind.data.frame(d1[,1:7],
                             'Jan 2020' = rowSums(d1[17]), 
                             'Feb 2020' = rowSums(d1[46]), 
                             'Mar 2020' = rowSums(d1[77]),
                             'Apr 2020' = rowSums(d1[107]),
                             'May 2020' = rowSums(d1[138]),
                             'Jun 2020' = rowSums(d1[168]),
                             'Jul 2020' = rowSums(d1[199]),
                             'Aug 2020' = rowSums(d1[228]))


summary(new_data)

sum(is.na(c(new_data$UID)))
sum(is.na(c(new_data$City)))
sum(is.na(c(new_data$State)))
sum(is.na(c(new_data$Country_Region)))
sum(is.na(c(new_data$Latitude)))
sum(is.na(c(new_data$Longitude)))
sum(is.na(c(new_data$City_State)))
sum(is.na(c(new_data$`Jan 2020`)))
sum(is.na(c(new_data$`Feb 2020`)))
sum(is.na(c(new_data$`Mar 2020`)))
sum(is.na(c(new_data$`Apr 2020`)))
sum(is.na(c(new_data$`May 2020`)))
sum(is.na(c(new_data$`Jun 2020`)))
sum(is.na(c(new_data$`Jul 2020`)))
sum(is.na(c(new_data$`Aug 2020`)))



#box plot 

cases<-select(new_data, "Jan 2020","Feb 2020","Mar 2020","Apr 2020","May 2020","Jun 2020","Jul 2020","Aug 2020") 

cases 

boxplot(cases,ylim= c(0,35000),ylab="Number of cases") 

  

 #scatter plot with correlation between August and March cases 

plot(cases$'Mar 2020',cases$`Aug 2020`,xlim =c(0, 220),ylim =c(0, 2000)) 

cor(cases$`Aug 2020`,cases$'Mar 2020')      

  

 #Taking only number of confirmed cases columns in eight months 

new_data_clustering<-new_data[,-c(1:7)] 

View(new_data_clustering) 

new_data_clustering<-as.data.frame(new_data_clustering) 

  
##Clustering
#------------
# Considering  March and August for clustering 

inputs<-c("Mar 2020","Aug 2020") 

Clu_Mar_Aug<-new_data_clustering[inputs] 

Clu_Mar_Aug<-as.data.frame(Clu_Mar_Aug) 

  

#Apply K-means clustering  algoritm 

cl_2<-kmeans(Clu_Mar_Aug,2) 

cl_3<-kmeans(Clu_Mar_Aug,3) 

cl_4<-kmeans(Clu_Mar_Aug,4) 

cl_5<-kmeans(Clu_Mar_Aug,5) 

cl_6<-kmeans(Clu_Mar_Aug,6) 

table(cl_2$cluster) 

  

#Elbow method to determine optimum number of clusters  

k_values<-c(2,3,4,5,6) 

ss_values<-c(cl_2$tot.withinss, cl_3$tot.withinss,cl_4$tot.withinss,cl_5$tot.withinss,cl_6$tot.withinss) 

plot(k_values,ss_values,type="b",frame=FALSE,xlab="Number of Clusters K", ylab="Total within-clusters sum of squares") 

  

#looks like the optimal number cluster is 3  

#plot of clustering 3  

#plot-1
plot(Clu_Mar_Aug,col=cl_3$cluster, xlim=c(0,10000), xlab="Number of Confirmed Cases in March 2020",ylab="Number of Confirmed Cases in August 2020", main="Kmeans Clustering Plot" ) 

#plot-2
fviz_cluster(cl_3, data = Clu_Mar_Aug)
 

# Determining the center of cluster 

cl_3$centers  

#Cluster size 

cl_3$size 

##Time Series Analysis
library(lubridate)
library(ggplot2)
library(prophet)
library(caret)
library(pROC)
### Data Preparing for Time series analysis
#----------
View(d1)
time<- d1[,-c(1:3,5:7)]
View(time)
#Aggregate in US region
Time_Series_Analysis<- aggregate(x=time[,-1], by=list(US=time$Country), FUN=sum)
View(Time_Series_Analysis)

# Switching rows  to columns for Time series analysis
Time_Series_Data<-data.frame(t(Time_Series_Analysis))
View(Time_Series_Data)

#Converting row names of date to  date
Time_Series_Data<- cbind(rownames(Time_Series_Data), data.frame(Time_Series_Data, row.names = NULL))
View(Time_Series_Data)
#Changing column name
colnames(Time_Series_Data)<-c("Date", "Confirmed_Cases-US")

#Selecting columns for Time Series analysis
Time_Series_Data<-Time_Series_Data[-1,]
typeof(Time_Series_Data$Date)
View(Time_Series_Data)
nrow(Time_Series_Data)
#Converting date columne from character to Date
#Removing X from Date data
Time_Series_Data$Formated_Date<- gsub("\\X", "", Time_Series_Data$Date )
Time_Series_Data$Formated_Date <- gsub(",", "", Time_Series_Data$Formated_Date)
Time_Series_Data$Date<-NULL
View(Time_Series_Data)

#Converting Confirmed_Cases-US to numeric type
str(Time_Series_Data)
Time_Series_Data$`Confirmed_Cases-US`<-as.numeric(Time_Series_Data$`Confirmed_Cases-US`)
str(Time_Series_Data)

#Converting Formated_Date to Date type
Time_Series_Data$Formated_Date<- as.Date(Time_Series_Data$Formated_Date, format = '%m.%d.%Y')
View(Time_Series_Data)
str(Time_Series_Data)
head(Time_Series_Data)
class(Time_Series_Data)
##Insight and Time Series plots
#plot
qplot(Formated_Date,`Confirmed_Cases-US`, data=Time_Series_Data)

ds<- Time_Series_Data$Formated_Date
y<-Time_Series_Data$`Confirmed_Cases-US`
dataframe<-data.frame(ds,y)
#Forecasting 
For<- prophet(dataframe)

#Predicting
Prediction<-make_future_dataframe(For, periods = 200) #200 days
Forcasting<-predict(For, Prediction)

#Plot forcasting
plot(For, Forcasting)
dyplot.prophet(For, Forcasting)

#Forecasting components for checking weekly vs days
prophet_plot_components(For, Forcasting)

#Evaluating Model performance
Predicted<-Forcasting$yhat[1:221]  #yhat is predicted value
Actual<-For$history$y
plot(Actual,Predicted)
abline(lm(Predicted~Actual), col='red')
summary(lm(Predicted~Actual))


##Logistic regression
#....................
#Using  new_data from clustring part  
Log_data<-new_data[,c(8:15)]
View(Log_data)

library(caret)
library(pROC)
library(plyr)
#Changing Column Name
colnames(Log_data)[colnames(Log_data) %in% c("Jan 2020","Feb 2020", "Mar 2020","Apr 2020","May 2020","Jun 2020","Jul 2020","Aug 2020")] <- c("Jan_2020","Feb_2020", "Mar_2020","Apr_2020","May_2020","Jun_2020","Jul_2020","Aug_2020")
View(Log_data)
str(Log_data)
#Range of data
range(Log_data)

##Adding a new categorical column 
#which will categorized the confirmed Covid-19 numbers  all to categorical
#variable such low=<1000=1, medium=1000-2000=2, High=2000-5000=3, Super-High=5000+=4

Logistic_label<-cut(Log_data$Aug_2020, breaks = c(0,1000,2000,5000,239756), 
                     labels = c(1, 2, 3, 4), right=F)
Logistic_label
first_10_rowa <- Log_data[1:10,"Aug_2020"]

class(Logistic_label)

#Combined the labels of Aug_2020 and data of Aug_2020
#1=Low, 2=Medium, 3=High, 4=Super-High
Logistic_reg<-cbind(Log_data, Logistic_label)
Logistic_reg<-as.data.frame(Logistic_reg)                    
View(Logistic_reg)
str(Logistic_reg)

#missing values
colSums(is.na(Logistic_reg)) #one missing value
nrow(Logistic_reg) #3224
Logistic_analysis<-na.omit(Logistic_reg)
nrow(Logistic_analysis) #3223
View(Logistic_analysis)
str(Logistic_analysis)

# train and test data
idx<-sample(2,nrow(Logistic_analysis),replace=TRUE,prob=c(0.7,0.3))
trainData<-Logistic_analysis[idx==1,]
testData<-Logistic_analysis[idx==2,]
str(trainData)
response.test<-testData$Logistic_label
testData$Logistic_label<-NULL

#Multinomial reggresion applying on train data
install.packages("MLmetrics")
library(nnet)
library(MLmetrics)
Formula<-factor(Logistic_label)~.
Log_model<-multinom(Formula,data=trainData)

#Prediction
predicted.probability <- predict(Log_model, newdata = data.frame(testData), type ="class")

#Confusion matrix
confusionMatrix(predicted.probability,response.test)

#performing 10-fold cross validation and using multinomial-logistic regression
ctrl<-trainControl(method="cv",number=10, savePredictions = TRUE, classProbs=TRUE, summaryFunction =multiClassSummary)

levels(Logistic_analysis$Logistic_label)<-make.names(levels(factor(Logistic_analysis$Logistic_label)))
logit_reg_model<-train(Formula,method='multinom',metric="Accuracy", data=Logistic_analysis,trControl=ctrl)
logit_reg_model$results




