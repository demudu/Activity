# Load the packages
library(caret)
library(dplyr)

library(ggplot2)
library(scales)

# set working directory
setwd("D:/AppliedStatistics/Cousera/8_PREDMACHLEARN/Course_Project")

# Read Data from Web

training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
                     header = TRUE)

testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
                    header = TRUE)

# Explore the DATA
dim(training)
dim(testing)
# Training data consist of 19,622 observations 
# Testing data consist of 20 observations

head(training)

# Summarize to percentages of each classe

training_figs_data = data.frame(table(training$classe)/nrow(training))

# Plot the percentages
names(training_figs_data) = c("Classes", "Percentage")
training.plot = ggplot(training_figs_data, aes(x=Classes, y=Percentage, colour=Classes, fill=Classes)) +
            geom_bar(stat="identity") + scale_y_continuous(labels=percent, limits=c(0,0.3)) + 
            geom_text(data=training_figs_data, aes(label=paste0(round(Percentage*100,1),"%"),
                     y=Percentage+0.012), size=4) +
            labs(title = "Percentage  of Activity by Classes ")

pdf("figures/Classes.pdf")
training.plot
dev.off()


# There are a total of 160 variables in the data set. We are to predict the variable classes using all other variables

# Since the testing onl;y consist of 20 observations, I will split the training set data to training and validation 

summary(training)

# 

# Create a function to cleanup the data set

cleandata <- function(myData) {
  # following variables are no use for our predictions
  # X - only a running number of observations
  # user_name - name is person who was doing the activity
  # raw_timestamp_part_1 & raw_timestamp_part_2 - 
  # cvtd_timestamp 
  # new_window &  num_window 
  
  myData <- myData[,-c(1:7)]
  
  #remove those variables with percentage of NA's exit 95%
  
  drops <- (colSums(is.na(myData))> round(dim(myData)[1])*0.95)
  myData <- myData[ , !(drops)]
  
  # Removing zero covariates
  
  nsv <- nearZeroVar(myData, saveMetrics = TRUE)
  myData <- myData[nsv$nzv==FALSE]  
}
# Cleanup the training data with the cleandata function
cleaneddata <- cleandata(training)

# Since training set data is very large with 19,622 observations, we split to training and validation set

# I sample 30% of the data for training model and 5% data for validation


train <- cleaneddata[sample(nrow(cleaneddata), size = round(0.3*dim(training)[1])),]
validation <- cleaneddata[sample(nrow(cleaneddata), size=round(0.1*dim(training)[1])),]


# Building a model

# the variable to be predicted consist of 5 classess. Thus this a classification problem
# as such I propose few models that handle classification prediction. Based accuracy model will be selected

# Model 1 - using decision tree

Model1 <- train(classe~., method="rpart",data=train)

model1sum <- confusionMatrix(validation$classe,predict(Model1,newdata = validation))

# Accuracy = 0.4941

# Model 2 - use bagging
Model2 <- train(classe~., method="treebag",data=train)
model2sum <- confusionMatrix(validation$classe,predict(Model2,newdata = validation))


# Model 3 - use Random Forest
Model3 <- train(classe~., method="rf",data=train)
model3sum <-confusionMatrix(validation$classe,predict(Model3,newdata = validation))

# Model 4 - use Boosting
Model4 <- train(classe~., method="gbm",data=train)
model4sum <-confusionMatrix(validation$classe,predict(Model4,newdata = validation))


##ACCURACY
#Based on the accuracy of the above 4 confusionmatrix, both Model 2 and Model 3 perform better than Model 1 and Model 4 . Between Model 2 and Model 3, Model 2 perform slightly better. As such Model 2 choosed to predict the validation data set with 20 observations.

###Prediction on the Testing Data set
#Clean the testing data set
testing<- cleandata(testing)


#Get the predictions
testing_predict <- predict(Model2,newdata = testing)


#My final predictions are :
testing_predict


