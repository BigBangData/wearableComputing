# The following script processes raw data from the human activity experiment 
# The objective is to produce a tidy data set per the course rubric. 


# Set working directory & load packages...
setwd("C:/Users/mikew_000/Documents/DataScience/wearableComputing")
library(plyr); library(dplyr); library(data.table)

# Read in variable reference files to dataframes...
activity <- read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE, sep=" ", col.names=c("activityCode", "activity"))
features <- read.table("UCI HAR Dataset/features.txt", header=FALSE, sep=" ", col.names=c("featuretCode", "featureDesc"))


# This function iterates thgough steps required to build the training and testing dataset...

assembleData <- function(fileDir) {
      
      # read in training or testing set into a dataframe and add column names from the features file...
      X_File <- paste("UCI HAR Dataset/", fileDir, "/X_", fileDir, ".txt", sep="")
      trainingSet <- read.table(X_File, header=FALSE, sep="")
      names(trainingSet) <- features$featureDesc
      
      # remove those columns that do not have "mean" or "std" in the name...
      features$flag <- grepl("mean|std", features[,2])
      features$Index <- 1:561
      trainingSet <- trainingSet[, features[which(features$flag == TRUE),4]]
      
      # read subject and lables files the train/test directories...
      subject_File <- paste("UCI HAR Dataset/", fileDir, "/subject_", fileDir, ".txt", sep="")
      y_File <- paste("UCI HAR Dataset/", fileDir, "/y_", fileDir, ".txt", sep="")
            
      subject <- read.table(subject_File, header=FALSE, sep="", col.names="Person")
      testLabels <- read.table(y_File, header=FALSE, sep="", col.names="activityCode")
      
      # Add activity code to trainingset w/o altering order (hence plyr join method) and name it, 'Activity' ...
      activity$activitytCode <- as.character(activity$activityCode)
      testLabels$activityCode <- as.character(testLabels$activityCode)
      testLabels$activityDesc <- join(testLabels, activity, by = "activityCode")
      trainingSet <- cbind(testLabels$activityDesc$activity, trainingSet)
      colnames(trainingSet)[1] <- "Activity"
      
      # reformat subject in a more readable way, e.g. "Person 01" and add to trainingSet...
      subject[,1] <- paste("Person", sprintf("%02.0f", subject[,1]))
      trainingSet <- cbind(subject, trainingSet)
}

testSet <- assembleData("test")
trainSet <- assembleData("train")

# After function runs twice, there will be two datasets, one for training and one for testing. 
# Combine these two sets...
tidyDataSet <- rbind(trainSet, testSet)

# calculate the average for all variables by subject and activity (step 5)
moltenTrainingSet <- melt(tidyDataSet, 1:2, 3:81)
finalOut <- dcast(moltenTrainingSet, Person + Activity ~ variable, mean)
write.table(finalOut, "finalOutput.txt", row.name=FALSE) 

Print("SCript is finished. You will find finalOutput.txt in the working directory. A nice tidy dataset!")
