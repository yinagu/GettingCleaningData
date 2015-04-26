library(dplyr)
library(reshape2)

# read data files
trainSub <- read.csv('./UCI HAR Dataset/train/subject_train.txt', sep="",header=FALSE)
trainY <- read.csv('./UCI HAR Dataset/train/y_train.txt',sep="",header=FALSE)
trainX <- read.csv('./UCI HAR Dataset/train/X_train.txt',sep="",header=FALSE)
train <- data.frame(trainSub, trainY, trainX)

testSub <- read.csv('./UCI HAR Dataset/test/subject_test.txt',sep="",header=FALSE)
testY <- read.csv('./UCI HAR Dataset/test/y_test.txt',sep="",header=FALSE)
testX <- read.csv('./UCI HAR Dataset/test/X_test.txt',sep="",header=FALSE)
test <- data.frame(testSub, testY, testX)

# merge training and test datasets
allData <- rbind(train, test)

# rename activity names
actCode <- read.csv('./UCI HAR Dataset/activity_labels.txt', sep="", header=FALSE)
allData$activity <- as.character(actCode[match(allData$activity, actCode[,1]), 2])

# rename column names by features
features <- read.csv('./UCI HAR Dataset/features.txt', sep="", header=FALSE)
unknown <- as.vector(features[ ,2])
colnames(allData) <- c('subject', 'activity', unknown)

# extract only mean, std column variables
names(allData) <- make.names(names=names(allData), unique=TRUE, allow_=TRUE)
meanStdData <- select(allData, subject, activity, contains("mean"), contains("std"), -contains("freq"), -contains("angle"))

# clean column names 
tidyNames <- gsub("\\.\\.\\.",".",colnames(meanStdData))
tidyName <- gsub("\\.\\.","",tidyNames)
colnames(meanStdData) <- tidyName

# create tidy dataset
tidyData <- meanStdData %>% group_by(subject, activity) %>% summarise_each(funs(mean))
# transform the wide short dataset into thin long dataset
tidyMelt <- melt(tidyData, id=c("subject","activity"), measure.vars=tidyName[-(1:2)])
colnames(tidyMelt) <- c("Subject", "Activity","Measurement","Average_value")

# output results
write.table(tidyMelt, file="tidy_average.txt", row.names=FALSE)