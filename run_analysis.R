#######################
#load packages and data
#######################

library(dplyr)

#load test data
vals_test <- read.table("UCI HAR Dataset/test/X_test.txt")
labs_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

#load training data
vals_train <- read.table("UCI HAR Dataset/train/X_train.txt")
labs_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

#load activity labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

#load features 
features <- read.table("UCI HAR Dataset/features.txt")


#######################
#Step 1: Merges the training and the test sets to create one data set.
#######################

#create test dataframe
testdf <- vals_test %>%
          mutate(activity = labs_test$V1) %>%#add activity labels
          mutate(subject = subject_test$V1) #Add subjects

#create training dataframe
trainingdf <- vals_train %>%
  mutate(activity = labs_train$V1) %>%#add activity labels
  mutate(subject = subject_train$V1) #Add subjects

#combine test and training into 1 df
combinedDf <- rbind(testdf, trainingdf)
combinedDf

#update column names 
#note this step is done last due to duplicate names in features, which cause errors when 
#added before completing the dplyr functions above
colnames(combinedDf) = c(features$V2, "activity", "subject")

#######################
#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
#######################

#find positions of the mean and standard deviation variables (i.e. those containg "mean()" or "std()")
meanstd.positions <- grep("mean\\(\\)|std\\(\\)" ,features$V2)
#Extracts columns with those postions, and also activity and subject columns
tidydata <- combinedDf[, c(meanstd.positions, 562:563)]
tidydata

#######################
#Step 3. Uses descriptive activity names to name the activities in the data set 
#######################

#find and replace each activity label with its descriptive name
for (i in 1:6) {
  tidydata$activity <- gsub(i, activity_labels[i, 2], tidydata$activity)
}

#######################
#Step 4. Appropriately labels the data set with descriptive variable names. 
#######################
names(tidydata) <- gsub("^t", "time", names(tidydata))
names(tidydata) <- gsub("^f", "frequency", names(tidydata))
names(tidydata) <- gsub("acc", "accelerometer", names(tidydata))
names(tidydata) <- gsub("acc", "gyroscope", names(tidydata))

#######################
#Step 5. From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.
#######################

tidydata2 <- tidydata %>%
             group_by(activity, subject) %>%
             summarise(across(everything(), list(mean)))

write.table(tidydata2, file = "/Users/natdeacon/N. Deacon Getting and Cleaning Data/step5data.txt", row.name = FALSE)







  
  
