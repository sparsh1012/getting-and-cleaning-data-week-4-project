#   run_analysis.R
#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, making a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
library(dplyr)
#  Reading Data

# Reading training data
trainingSubjects<-read.table(file.path("UCI HAR Dataset","train","subject_train.txt"))
trainingValues<-read.table(file.path("UCI HAR Dataset","train","X_train.txt"))
trainingActivity<-read.table(file.path("UCI HAR Dataset","train","y_train.txt"))

# Reading testing data
testSubjects<-read.table(file.path("UCI HAR Dataset","test","subject_test.txt"))
testValues<-read.table(file.path("UCI HAR Dataset","test","X_test.txt"))
testActivity<-read.table(file.path("UCI HAR Dataset","test","y_test.txt"))

# Reading features 
dataPath<-"UCI HAR Dataset"
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# Reading activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

# Step 1 - Merge the training and the test sets to create one data set
# concatenate individual data tables to make single data table
humanActivity <- rbind(cbind(trainingSubjects, trainingValues, trainingActivity),
                       cbind(testSubjects, testValues, testActivity))

# Remove individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, testSubjects, testValues, testActivity)

# Assign column names
colnames(humanActivity)<-c("subject",features[,2],"activity")
head(humanActivity)

# Step 2 - Extract only the measurements on the mean and standard deviation for each measurement
# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# ... and keep data in these columns only
humanActivity <- humanActivity[, columnsToKeep]

# Step 3 - Use descriptive activity names to name the activities in the dataset

# replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity,levels = activities[, 1], 
                                 labels = activities[, 2])

# Step 4 - Appropriately label the data set with descriptive variable names
# get column names
humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols

# Step 5 - Create a second, independent tidy set with the average of each variable for each 
# activity and each subject

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
























