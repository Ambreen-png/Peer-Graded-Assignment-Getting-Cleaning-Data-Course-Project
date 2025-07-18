# Load required package
library(dplyr)

# Step 0: Download and unzip data
if (!file.exists("./data")) { dir.create("./data") }

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/Dataset.zip")
unzip(zipfile = "./data/Dataset.zip", exdir = "./data")

# Step 1: Merge the training and the test sets
# Reading training data
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# Reading test data
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# Reading features and activity labels
features <- read.table("./data/UCI HAR Dataset/features.txt")
activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
colnames(activityLabels) <- c("activityId", "activityType")

# Assigning column names
colnames(x_train) <- features[, 2]
colnames(x_test) <- features[, 2]
colnames(y_train) <- colnames(y_test) <- "activityId"
colnames(subject_train) <- colnames(subject_test) <- "subjectId"

# Merging train and test sets
train <- cbind(subject_train, y_train, x_train)
test <- cbind(subject_test, y_test, x_test)
allData <- rbind(train, test)

# Step 2: Extract only mean and std measurements
data_mean_std <- allData %>%
  select(subjectId, activityId, contains("mean()"), contains("std()"))

# Step 3: Add descriptive activity names
data_named <- merge(data_mean_std, activityLabels, by = "activityId", all.x = TRUE)

# Step 4: Label dataset with descriptive variable names
names(data_named) <- gsub("^t", "Time", names(data_named))
names(data_named) <- gsub("^f", "Frequency", names(data_named))
names(data_named) <- gsub("Acc", "Accelerometer", names(data_named))
names(data_named) <- gsub("Gyro", "Gyroscope", names(data_named))
names(data_named) <- gsub("Mag", "Magnitude", names(data_named))
names(data_named) <- gsub("BodyBody", "Body", names(data_named))
names(data_named) <- gsub("-mean\\(\\)", "Mean", names(data_named), ignore.case = TRUE)
names(data_named) <- gsub("-std\\(\\)", "STD", names(data_named), ignore.case = TRUE)
names(data_named) <- gsub("-freq\\(\\)", "Frequency", names(data_named), ignore.case = TRUE)
names(data_named) <- gsub("angle", "Angle", names(data_named))
names(data_named) <- gsub("gravity", "Gravity", names(data_named))

# Step 5: Create tidy dataset with average of each variable for each subject & activity
tidyData <- data_named %>%
  select(-activityType) %>%
  group_by(subjectId, activityId) %>%
  summarise_all(mean) %>%
  arrange(subjectId, activityId)

# Save tidy dataset
write.table(tidyData, "secTidySet.txt", row.name = FALSE)
