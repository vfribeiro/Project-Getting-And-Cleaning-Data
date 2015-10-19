# R Script - run_analysis.R
# Coursera Getting and Cleaning Data Course Project
# for additional info, check README.md and CodeBook.md

##### SESSION A : Reading data and sanity checks
## reading train data
#
X_train <- read.csv("./UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
y_train <- read.csv("./UCI HAR Dataset/train/y_train.txt", sep="", header=FALSE)
subject_train <- read.csv("./UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

## reading test data
X_test <- read.csv("./UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
y_test <- read.csv("./UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE)
subject_test <- read.csv("./UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)

## reading class labels with their activity name and all features names
activity_labels <- read.csv("./UCI HAR Dataset/activity_labels.txt", 
                            sep="", header=FALSE, stringsAsFactors=FALSE)
# only column 2 is useful on features.txt
features_names <- read.csv("./UCI HAR Dataset/features.txt", 
                           sep="", header=FALSE, stringsAsFactors=FALSE)[,2]

## running some sanity checks before merging new data
# checking if all train data have the same amount of rows
#
if (nrow(X_train)==nrow(y_train) & nrow(X_train)==nrow(subject_train)) {
  print("All TRAIN data have the same amount of rows")
} else {
  print("WARNING: X_train, y_train and subject_train does NOT have the same amount of data!")
}

# checking if all test data have the same amount of rows
if (nrow(X_test)==nrow(y_test) & nrow(X_test)==nrow(subject_test)) {
  print("All TEST data have the same amount of rows")
} else {
  print("WARNING: X_test, y_test and subject_test does NOT have the same amount of data!")
}

# checking if features_names has any name that we intend to use for new columns
if (length(grep("activity\\_ID|subject\\_ID|train\\_Data|activity\\_Label)",features_names)>0)) {
  print("WARNING: new column names already in use by some features")
} else {
  print("All new column names not in use by any feature")
}

##### SESSION B : applying column names. Address Guidance #4
# Guidance #4 : Appropriately labels the data set with descriptive variable names
#
names(X_train) <- features_names
names(X_test) <- features_names
names(subject_train) <- "subject_ID"
names(subject_test) <- "subject_ID"
names(y_train) <- "activity_ID"
names(y_test) <- "activity_ID"
names(activity_labels) <- c("activity_ID", "activity_Label")

##### SESSION C: merges data in a single dataframe. Address Guidance #1
# Guidance #1 : Merges the training and test sets to create one data set 
#
train_data <- cbind(X_train, y_train, subject_train)
test_data <- cbind(X_test, y_test, subject_test)
all_data <- rbind(train_data,test_data)

##### SESSION D : filters merged data by mean/std. Adress Guidance #2
# Guidance #2 : Extracts only the measurements on the mean and standard deviation for each measurement
#
# first, create a vector with columns names having 'mean(' or 'std(' AND preserving 3 last new columns
filter_columns <- grep('mean\\(|std\\(|activity\\_ID|subject\\_ID', names(all_data))
# then create a new dataframe with mean, std columns only plus columns activity_ID, subject_ID, train_Data
filtered_data <- all_data[,filter_columns]

##### SESSION E : adding new column with activity Label. Address Guidance #3
# Guidance #3 : uses descriptive activity names to name the activities in the data set
#
filtered_data['activity_Label'] <- activity_labels[filtered_data$activity_ID,]$activity_Label

##### SESSION F : create a final dataset with the average for each variable, 
#####             for each activity and subject. Address Guidance #5
# Guidance #5 : From the data set in step 4, creates a second, independent tidy data set with 
#              the average of each variable for each activity and each subject.
#
final_dataset <- aggregate(filtered_data,list(activity_Label = filtered_data$activity_Label,
                                              subject_ID = filtered_data$subject_ID), FUN=mean)
# drops 3 last columns from the dataframe as they do not make sense with means or are repeated
# notice that after removing a column, there's one less column. This is the reason the same
# command is applied 3x
final_dataset[,69] <- NULL
final_dataset[,69] <- NULL
final_dataset[,69] <- NULL
write.csv(final_dataset, file = "finalTidyDataSet.csv", row.names = FALSE)

