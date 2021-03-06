# CodeBook - Getting and Cleaning Data - Project

## Summary

This CodeBook describes the variables, the data, and work done to clean up a dataset - [Human Activity Recognition Using Smartphones](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip ) - according the 5 requests provided by the **Getting and Cleaning Data Course Project Instructions**.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

A full description of the original dataset is available here :
<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>

The resulting filtered dataset can be found in the file `finalTidyDataSet.txt` in this repository. Please, refer to `README.md`, also in this repository, for more information about other files.

---

## Variables and data
All variables (or features) present in the `finalTidyDataSet.txt` are listed in the table below.

| Features                    |                             |                            | 
|-----------------------------|-----------------------------|----------------------------|
| activity_Label              | subject_ID                  | tBodyAcc-mean()-X          | 
| tBodyAcc-mean()-Y           | tBodyAcc-mean()-Z           | tBodyAcc-std()-X           |    
| tBodyAcc-std()-Y            | tBodyAcc-std()-Z            | tGravityAcc-mean()-X       |  
| tGravityAcc-mean()-Y        | tGravityAcc-mean()-Z        | tGravityAcc-std()-X        |  
| tGravityAcc-std()-Y         | tGravityAcc-std()-Z         | tBodyAccJerk-mean()-X      |
| tBodyAccJerk-mean()-Y       | tBodyAccJerk-mean()-Z       | tBodyAccJerk-std()-X       |  
| tBodyAccJerk-std()-Y        | tBodyAccJerk-std()-Z        | tBodyGyro-mean()-X         |  
| tBodyGyro-mean()-Y          | tBodyGyro-mean()-Z          | tBodyGyro-std()-X          |  
| tBodyGyro-std()-Y           | tBodyGyro-std()-Z           | tBodyGyroJerk-mean()-X     |  
| tBodyGyroJerk-mean()-Y      | tBodyGyroJerk-mean()-Z      | tBodyGyroJerk-std()-X      |   
| tBodyGyroJerk-std()-Y       | tBodyGyroJerk-std()-Z       | tBodyAccMag-mean()         |   
| tBodyAccMag-std()           | tGravityAccMag-mean()       | tGravityAccMag-std()       |   
| tBodyAccJerkMag-mean()      | tBodyAccJerkMag-std()       | tBodyGyroMag-mean()        |   
| tBodyGyroMag-std()          | tBodyGyroJerkMag-mean()     | tBodyGyroJerkMag-std()     |   
| fBodyAcc-mean()-X           | fBodyAcc-mean()-Y           | fBodyAcc-mean()-Z          |   
| fBodyAcc-std()-X            | fBodyAcc-std()-Y            | fBodyAcc-std()-Z           |   
| fBodyAccJerk-mean()-X       | fBodyAccJerk-mean()-Y       | fBodyAccJerk-mean()-Z      |   
| fBodyAccJerk-std()-X        | fBodyAccJerk-std()-Y        | fBodyAccJerk-std()-Z       |   
| fBodyGyro-mean()-X          | fBodyGyro-mean()-Y          | fBodyGyro-mean()-Z         |   
| fBodyGyro-std()-X           | fBodyGyro-std()-Y           | fBodyGyro-std()-Z          |   
| fBodyAccMag-mean()          | fBodyAccMag-std()           | fBodyBodyAccJerkMag-mean() | 
| fBodyBodyAccJerkMag-std()   | fBodyBodyGyroMag-mean()     | fBodyBodyGyroMag-std()     |
| fBodyBodyGyroJerkMag-mean() | fBodyBodyGyroJerkMag-std()  |                            |

Where :

* `activity_Label` is a label for the activity performed by a volunteer. It can be any of the following six activities : WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
* `subject_ID` is the volunteer unique ID
* all other features in the table above are fully explained in `features_info.txt` file, in the [original dataset](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip). But in short, these are coming from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.
* Please, notice that `run_analysis.R` generates a new dataset with only mean and standard deviation features from the original dataset. 

Each row of the dataset `finalTidyDataSet.txt` contains the average of each feature listed above for each activity and each subject.

Before generating this final dataset, project request items #1 to #5 are performed. 

---

## Work performed to clean up the data

The following steps are taken in `run_analysis.R` to generate `finalTidyDataSet.txt` from the original dataset :

1. All original data files are read in dataframes  
2. Quick sanity checks are performed :
    + A quick sanity check is done in the train data and in the test data to verify, before merging any data by columns, if amount of rows are the same.
    + Another quick sanity check is done in order to make sure that names that will be used for new columns (when merging data) does not exist in `features.txt.
3. Names to the dataframes columns are attributed according documentation :
    + `X_train` and `X_test` get names from `features.txt`
    + `subject_train` and `subject_test` single column is named `subject_ID`
    + `y_train` and `y_test` single columns are named `activity_ID`
    + `activity_Labels` columns are named `activity_ID` and `activity_Label`
    + **This step addressed, in advance, project request item #4**
4. Train data is merged with column bind. The same is done by test data. Then, a row bind is performed with the two datasets generating a data frame `all_data` containing all data from `X_train.txt`, `X_test.txt`, `y_train.txt`, `y_test.txt`, `subject_train.txt`, `subject_test.txt`.
    + **This step addressed project request item #1**
5. A new dataset `filtered_data` is create with `all_data` features that have `mean(` or `std(` (mean and standard deviation) and the columns `activity_ID`, `subject_ID`.
    + **This step addressed project request item #2**
6. A new column `activity_Label` is added to the dataframe `filtered_data` containing the label for the respective `activity_ID` according `activity_labels.txt` (loaded in `activity_Labels` dataframe)
    + **This step addressed project request item #3**
7. `final_dataset` is generated by aggregating data frame `filtered_data` with the average of each variable for each activity and each subject.
    + Three last non-useful columns are removed from `final_dataset` as their average does not make sense or they are repeated
    + A txt file `finalTidyDataSet.txt` is generated from `final_dataset`
    + **This step addressed project request item #5**

Additional commentaires can be found in `run_analysis.R`.

---

