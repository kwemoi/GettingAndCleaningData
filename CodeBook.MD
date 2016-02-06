# Getting and Cleaning Data Project Codebook.

##Introduction

### Dataset
This project uses data from user activity using a smartphone accelerometer data.
Reference:
 - http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

### Processing (Feature Selection )
The features selected for this database is produced by the provided script run_analysis.R.

The script combines several mean and standard deviation values from a complex dataset that comes from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. 
The script combines that data in a singe tidy data-table, and provides their mean values grouped by the subject (person) and activity.

The script 'run_analysis.R' performs the 5 steps described in the course project's definition.
  1. Merges the training and the test sets to create one data set.
  1. Extracts only the measurements on the mean and standard deviation for each measurement.
  1. Uses descriptive activity names to name the activities in the data set
  1. Appropriately labels the data set with descriptive variable names.
  1. From the data set in step 4, creates a second, independent tidy data set
     with the average of each variable for each activity and each subject. R
     source code with all routines for the analysis.

##Variables
All the measurements refer to either a period of time or frequency, and either a mean or standard deviation.
The measurements are finally averaged and grouped by Subject and Activity.

The Subject is a number: 1-30 referring to participating individuals.

The Activity is one of the following:
    1. LAYING
    1. SITTING
    1. STANDING
    1. WALKING
    1. WALKING_DOWNSTAIRS
    1. WALKING_UPSTAIRS

Full variables/features produced (68 variable):
	1. Subject
	1. Activity
	1. tBodyAccmeanX.mean
	1. tBodyAccmeanY.mean
	1. tBodyAccmeanZ.mean
	1. tBodyAccstdX.mean
	1. tBodyAccstdY.mean
	1. tBodyAccstdZ.mean
	1. tGravityAccmeanX.mean
	1. tGravityAccmeanY.mean
	1. tGravityAccmeanZ.mean
	1. tGravityAccstdX.mean
	1. tGravityAccstdY.mean
	1. tGravityAccstdZ.mean
	1. tBodyAccJerkmeanX.mean
	1. tBodyAccJerkmeanY.mean
	1. tBodyAccJerkmeanZ.mean
	1. tBodyAccJerkstdX.mean
	1. tBodyAccJerkstdY.mean
	1. tBodyAccJerkstdZ.mean
	1. tBodyGyromeanX.mean
	1. tBodyGyromeanY.mean
	1. tBodyGyromeanZ.mean
	1. tBodyGyrostdX.mean
	1. tBodyGyrostdY.mean
	1. tBodyGyrostdZ.mean
	1. tBodyGyroJerkmeanX.mean
	1. tBodyGyroJerkmeanY.mean
	1. tBodyGyroJerkmeanZ.mean
	1. tBodyGyroJerkstdX.mean
	1. tBodyGyroJerkstdY.mean
	1. tBodyGyroJerkstdZ.mean
	1. tBodyAccMagmean.mean
	1. tBodyAccMagstd.mean
	1. tGravityAccMagmean.mean
	1. tGravityAccMagstd.mean
	1. tBodyAccJerkMagmean.mean
	1. tBodyAccJerkMagstd.mean
	1. tBodyGyroMagmean.mean
	1. tBodyGyroMagstd.mean
	1. tBodyGyroJerkMagmean.mean
	1. tBodyGyroJerkMagstd.mean
	1. fBodyAccmeanX.mean
	1. fBodyAccmeanY.mean
	1. fBodyAccmeanZ.mean
	1. fBodyAccstdX.mean
	1. fBodyAccstdY.mean
	1. fBodyAccstdZ.mean
	1. fBodyAccJerkmeanX.mean
	1. fBodyAccJerkmeanY.mean
	1. fBodyAccJerkmeanZ.mean
	1. fBodyAccJerkstdX.mean
	1. fBodyAccJerkstdY.mean
	1. fBodyAccJerkstdZ.mean
	1. fBodyGyromeanX.mean
	1. fBodyGyromeanY.mean
	1. fBodyGyromeanZ.mean
	1. fBodyGyrostdX.mean
	1. fBodyGyrostdY.mean
	1. fBodyGyrostdZ.mean
	1. fBodyAccMagmean.mean
	1. fBodyAccMagstd.mean
	1. fBodyBodyAccJerkMagmean.mean
	1. fBodyBodyAccJerkMagstd.mean
	1. fBodyBodyGyroMagmean.mean
	1. fBodyBodyGyroMagstd.mean
	1. fBodyBodyGyroJerkMagmean.mean
	1. fBodyBodyGyroJerkMagstd.mean

## Units

* t/f-BodyAcc-XYZ: standard gravity units (g)
* t/f-GravityAcc-XYZ: standard gravity units (g)
* t/f-BodyAccJerk-XYZ: standard gravity units (g) / second
* t/f-BodyGyro-XYZ: radians / second
* t/f-BodyGyroJerk-XYZ: radians / second<sup>2</sup>
* t/f-BodyAccMag: standard gravity units (g), absolute value
* t/f-GravityAccMag: standard gravity units (g), absolute value
* t/f-BodyAccJerkMag: standard gravity units (g) / second, absolute value
* t/f-BodyGyroMag: radians / second, absolute value
* t/f-BodyGyroJerkMag: radians / second<sup>2</sup>, absolute value


