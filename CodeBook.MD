# Getting and Cleaning Data Project Codebook.

## Dataset
This project uses data from user activity using a smartphone accelerometer data.
Reference:
 - http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The steps taken in order to filter such dataset were:

1. Download data;
1. Unzip data;
1. Read test data features;
1. Read test data subject data;
1. Build the test dataset;
1. Read train set data;
1. Read train data features;
1. Read train data subject data;
1. Build the train dataset;
1. Merge the trainning and test datasets;
1. Replace Feature code with activity name;
1. Filter only __mean__ and __std__ columns;
1. Agregate dataset using __feature__ and __subject__ columns;
1. Write output.

## Variables

We filtered the original dataset calculating each variable average value then
grouping them by subject (```subject``` column) and activity(```feature```
column).

The following variables have each 3 readings (one for each axis - X,Y,Z)

1. tBodyAcc-mean
1. tBodyAcc-std
1. tGravityAcc-mean
1. tGravityAcc-std
1. tBodyAccJerk-mean
1. tBodyAccJerk-std
1. tBodyGyro-mean
1. tBodyGyro-std
1. tBodyGyroJerk-mean
1. tBodyGyroJerk-std
1. fBodyAcc-mean
1. fBodyAcc-std
1. fBodyAccJerk-mean
1. fBodyAccJerk-std
1. fBodyGyro-mean
1. fBodyGyro-std


The following variables only count with a single value

1. tBodyAccMag-mean
1. tBodyAccMag-std
1. tGravityAccMag-mean
1. tGravityAccMag-std
1. tBodyAccJerkMag-mean
1. tBodyAccJerkMag-std
1. tBodyGyroMag-mean
1. tBodyGyroMag-std
1. tBodyGyroJerkMag-mean
1. tBodyGyroJerkMag-std
1. fBodyAccMag-mean
1. fBodyAccMag-std
1. fBodyBodyAccJerkMag-mean
1. fBodyBodyAccJerkMag-std
1. fBodyBodyGyroMag-mean
1. fBodyBodyGyroMag-std
1. fBodyBodyGyroJerkMag-mean
1. fBodyBodyGyroJerkMag-std

We groupped all cases using two variables:

1. feature: one of the following:
    1. LAYING
    1. SITTING
    1. STANDING
    1. WALKING
    1. WALKING_DOWNSTAIRS
    1. WALKING_UPSTAIRS
1.  subject: 1-30 individuals;


Feature Selection 
=================

The features selected for this database is produced by the provided script run_analysis.R.

The script combines several mean and standard deviation values from a complex dataset that comes from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. 
The script combines that data in a singe tidy data-table, and provides their mean values grouped by the subject (person) and activity.

The following features are included:

1 Subject
2 Activity
3 tBodyAccmeanX.mean
4 tBodyAccmeanY.mean
5 tBodyAccmeanZ.mean
6 tBodyAccstdX.mean
7 tBodyAccstdY.mean
8 tBodyAccstdZ.mean
9 tGravityAccmeanX.mean
10 tGravityAccmeanY.mean
11 tGravityAccmeanZ.mean
12 tGravityAccstdX.mean
13 tGravityAccstdY.mean
14 tGravityAccstdZ.mean
15 tBodyAccJerkmeanX.mean
16 tBodyAccJerkmeanY.mean
17 tBodyAccJerkmeanZ.mean
18 tBodyAccJerkstdX.mean
19 tBodyAccJerkstdY.mean
20 tBodyAccJerkstdZ.mean
21 tBodyGyromeanX.mean
22 tBodyGyromeanY.mean
23 tBodyGyromeanZ.mean
24 tBodyGyrostdX.mean
25 tBodyGyrostdY.mean
26 tBodyGyrostdZ.mean
27 tBodyGyroJerkmeanX.mean
28 tBodyGyroJerkmeanY.mean
29 tBodyGyroJerkmeanZ.mean
30 tBodyGyroJerkstdX.mean
31 tBodyGyroJerkstdY.mean
32 tBodyGyroJerkstdZ.mean
33 tBodyAccMagmean.mean
34 tBodyAccMagstd.mean
35 tGravityAccMagmean.mean
36 tGravityAccMagstd.mean
37 tBodyAccJerkMagmean.mean
38 tBodyAccJerkMagstd.mean
39 tBodyGyroMagmean.mean
40 tBodyGyroMagstd.mean
41 tBodyGyroJerkMagmean.mean
42 tBodyGyroJerkMagstd.mean
43 fBodyAccmeanX.mean
44 fBodyAccmeanY.mean
45 fBodyAccmeanZ.mean
46 fBodyAccstdX.mean
47 fBodyAccstdY.mean
48 fBodyAccstdZ.mean
49 fBodyAccJerkmeanX.mean
50 fBodyAccJerkmeanY.mean
51 fBodyAccJerkmeanZ.mean
52 fBodyAccJerkstdX.mean
53 fBodyAccJerkstdY.mean
54 fBodyAccJerkstdZ.mean
55 fBodyGyromeanX.mean
56 fBodyGyromeanY.mean
57 fBodyGyromeanZ.mean
58 fBodyGyrostdX.mean
59 fBodyGyrostdY.mean
60 fBodyGyrostdZ.mean
61 fBodyAccMagmean.mean
62 fBodyAccMagstd.mean
63 fBodyBodyAccJerkMagmean.mean
64 fBodyBodyAccJerkMagstd.mean
65 fBodyBodyGyroMagmean.mean
66 fBodyBodyGyroMagstd.mean
67 fBodyBodyGyroJerkMagmean.mean
68 fBodyBodyGyroJerkMagstd.mean
