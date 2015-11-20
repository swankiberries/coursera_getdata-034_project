Coursera course: getdata-034
Date: 11/19/2015
===========================================================
CodeBook.md
===========================================================

This document describes the code in run_analysis.R

**Data set**
The original UCI HAR Dataset contained multiple files:
* activity_labels.txt (List of activities corresponding to labels)
* features.txt (List of features)
* subject_train.txt (List of subjects corresponding to training measurements)
* X_train.txt (Set of training measurements where columns corresponded to the feature list)
* y_train.txt (List of labels corresponding to training measurements
* subject_test.txt (List of subjects corresponding to test measurements)
* X_test.txt (Set of test measurements where columns corresponded to the feature list)
* y_test.txt (List of labels corresponding to test measurements

The features list included 3-axial signals in X, Y, and Z directions for acceleration and angular velocity detected by the mobile device.
The data set consists of estimated variables calculated from the features.
For the purposes of this analysis, only the mean and standard deviation were considered.

**Analysis**
The script performed analysis as follows:
1. Data files were individually read into objects using the read.table().
2. Each set of test data was appended to the corresponding training data using rbind().
3. The measurement data was filtered to only include features that contain "mean()" or "std()" in the name.
4. Activity names were merged into the list of labels for each measurement.
5. A final data frame was created by column binding sample, activity, and the mean/std values
Note: The dplyr package is required for the final summary!
6. To summarize the data, the mean of each column was generated by grouping by sample and activity.
7. Lastly, tidy_data.txt file is written to the working directory.