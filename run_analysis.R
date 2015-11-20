##-------------------------------------------------------------------------------------------------------
## This R script does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
##      with the average of each variable for each activity and each subject.
##-------------------------------------------------------------------------------------------------------

# Set working directory to UCI HAR Dataset folder
        setwd("E:/Coursera/Getting_And_Cleaning_Data/Project/UCI HAR Dataset")

# Read in relevant files from parent directory
        activity_labels <- read.table("activity_labels.txt")
        features <- read.table("features.txt")
# Read in training files
        setwd("train")
        subject_train <- read.table("subject_train.txt")
        X_train <- read.table("X_train.txt")
        y_train <- read.table("y_train.txt")
        setwd("..")
# Read in test files
        setwd("test")
        subject_test <- read.table("subject_test.txt")
        X_test <- read.table("X_test.txt")
        y_test <- read.table("y_test.txt")
        setwd("..")
        
# Append test data under train data
        X_data <- rbind(X_train,X_test)
        labels_data <- rbind(y_train,y_test)
        subject_data <- rbind(subject_train,subject_test)
        
# Remove extra data from X_data (only keep mean and std measurements)
        
        #Filter Features for rows that contain "mean" or "std"
        feat_index_mean <- grep("mean()",features[,2],value = FALSE,fixed = TRUE)
        feat_index_std <- grep("std()",features[,2],value = FALSE,fixed = TRUE)
        relevant_index <- sort(rbind(feat_index_mean,feat_index_std))
        relevant_features <- features[relevant_index,] #col1 = indices, col2 = feature names

        #Filter X_data for "mean" and "std" data
        relevant_data <- X_data[,relevant_features[,1]]
        colnames(relevant_data) <- relevant_features[,2]
        
# Add activity names to labels_data
        activity_data <- merge(labels_data,activity_labels,by = "V1",sort = FALSE)

# Create data frame that contains subject, activity, means, and stds
        data_frame <- cbind(subject_data,activity_data[,2],relevant_data)
        colnames(data_frame) <- c("subject","activity",colnames(relevant_data))
        
# Create tidy data that summarizes the average of each variable by subject+activity
        library(dplyr)
        tidy_data <- group_by(data_frame,subject,activity) %>% summarise_each(funs(mean))
        write.table(tidy_data,"tidy_data.txt",row.names = FALSE)
        