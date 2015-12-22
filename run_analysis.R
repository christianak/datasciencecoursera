
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

# You should create one R script called run_analysis.R that does the following. 
#	1	Merges the training and the test sets to create one data set.
#	2	Extracts only the measurements on the mean and standard deviation for each measurement. 
#	3	Uses descriptive activity names to name the activities in the data set
#	4	Appropriately labels the data set with descriptive variable names. 
#	5	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##############################################################################

# 'features_info.txt': Shows information about the variables used on the feature vector.

# 'features.txt': List of all features.

# 'activity_labels.txt': Links the class labels with their activity name.

# 'train/X_train.txt': Training set.

# 'train/y_train.txt': Training labels.

# 'test/X_test.txt': Test set.

# 'test/y_test.txt': Test labels.

# The following files are available for the train and test data. Their descriptions are equivalent. 

# 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

# 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

# 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

# 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

##############################################################################
#	1	Merges the training and the test sets to create one data set.
##############################################################################

# rm(list = ls(all = TRUE))

features <- read.table("UCI HAR Dataset/features.txt")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

training_set <- read.table("UCI HAR Dataset/train/X_train.txt")
training_labels <- read.table("UCI HAR Dataset/train/y_train.txt")

test_set <- read.table("UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("UCI HAR Dataset/test/y_test.txt")

training_ids <- read.table("UCI HAR Dataset/train/subject_train.txt")
test_ids <- read.table("UCI HAR Dataset/test/subject_test.txt")

# The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

train_acc_x <- read.table("UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt") 
test_acc_x <- read.table("UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt") 

# The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

train_body_acc_x <- read.table("UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt") 
test_body_acc_x <- read.table("UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt") 

# The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

train_gyro_acc_x <- read.table("UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt") 
test_gyro_acc_x <- read.table("UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt") 

objectnames <- ls()

for (i in 1:length(objectnames)) {
	cat(objectnames[i], dim(get(objectnames[i])), "\n")
	}

# activity_labels 6 2 
# features 561 2 
# test_acc_x 2947 128 
# test_body_acc_x 2947 128 
# test_gyro_acc_x 2947 128 
# test_ids 2947 1 
# test_labels 2947 1 
# test_set 2947 561 
# train_acc_x 7352 128 
# train_body_acc_x 7352 128 
# train_gyro_acc_x 7352 128 
# training_ids 7352 1 
# training_labels 7352 1 
# training_set 7352 561 

names(train_acc_x) <- paste("train_acc_", names(train_acc_x), sep = "")
names(train_body_acc_x) <- paste("train_body_", names(train_body_acc_x), sep = "")
names(train_gyro_acc_x) <- paste("train_gyro_", names(train_gyro_acc_x), sep = "")

names(test_acc_x) <- paste("test_acc_", names(test_acc_x), sep = "")
names(test_body_acc_x) <- paste("test_body_", names(test_body_acc_x), sep = "")
names(test_gyro_acc_x) <- paste("test_gyro_", names(test_gyro_acc_x), sep = "")

training_labels_names <- activity_labels[training_labels$V1,]
test_labels_names <- activity_labels[test_labels$V1,]

training_set$id <- as.character(training_ids$V1)
test_set$id <- as.character(test_ids$V1)

training_set$activity <- training_labels_names$V2
test_set$activity <- test_labels_names$V2

names(training_set)[1:(dim(training_set)[2] - 2)] <- as.character(features$V2)
names(test_set)[1:(dim(test_set)[2] - 2)] <- as.character(features$V2)

rm(training_labels)
rm(test_labels)

rm(training_labels_names)
rm(test_labels_names)

rm(features)
rm(activity_labels)
rm(i)
rm(objectnames)

ls()
# [1] "test_acc_x"       "test_body_acc_x"  "test_gyro_acc_x"  "test_ids"         "test_set"        
# [6] "train_acc_x"      "train_body_acc_x" "train_gyro_acc_x" "training_ids"     "training_set"    

objectnames <- ls()

for (i in 1:length(objectnames)) {
	cat(objectnames[i], dim(get(objectnames[i])), "\n")
	}

training_set$training_test <- "training"
test_set$training_test <- "test"

data <- rbind(training_set, test_set)

dim(data) # 10299   564

# write.csv(data, file = "data_training_test_set_merged_19dec15.csv")

##############################################################################
#	2	Extracts only the measurements on the mean and standard deviation for each measurement. 
##############################################################################

data <- data[,c(564, 563, 562, grep("mean", names(data)), grep("std", names(data)))]

write.table(data, file = "data_tidy_19dec15.txt", row.names = FALSE)

##############################################################################
#	3	Uses descriptive activity names to name the activities in the data set
##############################################################################

# done above

##############################################################################
#	4	Appropriately labels the data set with descriptive variable names. 
##############################################################################

# done above

##############################################################################
#	5	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##############################################################################

data2 <- aggregate(data[,4:(dim(data)[2])], by = list(data$training_test, data$activity, data$id), FUN = mean, na.rm = TRUE)
names(data2)[1:3] <- names(data)[1:3]

dim(data2) # 180  82

write.table(data2, file = "data_tidy2_aggregated_19dec15.txt", row.names = FALSE)

##############################################################################
