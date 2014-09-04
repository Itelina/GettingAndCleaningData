# The following run_analysis.R script will do the following:
# Part 1. Merges the training and the test sets to create one data set.
# Part 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# Part 3. Uses descriptive activity names to name the activities in the data set
# Part 4. Appropriately labels the data set with descriptive variable names. 
# Part 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Part 1: Merges the training and the test sets to create one data set.

# 1A. read input data files
setwd("~/Coursera")
xtrain <- read.table(file = "~/Coursera/UCI HAR Dataset/train/X_train.txt", header = FALSE)
features <- read.table(file = "~/Coursera/UCI HAR Dataset/features.txt", header = FALSE)
subjecttrain <- read.table(file = "~/Coursera/UCI HAR Dataset/train/subject_train.txt", header = FALSE)
ytrain <- read.table(file = "~/Coursera/UCI HAR Dataset/train/y_train.txt", header = FALSE)
activitylabel <- read.table(file = "~/Coursera/UCI HAR Dataset/activity_labels.txt", header = FALSE)
xtest <- read.table(file = "~/Coursera/UCI HAR Dataset/test/X_test.txt", header = FALSE)
ytest <- read.table(file = "~/Coursera/UCI HAR Dataset/test/y_test.txt", header = FALSE)
subjecttest <- read.table(file = "~/Coursera/UCI HAR Dataset/test/subject_test.txt", header = FALSE)

# 1B. combine subject and activity columns
xtrain2 <- cbind(xtrain, subjecttrain, ytrain)
xtest2 <- cbind(xtest, subjecttest, ytest)

# 1C. renaming variables to feature names
colnames(xtest2) <- c(as.character(features[, 2]), "subject", "activity")
colnames(xtrain2) <- c(as.character(features[, 2]), "subject", "activity")

# 1D. merge test and training datasets
completedata <- rbind(xtest2, xtrain2)

# Part 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
subfeatures <- grep("mean|std", features$V2)
nonfeatures <- grep("meanFreq", features$V2) # we are excluding the variables containing meanFreq() values
nfeatures <- subfeatures[which(!subfeatures %in% nonfeatures)]
completedata <- completedata[, c(nfeatures, 562, 563)]

# Part 3. Uses descriptive activity names to name the activities in the data set
completedata$activity <- factor(completedata$activity, labels = activitylabel$V2)

# Part 4. Appropriately labels the data set with descriptive variable names. 
completedata <- completedata[, c(67, 68,1:66)] #switching to make the subject and activity the first 2 columns
vnames <- colnames(completedata)
vnames <- gsub("-|\\(|\\)", "", vnames)
vnames <- gsub("mean", "Mean", vnames)
vnames <- gsub("std", "StandardDeviation", vnames)
vnames <- gsub("Acc", "Acceleration", vnames)
vnames <- gsub("Mag", "Magnitude", vnames)
colnames(completedata) <- vnames

# Part 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subjectactivity <- completedata[, 1:2]
metrics <- completedata[, 3:68]
tidydata <- aggregate(metrics, subjectactivity, mean) # useful link: http://lamages.blogspot.com/2012/01/say-it-in-r-with-by-apply-and-friends.html

#Last part: create data into text file
write.table(tidydata, file = "~/Coursera/tidydata.txt", row.name=FALSE)
