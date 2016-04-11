#install.packages("plyr")
library(plyr)

# check if a data folder exists; if not then create one
# file URL & destination file
# download the file & note the time
if (!file.exists("data")) {dir.create("data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "./data/activity.zip"
download.file(fileUrl, destfile)
dateDownloaded <- date()

# read in the data for test and training sets, & labels
test <- read.table("./data/activity/UCI HAR Dataset/test/X_test.txt")
testLabel <- read.table("./data/activity/UCI HAR Dataset/test/Y_test.txt")
testSubject <- read.table("./data/activity/UCI HAR Dataset/test/subject_test.txt")

training <- read.table("./data/activity/UCI HAR Dataset/train/X_train.txt")
trainingLabel <- read.table("./data/activity/UCI HAR Dataset/train/Y_train.txt")
trainingSubject <- read.table("./data/activity/UCI HAR Dataset/train/subject_train.txt")

activityLabels <- read.table("./data/activity/UCI HAR Dataset/activity_labels.txt")
features <- read.table("./data/activity/UCI HAR Dataset/features.txt")

# clean up the the labels by removing useless parts.
# relabel the names of columns 
features <- gsub("\\()", "", features$V2)

activityLabels <- activityLabels$V2
activityLabels <- tolower(activityLabels)
activityLabels <- sub("_", " ", activityLabels)

names(test) <- features; names(training) <- features
names(testLabel) <- "activity"; names(trainingLabel) <- "activity"
names(testSubject) <- "participant"; names(trainingSubject) <- "participant"

# create a DF & bind the training data to the bottom of the test data
# extract only the columns containing standard deviation or mean
# create a new, separate DF that holds only identifiers initially
# for each in criteria: add the DF criteria column to a new DF
DF <- rbind(test, training)

criteria <- grep("mean|std", names(DF))

DF_test <- data.frame(testLabel, testSubject)
DF_training <- data.frame(trainingLabel, trainingSubject)
DF_final <- rbind(DF_test, DF_training)

for (each in criteria){
  DF_final <- cbind(DF_final, DF[each])
}

# replace the activity numbers with their respective labels
# create a new tidy DF with the average of each variable for each activity & subject
# clean up the columns and column names from a result of aggregating
DF_final$activity <- mapvalues(DF_final$activity, 
                             from = levels(factor(DF_final$activity)), 
                             to = activityLabels)

tidy_DF <- aggregate(DF_final, list(DF_final$participant, DF_final$activity), mean)

tidy_DF$participant <- NULL; tidy_DF$activity <- NULL
names(tidy_DF)[1] <- "participant"; names(tidy_DF)[2] <- "activity"

# write out the dataframe into a file
write.table(file = "./data/tidydata.txt", x = tidy_DF, row.names = FALSE)
