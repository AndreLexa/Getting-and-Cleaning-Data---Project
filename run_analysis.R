# ===== # ===== # ===== # ===== # ===== # 
#  Getting and Cleaning Data - Project  #
#  by: Alexandre C. Tondolo             #
# ===== # ===== # ===== # ===== # ===== #
library(reshape2)

# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== #
## Part 1- Merges the training and the test sets to create one data set:

# Taking the path of all the data 
pathName_testSub <- "./UCI HAR Dataset/test/subject_test.txt"
pathName_testX <- "./UCI HAR Dataset/test/X_test.txt"
pathName_testY <- "./UCI HAR Dataset/test/y_test.txt"
pathName_trainSub <- "./UCI HAR Dataset/train/subject_train.txt"
pathName_trainX <- "./UCI HAR Dataset/train/X_train.txt"
pathName_trainY <- "./UCI HAR Dataset/train/y_train.txt"

# Reading the data
testSub <- read.table(pathName_testSub, header = FALSE)
testX <- read.table(pathName_testX, header = FALSE)
testY <- read.table(pathName_testY, header = FALSE)
trainSub <- read.table(pathName_trainSub, header = FALSE)
trainX <- read.table(pathName_trainX, header = FALSE)
trainY <- read.table(pathName_trainY, header = FALSE)

sub <- rbind(testSub, trainSub)  
Y <- rbind(testY, trainY) 
X <- rbind(testX, trainX)

# Using descriptive activity names to name the activities in the data set
labels_activity <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
aux <- as.factor(Y$V1)
levels(aux) <- labels_activity$V2
Y$V1 <- as.factor(aux)

# Set the X variables labels before merging all
labels_features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
names(X) <- labels_features$V2

# Subset the labels, I just want all of them who have "mean" or "std". 
subData_labels <- union(grep("mean", labels_features$V2, value = TRUE), 
                        grep("std", labels_features$V2, value = TRUE))
subData_labels <- setdiff(subData_labels, grep("meanFreq", subData_labels, value = TRUE))

# Now I subset my "full features data" into some data with just "mean" or "std" valiables
X <- subset(X, select= c(as.character(subData_labels))) 

# Set the Y variables labels before merging all
names(Y) <- c("Activities")

# Set the sub variables labels before merging all
sub$V1 <- as.character(sub$V1)
names(sub) <- c("Subjects")

# Merge all together
dataMerged <- cbind(sub, Y, X)

# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== #
## Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement:
##>> Done at Part 1 (line 41 - 46) <<##
# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== #
## Part 3 - Uses descriptive activity names to name the activities in the data set:
##>> Done at Part 1 (line 31 - 34) <<##
# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== #
## Part 4 - Appropriately labels the data set with descriptive variable names. 

names(dataMerged) <- gsub("^f", "Frequency_of_", names(dataMerged))
names(dataMerged) <- gsub("^t", "Time_of_", names(dataMerged))
names(dataMerged) <- gsub("Body", "Body_", names(dataMerged))
names(dataMerged) <- gsub("Acc", "Accelerometer_", names(dataMerged))
names(dataMerged) <- gsub("Gravity", "Gravity_", names(dataMerged))
names(dataMerged) <- gsub("Gyro", "Gyroscope_", names(dataMerged))
names(dataMerged) <- gsub("Mag", "Magnetometer_", names(dataMerged))
names(dataMerged) <- gsub("Jerk", "Jerk_", names(dataMerged))

# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== #
## Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of 
## each variable for each activity and each subject.

newTidy <- dataMerged

dm=melt(newTidy, id.var= c('Activities', 'Subjects'))
dmg=group_by(dm, Activities, Subjects, variable);

x=summarise(dmg, means=mean(value))
newTidy <- dcast(x, Activities+Subjects~variable)

# This is just to make newTidy more presentable (in order)
newTidy$Subjects <- as.numeric(newTidy$Subjects)
newTidy <- newTidy[order(newTidy$Subjects), ]


write.table(newTidy, file="tidyData.txt", row.name=FALSE)

# ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== # ===== #
