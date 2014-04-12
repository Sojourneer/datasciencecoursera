#setwd("I:/Coursera_DataScience/Project/getdata-projectfiles-UCI HAR Dataset")
library(plyr)

SA_TYPE = "factor"
DROP_MISSING_COMBINATIONS = F

# Set up control and lookup tables
activityLabelMap <- read.fwf("UCI HAR Dataset/activity_labels.txt",widths=c(1,-1,100),colClasses=c(SA_TYPE,"factor"))
names(activityLabelMap) <- c("activity","activitylabel")
features <- readLines("UCI HAR Dataset/features.txt")

colWidths <- ifelse(grepl("-mean\\(\\)|-std\\(\\)",features),16,-16)
msFeatures <- grep("-mean\\(\\)|-std\\(\\)",features,value=T)

# Read in the data. As per the instructions, we only pull out the required columns from the 'X' files.
ytrain <- read.csv("UCI HAR Dataset/train/y_train.txt", header=F, colClasses=SA_TYPE)
strain <- read.csv("UCI HAR Dataset/train/subject_train.txt", header=F, colClasses=SA_TYPE)
Xtrain <- read.fwf("UCI HAR Dataset/train/X_train.txt", widths=colWidths, header=F, colClasses="numeric")

ytest <- read.csv("UCI HAR Dataset/test/y_test.txt", header=F, colClasses=SA_TYPE)
stest <- read.csv("UCI HAR Dataset/test/subject_test.txt", header=F, colClasses=SA_TYPE)
Xtest <- read.fwf("UCI HAR Dataset/test/X_test.txt", widths=colWidths, header=F, colClasses="numeric")

# Spec lines 1+2: Merges the training and the test sets to create one data set.
#                 Extracts only the measurements on the mean and standard deviation for each measurement
# Label the X items with the set from which they came (we don't do this for the y & subject, as we are just going to combine those with X).
Xtrain$set = "Training"
Xtest$set = "Test"
X <- rbind(Xtrain,Xtest)
names(X) <- c(msFeatures,"set")

s <- rbind(strain,stest)
names(s) = c("subject")

y <- rbind(ytrain,ytest)
names(y) = c("activity")

# Spec line 3/4:   Uses descriptive activity names to name the activities in the dataset
# Execute the activity factor -> name mapping
yNames<- merge(y,activityLabelMap,by.x="activity",by.y="activity")

# Append to X the columns for subject and activitylabel
X <- cbind(X,subject=s$subject,activitylabel=yNames$activitylabel)
write.csv(X, file="result_combinedDataSet.csv")

# Spec line 5: Create a second, independent tidy set with the average of each variable for each activity & subject.

# Determine which of the data columns are required 
meanCols <- grep("-mean\\(\\)",names(X))

# Do the reduction. use the order implied in the instruction.
# Since the instructions didn't specify, we make it an option whether to drop missing combos
result <- ddply(X, .(activitylabel,subject), .drop=DROP_MISSING_COMBINATIONS, function(df) colMeans(df[,meanCols]) )

# Cleanup the names: Remove "-mean()". Then remove "-". Next remove the measurement line #. Finally apply the names.
newNames <- gsub("(.+)-mean\\(\\)","\\1",names(result))
newNames <- gsub("-","", newNames)                      
newNames <- gsub("^[0-9]+ *","",newNames)
names(result) <- newNames

# Write out the result
write.csv(result, file="result_meanByActivityAndSubject.csv")

