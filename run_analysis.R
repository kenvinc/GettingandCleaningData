setwd("~/Documents/gettingandcleaningdata/runanalysis/")
getwd()

# Source of data for this project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# merge 
train <- read.table("train/X_train.txt")
test <- read.table("test/X_test.txt")
X <- rbind(train, test)
train <- read.table("train/subject_train.txt")
test <- read.table("test/subject_test.txt")
L <- rbind(train, test)
train <- read.table("train/y_train.txt")
test <- read.table("test/y_test.txt")
Y <- rbind(train, test)

# extracts 
features <- read.table("features.txt")
goodfeatures <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, goodfeatures]
names(X) <- features[goodfeatures, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# descriptive activity
activitylabel <- read.table("activity_labels.txt")
activitylabel[, 2] = gsub("_", "", tolower(as.character(activitylabel[, 2])))
Y[,1] = activitylabel[Y[,1], 2]
names(Y) <- "activities"

# Labels
names(L) <- "subject"
merge <- cbind(L, Y, X)
write.table(merge, "mergeddata.txt")

# averages sheet
mysub = unique(L)[,1]
sub = length(unique(L)[,1])
act = length(activitylabel[,1])
col = dim(merge)[2]
result = merge[1:(sub *act), ]

row = 1
for (y in 1:sub) {
  for (x in 1:act) {
    result[row, 1] = mysub[y]
    result[row, 2] = activitylabel[x, 2]
    template <- merge[merge$subject==y & merge$activities==activitylabel[x, 2], ]
    result[row, 3:col] <- colMeans(template[, 3:col])
    row = row+1
  }
}
write.table(result, "dataaverages.txt")
