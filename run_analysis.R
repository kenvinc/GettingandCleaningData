setwd("~/Documents/gettingandcleaningdata/runanalysis/")
getwd()
# merge 
train <- read.table("train/X_train.txt")
test <- read.table("test/X_test.txt")
X <- rbind(train, test)
train <- read.table("train/y_train.txt")
test <- read.table("test/y_test.txt")
Y <- rbind(train, test)
train <- read.table("train/subject_train.txt")
test <- read.table("test/subject_test.txt")
Z <- rbind(train, test)
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
names(Z) <- "subject"
merge <- cbind(Z, Y, X)
write.table(merge, "mergeddata.txt")
# averages sheet
mysub = unique(Z)[,1]
col = dim(merge)[2]
sub = length(unique(L)[,1])
act = length(activitylabel[,1])
answer = merge[1:(sub *act), ]
row = 1
for (i in 1:sub) {
  for (n in 1:act) {
    answer[row, 1] = mysub[i]
    answer[row, 2] = activitylabel[n, 2]
    template <- merge[merge$subject==i & merge$activities==activitylabel[n, 2], ]
    answer[row, 3:col] <- colMeans(template[, 3:col])
    row = row+1
  }
}
write.table(answer, "dataaverages.txt")
