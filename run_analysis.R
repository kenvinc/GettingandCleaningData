setwd("~/Documents/gettingandcleaningdata/runanalysis/")
getwd()
# merge 
train <- read.table("train/X_train.txt")
test <- read.table("test/X_test.txt")
X <- rbind(train, test)
train1 <- read.table("train/y_train.txt")
test1 <- read.table("test/y_test.txt")
Y <- rbind(train1, test1)
train2 <- read.table("train/subject_train.txt")
test2 <- read.table("test/subject_test.txt")
Z <- rbind(train2, test2)
# extracts 
features <- read.table("features.txt")
aboutfeatures <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, aboutfeatures]
names(X) <- features[aboutfeatures, 2]
# descriptive activity
activitylabel <- read.table("activity_labels.txt")
activitylabel[, 2] = gsub("_", "", tolower(as.character(activitylabel[, 2])))
Y[,1] = activitylabel[Y[,1], 2]
names(Y) <- "activities"
# labels
names(Z) <- "names"
merge <- cbind(Z, Y, X)
write.table(merge, "mergeddata.txt")
mysub = unique(Z)[,1]
col = dim(merge)[2]
sub = length(unique(Z)[,1])
act = length(activitylabel[,1])
answer = merge[1:(sub *act), ]
# averages sheet
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
write.table(answer, "dataaverages.txt", row.name = FALSE)
