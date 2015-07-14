tempData1 <- read.table("UCI HAR Dataset/train/X_train.txt")
tempData2 <- read.table("UCI HAR Dataset/test/X_test.txt")
X <- rbind(tempData1, tempData2)

tempData1 <- read.table("UCI HAR Dataset/train/subject_train.txt")
tempData2 <- read.table("UCI HAR Dataset/test/subject_test.txt")
S <- rbind(tempData1, tempData2)

tempData1 <- read.table("UCI HAR Dataset/train/y_train.txt")
tempData2 <- read.table("UCI HAR Dataset/test/y_test.txt")
Y <- rbind(tempData1, tempData2)

features <- read.table("UCI HAR Dataset/features.txt")
temp_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, temp_features]
names(X) <- features[temp_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

names(S) <- "subject"
LVD <- cbind(S, Y, X)
write.table(LVD, "step4data.txt")

USubj = unique(S)[,1]
subj = length(unique(S)[,1])
Acti = length(activities[,1])
NC = dim(LVD)[2]
result = LVD[1:(subj*Acti), ]

row = 1
for (s in 1:subj) {
  for (a in 1:Acti) {
    result[row, 1] = USubj[s]
    result[row, 2] = activities[a, 2]
    tempData <- LVD[LVD$subject==s & LVD$activity==activities[a, 2], ]
    result[row, 3:NC] <- colMeans(tempData[, 3:NC])
    row = row+1
  }
}
write.table(result, "step5data.txt",row.name=FALSE)