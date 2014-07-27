##Packages:
library(plyr)
library(ggplot2)
library(RCurl)
library(data.table)

##Import Necessary Files
features <- read.table(paste(getwd(), "UCI HAR Dataset/features.txt", sep = "/"), quote="\"")
activity_labels <- read.table(paste(getwd(), "UCI HAR Dataset/activity_labels.txt", sep = "/"), quote="\"")

trFiles <- list.files(paste(getwd(), "UCI HAR Dataset/train", sep = "/"), full.names = TRUE)
subject_train <- read.table(trFiles[2], quote="\"")
X_train <- read.table(trFiles[3], quote="\"")
Y_train <- read.table(trFiles[4], quote="\"")

teFiles <- list.files(paste(getwd(), "UCI HAR Dataset/test", sep = "/"), full.names = TRUE)
subject_test <- read.table(teFiles[2], quote="\"")
X_test <- read.table(teFiles[3], quote="\"")
Y_test <- read.table(teFiles[4], quote="\"")


##Rename columns of identity and labels files
subject_test <- rename(subject_test, c("V1" = "subjectID"))
subject_train <- rename(subject_train, c("V1" = "subjectID"))
features <- rename(features, c("V1" = "featID", "V2" = "feature"))


#rename columns of vector files from features file
features_v <- as.vector(features$feature)
x_test_names <- as.vector(names(X_test))
x_train_names <- as.vector(names(X_train))
setnames(X_test, old = x_train_names, new = features_v)
setnames(X_train, old = x_train_names, new = features_v)
Y_test <- rename(Y_test, c("V1" = "IDact"))
Y_train <- rename(Y_train, c("V1" = "IDact"))
activity_labels <- rename(activity_labels, c("V1"= "IDact", "V2" = "Activity.Label"))

#join activity names to y files from activity names in labels file via common activity ID

y_test_labels <- join(Y_test, activity_labels, type = "inner")
y_train_labels <- join(Y_train, activity_labels, type = "inner")

#cbind activity class and labels to x files
X_train_sub <- cbind(y_train_labels, X_train)
X_test_sub <- cbind(y_test_labels, X_test)

#cbind subject IDs to x files
X_test_sub_lab <- cbind(subject_test, X_test_sub)
X_train_sub_lab <- cbind(subject_train, X_train_sub)

#rbind test and train and extract out requested measurements
X_all <- rbind(X_train_sub_lab, X_test_sub_lab)
XY_final <- X_all[, 1:9]

#Rename troublesome column names and boil down the averages
XY_final <- rename(XY_final, c("tBodyAcc-mean()-X" = "XMean", "tBodyAcc-mean()-Y" = "YMean", "tBodyAcc-mean()-Z" = "ZMean", "tBodyAcc-std()-X" = "Xstd", "tBodyAcc-std()-Y" = "Ystd", "tBodyAcc-std()-Z" = "Zstd"))
XY_final_ave <- ddply(XY_final, .(subjectID, Activity.Label), summarize, BodyAcc.MeanX = mean(XMean), BodyAcc.MeanY = mean(YMean), BodyAcc.ZMean = mean(ZMean), BodyAcc.MeanXstd = mean(Xstd), BodyAcc.MeanYstd = mean(Ystd), BodyAcc.MeanZstd = mean(Zstd))

#Export

write.table(XY_final_ave, file = paste(getwd(), "act_summary.txt", sep = "/"), sep = "\t")















