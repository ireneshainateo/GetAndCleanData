require(knitr)
require(markdown)
# setwd("~/Desktop/Coursera/UCI HAR Dataset")
setwd("~/Desktop/Coursera/UCI HAR Dataset")

packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

path <- getwd()
path

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
if (!file.exists(path)) {dir.create(path)}
download.file(url, file.path(path, f), method= "curl")

executable <- file.path( "User", "ireneteo", "Desktop", "Coursera", "UCI HAR Dataset", "UCI HAR Dataset")
parameters <- "x"
cmd <- paste(paste0("\"", executable, "\""), parameters, paste0("\"", file.path(path, f), "\""))
system(cmd)

pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive=TRUE)


# READ FILES

# Read the subject files
dtSubjectTest <- read.table("test/subject_test.txt")
dtSubjectTrain  <- read.table("train/subject_train.txt")

# Read the activity files
dtActivityTest <- read.table("test/y_test.txt")
dtActivityTrain  <- read.table("train/y_train.txt")

# Read the data files
fileToDataTable <- function (f) {
        df <- read.table(f)
        dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest  <- fileToDataTable(file.path(pathIn, "test" , "X_test.txt" ))



# 1. MERGE THE TRAINING AND THE TEST SETS

# Concatenate the data tables.
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

# Merge columns
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

# Set key
setkey(dt, subject, activityNum)



# 2.EXTRACT ONLY THE MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT

# Read the 'features.txt' file
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

# Subset only measurements for the mean and standard deviation
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

# Convert the column numbers to a vector of variable names matching columns in 'dt'
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

# Subset these variables using variable names
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt(, select, with=FALSE)



# 3. USE DESCRIPTIVE ACTIVITY NAMES

# Read 'activity_labels.txt' file
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))



# 4. LABEL THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES

# Merge activity labels
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)

# Add 'activityName' as a key
setkey(dt, subject, activityNum, activityName)

# Melt the data table to reshape it from a short and wide format to a tall and narrow format
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

# Merge activity name
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

# Create a new variable, 'activity' that is equivalent to 'activityName' as a factor class. 
dt$activity <- factor(dt$activityName)

# Create a new variable, 'feature' that is equivalent to 'featureName' as a factor class.
dt$feature <- factor(dt$featureName)

# Seperate features from 'featureName' using the helper function 'grepthis'
grepthis <- function (regex) {
        grepl(regex, dt$feature)
}

# Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))

# Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

# Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))


# Verify that all plausible combinations of 'feature' are accounted for by all plausible combinations of the factor class variables
r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2



# 5. CREATE A TIDY DATA SET

# Create a data set with the average of each variable for each subject and activity
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

# Make codebook
knit("makeCodebook.Rmd", output="codebook.md", encoding="ISO8859-1", quiet=TRUE)
markdownToHTML("codebook.md", "codebook.html")
