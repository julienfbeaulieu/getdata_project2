run_analysis <- function(){
  library(dplyr)
  
  
  ##Import data
  #general data
  activity.labels <- read.table("activity_labels.txt")
  features <- read.table("features.txt")
  
  #test data
  setwd("./test")
  subject.test <- read.table("subject_test.txt")
  x.test <- read.table("X_test.txt")
  y.test <- read.table("y_test.txt")
  setwd("../")
  
  #train data
  setwd("./train")
  subject.train <- read.table("subject_train.txt")
  x.train <- read.table("X_train.txt")
  y.train <- read.table("y_train.txt")
  setwd("../")
  
  #Comment: internial signals were not added according to recommendation from the FAQ
  # source: https://class.coursera.org/getdata-011/forum/thread?thread_id=69
  
  ## 1. Combine data
  #Combine test and train data
  x <- rbind(x.test,x.train)
  y <- rbind(y.test,y.train)
  subject <- rbind(subject.test,subject.train)
  
  #rename column names
  names(subject) <- c("subjectID")
  names(y) <- c("activityID")
  names (x) <- features$V2
  names(activity.labels) <- c("activityID","activityName")
  
  #Combine x, y, and subject data
  full.data.set <- cbind(subject,y,x)
  
  #Find columns with "mean()" or "std()"
  #comment: replace 'mean()' by 'mean' if variables containing this string were also of interest
  r1 <- grep("(?:mean()|std())", features$V2) + 2
  r2 <- c(1,2)
  columns <- c(r2,r1)
  
  #2. Extract only mean- or std-columns
  full.data.set <- full.data.set[,columns]
  
  ##3. Use descriptive activity names to name the activities in the data set
  full.data.set <- merge(full.data.set,activity.labels,by = "activityID")
  full.data.set <- select(full.data.set,-activityID)
  
  ##4. Appropriately label the data set with descriptive variable names. 
  # the t label is to be replaced by "time" and the "f" label by "frequency"
  # I don't believe spelling out the variables 
  # (e.g. "tBodyAcc-mean()-Y" becomes "mean of the time Body Acceleration in Y-direction")
  # would make them more "descriptive".
  # Readers are advised to the "README" for more information about variables
  names(full.data.set) <- sub("^t", "time", sub("^f", "frequency", names(full.data.set)))
  
  ##5. Create a second, independent tidy data set
  tidy.data.set <- ddply(full.data.set, c('subjectID', 'activityName'),numcolwise(mean))
  
  ##Export table
  write.table(tidy.data.set, "tidy_dataset.txt", row.name=FALSE)
}

