# HarvardX PH125.9x Data Science: Capstone
# Project: Choose Your Own: Vertebral Column Data Set (UCI Machine Learning Repo)
# By: Leighton Greenstein
# Last Modified: March 19, 2023

# Important Notes about Running this Script:
# 1. This script was developed and run on the following OS and machine:
#    Machine: Lenovo T460s (upgraded)
#    Processor: Intel(R) Core(TM) i5-7300U CPU @ 2.60GHz   
#    Installed Ram: 16.0 GB (15.8 GB usable)
#    Operating System: Windows 10 Pro
#        64-bit operating system, x64-based processor
#    R Version: R version 4.2.1 (2022-06-23 ucrt)
# 2. The total run-time for this script on the above hardware, not counting
#    installation of any required packages, is approximately 2 minutes to 
#    run all and 30 seconds to knit to pdf
# 3. Any of the libraries listed in the `package-inclusion-installation` are
#    installed beforehand using if(!require) statements
# 5. Functions have not been used for repetitive coding operations due 
#    the marking rubric requesting code that "runs easily, [and] is easy to 
#    follow".  For that reason, the code is written in script form
# 6. Vignettes used to facilitate code development are referenced in APA
# 7. Three hash tags identifies comments that provide a high level explanation of
#    what the code is producing
# 8. One hash tag identifies comments that are specific to what a few lines
#    of code are producing
# 9. Blocked header comments are included to describe general processes
# 10. If asked to restart R session for install of libraries, select no;
#     after the libraries are installed, R will need to be closed and reopened
#     and the script will need to be run again.  This is particularly the case
#     for the tidyverse because of all the additional libraries it uses; was
#     a bit finicky during testing


################################################################################
################################################################################
#               DOWNLOAD AND INSTALL PACKAGES IF MISSING                       #
################################################################################
################################################################################


if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")

if(!require(cowplot)) install.packages("cowplot", 
                                       repos = "http://cran.us.r-project.org")

if(!require(kableExtra)) install.packages("kableExtra", 
                                          repos = "http://cran.us.r-project.org")

if(!require(MASS)) install.packages("MASS", 
                                    repos = "http://cran.us.r-project.org")

if(!require(randomForest)) install.packages("randomForest", 
                                            repos = "http://cran.us.r-project.org")

if(!require(RCurl)) install.packages("RCurl", 
                                     repos = "http://cran.us.r-project.org")

if(!require(readr)) install.packages("readr", 
                                     repos = "http://cran.us.r-project.org")

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")


# initialize all libraries that are used throughout the project
library(caret)
library(cowplot)
library(kableExtra)
library(MASS)
library(randomForest)
library(RCurl)
library(readr)
library(tidyverse)

# Turn warning messages off
options(warn=-1)

################################################################################
################################################################################
#                                 DATA AQUISITION                              #
################################################################################
################################################################################

### Automatically download data from GitHub and create orthoData data frame
### to hold the data.  If GitHub is down, download the data from the UCI
### Machine Learning Repository.  If data no longer exists on machine learning
### repository, provide GitHub web address to download data manually
gitHubRepo <- "https://raw.githubusercontent.com/ljgreens/project-2/main/data/column_3C.dat"

# Check to see that GitHub Repo exists and download data and store to orthoData
# data frame
if(url.exists(gitHubRepo) == TRUE) {
  
  # Read the downloaded ortho-spine geometry and spine classification data
  # into a data frame using read_delim to parse the file based on auto
  # identification of the delimiter based on the GitHub location
  orthoData <- as.data.frame(read_delim(gitHubRepo, show_col_types = FALSE, col_names = FALSE))
  
} else {

  # Download the Vertebral Column Data Set from the UCI Machine Learning 
  # Repository (Mota et al., 2011).  Data download vignette provided by 
  # Irizarry (n.d.) Data Science Capstone project - 1
  
  UCI_repository <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00212/vertebral_column_data.zip"
  
  # Check to see that the UCI Machine Learning Repository Data exists, if not
  # send a message to the user to download the data from the GitHub location
  # manually by providing the website in the error message
  if(!url.exists(UCI_repository)) {
    stop("GitHub Connection Down and Data from UCI Machine Learning Repo not Available...
         Please download data from the gitHub using the following web address: 
         https://raw.githubusercontent.com/ljgreens/project-2/main/data/column_3C.dat")
  }
  
  # Complete the download and unzip of the data from the UCI 
  # Machine Learning Repository
  dl <- "vertebral_column_data.zip"
  if(!file.exists(dl))
    download.file(UCI_repository, dl)
  # Create data file object to hold the unzipped Vertebral Column data for
  # this project and unzip the downloaded folder to access contents
  dataFile <- "column_3C.dat"
  if(!file.exists(dataFile))
    unzip(dl)
  
  # Read the downloaded ortho-spine geometry and spine classification data
  # into a data frame using read_delim to parse the file based on auto
  # identification of the delimiter
  orthoData <- as.data.frame(read_delim(dataFile, show_col_types = FALSE, col_names = FALSE))
}


### Prepare the data for initial display as a table within the report

# Name the columns of the orthoData dataframe based on the 
# UCI Machine Learning Repository information (Mota et al., 2011)
columnNames <- c("pelvicIncidence", 
                 "pelvicTilt", 
                 "lumbarLordosis",
                 "sacralSlope", 
                 "pelvicRadius", 
                 "spondylolisthesisGrade",
                 "diseaseClassification")

# Apply the column names to the orthoData data frame
colnames(orthoData) <- columnNames

### Randomly Sample the orthoData Data Frame to display six rows of data

# Set seed to 1 so that the same random sample is generated each time the 
# script is run
set.seed(1, sample.kind = "Rounding")

# mirror the row index of the orthoData data frame for sample() to extract random 
# sample of indexes from the orthoData data frame
rows <- c(1:nrow(orthoData))

# extract the random sample of 6 indices to be applied to the orthoData data frame
snapshot_index <- sample(rows, 6, replace = FALSE)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(orthoData[snapshot_index,],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Sample Vertebral Column Data Acquired from UCI Machine Learning Repository") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 


################################################################################
################################################################################
#                                DATA CLEANING                                 #
################################################################################
################################################################################


### Determine whether the data set has any "na" values or "0" and store the 
### results to a data frame called `dataInspection`

# Obtain the names of the columns in the `orthoData` data frame
rowNames <- names(orthoData)
# Set the column names for the `dataInspection` data frame
colNames <- c("Column", "NA_Count")

# Create an empty data frame as a matrix sized to the rowNames and colNames
# as created above and cast to data frame
dataInspection <- data.frame(matrix(nrow = length(rowNames), ncol = length(colNames)))

# Rename the rows and columns
rownames(dataInspection) <- rowNames
colnames(dataInspection) <- colNames

# Set column data types
dataInspection$NA_Count   <- as.numeric(dataInspection$NA_Count)

# Determine if any of the columns in the `orthoData` dataframe have NA values
# and write the number of NA values to a dataframe called `dataInspection`
for(i in 1:nrow(dataInspection)) {
  dataInspection[i, "Column"] <- rowNames[i]
  dataInspection[i,"NA_Count"] <- length(which(is.na(orthoData[,i])))
}

# Change the column names for display in the table for output
colnames(dataInspection) <- c("Column", "NA Count")

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(dataInspection,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Counts of NA Values Within Vertebral Column Data Set") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Build a table to display the class of each column within the 
### Vertebral Column Data Set

# Create an empty data frame to hold the class values for each column of the
# Vertebral Column Data Set
columnData <- data.frame()

# Loop through all of the orthoData data frame column names and extract the
# class of the column, which is written to the `columnClass` data frame
for(i in 1:ncol(orthoData)) {
  columnData[i, 1] <- columnNames[i]
  columnData[i, 2] <- class(orthoData[,i])
  if(class(orthoData[,i]) != "numeric"){
    columnData[i, 3] <- NA
    columnData[i, 4] <- NA
  } else {
    columnData[i, 3] <- min(orthoData[,i])
    columnData[i, 4] <- max(orthoData[,i])
  }
}

# Provided updated display names for the columnData dat fram for display
# in the following table
columnDataNames <- c("Column Name", "Class", "Minimum", "Maximum")
colnames(columnData) <- columnDataNames

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(columnData,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Vertebral Column Data Set Metadata") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Secondary research on Spondylolisthesis Grade shows a grade and percentage
### of vertebrae slip. Build a data frame to display these researched values
### in the report.  Rating system is call Meyerding Classification System
### (Koslosky & Gendelberg, 2020)

# Create data frame to hold the grades and percentages of the Meyerding
# Classifications
meyerdingClassification <- data.frame(Grade = NULL, Percentage_Range = NULL)

# Populate the Meyerding Classification Grade and Percentage Range Manually
# so the values can be rendered into a nicely formatted knitr::kable
meyerdingClassification[1,"Grade"] <- 1
meyerdingClassification[1,"Percentage_Range"] <- "0% to 25%"
meyerdingClassification[2,"Grade"] <- 2
meyerdingClassification[2,"Percentage_Range"] <- "25% to 50%"
meyerdingClassification[3,"Grade"] <- 3
meyerdingClassification[3,"Percentage_Range"] <- "50% t0 75%"
meyerdingClassification[4,"Grade"] <- 4
meyerdingClassification[4,"Percentage_Range"] <- "75% to 100%"
meyerdingClassification[5,"Grade"] <- 5
meyerdingClassification[5,"Percentage_Range"] <- "> 100%"

# Rename the meyerdingClassification data frame columns for cleaner display
meyerdingClassificationNames <- c("Grade", "Percentage Range")
colnames(meyerdingClassification) <- meyerdingClassificationNames

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(meyerdingClassification,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Meyerding Classification Grades and Vertebrae Slip Percentages") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Review the data value computations for Pelvic Incidence using the 
### Formula PI = PT + SS (Le Huec et al., 2011) to validate the PI, PT, and SS
### Provided in the Vertebral Column Data Set do not have errors

# Subset the orthoData data frame to contain only the Pelvic Incidence (PI), 
# Pelvic Tilt (PT), and Sacral Slope (SS) to compute the PI from the 
# Vertebral Column Data Set using the measured PT and SS
pelvicIncidence <- orthoData %>% dplyr::select(pelvicIncidence,
                                               pelvicTilt,
                                               sacralSlope)

# Compute PI = PT + SS and the difference between the computed PI and the
# PI that came as raw data from the Vertebral Column Data Set
pelvicIncidence <- pelvicIncidence %>%
  mutate(PI_Computed = pelvicTilt + sacralSlope) %>%
  mutate(Delta_PI = pelvicIncidence - PI_Computed)

# Set seed to 1 so that the same random sample is generated each time the 
# script is run
set.seed(1, sample.kind = "Rounding")

# mirror the row index of the pelvicIncidence data frame for sample() to extract random 
# sample of indexes from the pelvicIncidence data frame
rows <- c(1:nrow(pelvicIncidence))

# extract the random sample of 6 indices to be applied to the pelvicIncidence data frame
snapshot_index <- sample(rows, 6, replace = FALSE)

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(pelvicIncidence[snapshot_index,],
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Pelvic Incidence, Pelvic Tilt, and Sacral Slope Data Verification") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 


### Compute Summary Statistics of the Change in PI between the values given
### in the Vertebral Column Data Set and the Computed (Delta_PI) to see
### whether the range of differences is outside expected measurement uncertainty

# Summary Statistics of delta_PI in the pelvicIncidence data frame
summaryData <- summary(pelvicIncidence$Delta_PI)

# Create data frame to hold the Delta_PI summary statistics
summary_df <- data.frame(Minimum = NULL,
                         FirstQuartile = NULL,
                         Median = NULL,
                         Mean = NULL,
                         ThirdQuartile = NULL,
                         Maximum = NULL)

# Populate the summary data frame with Delta_PI summary statistics
summary_df[1, "Minimum"] <- summaryData[1]
summary_df[1, "FirstQuartile"] <- summaryData[2]
summary_df[1, "Median"] <- summaryData[3]
summary_df[1, "Mean"] <- summaryData[4]
summary_df[1, "ThirdQuartile"] <- summaryData[5]
summary_df[1, "Maximum"] <- summaryData[6]


# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(summary_df,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Summary Statistics for Delta Pelvic Incidence") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Set options to turn off scientific notation to show mean of Delta_PI
# in the inline code below as a decimal
options(scipen = 100)

################################################################################
################################################################################
#                                DATA SPLITTING                                #
################################################################################
################################################################################

### Determine the number of observations in each classification and the 
### proportion to include in the training data set (1/2) and set the minimum 
### number of observations to 30 based on the Central Limit Theorem (Irizarry, 
### 2022, Chapter 14) to hopefully obtain normally distributed predictors

# Create data frame grouped by class that contains half of the data listed
# as training data and the other half as verification data
orthoData_ClassSummary <- orthoData %>%
  group_by(diseaseClassification) %>%
  summarize(count = n()) %>% 
  mutate(training_counts = round(count*(1/2),0)) %>%
  mutate(verification_counts = count - training_counts) 

# Loop to apply the minimum 30 observation Central Limit Theorem Rule
# to ensure no matter the split (1/2, 2/3, etc) the classes will have
# 30 or more observations per class
for(i in 1:nrow(orthoData_ClassSummary)) {
  if(orthoData_ClassSummary[i, "verification_counts"] < 30) {
    orthoData_ClassSummary[i, "verification_counts"] <- 30
    orthoData_ClassSummary[i, "training_counts"] <- orthoData_ClassSummary[i, "count"] - orthoData_ClassSummary[i, "verification_counts"]
  }
}

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(orthoData_ClassSummary,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Training Data and Verification Data Class Counts") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Split the data into training and verification data frames
### Note: training data frame will be further split into training and test
### data frames further on in the script

# Create 3 data frames to hold the three different classifications of the 
# Vertebral Column Data Set: DH (Disk Hernia), SL (Spondylolisthesis),
# and NO (Normal)

dataNO <- orthoData %>% filter(diseaseClassification == "NO")
dataDH <- orthoData %>% filter(diseaseClassification == "DH")
dataSL <- orthoData %>% filter(diseaseClassification == "SL")

# Because createDataPartition() attempts to create training and test 
# data that are somewhat similar (Dalpaiz, 2020, Chapter 21), sample() will be 
# used instead to generate a split of based on the training counts and 
# verification counts created previously from each of the following 
# data frames: dataNO, dataDH, dataSL

# Set the seed to 1 so the same sample is generated if the script is re-run
# Comment out the set.seed to random generate data splits that can be
# used to verify that the data splits are multi-variate normal when
# by the histograms, summary stats, and qq-plots that are produced for
# each of the predcitors later on in the script
set.seed(1, sample.kind = "Rounding")

# mirror the row index of the dataNO, dataDH, and dataSL data frames 
# as vectors for sample() to extract random sample of indexes to break 
# the data into a training and verification data sets
rowsNO <- c(1:nrow(dataNO))
rowsDH <- c(1:nrow(dataDH))
rowsSL <- c(1:nrow(dataSL))

# Pull the number of training samples for each disease classification type
# from the `orthoData_ClassSummary` data frame
nSampleNOTrain <- orthoData_ClassSummary %>% filter(diseaseClassification == "NO") %>%
  summarize(n = training_counts) %>% as.numeric()

nSampleDHTrain <- orthoData_ClassSummary %>% filter(diseaseClassification == "DH") %>%
  summarize(n = training_counts) %>% as.numeric()

nSampleSLTrain <- orthoData_ClassSummary %>% filter(diseaseClassification == "SL") %>%
  summarize(n = training_counts) %>% as.numeric()


# extract the random sample of indices to be extracted from each of the 
# following data frames: dataNO, dataDH, dataSL
indexNO <- sample(rowsNO, nSampleNOTrain, replace = FALSE)
indexDH <- sample(rowsDH, nSampleDHTrain, replace = FALSE)
indexSL <- sample(rowsSL, nSampleSLTrain, replace = FALSE)

# Create the training data data frames for each disease classification
trainNO <- dataNO[indexNO,]
trainDH <- dataDH[indexDH,]
trainSL <- dataSL[indexSL,]

# Combine all of the disease classification training data frames into a
# single training data data frame
train <- rbind(trainNO, trainDH, trainSL)

# Create the verification data data frames for each disease classification 
verificationNO <- dataNO[-indexNO,]
verificationDH <- dataDH[-indexDH,]
verificationSL <- dataSL[-indexSL,]

# Combine all of the disease classification verification data frames into a
# single verification data data frame
verification <- rbind(verificationNO, verificationDH, verificationSL)

### Determine the number of observations in each classification and the 
### proportion to include in the training and test data set (1/2) and 
### set the minimum number of observations to 15 based on the Central Limit 
### Theorem (Irizarry, 2022, Chapter 14) where as little ast 10 observations
### can present normal data

# Create data frame grouped by class that contains half of the data listed
# as training data and the other half as test data from the first split
# of data into training and verification data sets
trainData_ClassSummary <- train %>%
  group_by(diseaseClassification) %>%
  summarize(count = n()) %>% 
  mutate(training_counts = round(count*(1/2),0)) %>%
  mutate(test_counts = count - training_counts) 

# Loop to apply the minimum 15 observations 
# to ensure no matter the split (1/2, 2/3, etc) the classes will have
# 15 or more observations per class
for(i in 1:nrow(trainData_ClassSummary)) {
  if(trainData_ClassSummary[i, "test_counts"] < 15) {
    trainData_ClassSummary[i, "test_counts"] <- 15
    trainData_ClassSummary[i, "training_counts"] <- trainData_ClassSummary[i, "count"] - trainData_ClassSummary[i, "test_counts"]
  }
}


### Split the data into training and test data frames

# Create 3 data frames to hold the three different outcomes of the 
# Vertebral Column Data Set: DH (Disk Hernia), SL (Spondylolisthesis),
# and NO (Normal) based on the data within the current training data set

dataNO <- train %>% filter(diseaseClassification == "NO")
dataDH <- train %>% filter(diseaseClassification == "DH")
dataSL <- train %>% filter(diseaseClassification == "SL")

# Because createDataPartition() attempts to create training and test 
# data that are somewhat similar (Dalpaiz, 2020, Chapter 21), sample() will be 
# used instead to generate a split of 207 samples from each of the following 
# data frames: dataNO, dataDH, dataSL

# Set the seed to 1 so the same sample is generated if the script is re-run
set.seed(1, sample.kind = "Rounding")

# mirror the row index of the dataNO, dataDH, and dataSL data frames 
# as vectors for sample() to extract random sample of indexes to break 
# the data into a training and verification data sets
rowsNO <- c(1:nrow(dataNO))
rowsDH <- c(1:nrow(dataDH))
rowsSL <- c(1:nrow(dataSL))

# Pull the number of training samples for each disease classification type
# from the `trainData_ClassSummary` data frame
nSampleNOTrain <- trainData_ClassSummary %>% filter(diseaseClassification == "NO") %>%
  summarize(n = training_counts) %>% as.numeric()

nSampleDHTrain <- trainData_ClassSummary %>% filter(diseaseClassification == "DH") %>%
  summarize(n = training_counts) %>% as.numeric()

nSampleSLTrain <- trainData_ClassSummary %>% filter(diseaseClassification == "SL") %>%
  summarize(n = training_counts) %>% as.numeric()


# extract the random sample of 207 indices to be extracted from each of the 
# following data frames: dataNO, dataDH, dataSL
indexNO <- sample(rowsNO, nSampleNOTrain, replace = FALSE)
indexDH <- sample(rowsDH, nSampleDHTrain, replace = FALSE)
indexSL <- sample(rowsSL, nSampleSLTrain, replace = FALSE)

# Create the training data data frames for each disease classification
trainNO <- dataNO[indexNO,]
trainDH <- dataDH[indexDH,]
trainSL <- dataSL[indexSL,]

# Combine all of the disease classification training data frames into a
# single training data data frame
train <- rbind(trainNO, trainDH, trainSL)

# Create the test data data frames for each disease classification 
testNO <- dataNO[-indexNO,]
testDH <- dataDH[-indexDH,]
testSL <- dataSL[-indexSL,]

# Combine all of the disease classification test data frames into a
# single verification data data frame
test <- rbind(testNO, testDH, testSL)

### Display the counts of the training, test, and verification data frames

# Summarize the counts of classes contained within the training data frame
trainSplit <- train %>% group_by(diseaseClassification) %>%
  summarize(count = n())

# Summarize the counts of classes contained within the test data frame
testSplit <- test %>% group_by(diseaseClassification) %>%
  summarize(count = n())

# Summarize the counts of classes contained within the verification data frame
verificationSplit <- verification %>% group_by(diseaseClassification) %>%
  summarize(count = n())

### Display the training data frame class counts in a knitr::kable

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(trainSplit,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Training Data Configuration") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Display the test data frame class counts in a knitr::kable

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(testSplit,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Test Data Configuration") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Display the verification data frame class counts in a knitr::kable

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(verificationSplit,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Verification Data Configuration") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Environment cleanup for memory management and removing R objects not needed

# Create list of R objects from environment
objectsR <- objects()
# Create list of R object to keep in memory
keep <- c("orthoData", "train", "verification",
          "test", "startTime")
# Perform the enviroment object removal
removeIndex <- !objectsR %in% keep
removeObjects <- as.list(objectsR[removeIndex])
do.call(rm, removeObjects)
rm(removeObjects, keep, objectsR, removeIndex)

# Clean up memory: reduce the chance of a memory issue while running the script
gc()

################################################################################
################################################################################
#                   DATA EXPLORATION AND VISUALIZATION                         #
################################################################################
################################################################################

### Create summary statistics and histogram plots for all of the predictors
### in the Vertebral Column Training Data to be analyzes for being normally
### distributed and to be used for machine learning algorithm selection

### Pelvic Incidence summary statistics and histogram

# Summary Statistics of pelvicIncidence from training data
pelvicIncidenceSummmary <- summary(train$pelvicIncidence)

# Create data frame to hold the pelvicIncidence and other predictor
# summary statistics to display all together later on
summary_df <- data.frame(Predictor <- NULL,
                         Minimum = NULL,
                         FirstQuartile = NULL,
                         Median = NULL,
                         Mean = NULL,
                         ThirdQuartile = NULL,
                         Maximum = NULL)

# Populate the summary data frame with pelvicIncidence summary statistics
summary_df[1, "Predictor"] <- "Pelvic Incidence"
summary_df[1, "Minimum"] <- pelvicIncidenceSummmary[1]
summary_df[1, "FirstQuartile"] <- pelvicIncidenceSummmary[2]
summary_df[1, "Median"] <- pelvicIncidenceSummmary[3]
summary_df[1, "Mean"] <- pelvicIncidenceSummmary[4]
summary_df[1, "ThirdQuartile"] <- pelvicIncidenceSummmary[5]
summary_df[1, "Maximum"] <- pelvicIncidenceSummmary[6]

# Create the ggplot piHist object for later display in a cowplot grid
piHist <- ggplot(train, aes(x=pelvicIncidence)) + 
  geom_histogram(bins=10, fill="aquamarine3", color = "black", alpha=0.9) +
  ggtitle("Train Data PI Histogram") +
  xlab("Pelvic Incidence Values") + 
  ylab("Frequency")


### Pelvic Tilt summary statistics and histogram

# Summary Statistics of pelvicTilt from training data
pelvicTiltSummmary <- summary(train$pelvicTilt)

# Populate the summary data frame with pelvicTilt summary statistics
summary_df[2, "Predictor"] <- "Pelvic Tilt"
summary_df[2, "Minimum"] <- pelvicTiltSummmary[1]
summary_df[2, "FirstQuartile"] <- pelvicTiltSummmary[2]
summary_df[2, "Median"] <- pelvicTiltSummmary[3]
summary_df[2, "Mean"] <- pelvicTiltSummmary[4]
summary_df[2, "ThirdQuartile"] <- pelvicTiltSummmary[5]
summary_df[2, "Maximum"] <- pelvicTiltSummmary[6]

# Create the ggplot ptHist object for later display in a cowplot grid
ptHist <- ggplot(train, aes(x=pelvicTilt)) + 
  geom_histogram(bins=10, fill="palevioletred1", color="black", alpha=0.9) +
  ggtitle("Train Data PT Histogram") +
  xlab("Pelvic Tilt Values") + 
  ylab("Frequency")


### Lumbar Lordosis summary statistics and histogram

# Summary Statistics of lumbarLordosis from training data
lumbarLordosisSummmary <- summary(train$lumbarLordosis)

# Populate the summary data frame with lumbarLordosis summary statistics
summary_df[3, "Predictor"] <- "Lumbar Lordosis"
summary_df[3, "Minimum"] <- lumbarLordosisSummmary[1]
summary_df[3, "FirstQuartile"] <- lumbarLordosisSummmary[2]
summary_df[3, "Median"] <- lumbarLordosisSummmary[3]
summary_df[3, "Mean"] <- lumbarLordosisSummmary[4]
summary_df[3, "ThirdQuartile"] <- lumbarLordosisSummmary[5]
summary_df[3, "Maximum"] <- lumbarLordosisSummmary[6]

# Create the ggplot llHist object for later display in a cowplot grid
llHist <- ggplot(train, aes(x=lumbarLordosis)) + 
  geom_histogram(bins=10, fill="sienna1", color="black", alpha=0.9) +
  ggtitle("Train Data LL Histogram") +
  xlab("Lumbar Lordosis Values") + 
  ylab("Frequency")


### Sacral Slope summary statistics and histogram

# Summary Statistics of sacralSlope from training data
sacralSlopeSummmary <- summary(train$sacralSlope)

# Populate the summary data frame with lumbarLordosis summary statistics
summary_df[4, "Predictor"] <- "Sacral Slope"
summary_df[4, "Minimum"] <- sacralSlopeSummmary[1]
summary_df[4, "FirstQuartile"] <- sacralSlopeSummmary[2]
summary_df[4, "Median"] <- sacralSlopeSummmary[3]
summary_df[4, "Mean"] <- sacralSlopeSummmary[4]
summary_df[4, "ThirdQuartile"] <- sacralSlopeSummmary[5]
summary_df[4, "Maximum"] <- sacralSlopeSummmary[6]

# Create the ggplot ssHist object for later display in a cowplot grid
ssHist <- ggplot(train, aes(x=sacralSlope)) + 
  geom_histogram(bins=10, fill="sienna1", color="black", alpha=0.9) +
  ggtitle("Train Data SS Histogram") +
  xlab("Sacral Slope Values") + 
  ylab("Frequency")

### Pelvic Radius summary statistics and histogram

# Summary Statistics of pelvicRadius from training data
pelvicRadiusSummmary <- summary(train$pelvicRadius)

# Populate the summary data frame with lumbarLordosis summary statistics
summary_df[5, "Predictor"] <- "Pelvic Radius"
summary_df[5, "Minimum"] <- pelvicRadiusSummmary[1]
summary_df[5, "FirstQuartile"] <- pelvicRadiusSummmary[2]
summary_df[5, "Median"] <- pelvicRadiusSummmary[3]
summary_df[5, "Mean"] <- pelvicRadiusSummmary[4]
summary_df[5, "ThirdQuartile"] <- pelvicRadiusSummmary[5]
summary_df[5, "Maximum"] <- pelvicRadiusSummmary[6]

# Create the ggplot prHist object for later display in a cowplot grid
prHist <- ggplot(train, aes(x=pelvicRadius)) + 
  geom_histogram(bins=10, fill="mediumpurple", color="black", alpha=0.9) +
  ggtitle("Train Data PR Histogram") +
  xlab("Pelvic Radius Values") + 
  ylab("Frequency")

### Spondylolisthesis Grade summary statistics and histogram

# Summary Statistics of spondylolisthesisGrade from training data
spondylolisthesisGradeSummmary <- summary(train$spondylolisthesisGrade)

# Populate the summary data frame with lumbarLordosis summary statistics
summary_df[6, "Predictor"] <- "Spondylolisthesis Grade"
summary_df[6, "Minimum"] <- spondylolisthesisGradeSummmary[1]
summary_df[6, "FirstQuartile"] <- spondylolisthesisGradeSummmary[2]
summary_df[6, "Median"] <- spondylolisthesisGradeSummmary[3]
summary_df[6, "Mean"] <- spondylolisthesisGradeSummmary[4]
summary_df[6, "ThirdQuartile"] <- spondylolisthesisGradeSummmary[5]
summary_df[6, "Maximum"] <- spondylolisthesisGradeSummmary[6]

# Create the ggplot sgHist object for later display in a cowplot grid
sgHist <- ggplot(train, aes(x=spondylolisthesisGrade)) + 
  geom_histogram(bins=10, fill="darkcyan", color="black", alpha=0.9) +
  ggtitle("Train Data SG Histogram") +
  xlab("Spondylolisthesis Grade Values") + 
  ylab("Frequency")


### Display a combined summary statistics data frame containing the summary 
### statistics for each of the predictors in the training data; this summary
### is important to see the relationship between mean and medians which can 
### be used to identify a skew (non-normal) characteristic of the predictors

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(summary_df,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Summary Statistics of Vertebral Column Data Training Set Predictors") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Use the library(cowplot) to combine and create a layout for the histogram
### plots of each predictor in the training data, vignette Wilke (2022)
plot_grid(piHist, ptHist, llHist, ssHist, prHist, sgHist, ncol = 2, nrow = 3)

### Create a QQ-Plot of all predictors in the same plot to visualize their
### relationship to a Normal Distribution.

# Create vector to hold the measures and predictor geometry which will be used
# to display the qq-plot of the measures and color by geometry type within
# a single plot
measureDataNames <- c("Measure", "Geometry")

# Select the Pelvic Incidence measures and create a PI data frame with only the
# Geometry and Measures for the single QQ-plot showing all predictors
PI <- train %>% dplyr::select(pelvicIncidence)
PI <- PI %>% mutate(Geometry = "Pelvic Incidence")
colnames(PI) <- measureDataNames

# Select the Pelvic Tilt measures and create a PT data frame with only the
# Geometry and Measures for the single QQ-plot showing all predictors
PT <- train %>% dplyr::select(pelvicTilt)
PT <- PT %>% mutate(Geometry = "Pelvic Tilt")
colnames(PT) <- measureDataNames

# Select the Lumbar Lordosis measures and create a LL data frame with only the
# Geometry and Measures for the single QQ-plot showning all predictors
LL <- train %>% dplyr::select(lumbarLordosis)
LL <- LL %>% mutate(Geometry = "Lumbar Lordosis")
colnames(LL) <- measureDataNames

# Select the Sacral Slope measures and create a SS data frame with only the
# Geometry and Measures for the single QQ-plot showing all predictors
SS <- train %>% dplyr::select(sacralSlope)
SS <- SS %>% mutate(Geometry = "Sacral Slope")
colnames(SS) <- measureDataNames

# Select the Pelvic Radius measures and create a PR data frame with only the
# Geometry and Measures for the single QQ-plot showing all predictors
PR <- train %>% dplyr::select(pelvicRadius)
PR <- PR %>% mutate(Geometry = "Pelvic Radius")
colnames(PR) <- measureDataNames

# Select the Spondylolisthesis Grade measures and create a SG data frame with 
# only the Geometry and Measures for the single QQ-plot showing all predictors
SG <- train %>% dplyr::select(spondylolisthesisGrade)
SG <- SG %>% mutate(Geometry = "Spondylolisthesis Grade")
colnames(SG) <- measureDataNames

# combine all of the segregated predictor measures and geometry into a 
# single data frame to plot using ggplot
measureData <- rbind(PI, PT, LL, SS, PR, SG)

# Create and display the QQ-plot to show the degree of normality
# of each predictor in the training data set
ggplot(data = measureData, aes(sample = Measure, color = Geometry)) + 
  stat_qq() + stat_qq_line() +
  ggtitle("Training Data Normality Assessment of Predictors Using QQ-Plot") +
  theme(legend.position="bottom") +
  guides(color = guide_legend(title = "Predictor"))

### Predictor variability data exploration and visualization

# Create an empty data frame as a matrix sized for the predictor train data set
predictorVariability <- data.frame(matrix(nrow = ncol(train)-1, ncol = 2))

# Provide names based on the number of columns set with the create and 
# cast matrix to data frame line above
predictorVariabilityNames <- c("predictor", "standardDeviation")
colnames(predictorVariability) <- predictorVariabilityNames

# Extract the names of the training data data frame
predictorNames <- names(train)

# Apply the predictor names from the predictorNames object and the
# associated standard deviation of the predictor to the predictorVariability
# data frame which will be used to display a predictor variability table and
# a predictor variability bar plot to compare the predictor variability
for(i in 1:ncol(train)-1) {
  predictorVariability[i, "predictor"] <- predictorNames[i]
  predictorVariability[i, "standardDeviation"] <- sd(train[,i])
}

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(predictorVariability,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Variability of Predictors") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Quick barplot of the standard deviation of the predictors
barplot(predictorVariability$standardDeviation,
        main = "Standard Deviation of Predictors",
        xlab = "Predictor",
        ylab = "Standard Deviation",
        names.arg = c("PI", "PT", "LL", "SS", "PR", "SG"))

### Show the class prevalence of the train data classes based on the counts
### of each class

# Create class prevalence data frame the summarizes the counts of each 
# training data class
classificationPrevalence <- train %>% group_by(diseaseClassification) %>%
  summarize(count = n())

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(classificationPrevalence,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Training Data Class Prevalence") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Quick barplot of the Disease Prevalence in the training data
barplot(classificationPrevalence$count,
        main = "Training Data Class Prevalence",
        xlab = "Disease",
        ylab = "Count",
        names.arg = c(classificationPrevalence$diseaseClassification))

### Estimate the parameters required to run the QDA machine learning method
### These estimates are create based on the number of classes within the
### Vertebral Column Data set and the number of predictors to be included
### for training a QDA model.  Results are then used to determine if enough
### degree of freedom remains so the model does not over fit

# Create a vector to hold the number of predictors to estimate parameters
predictorsUsed <- 1:5

# Set the value of K to hold the number of classes within the Vertebral
# Column Data Set
K <- nrow(classificationPrevalence)

# Create an empty data frame cast from a matrix to hold the number of 
# compute parameters estimated to run QDA
parametersRequired <- data.frame(matrix(nrow = length(predictorsUsed), ncol = 3))
parametersRequiredNames <- c("predictorsUsed", "classesK", "parametersRequired")
colnames(parametersRequired) <- parametersRequiredNames

# Make the estimated required parameters for each class and number of predictors
# combination and store the results to the parameters required data frame
for(i in 1:nrow(parametersRequired)) {
  parametersRequired[i, "predictorsUsed"] <- predictorsUsed[i]
  parametersRequired[i, "classesK"] <- K
  parametersRequired[i, "parametersRequired"] <- K*((2*predictorsUsed[i] + predictorsUsed[i]*(predictorsUsed[i]-1))/2)
}

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(parametersRequired,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Estimated QDA Parameter Requirements") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### QDA Fitting and Accuracy Generation from Confusion Matrix Output

# Vignette on cross validation (Dalpaiz, 2020, Chapter 21)
# perform the cross validation on the training set with 5 folds 
train_qda <- train(diseaseClassification ~ pelvicIncidence 
                   + pelvicTilt
                   + lumbarLordosis
                   + sacralSlope
                   + pelvicRadius,
                   method = "qda",
                   trControl = trainControl(method = "cv", number = 5),
                   data = train)

# Create object to hold the confuction matrix accuracy from cross-validation
qdaAccuracy <- confusionMatrix(predict(train_qda, test),
                factor(test$diseaseClassification))$overall["Accuracy"]

### Random Forest Fitting and Accuracy Generation from Confusion Matrix Output

# Vignette on cross validation (Dalpaiz, 2020, Chapter 21)
# perform the cross validation on the training set with 5 folds 
train_rf <- train(diseaseClassification ~ .,
                   method = "rf",
                   trControl = trainControl(method = "cv", number = 5),
                   data = train)

# Create variable importance object to be used to summarize the predictor utility
rfImportance <- varImp(train_rf)

# Create object to hold the confuction matrix accuracy from cross-validation
rfAccuracy <- confusionMatrix(predict(train_rf, test),
                factor(test$diseaseClassification))$overall["Accuracy"]

### Ready and display the variable importance information obtained from Random 
### Forest 5-fold cross validation fitting 

# Cast the variable importance generated by the Random Forest helper function
# to a data frame that can be displayed
rfImportanceDF <- as.data.frame(rfImportance$importance)

# Extract row names from the cast list to data frame
rfImportanceRNames <- rownames(rfImportanceDF)

# Add the row names back into the `rfImportanceDF` as a new column called
# `Predictor`
rfImportanceDF <- rfImportanceDF %>% mutate(Predictor = rfImportanceRNames)

# Rearrange the order of the data frame so the most important predictor first
rfImportanceDF <- arrange(rfImportanceDF, desc(Overall))

# Swap the columns of the `rfImportanceDF` so the `Predictor` column is first
rfImportanceDF <- rfImportanceDF %>% dplyr::select(Predictor, Overall)

# Quick barplot of the variable importance of the predictors
barplot(rfImportanceDF$Overall,
        main = "Importance of Predictors Generated from Random Forest",
        xlab = "Predictor",
        ylab = "Percent Importance",
        names.arg = c("SL", "SS", "PR", "PI", "PT", "LL"))

### Display the Random Forest variable importance generated from the 
### Random Forest 5-fold cross-validation fitting

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(rfImportanceDF,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Random Forest Variable Importance") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Note the KNN training, predicting, and accuracy assessment for an
# increasing number of predictors could have been coded with a function
# to reduce the amount of duplicated code.  However, the marking rubric
# requests code that "runs easily, [and] is easy to follow".  For that
# reason, the code is written in script form instead of making use of
# only simple functions

# Create data frame to hold the results of the KNN method with the predictors
# listed in their order of Importance and whether they were included in the 
# computation for KNN and their variable importance value
knnResults <- data.frame(matrix(nrow = 0, ncol = nrow(rfImportanceDF)))
colnames(knnResults) <- rownames(rfImportanceDF)

# Set neighborhood value for all of predictor increments going from 1 to 40
# Intitially tested with more values to see where the accuracy peaked and
# where the accuracy dropped off to get the range to use in practice
grid = expand.grid(k = c(1:40))

# set seed for consistent results
set.seed(1, sample.kind = "Rounding")

##################################  SG Only ####################################

# Vignette on cross validation (Dalpaiz, 2020, Chapter 21)
# perform the cross validation on the training set with 5 folds 

# Train, Predict, Summarize Accuracy for spondylolisthesisGrade
# (no cross-validation or tuning at this stage -- getting baseline results)
train_knn_1 <- train(diseaseClassification ~ spondylolisthesisGrade,
                     method = "knn",
                     data = train,
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid=grid)

# Create object to hold the confusion matrix accuracy from cross-validation
knnAccuracy_1 <- confusionMatrix(predict(train_knn_1, test),
                                 factor(test$diseaseClassification))$overall["Accuracy"]

# Write the KNN model parameters to the knnResults data frame along with 
# the accuracy for the test and training data sets to assess the 
# fit as over, under, or best later on
knnResults[1, "ID"] <- 1
knnResults[1, "spondylolisthesisGrade"] <- "Applied"
knnResults[1, "pelvicIncidence"] <- NA
knnResults[1, "pelvicRadius"] <- NA
knnResults[1, "sacralSlope"] <- NA
knnResults[1, "lumbarLordosis"] <- NA
knnResults[1, "pelvicTilt"] <- NA
knnResults[1, "K"] <- train_knn_1$bestTune$k
knnResults[1, "TrainAccuracy"] <- train_knn_1$results$Accuracy[train_knn_1$bestTune$k]
knnResults[1, "TestAccuracy"] <- knnAccuracy_1
knnResults[1, "Fit"] <- ifelse(knnResults[1, "TrainAccuracy"] < knnResults[1, "TestAccuracy"],
                               "Under", "Over")




##################################  SG + SS ####################################

# Vignette on cross validation (Dalpaiz, 2020, Chapter 21)
# perform the cross validation on the training set with 5 folds 

# Train, Predict, Summarize Accuracy for spondylolisthesisGrade + pelvicIncidence
# (no cross-validation or tuning at this stage -- getting baseline results)
train_knn_2 <- train(diseaseClassification ~ spondylolisthesisGrade
                     + sacralSlope,
                     method = "knn",
                     data = train,
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid=grid)

# Create object to hold the confusion matrix accuracy from cross-validation
knnAccuracy_2 <- confusionMatrix(predict(train_knn_2, test),
                                 factor(test$diseaseClassification))$overall["Accuracy"]

# Write the KNN model parameters to the knnResults data frame along with 
# the accuracy for the test and training data sets to assess the 
# fit as over, under, or best later on
knnResults[2, "ID"] <- 2
knnResults[2, "spondylolisthesisGrade"] <- "Applied"
knnResults[2, "pelvicIncidence"] <- NA
knnResults[2, "pelvicRadius"] <- NA
knnResults[2, "sacralSlope"] <- "Applied"
knnResults[2, "lumbarLordosis"] <- NA
knnResults[2, "pelvicTilt"] <- NA
knnResults[2, "K"] <- train_knn_2$bestTune$k
knnResults[2, "TrainAccuracy"] <- train_knn_2$results$Accuracy[train_knn_2$bestTune$k]
knnResults[2, "TestAccuracy"] <- knnAccuracy_2
knnResults[2, "Fit"] <- ifelse(knnResults[2, "TrainAccuracy"] < knnResults[2, "TestAccuracy"],
                               "Under", "Over")

#############################  SG + SS + PR ####################################


# Vignette on cross validation (Dalpaiz, 2020, Chapter 21)
# perform the cross validation on the training set with 5 folds 

# Train, Predict, Summarize Accuracy for spondylolisthesisGrade + pelvicIncidence
# (no cross-validation or tuning at this stage -- getting baseline results)
train_knn_3 <- train(diseaseClassification ~ spondylolisthesisGrade
                     + sacralSlope
                     + pelvicRadius,
                     method = "knn",
                     data = train,
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid=grid)

# Create object to hold the confusion matrix accuracy from cross-validation
knnAccuracy_3 <- confusionMatrix(predict(train_knn_3, test),
                                 factor(test$diseaseClassification))$overall["Accuracy"]

# Write the KNN model parameters to the knnResults data frame along with 
# the accuracy for the test and training data sets to assess the 
# fit as over, under, or best later on
knnResults[3, "ID"] <- 3
knnResults[3, "spondylolisthesisGrade"] <- "Applied"
knnResults[3, "pelvicIncidence"] <- NA
knnResults[3, "pelvicRadius"] <- "Applied"
knnResults[3, "sacralSlope"] <- "Applied"
knnResults[3, "lumbarLordosis"] <- NA
knnResults[3, "pelvicTilt"] <- NA
knnResults[3, "K"] <- train_knn_3$bestTune$k
knnResults[3, "TrainAccuracy"] <- train_knn_3$results$Accuracy[train_knn_3$bestTune$k]
knnResults[3, "TestAccuracy"] <- knnAccuracy_3
knnResults[3, "Fit"] <- ifelse(knnResults[3, "TrainAccuracy"] < knnResults[3, "TestAccuracy"],
                               "Under", "Over")


#############################  SG + SS + PR + PI ################################


# Vignette on cross validation (Dalpaiz, 2020, Chapter 21)
# perform the cross validation on the training set with 5 folds 

# Train, Predict, Summarize Accuracy for spondylolisthesisGrade + pelvicIncidence
# (no cross-validation or tuning at this stage -- getting baseline results)
train_knn_4 <- train(diseaseClassification ~ spondylolisthesisGrade
                     + sacralSlope
                     + pelvicRadius
                     + pelvicIncidence,
                     method = "knn",
                     data = train,
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid=grid)

# Create object to hold the confusion matrix accuracy from cross-validation
knnAccuracy_4 <- confusionMatrix(predict(train_knn_4, test),
                                 factor(test$diseaseClassification))$overall["Accuracy"]

# Write the KNN model parameters to the knnResults data frame along with 
# the accuracy for the test and training data sets to assess the 
# fit as over, under, or best later on
knnResults[4, "ID"] <- 4
knnResults[4, "spondylolisthesisGrade"] <- "Applied"
knnResults[4, "pelvicIncidence"] <- "Applied"
knnResults[4, "pelvicRadius"] <- "Applied"
knnResults[4, "sacralSlope"] <- "Applied"
knnResults[4, "lumbarLordosis"] <- NA
knnResults[4, "pelvicTilt"] <- NA
knnResults[4, "K"] <- train_knn_4$bestTune$k
knnResults[4, "TrainAccuracy"] <- train_knn_4$results$Accuracy[train_knn_4$bestTune$k]
knnResults[4, "TestAccuracy"] <- knnAccuracy_4
knnResults[4, "Fit"] <- ifelse(knnResults[4, "TrainAccuracy"] < knnResults[4, "TestAccuracy"],
                               "Under", "Over")

#######################  SG + SS + PR + PI + PT ################################


# Vignette on cross validation (Dalpaiz, 2020, Chapter 21)
# perform the cross validation on the training set with 5 folds 

# Train, Predict, Summarize Accuracy for spondylolisthesisGrade + pelvicIncidence
# (no cross-validation or tuning at this stage -- getting baseline results)
train_knn_5 <- train(diseaseClassification ~ spondylolisthesisGrade
                     + sacralSlope
                     + pelvicRadius
                     + pelvicIncidence
                     + pelvicTilt,
                     method = "knn",
                     data = train,
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid=grid)

# Create object to hold the confusion matrix accuracy from cross-validation
knnAccuracy_5 <- confusionMatrix(predict(train_knn_5, test),
                                 factor(test$diseaseClassification))$overall["Accuracy"]

# Write the KNN model parameters to the knnResults data frame along with 
# the accuracy for the test and training data sets to assess the 
# fit as over, under, or best later on
knnResults[5, "ID"] <- 5
knnResults[5, "spondylolisthesisGrade"] <- "Applied"
knnResults[5, "pelvicIncidence"] <- "Applied"
knnResults[5, "pelvicRadius"] <- "Applied"
knnResults[5, "sacralSlope"] <- "Applied"
knnResults[5, "lumbarLordosis"] <- NA
knnResults[5, "pelvicTilt"] <- "Applied"
knnResults[5, "K"] <- train_knn_5$bestTune$k
knnResults[5, "TrainAccuracy"] <- train_knn_5$results$Accuracy[train_knn_5$bestTune$k]
knnResults[5, "TestAccuracy"] <- knnAccuracy_5
knnResults[5, "Fit"] <- ifelse(knnResults[5, "TrainAccuracy"] < knnResults[5, "TestAccuracy"],
                               "Under", "Over")


# Extract the training results (bestTrain) for each knn model (additive most
# important predictor next) method into separate data frames and add
# a new column to hold the predictors used in that model to the data frame
knnOnePred <- train_knn_1$results %>% mutate(Predictors = "SG")
knnTwoPred <- train_knn_2$results %>% mutate(Predictors = "SG + SS")
knnThreePred <- train_knn_3$results %>% mutate(Predictors = "SG + SS + PR")
knnFourPred <- train_knn_4$results %>% mutate(Predictors = "SG + SS + PR + PI")
knnFivePred <- train_knn_5$results %>% mutate(Predictors = "SG + SS + PR + PI + PT")

# Combine all of the results into a single data frame where the predictors
# can be passed to the color aesthetic within ggplot to show all of the
# methods colorized in a single plot so they can be visualized and compared
knnResultsCombined <- rbind(knnOnePred, 
                            knnTwoPred,
                            knnThreePred,
                            knnFourPred,
                            knnFivePred)

# Plot the knnResultsCombined data frame colorizing each knn method as described
# above when combining the results of the methods
ggplot(data = knnResultsCombined, aes(x = k, y = Accuracy, color = Predictors)) + 
  geom_point() + geom_line() +
  ggtitle("KNN Iterative Predictor Addition, Cross-Validation, and Tuning") +
  xlab("K") +
  ylab("Accuracy") + 
  theme(legend.position="bottom")

### Determine the best KNN solution based on the criteria from 
### Dalpaiz (2022, Chapter 7): Best performance and least complex where the
### results from the test are better than the results from the training

knnBest <- knnResults %>% filter(Fit == "Under") %>%
  filter(TestAccuracy == max(TestAccuracy))

# Replace "Under" with "Best" in the `knnResults` data frame
knnResults[knnBest$ID, "Fit"] <- "Best"

### Reformat the `knnResults` data frame for plotting on the pdf page size

# Remove ID
knnResults <- subset(knnResults, select = -c(ID))

# Change column names to abbreviations to display within the train pdf page
knnResultsNames <- c("SG", "SS", "PR", "PI", "PT", "LL", "K", "Train Accuracy",
                     "Test Accuracy", "Fit")

colnames(knnResults) <- knnResultsNames

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(knnResults,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "KNN Iterative Predictor Addition, Cross-Validation, Tuning and Fit") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

################################################################################
################################################################################
#                                 RESULTS                                      #
################################################################################
################################################################################

### Create a descriptive model of a confusion matrix based on the 
### confusion matrix model provided by Irizarry (2022, Chapter 27)

# Create the empty confusion matrix descriptive data frame to show as a table
cmTable <- data.frame()

# Populate the confusion matrix descriptive data frame to show as a table
cmTable[1, "Actually Positive"] <- "True Positive (TP)"
cmTable[1, "Actually Negative"] <- "False Positive (FP)"
cmTable[2, "Actually Positive"] <- "False Negative (FN)"
cmTable[2, "Actually Negative"] <- "True Negative (TN)"

# Apply the rownames to the confusion matrix data frame to show as a table
rownames(cmTable) <- c("Predicted Positive", "Predicted Negative")

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(cmTable,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = TRUE,
             caption = "Confusion Matrix") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Prediction accuracy results for verification data used to assess 
### the fit of a model as over, under, or best

# Compute the accuracy of the cross-validated QDA training model on the 
# verification data set
qdaAccuracy_verification <- confusionMatrix(predict(train_qda, verification),
                                            factor(verification$diseaseClassification))$overall["Accuracy"]

# Compute the accuracy of the cross-validated KNN-5 training model on the 
# verification data set
knnAccuracy_5_verification <- confusionMatrix(predict(train_knn_5, verification),
                                              factor(verification$diseaseClassification))$overall["Accuracy"]

# Compute the accuracy of the cross-validated Random Forest training model on the 
# verification data set
rfAccuracy_verification <- confusionMatrix(predict(train_rf, verification),
                                           factor(verification$diseaseClassification))$overall["Accuracy"]

### Summarize the test accuracy for the cross-validated models and the 
### verification accuracy in a table

# Create the data frame to holde the verification accuracy results
accuracySummary <- data.frame()

# Populated the data frame with the QDA verification accuracy and test
# accuracy to be used to determine the models fit assessment established by 
# Dalpaiz (2020; 2021)
accuracySummary[1, "ID"] <- 1
accuracySummary[1, "Method"] <- "QDA"
accuracySummary[1, "TestAccuracy"] <- qdaAccuracy
accuracySummary[1, "VerificationAccuracy"] <- qdaAccuracy_verification
accuracySummary[1, "Fit"] <- ifelse(accuracySummary[1, "VerificationAccuracy"] 
                                    > accuracySummary[1, "TestAccuracy"], 
                                    "Under", "Over")

# Populated the data frame with the KNN-5 verification accuracy and test
# accuracy to be used to determine the models fit assessment established by 
# Dalpaiz (2020; 2021)
accuracySummary[2, "ID"] <- 2
accuracySummary[2, "Method"] <- "KNN-5"
accuracySummary[2, "TestAccuracy"] <- knnAccuracy_5
accuracySummary[2, "VerificationAccuracy"] <- knnAccuracy_5_verification
accuracySummary[2, "Fit"] <- ifelse(accuracySummary[2, "VerificationAccuracy"] 
                                    > accuracySummary[2, "TestAccuracy"], 
                                    "Under", "Over")


# Populated the data frame with the Random Forest verification accuracy and test
# accuracy to be used to determine the models fit assessment established by 
# Dalpaiz (2020; 2021)
accuracySummary[3, "ID"] <- 3
accuracySummary[3, "Method"] <- "Random Forest"
accuracySummary[3, "TestAccuracy"] <- rfAccuracy
accuracySummary[3, "VerificationAccuracy"] <- rfAccuracy_verification
accuracySummary[3, "Fit"] <- ifelse(accuracySummary[3, "VerificationAccuracy"] 
                                    > accuracySummary[3, "TestAccuracy"], 
                                    "Under", "Over")

### Determine the best solution based on the criteria from 
### Dalpaiz (2022, Chapter 7): Best performance and least complex where the
### results from the test are better than the results from the training

Best <- accuracySummary %>% filter(Fit == "Under") %>%
  filter(VerificationAccuracy == max(VerificationAccuracy))

# Replace "Under" with "Best" in the `accuracySummary` data frame
accuracySummary[Best$ID, "Fit"] <- "Best"

### Reformat the `accuracySummary` data frame for display as a table within
### the knit pdf document

# Remove ID
accuracySummary <- subset(accuracySummary, select = -c(ID))
# Change the names of the columns
accuracySummaryNames <- c("Method", "Test Accuracy", "Verification Accuracy",
                          "Fit")
colnames(accuracySummary) <- accuracySummaryNames

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(accuracySummary,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Verification Data Fit Assesment Based on Model Accuracy") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Build confusion matrices for Random Forest and QDA and extract the
### metrics that were determined to be of use for analyzing the 
### machine learning results from QDA and Random Forest based on the
### class prevalence bias and costs of errors in medicine

# Create the confusion matrix from the verification data set using the
# cross-validated Random Forest model
rfConfusionMatrix <- confusionMatrix(predict(train_rf, verification),
                factor(verification$diseaseClassification))

# Create the confusion matrix from the verification data set using the
# cross-validated QDA model
qdaConfusionMatrix <- confusionMatrix(predict(train_qda, verification),
                factor(verification$diseaseClassification))

# Create a data frame to hold the 'byclass' confusion matrix computed
# metrics identified as valuable for results assessment
qdaMetrics <- as.data.frame(qdaConfusionMatrix$byClass)

# Add new columns to hold the class and method
qdaMetrics <- qdaMetrics %>% 
  mutate(Class = row.names(qdaMetrics)) %>%
  mutate(Method = "QDA")

# Create a data frame to hold the 'byclass' confusion matrix computed
# metrics identified as valuable for results assessment
rfMetrics <- as.data.frame(rfConfusionMatrix$byClass)

# Add new columns to hold the class and method
rfMetrics <- rfMetrics %>% 
  mutate(Class = row.names(rfMetrics)) %>%
  mutate(Method = "Random Forest")

# Combine the QDA metrics and Random Forest metrics data frames
finalMetrics <- rbind(qdaMetrics, rfMetrics)

# Change order of columns and metrics to include in the finalMetrics
# tabular output using confusion matrix derived metrics for QDA and RF
columnOrder <- c("Method", "Class", "Prevalence", "Sensitivity", 
                 "Specificity", "Balanced Accuracy", "F1")
finalMetrics <- finalMetrics[,columnOrder]

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(finalMetrics,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Random Forest and QDA Performance Metrics") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 


# Create a new data frame as a copy of the finalMetrics data frame that was
# output to a table and create a new column called 'Method_Class' that can
# be used to generate colorized plots that are able to compare each method
# and class specific metric in a single plot
finalMetricsPlot <- finalMetrics %>% 
  mutate(Method_Class = paste0(Method, " ", Class)) %>%
  mutate(Method_Class = str_replace(Method_Class, 
                                    pattern = "Class", replacement = "")) %>%
  mutate(Method_Class = str_replace(Method_Class, 
                                    pattern = "Random Forest", replacement = "RF"))

# Rename the column names of the finalMetricsPlot data frame for appearance
finalMetricsPlotNames <- c("Method", 
                           "Class", 
                           "Prevalence", 
                           "Sensitivity", 
                           "Specificity", 
                           "Balanced_Accuracy",
                           "F1")
colnames(finalMetricsPlot) <- finalMetricsPlotNames


### STHDA (n.d) vignette for the following bar plots to get the horizontal 
### stacked format to display side-by-side classes of Random Forest and QDA
### results for Sensitivity, Specificity, Balanced Accuracy, and F1-Score

# Sensitivity Horizontal Stacked Class BarPlot
sensitivityBarPlot <- ggplot(data = finalMetricsPlot, 
                             aes(x = Class, y= Sensitivity, fill = Method)) + 
  geom_bar(stat = "identity", alpha=0.9, position = position_dodge()) +
  ggtitle("Sensitivity Comparision") +
  xlab("Method and Class") + 
  ylab("Value") +
  theme(legend.position="bottom", legend.title = element_blank()) 


# Specificity Horizontal Stacked Class BarPlot
specificityBarPlot <- ggplot(data = finalMetricsPlot, 
                             aes(x = Class, y = Specificity, fill = Method)) + 
  geom_bar(stat = "identity", alpha=0.9, position = position_dodge()) +
  ggtitle("Specificity Comparision") + 
  xlab("Method and Class") + 
  ylab("Value") +
  theme(legend.position="bottom", legend.title = element_blank()) 

# Balanced Accuracy Horizontal Stacked Class BarPlot
balancedAccuracyBarPlot <- ggplot(data = finalMetricsPlot, 
                             aes(x = Class, y = Balanced_Accuracy, fill = Method)) + 
  geom_bar(stat = "identity", alpha=0.9, position = position_dodge()) +
  ggtitle("Balanced Accuracy Comparision") + 
  xlab("Method and Class") + 
  ylab("Value") +
  theme(legend.position="bottom", legend.title = element_blank()) 

# F1-Score Horizontal Stacked Class BarPlot
F1BarPlot <- ggplot(data = finalMetricsPlot, 
                             aes(x = Class, y = F1, fill = Method)) + 
  geom_bar(stat = "identity", alpha=0.9, position = position_dodge()) +
  ggtitle("F1-Score Comparision") + 
  xlab("Method and Class") + 
  ylab("Value") +
  theme(legend.position="bottom", legend.title = element_blank()) 

# Use the library(cowplot) to combine and create a layout for the histogram
# plots of each predictor in the training data, vignette Wilke (2022)
plot_grid(sensitivityBarPlot, 
          specificityBarPlot, 
          balancedAccuracyBarPlot, 
          F1BarPlot, ncol = 2, nrow = 2)

### Extract the prevalence metrics from the confusion matrix metrics
### that were already packaged into the qdaMetrics and rfMetrics data frames
### so this prevalence information can be display in a table as a reminder and
### for discussion in the report

# Combine the QDA and Random Forest metrics data frames
percentageMetrics <- rbind(qdaMetrics, rfMetrics)

# Change order of columns and metrics to include in the finalMetrics
# tabular output using confusion matrix derived metrics for QDA and RF

percentageColumnOrder <- c("Method", "Class", "Prevalence")
percentageMetrics <- percentageMetrics[,percentageColumnOrder]

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(percentageMetrics,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Random Forest and QDA Verification Data Prevalence") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

### Extract the precision metrics from the confusion matrix metrics
### that were already packaged into the qdaMetrics and rfMetrics data frames
### so this precision information can be display in a table and plot for
### analysis of the results of QDA and Random Forest

# Combine the QDA and Random Forest metrics data frames
precisionMetrics <- rbind(qdaMetrics, rfMetrics)

# Change order of columns and metrics to include in the finalMetrics
# tabular output using confusion matrix derived metrics for QDA and RF

precisionColumnOrder <- c("Method", "Class", "Precision")
precisionMetrics <- precisionMetrics[,precisionColumnOrder]

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(precisionMetrics,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "QDA and Random Forest Precision Comparision") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 

# Precision Horizontal Stacked Class BarPlot
precisionBarPlot <- ggplot(data = precisionMetrics, 
                           aes(x = Class, y = Precision, fill = Method)) + 
  geom_bar(stat = "identity", alpha=0.9, position = position_dodge()) +
  ggtitle("QDA and Random Forest Precision Comparision") + 
  xlab("Method and Class") + 
  ylab("Value") +
  theme(legend.position="bottom", legend.title = element_blank()) 

precisionBarPlot

### Create ensembled accuracy metrics of balanced accuracy, F1-Score, and
### precision to evaluate against a cut-off mean of 0.70 to determine
### if the QDA machine learning solution has utility to use a specific
### classification within a future image augementation system

# Filter out QDA metrics based on the SL class 
qdaSL_1 <- finalMetrics %>% filter(Method == "QDA", Class == "Class: SL")
qdaSL_2 <- precisionMetrics %>% filter(Method == "QDA", Class == "Class: SL")

# Compute the mean of the ensembled metric for the SL class
qdaMeanSL <- (qdaSL_1$`Balanced Accuracy` + qdaSL_1$F1 + qdaSL_2$Precision)*(1/3)

# Filter out QDA metrics based on the NO class 
qdaNO_1 <- finalMetrics %>% filter(Method == "QDA", Class == "Class: NO")
qdaNO_2 <- precisionMetrics %>% filter(Method == "QDA", Class == "Class: NO")

# Compute the mean of the ensembled metric for the NO class
qdaMeanNO <- (qdaNO_1$`Balanced Accuracy` + qdaNO_1$F1 + qdaNO_2$Precision)*(1/3)

# Filter out QDA metrics based on the DH class 
qdaDH_1 <- finalMetrics %>% filter(Method == "QDA", Class == "Class: DH")
qdaDH_2 <- precisionMetrics %>% filter(Method == "QDA", Class == "Class: DH")

# Compute the mean of the ensembled metric for the DH class
qdaMeanDH <- (qdaDH_1$`Balanced Accuracy` + qdaDH_1$F1 + qdaDH_2$Precision)*(1/3)

# Create a date frame to hold the ensembled metric means
qdaMeans <- data.frame()

# Populate the ensembled metric means into the qdaMeans data frame so they 
# can be presented in a table and analyzed
qdaMeans[1, "Method"] <- "QDA"
qdaMeans[1, "DH Mean"] <- qdaMeanDH
qdaMeans[1, "NO Mean"] <- qdaMeanNO
qdaMeans[1, "SL Mean"] <- qdaMeanSL

# Build and display table using knitr vignette 
# (Xie et. al., 2022, Chapter 10; Pileggi, 2022; Zhu, 2021)
knitr::kable(qdaMeans,
             booktabs = TRUE,
             longtable = TRUE,
             linesep = "",
             row.names = FALSE,
             caption = "Mean QDA Performance Metrics (Ensemble of Balanced Accuracy, F1-Score, and Precision)") %>%
  kable_styling(latex_options = c("striped"), 
                position = "center",
                font_size = 8) 


# References

# | Daffodil Software. (2017, July 30). *9 Applications of Machine Learning from*
# |    *Day-to-Day Life.* Medium. https://medium.com/app-affairs/9-applications
# |     -of-machine-learning-from-day-to-day-life-112a47a429d0
# 
# | Dalpaiz, D. (2020). *R for Statistical Learning.*
# |    https://daviddalpiaz.github.io/r4sl/
#     
# | Dalpaiz, D. (2022). *Applied Statistics with R.* https://book.stat420.org
# 
# | Dydyk, A.M., Massa, R.N., Mesfin, F.B. (2022, January 18). *Disc Herniation.*
# |    National Library of Medicine. https://www.ncbi.nlm.nih.gov/books/
# |    NBK441822/#:~:text=The%20incidence%20of%20a%20herniated,1%2D3%20percent
# |    %20of%20patients.
# 
# 
# | Irizarry, R.A., (n.d.) Professional Certificate in Data Science [MOOC]. 
# |    HarvardX https://www.edx.org/professional-certificate/harvardx-data-science
# 
# | Irizarry, R.A., (2022). *Introduction to Data Science: Data Analysis*
# |    *and Prediction Algorithms with R* 
# |    bookdown. http://rafalab.dfci.harvard.edu/dsbook/
# 
# | Koslosky, E., & Gendelberg, D. (2020). Classification in Brief: The Meyerding
# |    Classification System of Spondylolisthesis. *Clinical Orthopaedics* 
# |    *and Related Research, 478*(5), 1125-1130. 10.1097/CORR.0000000000001153 
# 
# | Kuhn, M. (2019, March 27). *The caret Package.* GitHub. 
# |    https://topepo.github.io/caret/index.html
# 
# | Kundu, R. (2023, March 2). *Confusion Matrix: How To Use It & Interpret* 
# |    *Results [Examples].* V7 Labs. https://www.v7labs.com/blog/
# |    confusion-matrix-guide
# 
# 
# | Le Huec, J.C., Aunoble, S., Philippe, L. (2011). Pelvic parameters: origins
# |    and significance. *European spine journal, 20*(5), 564-571. 
# |    10.1007/s00586-011-1940-1.
# 
# | Mota, H., Barreto, G., Neto, A. (2011). Vertebral Column Data Set [Data set].
# |    UCI Machine Learning Repository.
# |    https://archive.ics.uci.edu/ml/datasets/vertebral%2Bcolumn
# 
# | O'Brien, R., & Ishwaran, H. (2019). A Random Forests Quantile Classifier for 
# |    Class Imbalanced Data. *Pattern Recognition, 90*, 232–249. doi:10.1016/j.
# |    patcog.2019.01.036.
#   
# | Olugbenga, M. (2023, January 25). *Balanced Accuracy: When Should You Use It?*
# |    neptune.ai. https://neptune.ai/blog/balanced-accuracy
#   
# | Pileggi, S. (2022, January 23). *Report Ready PDF tables with rmarkdown,*
# |    *knitr, kableExtra, and LaTeX.* Piping hot data.
# |    https://www.pipinghotdata.com/posts/2022-01-24-report-ready-pdf-tables-
# |    with-rmarkdown-knitr-kableextra-and-latex/
#     
# | Podgorelec, V., Kokol, P., Stiglic, B., Rozman, I. (2002). Decision trees: 
# |    an overview and their use in medicine. *Journal of Medical Systems, 26*
# |    (5), 445-463. 10.1023/a:1016409317640
#   
# | Speck, M. (2017, May 15). *What is K-Nearest Neighbors?* Medium.
# |    https://medium.com/@mjspeck/what-is-k-nearest-neighbors-c9b4cdf9f35c
#   
# | STHDA. (n.d.). *ggplot2 barplots : Quick start guide - R software and data*
# |    *visualization.* Statistical Tools for High-Throughput Data Analysis.
# |    http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-
# |    software-and-data-visualization
#   
#   
# | Tenny, S., & Gillis, C.C. (2022, May 24). *Spondylolisthesis.*
# |    https://www.ncbi.nlm.nih.gov/books/NBK430767/#:~:text=Current%20estimates
# |    %20for%20prevalence%20are,for%2075%25%20of%20all%20cases.
#   
# | The R Foundation. (n.d.) What is R? https://www.r-project.org/about.html
#   
# | Wickham, H. (2014). Tidy Data. *Journal of Statistical Software, 59*(10),
# |    1-23. https://doi.org/10.18637/jss.v059.i10
# 
# | Wilke, C.O., (2022, December 15). *Introduction to cowplot.* CRAN.
# |    https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.
# |    html#:~:text=The%20cowplot%20package%20is%20a,or%20mix%20plots%20with%20
# |    images.
#   
# | Xie, Y., Dervieux, C., Riederer, E. (2022). *R Markdown Cookbook.*
# |    Chapman & Hall/CRC. https://bookdown.org/yihui/rmarkdown-cookbook/
#     
# | Zhu, H. (2021). *Create Awesome LaTeX Table with knitr::kable and kableExtra.*
# |    R-project. https://cran.r-project.org/web/packages/kableExtra/vignettes/
# |    awesome_table_in_pdf.pdf
  

  