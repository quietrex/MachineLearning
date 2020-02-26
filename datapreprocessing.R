# Library Initialization

library(plyr)
library(dplyr)
library(dummies)
library(ggplot2)
library(tidyverse)
library(DataExplorer)
library(scales)
library(psych)
library(DataExplorer)
library(caTools)
library(ROSE) # Data sampling
library(reshape) # To show outliers
library(naniar) # Missing columns 
library(ggplot2)
library(corrplot)

# Set Seed 
set.seed(2019)

# Getting the dataset
# The dataset in loaded from a csv file downloaded from openml (https://www.openml.org/d/41162) into the dataframe as kickDataset.
setwd("D:/Master Semester 2/Sem 2")
kickDataset = read.csv('kick.csv', header = T, stringsAsFactors = FALSE)

# Data Preprocessing 

# Create temporary dataset to store original dataset named kickDataset_temp
KickDataset_OriginalData <- kickDataset

# Checking inconsistent data and missing values
str(kickDataset)               # Sharing same attributes
str(KickDataset_OriginalData)  # Sharing same attributes

# Dimension of dataset
dim(kickDataset)

# Check table
table(kickDataset$IsBadBuy)

# Check class distribution
prop.table(table(kickDataset$IsBadBuy))

# variable name
names(kickDataset)

# To transform all non-numeric / nominal / ordinal data / target variable into factor
kickDataset$IsBadBuy <- factor(kickDataset$IsBadBuy)                         #Target variable e.g. 0 / 1
kickDataset$PurchDate <- factor(kickDataset$PurchDate)                       # Ordinal data e.g. date
kickDataset$Auction <- factor(kickDataset$Auction)                           # Nominal data e.g. Adesa / Manheim / Other
kickDataset$Make <- factor(kickDataset$Make)                                 # Nominal data e.g. Mazda / Ford / Mitsubishi / etc.
kickDataset$Model <- factor(kickDataset$Model)                               # Nominal data e.g. STRATUS V6 / Neon / Spectra / etc.
kickDataset$Trim <- factor(kickDataset$Trim)                                 # Nominal data e.g. i / ST / etc.
kickDataset$SubModel <- factor(kickDataset$SubModel)                         # Nominal data e.g. 4D SEDAN I / 2D COUPE ZX3 / etc.
kickDataset$Color <- factor(kickDataset$Color)                               # Nominal data e.g. Silver / Yellow / etc.
kickDataset$Transmission <- factor(kickDataset$Transmission)                 # Nominal data e.g. Auto / Manual / etc.
kickDataset$WheelTypeID <- factor(kickDataset$WheelTypeID)                   # Nominal data e.g. 0 / 1 / 2 / etc.
kickDataset$WheelType <- factor(kickDataset$WheelType)                       # Nominal data e.g. Alloy / Covers / etc.
kickDataset$Nationality <- factor(kickDataset$Nationality)                   # Nominal data e.g. American / Asian / etc.
kickDataset$Size <- factor(kickDataset$Size)                                 # Nominal data e.g. Medium / Compact / etc.
kickDataset$TopThreeAmericanName <- factor(kickDataset$TopThreeAmericanName) # Nominal data e.g. CHRYSLER / FORD / etc.
kickDataset$PRIMEUNIT <- factor(kickDataset$PRIMEUNIT)                       # Nominal data e.g. Yes / No
kickDataset$AUCGUART <- factor(kickDataset$AUCGUART)                         # Nominal data e.g. Green / Red 
kickDataset$BYRNO <- factor(kickDataset$BYRNO)                               # Nominal data e.g. Code assigned to purchaser
kickDataset$VNZIP1 <- factor(kickDataset$VNZIP1)                             # Nominal data e.g. Zip Code
kickDataset$VNST <- factor(kickDataset$VNST)                                 # Nominal data e.g. State
kickDataset$IsOnlineSale <- factor(kickDataset$IsOnlineSale)                 # Nominal data e.g. True /False

# variable structure
str(kickDataset)

# Numeric data
Vechicle_Year <- kickDataset$VehYear # Vehicle Year
Vechicle_Age <- kickDataset$VehicleAge
Vehicle_Odometer <- kickDataset$VehOdo
Vehicle_MMRAcquisition_AuctionAveragePrice <- kickDataset$MMRAcquisitionAuctionAveragePrice
Vehicle_MMRAcquisition_AuctionCleanPrice <- kickDataset$MMRAcquisitionAuctionCleanPrice
Vehicle_MMRAcquisition_RetailAveragePrice <- kickDataset$MMRAcquisitionRetailAveragePrice
Vehicle_MMRAcquisiton_RetailCleanPrice <- kickDataset$MMRAcquisitonRetailCleanPrice
Vehicle_MMRCurrent_AuctionAveragePrice <- kickDataset$MMRCurrentAuctionAveragePrice
Vehicle_MMRCurrent_AuctionCleanPrice <- kickDataset$MMRCurrentAuctionCleanPrice
Vehicle_MMRCurrent_RetailAveragePrice <- kickDataset$MMRCurrentRetailAveragePrice
Vehicle_MMRCurrent_RetailCleanPrice <- kickDataset$MMRCurrentRetailCleanPrice
Vechicle_Acquisition_SoldCost <- kickDataset$VehBCost
Vechicle_WarrantyCost <- kickDataset$WarrantyCost

gg_miss_var(kickDataset)

checking_missing_val <- function(df) {
        missing_col = sum(is.na(df))
        missing_noOfRecord = colSums(sapply(df,is.na))
        x <- list(missing_col, missing_noOfRecord)
        return(x)
}

checkingMissingValue <- function(ds) {
  total_missing_column <- checking_missing_val(ds)[[1]]
  missing_records = checking_missing_val(ds)[[2]]
  
  print("Total missing column in the dataset is:")
  print(toString(total_missing_column)) 
  
  for (i in 1:ncol(ds))
    if(toString(missing_records[i]) != 0) {
      #print("Variable that is found having missing values is:")
      print(missing_records[i])
    }
}

# Check missing values
checkingMissingValue(kickDataset)

# Define number of observations in the dataset
NumOfObservation <- nrow(kickDataset) #72983
NumOfObservation

# Descriptive statistic of entire dataset
summary(kickDataset)

tempkickDs_DescriptiveStat_DV <- select(kickDataset, IsBadBuy)

tempkickDs_DescriptiveStat_categorical <- select(kickDataset, c(IsOnlineSale, Auction, PurchDate, Make, Model, Trim, SubModel, Color, Transmission, 
                                                                WheelTypeID, WheelType, Nationality, Size, TopThreeAmericanName, PRIMEUNIT, AUCGUART, 
                                                                BYRNO, VNZIP1, VNST) )

tempkickDs_DescriptiveStat_numerical <- select(kickDataset, c(VehYear, VehicleAge, MMRAcquisitionAuctionAveragePrice, MMRAcquisitionAuctionCleanPrice, 
                                                              MMRAcquisitionRetailAveragePrice, MMRAcquisitonRetailCleanPrice, MMRCurrentAuctionAveragePrice,
                                                              MMRCurrentAuctionCleanPrice, MMRCurrentRetailAveragePrice, MMRCurrentRetailCleanPrice, 
                                                              VehOdo, VehBCost, WarrantyCost) )

# Dependent variable
barplot(table(tempkickDs_DescriptiveStat_DV$IsBadBuy))

# Categorical variable structure
str(tempkickDs_DescriptiveStat_categorical)
str(tempkickDs_DescriptiveStat_numerical)

# variable frequency
table(tempkickDs_DescriptiveStat_categorical$PurchDate)
table(tempkickDs_DescriptiveStat_categorical$Make)
table(tempkickDs_DescriptiveStat_categorical$Trim)
table(tempkickDs_DescriptiveStat_categorical$SubModel)
table(tempkickDs_DescriptiveStat_categorical$VNZIP1)
table(tempkickDs_DescriptiveStat_categorical$VNST)
# Selected column with more than 80% missing values
summary(tempkickDs_DescriptiveStat_categorical$PRIMEUNIT)
summary(tempkickDs_DescriptiveStat_categorical$AUCGUART)

# Categorical variable summary
summary(tempkickDs_DescriptiveStat_categorical)

# Numerical variable summary
summary(tempkickDs_DescriptiveStat_numerical)

# Describe numerical variable
describe(tempkickDs_DescriptiveStat_numerical)

# Vehicle Odometer
ggplot(data = tempkickDs_DescriptiveStat_numerical, aes(tempkickDs_DescriptiveStat_numerical$VehOdo)) + 
  geom_histogram(aes(y =..density..), fill = "purple") +
  geom_density() + labs(title = "Histogram for Odometer", x = "Vehicle Odometer") 

# Vehicle Age
ggplot(data = tempkickDs_DescriptiveStat_numerical, aes(tempkickDs_DescriptiveStat_numerical$VehicleAge)) + 
geom_histogram(fill = "purple") + labs(title = "Histogram for Vehicle Age", x = "Vehicle Age", y = "Frequency") 

# Warranty Cost
ggplot(data = tempkickDs_DescriptiveStat_numerical, aes(tempkickDs_DescriptiveStat_numerical$WarrantyCost)) + 
  geom_histogram(aes(y =..density..), fill = "purple") +
  geom_density()+ labs(title = "Histogram for Warranty Cost", x = "Warranty Cost", y = "Frequency") 

# VehBCost
ggplot(data = tempkickDs_DescriptiveStat_numerical, aes(tempkickDs_DescriptiveStat_numerical$VehBCost)) + 
  geom_histogram(aes(y =..density..), fill = "purple") +
  geom_density()+ labs(title = "Histogram for VehBCost", x = "VehBCost", y = "Frequency") 

# VehBCost
ggplot(data = tempkickDs_DescriptiveStat_numerical, aes(tempkickDs_DescriptiveStat_numerical$VehBCost)) + 
  geom_histogram(aes(y =..density..), fill = "purple") +
  geom_density()+ labs(title = "Histogram for VehBCost", x = "VehBCost", y = "Frequency") 

ggplot(data = tempkickDs_DescriptiveStat_numerical, aes(tempkickDs_DescriptiveStat_numerical$MMRAcquisitionAuctionAveragePrice)) + 
  geom_histogram(aes(y =..density..), fill = "purple") +
  geom_density()+ labs(title = "Histogram for MMRAcquisitionAuctionAveragePrice", x = "MMRAcquisitionAuctionAveragePrice", y = "Frequency") 

ggplot(data = tempkickDs_DescriptiveStat_numerical, aes(tempkickDs_DescriptiveStat_numerical$MMRAcquisitionAuctionCleanPrice)) + 
  geom_histogram(aes(y =..density..), fill = "purple") +
  geom_density()+ labs(title = "Histogram for MMRAcquisitionAuctionCleanPrice", x = "MMRAcquisitionAuctionCleanPrice", y = "Frequency") 

plot(tempkickDs_DescriptiveStat_numerical$MMRAcquisitionAuctionAveragePrice, 
     tempkickDs_DescriptiveStat_numerical$MMRAcquisitionAuctionCleanPrice,
     main = "MMR Acquisition Auction Prices Scatter Plot",
     xlab = "Acquistion Auction Average Price", ylab = "Acquistion Auction Clean Price")

plot(tempkickDs_DescriptiveStat_numerical$MMRAcquisitionRetailAveragePrice, 
     tempkickDs_DescriptiveStat_numerical$MMRAcquisitonRetailCleanPrice,
     main = "MMR Acquisition Retail Prices Scatter Plot",
     xlab = "Acquistion Retail Average Price", ylab = "Acquistion Retail Clean Price")

plot(tempkickDs_DescriptiveStat_numerical$MMRCurrentAuctionAveragePrice, 
     tempkickDs_DescriptiveStat_numerical$MMRCurrentAuctionCleanPrice,
     main = "MMR Current Auction Prices Scatter Plot",
     xlab = "Current Auction Average Price", ylab = "Current Auction Clean Price")

plot(tempkickDs_DescriptiveStat_numerical$MMRCurrentRetailAveragePrice, 
     tempkickDs_DescriptiveStat_numerical$MMRCurrentRetailCleanPrice,
     main = "MMR Current Retail Scatter Plot",
     xlab = "Current Retail Average Price", ylab = "Current Retail Clean Price")

plot(tempkickDs_DescriptiveStat_numerical$WarrantyCost, 
     tempkickDs_DescriptiveStat_numerical$VehBCost,
     main = "Warranty and VehBCost Scatter Plot",
     xlab = "Warranty Cost", ylab = "VehBCost")

plot(tempkickDs_DescriptiveStat_numerical$VehicleAge, 
     tempkickDs_DescriptiveStat_numerical$VehYear,
     main = "Vehicle Age and Vehicle Year",
     xlab = "Vehicle Age", ylab = "Vehicle Year")

glimpse(tempkickDs_DescriptiveStat_numerical)

# Normality Checking
var_Outliers <- melt(tempkickDs_DescriptiveStat_numerical) # To select outliers
p <- ggplot(var_Outliers, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

ggplot(tempkickDs_DescriptiveStat_numerical, aes(sample = MMRCurrentRetailCleanPrice, colour = "purple")) +
  labs(title="MMRCurrentRetailCleanPrice")+
  stat_qq() +
  stat_qq_line()

ggplot(tempkickDs_DescriptiveStat_numerical, aes(sample = MMRAcquisitionAuctionCleanPrice, colour = "purple")) +
  labs(title="MMRAcquisitionAuctionCleanPrice")+
  stat_qq() +
  stat_qq_line()

ggplot(tempkickDs_DescriptiveStat_numerical, aes(sample = MMRAcquisitionRetailAveragePrice, colour = "purple")) +
  labs(title="MMRAcquisitionRetailAveragePrice")+
  stat_qq() +
  stat_qq_line()

ggplot(tempkickDs_DescriptiveStat_numerical, aes(sample = MMRAcquisitonRetailCleanPrice, colour = "purple")) +
  labs(title="MMRAcquisitonRetailCleanPrice")+
  stat_qq() +
  stat_qq_line()

ggplot(tempkickDs_DescriptiveStat_numerical, aes(sample = MMRCurrentAuctionCleanPrice, colour = "purple")) +
  labs(title="MMRCurrentAuctionCleanPrice")+
  stat_qq() +
  stat_qq_line()

ggplot(tempkickDs_DescriptiveStat_numerical, aes(sample = MMRCurrentRetailAveragePrice, colour = "purple")) +
  labs(title="MMRCurrentRetailAveragePrice")+
  stat_qq() +
  stat_qq_line()

ggplot(tempkickDs_DescriptiveStat_numerical, aes(sample = MMRCurrentRetailCleanPrice, colour = "purple")) +
  labs(title="MMRCurrentRetailCleanPrice")+
  stat_qq() +
  stat_qq_line()

ggplot(tempkickDs_DescriptiveStat_numerical, aes(sample = VehBCost, colour = "purple")) +
  labs(title="VehBCost")+
  stat_qq() +
  stat_qq_line()

ggplot(tempkickDs_DescriptiveStat_numerical, aes(sample = WarrantyCost, colour = "purple")) +
  labs(title="WarrantyCost")+
  stat_qq() +
  stat_qq_line()

# Variables which found missing values
# Trim, SubModel, Color, Transmission, WheelTypeID, WheelType, Nationality, Size, TopThreeAmericanName,
# MMRAcquisitionAuctionAveragePrice, MMRAcquisitionAuctionCleanPrice, MMRAcquisitionRetailAveragePrice, MMRAcquisitonRetailCleanPrice,
# MMRCurrentAuctionAveragePrice, MMRCurrentAuctionCleanPrice, MMRCurrentRetailAveragePrice, MMRCurrentRetailAveragePrice, MMRCurrentRetailCleanPrice
# PRIMEUNIT, AUCGUART, VehBCost 

# 95% of total observations of both primeunit and aucguart have exceeded 80% missing values of the entire dataset. 
# Thus it will be ignored/ deleted at the follow next stage.
PrimeUnit_Ms_Val = (sum(is.na(kickDataset$PRIMEUNIT)) / NumOfObservation) * 100 
paste(toString(round(PrimeUnit_Ms_Val, 2)), "% of PrimeUnit has exceeded the limit of 80% boundary.")
Aucguart_Ms_Val = (sum(is.na(kickDataset$AUCGUART)) / NumOfObservation) * 100
paste(toString(round(Aucguart_Ms_Val, 2)), "% of Aucguart has exceeded the limit of 80% boundary.")

# Deleting variables PRIMEUNIT and AUCGUART and store it to existing dataset - kickDataset. Therefore, remaining of 31 variables.
kickDataset <- select(kickDataset, -PRIMEUNIT)
kickDataset <- select(kickDataset, -AUCGUART)

# Also delete purchase date, since it does not contain any meaningful information
kickDataset <- select(kickDataset, -PurchDate)
kickDataset <- select(kickDataset, -WheelTypeID)

# Reproduce missing values
# Check missing values
checkingMissingValue(kickDataset)

#psych package
#describe
#boxplot(kickDataset$MMRCurrentRetailCleanPrice)
#summary(kickDataset)

Modes <- function(x) {
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    ux[tab == max(tab)]
}

# Replace Trim to the most frequent keyword from Trim variable in the dataset
kickDataset$Trim <- as.character(kickDataset$Trim)
kickDataset$Trim[is.na(kickDataset$Trim)] <- Modes(kickDataset$Trim)
kickDataset$Trim <- as.factor(kickDataset$Trim)

# Other seem fine. So, to treat missing values as of others.

# ---------------------------------------------------------------------------------------------------- STAGE 1 - Missing values imputation

# Transforming variable back to character to begin mising values imputation.
kickDataset$Make <- as.character(kickDataset$Make)
kickDataset$Color <- as.character(kickDataset$Color)

# List of make of the vechicle
list_make <- as.list(kickDataset$Make)

# Backing up the dataset
entireDs <- kickDataset

list_color <- as.list(kickDataset$Color)
list_of_ms_color = c()
for (j in 1:length(list_color)) {
    if(is.na(list_color[j])) {
        #   temp$Color[j] <- modeColor
        list_of_ms_color <- c(list_of_ms_color, j)
    }
}

# List of row number and total length of the list which contain missing values from the variable color
list_of_ms_color
length(list_of_ms_color)

# Replacing mode for missing values from variable Color------------------------------------------------(1)
for ( k in 1: length(list_of_ms_color)) {
  print(list_of_ms_color[k])
  
  temp_make <- kickDataset[list_of_ms_color[k],]$Make
  print(temp_make)
  
  temp <- filter(kickDataset, Make == temp_make)
  modeColor <- Modes(temp$Color) # 
  print(modeColor)
  
  kickDataset[list_of_ms_color[k],]$Color <- modeColor
}

kickDataset$Make <- factor(kickDataset$Make)
kickDataset$Color <- factor(kickDataset$Color)

str(kickDataset)
# Replacing missing values for the transmission variable-----------------------------------------------(2)
kickDataset$Transmission <- as.character(kickDataset$Transmission)

# To ensure variable has transformed properly to non-factor
str(kickDataset)

entireDs <- kickDataset

# Getting transmission frequency
table(kickDataset$Transmission)

kickDataset$Transmission[kickDataset$Transmission == 'Manual'] <- "MANUAL" # Putting only AUTO and MANUAL

list_transmission <- as.list(kickDataset$Transmission) # Getting transmission in a list named list_tranmission
list_of_ms_transmission = c() # Empty List that will store only row number that has missing value.

for (j in 1:length(list_transmission)) {
  if(is.na(list_transmission[j])) {
    list_of_ms_transmission <- c(list_of_ms_transmission, j)
  }
}
list_of_ms_transmission # Row number that is seen empty in the list.
length(list_of_ms_transmission)

# Replacing mode by distinct year for the variable transmission

for ( k in 1: length(list_of_ms_transmission)) {
  print(list_of_ms_transmission[k])
  
  temp_year <- kickDataset[list_of_ms_transmission[k],]$VehYear
  print(temp_year)
  
  temp <- filter(kickDataset, VehYear == temp_year)
  modeTransmission <- Modes(temp$Transmission)
  print(modeTransmission)
  
  
  kickDataset[list_of_ms_transmission[k],]$Transmission <- modeTransmission
}

kickDataset$Transmission <- factor(kickDataset$Transmission)

str(kickDataset)
# Replacing missing values for the nationality variable-----------------------------------------------(3)
# Check missing values
checkingMissingValue(kickDataset)

table(kickDataset$Nationality)

kickDataset$Nationality <- as.character(kickDataset$Nationality)
modeNationality <- Modes(kickDataset$Nationality)
modeNationality

# Putting nationality into a list
list_nationality <- as.list(kickDataset$Nationality)
list_of_ms_nationality = c()
for (j in 1:length(list_nationality)) {
  if(is.na(list_nationality[j])) {
    list_of_ms_nationality <- c(list_of_ms_nationality, j) # Getting column with missing value
  }
}
list_of_ms_nationality
length(list_of_ms_nationality)

for ( k in 1: length(list_of_ms_nationality)) {
  Column_Nationality_Ms = list_of_ms_nationality[k]
  kickDataset[Column_Nationality_Ms, ]$Nationality <- modeNationality
}

kickDataset$Nationality <- factor(kickDataset$Nationality)

# Replacing missing values for the variable size ----------------------------------------------------(4)
# Check missing values
checkingMissingValue(kickDataset)

table(kickDataset$Size)

kickDataset$Size <- as.character(kickDataset$Size)
str(kickDataset)
modeSize <- Modes(kickDataset$Size)
modeSize

list_size <- as.list(kickDataset$Size)
list_of_ms_size = c()
for (j in 1:length(list_size)) {
  if(is.na(list_size[j])) {
    list_of_ms_size <- c(list_of_ms_size, j)
  }
}
list_of_ms_size
length(list_of_ms_size)

for ( k in 1: length(list_of_ms_size)) {
  print(list_of_ms_size[k])
  
  kickDataset[list_of_ms_size[k],]$Size <- modeSize
}

kickDataset$Size <- factor(kickDataset$Size)
str(kickDataset)

# Replacing missing values for the variable TopThreeAmericanName -----------------------------------------------(5)
# Check missing values
checkingMissingValue(kickDataset)

kickDataset$TopThreeAmericanName <- as.character(kickDataset$TopThreeAmericanName)
str(kickDataset)
modeTopThreeAmericanName <- Modes(kickDataset$TopThreeAmericanName)
modeTopThreeAmericanName

table(kickDataset$TopThreeAmericanName)

list_TopThreeAmericanName <- as.list(kickDataset$TopThreeAmericanName)
list_of_ms_TopThreeAmericanName = c()
for (j in 1:length(list_TopThreeAmericanName)) {
  if(is.na(list_TopThreeAmericanName[j])) {
    list_of_ms_TopThreeAmericanName <- c(list_of_ms_TopThreeAmericanName, j)
  }
}
list_of_ms_TopThreeAmericanName
length(list_of_ms_TopThreeAmericanName)

for ( k in 1: length(list_of_ms_TopThreeAmericanName)) {
  Missing_TopThreeAmericanName_Column = list_of_ms_TopThreeAmericanName[k]
  
  kickDataset[Missing_TopThreeAmericanName_Column, ]$TopThreeAmericanName <- modeTopThreeAmericanName
}

kickDataset$TopThreeAmericanName <- factor(kickDataset$TopThreeAmericanName)
str(kickDataset)

# Replacing missing values for the variable WheelType ----------------------------------------------------(6)
# Check missing values
checkingMissingValue(kickDataset)

table(kickDataset$WheelType)

kickDataset$WheelType

kickDataset$WheelType <- as.character(kickDataset$WheelType)
str(kickDataset)
modeWheelType <- Modes(kickDataset$WheelType)
modeWheelType

list_WheelType <- as.list(kickDataset$WheelType)
list_of_ms_WheelType = c()
for (j in 1:length(list_WheelType)) {
  if(is.na(list_WheelType[j])) {
    list_of_ms_WheelType <- c(list_of_ms_WheelType, j)
  }
}
list_of_ms_WheelType
length(list_of_ms_WheelType)

for ( k in 1: length(list_of_ms_WheelType)) {
  #  print(list_of_ms_WheelType[k])
  kickDataset[list_of_ms_WheelType[k],]$WheelType <- modeWheelType
}

kickDataset$WheelType <- factor(kickDataset$WheelType)
str(kickDataset)

# Replacing missing values for the variable SubModel ----------------------------------------------------(7)
# Check missing values
checkingMissingValue(kickDataset)
kickDataset$SubModel <- as.character(kickDataset$SubModel)
str(kickDataset)
modeSubModel <- Modes(kickDataset$SubModel)
modeSubModel

table(kickDataset$SubModel)

list_sub_model <- as.list(kickDataset$SubModel)
list_of_ms_SubModel = c()
for (j in 1:length(list_sub_model)) {
  if(is.na(list_sub_model[j])) {
    list_of_ms_SubModel <- c(list_of_ms_SubModel, j)
  }
}
list_of_ms_SubModel # List of missing column in sub model
length(list_of_ms_SubModel)

for ( k in 1: length(list_of_ms_SubModel)) {
  Ms_SubModel_Column = list_of_ms_SubModel[k]
  
  kickDataset[list_of_ms_SubModel[k],]$SubModel <- modeSubModel
}

kickDataset$SubModel <- factor(kickDataset$SubModel)
str(kickDataset)

# Replacing/Removing missing values for the variable WheelTypeID --------------------------------------------------(8)
# Check missing values
checkingMissingValue(kickDataset)

# variable WheelTypeID may not contain crucial information that we may need for the analysis, therefore, we will consider to remove this variable instead.
# More importantly, wheeltype itself has been enough.

# Deleting variables WheelTypeID and store it to existing dataset - kickDataset. Therefore, remaining of 30 variables.
#kickDataset <- select(kickDataset, -WheelTypeID)
str(kickDataset)

# Replacing/Removing missing values for the variable VehBCost --------------------------------------------------(8)
# Check missing values
checkingMissingValue(kickDataset)

# Imputing continuous data for variable VehBCost ---------------------------------------------------------------(9)
tempKickDataset <- kickDataset

# Check missing values
checkingMissingValue(kickDataset)

# Showing in the histogram, though it seems like a normal distribution. however, due to the fact of having outliers which it departs far from the other data.
# Therefore, we are opt to choose median as a standard missing value imputation method. Outliers will be treating later in the following stage.
summary(kickDataset$VehBCost)
hist(kickDataset$VehBCost)

kickDataset$VehBCost = ifelse( is.na(kickDataset$VehBCost), median(kickDataset$VehBCost, na.rm = T), kickDataset$VehBCost)

# Imputing continuous data for variable MMRAcquisitionAuctionAveragePrice ---------------------------------------------------------------(10)
tempKickDataset <- kickDataset

# Check missing values
checkingMissingValue(kickDataset)

# again, data from MMRAcquisitionAuctionAveragePrice has shown outliers in the data with Maximum value of 35722.
# Median will be chosen, since if to replace data with average number, it will not be appropriate since it appears outliers in the dataset.
# Thus, we replace it with median.
summary(kickDataset$MMRAcquisitionAuctionAveragePrice)
hist(kickDataset$MMRAcquisitionAuctionAveragePrice)

# we do notice the range from median and mean are quite close to each other, so we can consider replacing both either mean or median
kickDataset$MMRAcquisitionAuctionAveragePrice = ifelse( is.na(kickDataset$MMRAcquisitionAuctionAveragePrice), 
                                                        median(kickDataset$MMRAcquisitionAuctionAveragePrice, na.rm = T), 
                                                        kickDataset$MMRAcquisitionAuctionAveragePrice)

# Checking will be gone through all of the continuous variable with missing values

# MMRAcquisitionAuctionCleanPrice
summary(kickDataset$MMRAcquisitionAuctionCleanPrice)
hist(kickDataset$MMRAcquisitionAuctionCleanPrice)

kickDataset$MMRAcquisitionAuctionCleanPrice = ifelse( is.na(kickDataset$MMRAcquisitionAuctionCleanPrice), 
                                                        median(kickDataset$MMRAcquisitionAuctionCleanPrice, na.rm = T), 
                                                        kickDataset$MMRAcquisitionAuctionCleanPrice)

# MMRAcquisitionRetailAveragePrice

summary(kickDataset$MMRAcquisitionRetailAveragePrice)
hist(kickDataset$MMRAcquisitionRetailAveragePrice)

kickDataset$MMRAcquisitionRetailAveragePrice = ifelse( is.na(kickDataset$MMRAcquisitionRetailAveragePrice), 
                                                      median(kickDataset$MMRAcquisitionRetailAveragePrice, na.rm = T), 
                                                      kickDataset$MMRAcquisitionRetailAveragePrice)

# Checking missing values
checkingMissingValue(kickDataset)

# MMRAcquisitonRetailCleanPrice
summary(kickDataset$MMRAcquisitonRetailCleanPrice)
hist(kickDataset$MMRAcquisitonRetailCleanPrice)

kickDataset$MMRAcquisitonRetailCleanPrice = ifelse( is.na(kickDataset$MMRAcquisitonRetailCleanPrice), 
                                                       median(kickDataset$MMRAcquisitonRetailCleanPrice, na.rm = T), 
                                                       kickDataset$MMRAcquisitonRetailCleanPrice)

# MMRCurrentAuctionAveragePrice 
summary(kickDataset$MMRCurrentAuctionAveragePrice)
hist(kickDataset$MMRCurrentAuctionAveragePrice)

kickDataset$MMRCurrentAuctionAveragePrice = ifelse( is.na(kickDataset$MMRCurrentAuctionAveragePrice), 
                                                    median(kickDataset$MMRCurrentAuctionAveragePrice, na.rm = T), 
                                                    kickDataset$MMRCurrentAuctionAveragePrice)

# MMRCurrentAuctionCleanPrice 
summary(kickDataset$MMRCurrentAuctionCleanPrice)
hist(kickDataset$MMRCurrentAuctionCleanPrice)

kickDataset$MMRCurrentAuctionCleanPrice = ifelse( is.na(kickDataset$MMRCurrentAuctionCleanPrice), 
                                                    median(kickDataset$MMRCurrentAuctionCleanPrice, na.rm = T), 
                                                    kickDataset$MMRCurrentAuctionCleanPrice)

checkingMissingValue(kickDataset)

# MMRCurrentRetailAveragePrice
summary(kickDataset$MMRCurrentRetailAveragePrice)
hist(kickDataset$MMRCurrentRetailAveragePrice)

kickDataset$MMRCurrentRetailAveragePrice = ifelse( is.na(kickDataset$MMRCurrentRetailAveragePrice), 
                                                  median(kickDataset$MMRCurrentRetailAveragePrice, na.rm = T), 
                                                  kickDataset$MMRCurrentRetailAveragePrice)

# MMRCurrentRetailCleanPrice 
summary(kickDataset$MMRCurrentRetailCleanPrice)
hist(kickDataset$MMRCurrentRetailCleanPrice)

kickDataset$MMRCurrentRetailCleanPrice = ifelse( is.na(kickDataset$MMRCurrentRetailCleanPrice), 
                                                   median(kickDataset$MMRCurrentRetailCleanPrice, na.rm = T), 
                                                   kickDataset$MMRCurrentRetailCleanPrice)

# All missing values have been imputed with correct data
checkingMissingValue(kickDataset)

write.csv(kickDataset, 'kickDataset_1_NoMissingValueDs.csv', row.names = TRUE)

# ---------------------------------------------------------------------------------------------------- STAGE 2 - To check data gaussian-ness / data distribution
# ---------------------------------------------------------------------------------------------------- STAGE 2 - To find normality issues

kickDataset_1_NoMissingValueDs = read.csv('kickDataset_1_NoMissingValueDs.csv', header = T, stringsAsFactors = FALSE)

# To select continuous variable only - to check data distribution, if outliers are existed in the dataset
summary(kickDataset_1_NoMissingValueDs)
str(kickDataset_1_NoMissingValueDs) 
kickDataset_1_NoMissingValueDs <- kickDataset_1_NoMissingValueDs[, -1]

# Checking normality issues and data gaussian------------------------------------------------------A. VehicleAge
summary(kickDataset_1_NoMissingValueDs$VehicleAge) # average seems normal with close range.
describe(kickDataset_1_NoMissingValueDs$VehicleAge) # skew and kurtosis are within range
hist(kickDataset_1_NoMissingValueDs$VehicleAge, col = "lightblue", main = "Histogram", xlab = "Vehicle Age", prob = TRUE)

# Checking normality issues and data gaussian------------------------------------------------------B. VehOdo
summary(kickDataset_1_NoMissingValueDs$VehOdo)
describe(kickDataset_1_NoMissingValueDs$VehOdo)
hist(kickDataset_1_NoMissingValueDs$VehOdo, col = "lightblue", main = "Histogram", xlab = "Vehicle Odometer", prob = TRUE)
lines(density(kickDataset_1_NoMissingValueDs$VehOdo)) # Right skewed

# Checking normality issues and data gaussian------------------------------------------------------C. VehYear
summary(kickDataset_1_NoMissingValueDs$VehYear)
describe(kickDataset_1_NoMissingValueDs$VehYear)
hist(kickDataset_1_NoMissingValueDs$VehYear, col = "lightblue", main = "Histogram", xlab = "Vehicle Year", prob = TRUE) # Normal distribution

var_Outliers <- melt(kickDataset_1_NoMissingValueDs) # To select outliers
p <- ggplot(var_Outliers, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

# Checking normality issues and data gaussian------------------------------------------------------D. MMRAcquistionAuctionAveragePrice
summary(kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionAveragePrice) # Huge range from 0 to 35722 while median is only 6097
describe(kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionAveragePrice) # though it is in +- 2 skewness and kurtosis, but it still have large range from min and max value (outliers)
# skewness and kurtosis of 0.46 and 1.59 respectively.
ggplot(kickDataset_1_NoMissingValueDs, aes(sample = MMRAcquisitionAuctionAveragePrice, colour = "purple")) + 
  labs(title="MMRAcquisitionAuctionAveragePrice")+
  stat_qq() +
  stat_qq_line()

# Box plot
ggplot(kickDataset_1_NoMissingValueDs, aes(x = "", y = MMRAcquisitionAuctionAveragePrice)) +  # Verified through boxplot, it has outliers, therefore
geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=1.5, notch=FALSE)

# To put value within 5% and 95% interquantile range by using squish function
quantileRange_MMRAcquisitionAuctionAveragePrice <- round( quantile(kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionAveragePrice, probs = c(0.05, 0.95)) )
kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionAveragePrice <- squish( kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionAveragePrice, quantileRange_MMRAcquisitionAuctionAveragePrice)

#Result, code has been rerun again, and the result is fine, no more outlier in the dataset.
boxplot(kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionAveragePrice)

# Checking normality issues and data gaussian------------------------------------------------------E. MMRAcquisitionAuctionCleanPrice
summary(kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionCleanPrice) # Huge range from 0 (min) to 36859 (max)
describe(kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionCleanPrice)
ggplot(kickDataset_1_NoMissingValueDs, aes(sample = MMRAcquisitionAuctionCleanPrice, colour = "purple")) + 
  labs(title="MMRAcquisitionAuctionAveragePrice")+
  stat_qq() +
  stat_qq_line()

#May assume it still have outliers due to the huge range from the minimum and maximum value. and more importantly, at the end of qqplot, it shown data which are on shallow area.
# Box plot
ggplot(kickDataset_1_NoMissingValueDs, aes(x = "", y = MMRAcquisitionAuctionCleanPrice)) +  #Verified through boxplot, therefore we need to treat such outliers by squishing data that exceeds 0.95 interquartile range back to 0.95 range.
geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=1.5, notch=FALSE)

MMRAcquisitionAuctionCleanPrice_quantileRange <- round( quantile(kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionCleanPrice, probs = c(0.05, 0.95)) )
kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionCleanPrice <- squish( kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionCleanPrice, MMRAcquisitionAuctionCleanPrice_quantileRange)

# Result has shown nicely based on the output.
boxplot(kickDataset_1_NoMissingValueDs$MMRAcquisitionAuctionCleanPrice) 

# Checking normality issues and data gaussian------------------------------------------------------F. MMRAcquisitionRetailAveragePrice

summary(kickDataset_1_NoMissingValueDs$MMRAcquisitionRetailAveragePrice) # Huge range from 0 (min) to 39080 (max)
describe(kickDataset_1_NoMissingValueDs$MMRAcquisitionRetailAveragePrice) # though skewness and kurtosis is in acceptable range(+- 2), however, it may still have outliers based on the huge range of minimum value and maximum value
# tail of result on qqplot, also shown it may have unreliable data on the dataset.
ggplot(kickDataset_1_NoMissingValueDs, aes(sample = MMRAcquisitionRetailAveragePrice, colour = "purple")) + 
  labs(title="MMRAcquisitionRetailAveragePrice")+
  stat_qq() +
  stat_qq_line()

# Therefore, boxplot to check if data have outliers
# Box plot
ggplot(kickDataset_1_NoMissingValueDs, aes(x = "", y = MMRAcquisitionRetailAveragePrice)) +  # Data appeared to have outliers, therefore, we may need to treat such outliers by squishing data into 0.05 and 0.95 interquartile range.
  geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=1.5, notch=FALSE)

MMRAcquisitionRetailAveragePrice_quantileRange <- round( quantile(kickDataset_1_NoMissingValueDs$MMRAcquisitionRetailAveragePrice, probs = c(0.05, 0.95)) )
kickDataset_1_NoMissingValueDs$MMRAcquisitionRetailAveragePrice <- squish( kickDataset_1_NoMissingValueDs$MMRAcquisitionRetailAveragePrice, MMRAcquisitionRetailAveragePrice_quantileRange)

# Result has shown nicely, skewness has been much more closely to 0, which ideally is 0, though acceptable range is in (+- 2)
boxplot(kickDataset_1_NoMissingValueDs$MMRAcquisitionRetailAveragePrice) 

# Checking normality issues and data gaussian------------------------------------------------------G. MMRAcquisitonRetailCleanPrice

summary(kickDataset_1_NoMissingValueDs$MMRAcquisitonRetailCleanPrice) # Huge range from 0 (min) to 41482 (max)
describe(kickDataset_1_NoMissingValueDs$MMRAcquisitonRetailCleanPrice) # though skewness and kurtosis is in acceptable range(+- 2), however, it may still have outliers due to the huge range of minimum value and maximum value
# tail of result on qqplot, also shown it may have unreliable data on the dataset.
ggplot(kickDataset_1_NoMissingValueDs, aes(sample = MMRAcquisitonRetailCleanPrice, colour = "purple")) + 
  labs(title="MMRAcquisitonRetailCleanPrice")+
  stat_qq() +
  stat_qq_line()

# Therefore, to validate if this variable has shown outliers
# Box plot
ggplot(kickDataset_1_NoMissingValueDs, aes(x = "", y = MMRAcquisitonRetailCleanPrice)) +  # Data appeared to have outliers, 
geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=1.5, notch=FALSE)

# Therefore, we may need to treat such outliers by squishing data into 0.05 and 0.95 interquartile range.
MMRAcquisitonRetailCleanPrice_quantileRange <- round( quantile(kickDataset_1_NoMissingValueDs$MMRAcquisitonRetailCleanPrice, probs = c(0.05, 0.95)) )
kickDataset_1_NoMissingValueDs$MMRAcquisitonRetailCleanPrice <- squish( kickDataset_1_NoMissingValueDs$MMRAcquisitonRetailCleanPrice, MMRAcquisitonRetailCleanPrice_quantileRange)

# Result has shown nicely, and all the outliers have been treated. Thought it does not change the skewness, but the kurtosis has through 0.01 closely to 0. which is a good sign. and the result is acceptable.
boxplot(kickDataset_1_NoMissingValueDs$MMRAcquisitonRetailCleanPrice) 

# Checking normality issues and data gaussian------------------------------------------------------H. MMRCurrentAuctionAveragePrice

summary(kickDataset_1_NoMissingValueDs$MMRCurrentAuctionAveragePrice) # same goes with this variable, it appears to have huge range of minimum value and maximum value from 0 to 35722, while the median is at range of 6062.
describe(kickDataset_1_NoMissingValueDs$MMRCurrentAuctionAveragePrice) # though the skewness is in acceptable range, but we cannot ignore possible outliers due to the huge range of minimum and maximum values
#Therefore, we plot it with qqnorm to check data normality issue
# Outliers appeared at the lower tail of qqplot
ggplot(kickDataset_1_NoMissingValueDs, aes(sample = MMRCurrentAuctionAveragePrice, colour = "purple")) + 
  labs(title="MMRCurrentAuctionAveragePrice")+
  stat_qq() +
  stat_qq_line()

# Box plot
ggplot(kickDataset_1_NoMissingValueDs, aes(x = "", y = MMRCurrentAuctionAveragePrice)) +  # Possible outliers can be spotted through boxplot
  geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=1.5, notch=FALSE)

# it appeared outliers, therefore we treat it by squishing data into 0.05 and 0.95 interquartile range by using function squish
MMRCurrentAuctionAveragePrice_quantileRange <- round( quantile(kickDataset_1_NoMissingValueDs$MMRCurrentAuctionAveragePrice, probs = c(0.05, 0.95)) )
kickDataset_1_NoMissingValueDs$MMRCurrentAuctionAveragePrice <- squish( kickDataset_1_NoMissingValueDs$MMRCurrentAuctionAveragePrice, MMRCurrentAuctionAveragePrice_quantileRange)

# Result has shown nicely as skewness and kurtosis and drag closely to 0, which ideal result is 0. though, outliers have been treated perfectly based on the result findings.
boxplot(kickDataset_1_NoMissingValueDs$MMRCurrentAuctionAveragePrice, xlab = "MMRCurrentAuctionAveragePrice")

# Checking normality issues and data gaussian------------------------------------------------------I. MMRCurrentAuctionCleanPrice

summary(kickDataset_1_NoMissingValueDs$MMRCurrentAuctionCleanPrice) # This variable has a huge range from minimum value to the maximum value of 0 to 36989, it may appear outliers to the dataset.
describe(kickDataset_1_NoMissingValueDs$MMRCurrentAuctionCleanPrice) # We also check the skewness and kurtosis of the data, though it appear within acceptable range of (+- 2)
# We check the normality issue with qqnorm, to further verify if the skewness and kurtosis is correct.
ggplot(kickDataset_1_NoMissingValueDs, aes(sample = MMRCurrentAuctionCleanPrice, colour = "purple")) + 
  labs(title="MMRCurrentAuctionCleanPrice")+
  stat_qq() +
  stat_qq_line()

# Box plot
ggplot(kickDataset_1_NoMissingValueDs, aes(x = "", y = MMRCurrentAuctionCleanPrice)) +  # Possible outliers can be spotted through boxplot
  geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=1.5, notch=FALSE)

# Due to the result in qqplot and boxplot, we can confirm that data has outliers. So we can begin our outliers treatment by squishing data not within 0.95 will be squishing into the dataset, resulting more gaussian result.
MMRCurrentAuctionCleanPrice_quantileRange <- round( quantile(kickDataset_1_NoMissingValueDs$MMRCurrentAuctionCleanPrice, probs = c(0.05, 0.95)) )
kickDataset_1_NoMissingValueDs$MMRCurrentAuctionCleanPrice <- squish( kickDataset_1_NoMissingValueDs$MMRCurrentAuctionCleanPrice, MMRCurrentAuctionCleanPrice_quantileRange)

# Result has shown nicely to the dataset. Skewness and kurtosis have dragged closely to 0 and it appeared to have more smooth graph in the qqplot, and more importantly, it has no more outliers in the dataset. Therefore, we can say it has more realiable dataset.
boxplot(kickDataset_1_NoMissingValueDs$MMRCurrentAuctionCleanPrice, xlab = "MMRCurrentAuctionCleanPrice")

# Checking normality issues and data gaussian------------------------------------------------------J. MMRCurrentRetailAveragePrice
summary(kickDataset_1_NoMissingValueDs$MMRCurrentRetailAveragePrice) # Again, huge range of minimum value and maximum value from 0 to 39080. This data may have outliers that affecting the outcomes if we bring this directly to the analysis stage. 
describe(kickDataset_1_NoMissingValueDs$MMRCurrentRetailAveragePrice) # Therefore, we check normality and gaussian ness of the data. Though it appeared, it has an acceptable range of skewness and kurtosis data, we cannot ignore possible outliers in the data.
# Result from qq plot and box plot have clearly verified that data have a huge outliers in the dataset, and it shouldnt be brought directly to the analysis without doing outliers treatement.
ggplot(kickDataset_1_NoMissingValueDs, aes(sample = MMRCurrentRetailAveragePrice, colour = "purple")) + 
  labs(title="MMRCurrentRetailAveragePrice")+
  stat_qq() +
  stat_qq_line()

# Box plot
ggplot(kickDataset_1_NoMissingValueDs, aes(x = "", y = MMRCurrentRetailAveragePrice)) +  # Possible outliers can be spotted through boxplot
  geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=1.5, notch=FALSE)

# We treat the data by squishing the dataset into 0.05 and 0.95, therefore, the data outside the 0.95 (which is an outliers), will be ignored and reproduce a more reliable input for the analysis.
MMRCurrentRetailAveragePrice_quantileRange <- round( quantile(kickDataset_1_NoMissingValueDs$MMRCurrentRetailAveragePrice, probs = c(0.05, 0.95)) )
kickDataset_1_NoMissingValueDs$MMRCurrentRetailAveragePrice <- squish( kickDataset_1_NoMissingValueDs$MMRCurrentRetailAveragePrice, MMRCurrentRetailAveragePrice_quantileRange)

boxplot(kickDataset_1_NoMissingValueDs$MMRCurrentRetailAveragePrice)

# Though before the treatment, the skewness and kurtosis seem to be in acceptable outcome, but after the treatment, it has further closely to the 0, as ideally, skewness and kurtosis should be in 0.
# More importantly, it appeared no outlier in the boxplot and look fine in the qq plot.

# Checking normality issues and data gaussian------------------------------------------------------K. MMRCurrentRetailCleanPrice
summary(kickDataset_1_NoMissingValueDs$MMRCurrentRetailCleanPrice) # Though, the range of 3rd quartile and maximum value is from 12308 to 41062, but we cannot ignore the huge range of minimum value and maximum value of 0 to 41062.
describe(kickDataset_1_NoMissingValueDs$MMRCurrentRetailCleanPrice) # Therefore, it may appear to have data normality issue or outliers in the dataset. based on result of describe table, the skewness and kurtosis seem to at the acceptable range of 0.2 and 0.86, which ideally is in +- 2
# We again check the data normality issue if the data has appeared outliers at the lower end. yes it do, we verify it by using boxplot to check possible outliers in the dataset.
ggplot(kickDataset_1_NoMissingValueDs, aes(sample = MMRCurrentRetailCleanPrice, colour = "purple")) + 
  labs(title="MMRCurrentRetailCleanPrice")+
  stat_qq() +
  stat_qq_line()

# Box plot
ggplot(kickDataset_1_NoMissingValueDs, aes(x = "", y = MMRCurrentRetailCleanPrice)) +  # Possible outliers can be spotted through boxplot
  geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=1.5, notch=FALSE)

# Therefore, we treat the outliers by squishing data that exceeds 0.95, and bring back to the range of 0.95, resulting more realiable result for later analysis.
MMRCurrentRetailCleanPrice_quantileRange <- round( quantile(kickDataset_1_NoMissingValueDs$MMRCurrentRetailCleanPrice, probs = c(0.05, 0.95)) )
kickDataset_1_NoMissingValueDs$MMRCurrentRetailCleanPrice <- squish( kickDataset_1_NoMissingValueDs$MMRCurrentRetailCleanPrice, MMRCurrentRetailCleanPrice_quantileRange)

boxplot(kickDataset_1_NoMissingValueDs$MMRCurrentRetailCleanPrice, xlab = "MMRCurrentRetailCleanPrice") # Data appeared to have huge variance of outliers in the dataset, we may assume it causing the huge range of minimum and maximum values to the dataset.
# Though before outliers treatment, the result is in accepatable range of +-2, which ideally is 0. After outliers treatment, the kurtosis has appeared to be more closely to 0, from (-0.92) to (-0.87), despite not a big change to the data normality, but it has increased quite well without affecting the dataset.
# More importantly, it has shown no outlier from the boxplot and the qqplot has performing quite well.

# Checking normality issues and data gaussian------------------------------------------------------L. VehBCost
summary(kickDataset_1_NoMissingValueDs$VehBCost) # Vehicle BCost has appeared to have a huge range from minimum value of 1 to maximum value of 45469, which 3rd quartile range is at 7900. which meaning, the maximum value has been very far from the data that is in first quartile to 3rd quartile.
describe(kickDataset_1_NoMissingValueDs$VehBCost) # Therefore, we use describe function to verify if data appeared data normality issue. though the skewness of 0.7 is in acceptable range of (+-2), however, the kurtosis does not closely to the acceptable range of (+-2), therefore, we may assume it has possible outliers and severe issue at the kurtosis.
# We then plot the variable by using qqplot. Result has shown, outliers have been seen at the lower end of the graph from value (10000 to 40000), which is not a good sign. therefore, we can further verified it has outliers.
ggplot(kickDataset_1_NoMissingValueDs, aes(sample = VehBCost, colour = "purple")) + 
  labs(title="VehBCost")+
  stat_qq() +
  stat_qq_line()

# Box plot
ggplot(kickDataset_1_NoMissingValueDs, aes(x = "", y = VehBCost)) +  # boxplot again agreed to the qqplot result.
  geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=1.5, notch=FALSE)

# Therefore, we treat the outliers that affecting the data normality by squishing the huge variance from the dataset into 0.05 and 0.95 interquartile range.
VehBCost_quantileRange <- round( quantile(kickDataset_1_NoMissingValueDs$VehBCost, probs = c(0.05, 0.95)) )
kickDataset_1_NoMissingValueDs$VehBCost <- squish( kickDataset_1_NoMissingValueDs$VehBCost, VehBCost_quantileRange)

boxplot(kickDataset_1_NoMissingValueDs$VehBCost, xlab = "VehBCost") 
# Result has been performing so well, as the skewness and kurtosis both are at acceptable range of (+- 2).
# More importantly, it appears to have no more outliers in the dataset and the outliers that appeared in the lower end before treatment, has been shown nicely in the qqplot (after outliers treatment)

# Checking normality issues and data gaussian------------------------------------------------------M. WarrantyCost
summary(kickDataset_1_NoMissingValueDs$WarrantyCost) # Though the maximum and minimum values are not too far away from each other from 462 to 7498. However, maximum value appeared to have a huge range from 1155 to 7498, which means, data may have heavily skewed to the wrong direction, this may also verify it has possible outliers in the dataset.
describe(kickDataset_1_NoMissingValueDs$WarrantyCost) # We further verify the result, by showing skewness and kurtosis of the findings, where it result, both skewness and kurtosis are not appeared to at the acceptable range of (+- 2)

# The skewness and kurtosis have not performing so well as it does not fit well to the acceptable line drawn on the qqplot.
ggplot(kickDataset_1_NoMissingValueDs, aes(sample = WarrantyCost, colour = "purple")) + 
  labs(title="WarrantyCost")+
  stat_qq() +
  stat_qq_line()

# Box plot
ggplot(kickDataset_1_NoMissingValueDs, aes(x = "", y = WarrantyCost)) +  # Based on the boxplot, it appeared to have huge variance of outliers in the dataset.
  geom_boxplot(outlier.colour="red", 
               outlier.shape=16,
               outlier.size=1.5, notch=FALSE)

#In order to treat such outliers we, squish the data that exceed 0.95 interquartile range back into 0.95 and data that lower than 0.05, we put it closely to 0.05 interquartile range.
WarrantyCost_quantileRange <- round( quantile(kickDataset_1_NoMissingValueDs$WarrantyCost, probs = c(0.05, 0.95)) )
kickDataset_1_NoMissingValueDs$WarrantyCost <- squish( kickDataset_1_NoMissingValueDs$WarrantyCost, WarrantyCost_quantileRange)

boxplot(kickDataset_1_NoMissingValueDs$WarrantyCost, xlab = "WarrantyCost") 

# After the adjustment, the result from the skewness and kurtosis fell back into +- 2, which is at acceptable range. and more importantly, the 3rd quartile range and maximum value of 1623 to 2198 has not been too far from each other. Meaning, data has been adjusted closely to each other.
# More importantly, the data normality from qq plot has shown perfectly well compared to the qqplot before making any adjustment. and also, the box plot appeared to have no more outliers to the dataset.

kickDataset_1_NoMissingValueDs_tmp <- select(kickDataset_1_NoMissingValueDs, c(VehYear, MMRAcquisitionAuctionAveragePrice, MMRAcquisitionAuctionCleanPrice, 
                                                              MMRAcquisitionRetailAveragePrice, MMRAcquisitonRetailCleanPrice, MMRCurrentAuctionAveragePrice,
                                                              MMRCurrentAuctionCleanPrice, MMRCurrentRetailAveragePrice, MMRCurrentRetailCleanPrice, 
                                                              VehBCost, WarrantyCost) )

# Check outlier again
var_Outliers <- melt(kickDataset_1_NoMissingValueDs_tmp) # To select outliers
p <- ggplot(var_Outliers, aes(factor(variable), value)) 
p + geom_boxplot() + facet_wrap(~variable, scale="free")

#Saving into csv
write.csv(kickDataset_1_NoMissingValueDs, 'kickDataset_2_Normality_Checked.csv', row.names = TRUE)

setwd("D:/Master Semester 2/Sem 2")
kickDataset_2_Normality_Checked = read.csv('kickDataset_2_Normality_Checked.csv', header = T, stringsAsFactors = FALSE)

str(kickDataset_2_Normality_Checked)
kickDataset_2_Normality_Checked <- kickDataset_2_Normality_Checked[, -1] # Remove id
kickDs <- kickDataset_2_Normality_Checked

#------------------------------------------------------------------------------------------------------- Performing EDA
# Check correlation, Gaussian ness
checkingMissingValue(kickDs)

library(dplyr)
# higher price lead to higher percentage in bad buy
f_df <- filter(kickDs, IsBadBuy == 0)
table(f_df$Transmission)
f_df2 <- filter(kickDs, IsBadBuy == 1)
table(f_df2$Transmission)


# Odometer plotting
ggplot(data = kickDs, aes(kickDs$VehOdo)) + 
  geom_histogram(aes(y =..density..), fill = "purple") +
  geom_density()

var_Odo_hist = hist(kickDs$VehOdo, breaks = 100, plot = F)
var_Odo_color = ifelse(var_Odo_hist$breaks <= 75000, rgb(0.2,0.8,0.5,0.5) , ifelse (var_Odo_hist$breaks >=75000, "purple", rgb(0.2,0.2,0.2,0.2) ))
plot(var_Odo_hist, col = var_Odo_color, border = F, main = "Histogram for odometer", xlab = "Odometer", xlim = c(4000, 120000))

# Vehicle Age
var_VehicleAge_hist = hist(kickDs$VehicleAge, breaks = 30, plot = F)
plot(var_VehicleAge_hist, col = rgb(0.2,0.8,0.5,0.5), border = F, main = "Histogram for Vehicle Age", xlab = "Vehicle Age", xlim = c(0, 8))

# Warranty Cost
var_WarrantyCost_hist = hist(kickDs$WarrantyCost, breaks = 30, plot = F)
plot(var_WarrantyCost_hist, col = rgb(0.2,0.7,0.5,0.5), border = F, main = "Histogram for Warranty Cost", xlab = "Warranty Cost", xlim = c(500, 2200))

str(kickDs)

kickDs$IsBadBuy <- factor(kickDs$IsBadBuy)
kickDs$Auction <- factor(kickDs$Auction)
kickDs$Make <- factor(kickDs$Make)
kickDs$Model <- factor(kickDs$Model)
kickDs$Trim <- factor(kickDs$Trim)
kickDs$SubModel <- factor(kickDs$SubModel)
kickDs$Color <- factor(kickDs$Color)
kickDs$Transmission <- factor(kickDs$Transmission)
kickDs$WheelType <- factor(kickDs$WheelType)
kickDs$Nationality <- factor(kickDs$Nationality)
kickDs$Size <- factor(kickDs$Size)
kickDs$TopThreeAmericanName <- factor(kickDs$TopThreeAmericanName)
kickDs$VNST <- factor(kickDs$VNST)
kickDs$IsOnlineSale <- factor(kickDs$IsOnlineSale)

plot_correlation(kickDataset_2_Normality_Checked, type = 'continuous')

library(corrplot)
# Generating correlation matrix
cor_kickDs <- cor(kickDataset_2_Normality_Checked[, c("WarrantyCost", "VehYear", "VehBCost", 
                                                      "VehicleAge", "VehOdo", "BYRNO", "VNZIP1", 
                                                      "MMRAcquisitionAuctionAveragePrice", "MMRAcquisitionAuctionCleanPrice",
                                                      "MMRAcquisitionRetailAveragePrice", "MMRAcquisitonRetailCleanPrice",
                                                      "MMRCurrentAuctionAveragePrice", "MMRCurrentAuctionCleanPrice",
                                                      "MMRCurrentRetailAveragePrice", "MMRCurrentRetailCleanPrice")])


require(ggplot2)
require(plyr)    

# Graph 1
ggplot(data=kickDataset,aes(x=as.factor(Color),fill=IsBadBuy)) + 
  geom_bar(data=subset(kickDataset,IsBadBuy==0)) + 
  geom_bar(data=subset(kickDataset,IsBadBuy==1),aes(y=..count..*(-1))) + 
  coord_flip()

# Graph 2
kd_VehOdometer <- aggregate(kickDataset$VehOdo, by=list(kickDataset$Make), FUN=mean)  # aggregate
colnames(kd_VehOdometer) <- c("make", "VehOdo")  # change column names
kd_VehOdometer <- kd_VehOdometer[order(kd_VehOdometer$VehOdo), ]  # sort
kd_VehOdometer$make <- factor(kd_VehOdometer$make, levels = kd_VehOdometer$make)  # to retain the order in plot.
head(kd_VehOdometer, 4)

ggplot(kd_VehOdometer, aes(x=make, y=VehOdo)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=make, 
                   xend=make, 
                   y=min(VehOdo), 
                   yend=max(VehOdo)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip()

# Graph 3
library(ggplot2)
theme_set(theme_classic())

# Histogram on a Categorical variable
g <- ggplot(kickDataset, aes(Make))
g + geom_bar(aes(fill=IsBadBuy), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Make Vs IsBadBuy", 
       subtitle="Make across IsBadBuy Classes") 

#Graph 4
ggplot(data=kickDataset,aes(x=as.factor(VehicleAge),fill=IsBadBuy)) + 
  geom_bar(data=subset(kickDataset,IsBadBuy==0)) + 
  geom_bar(data=subset(kickDataset,IsBadBuy==1),aes(y=..count..*(-1))) + 
  coord_flip()

# Graph 5
library(ggplot2)
ggplot(data = kickDataset, 
       mapping = aes(x = Make, y = ifelse(test = IsBadBuy == 1, yes = -VehBCost, no = VehBCost), 
                     fill = IsBadBuy)) +
  geom_col() +
  coord_flip() +
  labs(title="Make on Vehicle Acquisition Cost by IsBadBuy",
       y = "Vehicle Acquisition Cost")

#Graph 6
kd_VehBCost <- aggregate(kickDataset$VehBCost, by=list(kickDataset$Make), FUN=mean)  # aggregate
colnames(kd_VehBCost) <- c("make", "Avg_VehBCost")  # change column names
kd_VehBCost <- kd_VehBCost[order(kd_VehBCost$VehBCost), ]  # sort
kd_VehBCost$make <- factor(kd_VehBCost$make, levels = kd_VehBCost$make)  # to retain the order in plot.
head(kd_VehBCost, 4)

library(ggthemes)
library(ggplot2)
theme_set(theme_bw())

# Draw plot
ggplot(kd_VehOdometer, aes(x=make, y=VehBCost)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Make vs Avg. VehBCost Bar Chart", 
       subtitle="Make Vs Avg. VehBCost") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#------------------------------------------------------------------------------------------------------- Feature Engineering

#Saving into csv
write.csv(kickDs, 'kickDataset_3_GaussianNessChecked.csv', row.names = TRUE)

setwd("D:/Master Semester 2/Sem 2")
kickDataset_3_GaussianNessChecked = read.csv('kickDataset_3_GaussianNessChecked.csv', header = T, stringsAsFactors = FALSE)

str(kickDataset_3_GaussianNessChecked)
kickDataset_3_GaussianNessChecked <- kickDataset_3_GaussianNessChecked[, -1] # Remove id

kickDataset_Preprocessed <- kickDataset_3_GaussianNessChecked # Rename variable
str(kickDataset_Preprocessed)

str(kickDataset_Preprocessed)
kickDataset_Preprocessed$IsBadBuy <- factor(kickDataset_Preprocessed$IsBadBuy)
kickDataset_Preprocessed$Auction <- factor(kickDataset_Preprocessed$Auction)
kickDataset_Preprocessed$Make <- factor(kickDataset_Preprocessed$Make)
kickDataset_Preprocessed$Model <- factor(kickDataset_Preprocessed$Model)
kickDataset_Preprocessed$Trim <- factor(kickDataset_Preprocessed$Trim)
kickDataset_Preprocessed$SubModel <- factor(kickDataset_Preprocessed$SubModel)
kickDataset_Preprocessed$Color <- factor(kickDataset_Preprocessed$Color)
kickDataset_Preprocessed$Transmission <- factor(kickDataset_Preprocessed$Transmission)
kickDataset_Preprocessed$WheelType <- factor(kickDataset_Preprocessed$WheelType)
kickDataset_Preprocessed$Nationality <- factor(kickDataset_Preprocessed$Nationality)
kickDataset_Preprocessed$Size <- factor(kickDataset_Preprocessed$Size)
kickDataset_Preprocessed$TopThreeAmericanName <- factor(kickDataset_Preprocessed$TopThreeAmericanName)
kickDataset_Preprocessed$VNST <- factor(kickDataset_Preprocessed$VNST)
kickDataset_Preprocessed$IsOnlineSale <- factor(kickDataset_Preprocessed$IsOnlineSale)

Modes <- function(x) { # Getting highest frequency from x as input
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

# Realizing sub model have 863 factor, so we make combine the model. from SubModel to SubModel type
table(kickDataset_Preprocessed$SubModel)

kickDataset_Preprocessed$SubModel <- as.character(kickDataset_Preprocessed$SubModel)

Modes(kickDataset_Preprocessed$SubModel) # Realizing majority data is sedan, so we make a group of sedan
kickDataset_Preprocessed$SubModel_Type <- "SEDAN"
kickDataset_Preprocessed$SubModel_Type[grep("CAB", kickDataset_Preprocessed$SubModel)] = 'CAB'
kickDataset_Preprocessed$SubModel_Type[grep("SUV", kickDataset_Preprocessed$SubModel)] = 'SUV'
kickDataset_Preprocessed$SubModel_Type[grep("WAGON", kickDataset_Preprocessed$SubModel)] = 'WAGON'
kickDataset_Preprocessed$SubModel_Type[grep("MINIVAN", kickDataset_Preprocessed$SubModel)] = 'MINIVAN'
kickDataset_Preprocessed$SubModel_Type[grep("CONVERTIBLE", kickDataset_Preprocessed$SubModel)] = 'CONVERTIBLE'
kickDataset_Preprocessed$SubModel_Type[grep("COUPE", kickDataset_Preprocessed$SubModel)] = 'COUPE'
kickDataset_Preprocessed$SubModel_Type[grep("SPORT", kickDataset_Preprocessed$SubModel)] = 'SPORT'
kickDataset_Preprocessed$SubModel_Type[grep("CUV", kickDataset_Preprocessed$SubModel)] = 'CUV'
kickDataset_Preprocessed$SubModel_Type[grep("PASSENGER", kickDataset_Preprocessed$SubModel)] = 'PASSENGER'
kickDataset_Preprocessed$SubModel_Type[grep("HATCHBACK", kickDataset_Preprocessed$SubModel)] = 'HATCHBACK'
kickDataset_Preprocessed$SubModel_Type[grep("UTILITY", kickDataset_Preprocessed$SubModel)] = 'UTILITY'
kickDataset_Preprocessed$SubModel_Type[grep("CROSSOVER", kickDataset_Preprocessed$SubModel)] = 'CROSSOVER'
kickDataset_Preprocessed$SubModel_Type[grep("SPYDER", kickDataset_Preprocessed$SubModel)] = 'SPYDER'
kickDataset_Preprocessed$SubModel_Type[grep("ROADSTER", kickDataset_Preprocessed$SubModel)] = 'ROADSTER'
kickDataset_Preprocessed$SubModel_Type[grep("HATCKBACK", kickDataset_Preprocessed$SubModel)] = 'HATCKBACK'
kickDataset_Preprocessed$SubModel_Type[grep("MAZDA", kickDataset_Preprocessed$SubModel)] = 'MAZDA'
kickDataset_Preprocessed$SubModel_Type[grep("CARGO", kickDataset_Preprocessed$SubModel)] = 'CARGO'
kickDataset_Preprocessed$SubModel_Type[grep("HARDTOP", kickDataset_Preprocessed$SubModel)] = 'HARDTOP'
kickDataset_Preprocessed$SubModel_Type[grep("LIFTBACK", kickDataset_Preprocessed$SubModel)] = 'LIFTBACK'
kickDataset_Preprocessed$SubModel_Type[grep("BASE", kickDataset_Preprocessed$SubModel)] = 'BASE'
kickDataset_Preprocessed$SubModel_Type[grep("JEEP", kickDataset_Preprocessed$SubModel)] = 'JEEP'

table(kickDataset_Preprocessed$SubModel_Type)

# To ensure all sub model are placed into one group.
temp_SubModel = kickDataset_Preprocessed
temp_SubModel$SubModel[grep("SEDAN", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("CAB", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("SUV", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("WAGON", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("MINIVAN", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("CONVERTIBLE", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("COUPE", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("SPORT", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("CUV", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("PASSENGER", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("HATCHBACK", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("UTILITY", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("CROSSOVER", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("SPYDER", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("ROADSTER", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("CROSSOVER", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("HATCKBACK", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("MAZDA", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("CARGO", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("HARDTOP", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("LIFTBACK", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("BASE", temp_SubModel$SubModel)] = 'NA'
temp_SubModel$SubModel[grep("JEEP", temp_SubModel$SubModel)] = 'NA'

temp_SubModel$SubModel
table(temp_SubModel$SubModel)

# Put all sub model type with less than 500 into 'Others'
table(kickDataset_Preprocessed$SubModel_Type)
kickDataset_Preprocessed$SubModel_Type[grep("BASE", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("CARGO", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("CONVERTIBLE", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("CROSSOVER", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("HARDTOP", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("HATCHBACK", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("HATCKBACK", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("JEEP", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("LIFTBACK", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("MAZDA", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("ROADSTER", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'
kickDataset_Preprocessed$SubModel_Type[grep("SPYDER", kickDataset_Preprocessed$SubModel_Type)] = 'OTHER'

kickDataset_Preprocessed$SubModel_Type <- factor(kickDataset_Preprocessed$SubModel_Type)
kickDataset_Preprocessed$SubModel <- factor(kickDataset_Preprocessed$SubModel)

# . from SubModel to Sub Model Door
kickDataset_Preprocessed$SubModel <- as.character(kickDataset_Preprocessed$SubModel)
str(kickDataset_Preprocessed)

# To check door frequencies from 2,4,3,5, and 6 Doors, and we found out, this dataset has majority of 4 Doors
# kickDataset_Preprocessed$SubModel[grep("2D", kickDataset_Preprocessed$SubModel)]
# kickDataset_Preprocessed$SubModel[grep("4D", kickDataset_Preprocessed$SubModel)]

kickDataset_Preprocessed$SubModel_Door <- 4
kickDataset_Preprocessed$SubModel_Door[grep("2D", kickDataset_Preprocessed$SubModel)] <- 2
kickDataset_Preprocessed$SubModel_Door[grep("3D", kickDataset_Preprocessed$SubModel)] <- 3
kickDataset_Preprocessed$SubModel_Door[grep("5D", kickDataset_Preprocessed$SubModel)] <- 5
kickDataset_Preprocessed$SubModel_Door[grep("6D", kickDataset_Preprocessed$SubModel)] <- 6

table(kickDataset_Preprocessed$SubModel_Door) # Realizing majority of 69491 of vehicles have 4 doors, so we decided to make it as 0 or 1 only

# From here we do one step further.
#kickDataset_Preprocessed$SubModel_With_4Door <- 0
#kickDataset_Preprocessed$SubModel_With_4Door[grep(4, kickDataset_Preprocessed$SubModel_Door)] <- 1

#table(kickDataset_Preprocessed$SubModel_With_4Door)
#kickDataset_Preprocessed$SubModel_With_4Door <- factor(kickDataset_Preprocessed$SubModel_With_4Door)
kickDataset_Preprocessed$SubModel_Door <- factor(kickDataset_Preprocessed$SubModel_Door)

str(kickDataset_Preprocessed)

#  from Model to Model Wheel Drive

kickDataset_Preprocessed$Model <- as.character(kickDataset_Preprocessed$Model)
# Since majority of wheel drive in this dataset is 2, so we set as 2WD.
kickDataset_Preprocessed$Model[grep("2WD", kickDataset_Preprocessed$Model)]  # 10009 entries with count 61591
kickDataset_Preprocessed$Model[grep("FWD", kickDataset_Preprocessed$Model)]  # 6197 entries with count 7197
kickDataset_Preprocessed$Model[grep("RWD", kickDataset_Preprocessed$Model)]  # 16 entries with count 15
kickDataset_Preprocessed$Model[grep("AWD", kickDataset_Preprocessed$Model)]  # 751 entries with count 751
kickDataset_Preprocessed$Model[grep("4WD", kickDataset_Preprocessed$Model)]  # 2429 entries with count 3429
kickDataset_Preprocessed$Model[grep("6WD", kickDataset_Preprocessed$Model)]  # 0 entries
kickDataset_Preprocessed$Model[grep("8WD", kickDataset_Preprocessed$Model)]  # 0 entries

kickDataset_Preprocessed$Model_WheelDrive <- "2WD"
kickDataset_Preprocessed$Model_WheelDrive[grep("FWD", kickDataset_Preprocessed$Model)] <- 'FWD'
kickDataset_Preprocessed$Model_WheelDrive[grep("RWD", kickDataset_Preprocessed$Model)] <- 'RWD'
kickDataset_Preprocessed$Model_WheelDrive[grep("2WD", kickDataset_Preprocessed$Model)] <- '2WD'
kickDataset_Preprocessed$Model_WheelDrive[grep("AWD", kickDataset_Preprocessed$Model)] <- 'AWD'
kickDataset_Preprocessed$Model_WheelDrive[grep("4WD", kickDataset_Preprocessed$Model)] <- '4WD'
kickDataset_Preprocessed$Model_WheelDrive[grep("6WD", kickDataset_Preprocessed$Model)] <- '6WD'
kickDataset_Preprocessed$Model_WheelDrive[grep("8WD", kickDataset_Preprocessed$Model)] <- '8WD'

table(kickDataset_Preprocessed$Model_WheelDrive)

# 2WD   4WD   AWD   FWD   RWD 
#61591  3429   751  7197    15 

# We also realized, most of the data is with 2WD (2 wheel drive), therefore, we put into 1 or 0.
#kickDataset_Preprocessed$Model_With_2WD <- 0
#kickDataset_Preprocessed$Model_With_2WD[grep('2WD', kickDataset_Preprocessed$Model_WheelDrive)] <- 1

#table(kickDataset_Preprocessed$Model_With_2WD)
table(kickDataset_Preprocessed$Model_WheelDrive)

#kickDataset_Preprocessed$Model_With_2WD <- factor(kickDataset_Preprocessed$Model_With_2WD)
kickDataset_Preprocessed$Model_WheelDrive <- factor(kickDataset_Preprocessed$Model_WheelDrive)

kickDataset_Preprocessed$Model <- factor(kickDataset_Preprocessed$Model)
kickDataset_Preprocessed$SubModel <- factor(kickDataset_Preprocessed$SubModel)

kickDataset_Preprocessed$SubModel_Door <- factor(kickDataset_Preprocessed$SubModel_Door)
#kickDataset_Preprocessed$SubModel_With_4Door <- factor(kickDataset_Preprocessed$SubModel_With_4Door)


str(kickDataset_Preprocessed)

#https://www.nadaguides.com/cars/articles/differences-between-i4-i6-v6-v8-engines
# 4 types of engines
kickDataset_Preprocessed$Model_I4_Engine <- 0
kickDataset_Preprocessed$Model_I4_Engine[grep("I4", kickDataset_Preprocessed$Model)] <- 1
kickDataset_Preprocessed$Model_I4_Engine[grep("I 4", kickDataset_Preprocessed$Model)] <- 1
kickDataset_Preprocessed$Model_I4_Engine[grep("I-4", kickDataset_Preprocessed$Model)] <- 1
table(kickDataset_Preprocessed$Model_I4_Engine)

kickDataset_Preprocessed$Model_I6_Engine <- 0
kickDataset_Preprocessed$Model_I6_Engine[grep("I6", kickDataset_Preprocessed$Model)] <- 1
kickDataset_Preprocessed$Model_I6_Engine[grep("I 6", kickDataset_Preprocessed$Model)] <- 1
kickDataset_Preprocessed$Model_I6_Engine[grep("I-6", kickDataset_Preprocessed$Model)] <- 1
table(kickDataset_Preprocessed$Model_I6_Engine)

kickDataset_Preprocessed$Model_Cylinder <- "V4"
kickDataset_Preprocessed$Model_Cylinder[grep("V6", kickDataset_Preprocessed$Model)] <- "V6"
kickDataset_Preprocessed$Model_Cylinder[grep("V8", kickDataset_Preprocessed$Model)] <- "V8"
table(kickDataset_Preprocessed$Model_Cylinder)

kickDataset_Preprocessed$Model_I4_Engine <- factor(kickDataset_Preprocessed$Model_I4_Engine)
kickDataset_Preprocessed$Model_I6_Engine <- factor(kickDataset_Preprocessed$Model_I6_Engine)
kickDataset_Preprocessed$Model_Cylinder <- factor(kickDataset_Preprocessed$Model_Cylinder)

str(kickDataset_Preprocessed)

kickDataset_Preprocessed_1 <- kickDataset_Preprocessed # Backup ds / without fitting into random forest
kickDataset_Preprocessed_2 <- kickDataset_Preprocessed # Backup ds / fitting into random forest after removing unneeded variable.

# Before running into randomForest, we remove unneeded variable that has more than 30 levels.
kickDataset_Preprocessed_2 <- select(kickDataset_Preprocessed_2, -Make)
kickDataset_Preprocessed_2 <- select(kickDataset_Preprocessed_2, -Model)
kickDataset_Preprocessed_2 <- select(kickDataset_Preprocessed_2, -Trim)
kickDataset_Preprocessed_2 <- select(kickDataset_Preprocessed_2, -SubModel)
kickDataset_Preprocessed_2 <- select(kickDataset_Preprocessed_2, -VNST)
str(kickDataset_Preprocessed_2)

plot_correlation(kickDataset_Preprocessed_2, type = 'continuous')

var_Correlated_Val <- c("IsBadBuy", "MMRAcquisitionAuctionAveragePrice", "MMRAcquisitionAuctionCleanPrice",
       "MMRAcquisitionRetailAveragePrice", "MMRAcquisitonRetailCleanPrice",
       "MMRCurrentAuctionAveragePrice", "MMRCurrentAuctionCleanPrice",
       "MMRCurrentRetailAveragePrice", "MMRCurrentRetailCleanPrice", "VehBCost")

# Generating correlation matrix
cor_kickDataset_Preprocessed_2 <- cor(kickDataset_Preprocessed_2[, c("MMRAcquisitionAuctionAveragePrice", "MMRAcquisitionAuctionCleanPrice",
                                                 "MMRAcquisitionRetailAveragePrice", "MMRAcquisitonRetailCleanPrice",
                                                 "MMRCurrentAuctionAveragePrice", "MMRCurrentAuctionCleanPrice",
                                                 "MMRCurrentRetailAveragePrice", "MMRCurrentRetailCleanPrice", 
                                                 "VehBCost")])

# Building the correlation plot
corrplot(cor_kickDataset_Preprocessed_2, method="number")

library(randomForest) # Build the random forest to understand which variable has the highest impact.
important_var <- randomForest(IsBadBuy ~ .,  data = kickDataset_Preprocessed_2[, var_Correlated_Val], 
                              mtry = 5, ntree = 500, importantce = TRUE)
varImpPlot(important_var)

str(kickDataset_Preprocessed_2)

# Selected all columns. 30 variables
selectedColumns_all <- c("IsBadBuy","Auction","VehYear","VehicleAge", "Color",
                         "Transmission","WheelType","VehOdo","Nationality",
                         "Size", "TopThreeAmericanName", 
                         "MMRAcquisitionAuctionAveragePrice", "MMRAcquisitionAuctionCleanPrice", 
                         "MMRAcquisitionRetailAveragePrice", "MMRAcquisitonRetailCleanPrice", 
                         "MMRCurrentAuctionAveragePrice", "MMRCurrentAuctionCleanPrice", 
                         "MMRCurrentRetailAveragePrice", "MMRCurrentRetailCleanPrice",
                         "BYRNO","VNZIP1", "VehBCost", "IsOnlineSale",
                         "WarrantyCost","SubModel_Type","SubModel_Door", 
                         "Model_WheelDrive", "Model_I4_Engine", "Model_I6_Engine", "Model_Cylinder") 

# Removing Un needed feature or feature with more than 100 factors
# Select columns after removing highly correlated column. 23 variables
selectedColumns_importanceVar <- c("IsBadBuy","Auction","VehYear","VehicleAge", "Color",
                         "Transmission","WheelType","VehOdo","Nationality",
                         "Size", "TopThreeAmericanName", 
                         "MMRAcquisitionAuctionCleanPrice", 
                         "BYRNO","VNZIP1", "VehBCost", "IsOnlineSale",
                         "WarrantyCost","SubModel_Type","SubModel_Door", 
                         "Model_WheelDrive", "Model_I4_Engine", "Model_I6_Engine", "Model_Cylinder") 

str(kickDataset_Preprocessed_2)
write.csv(kickDataset_Preprocessed_2, 'kickDataset_4_Transformed.csv', row.names = TRUE)


