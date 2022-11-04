#!/usr/bin/Rscript

library(tidyverse)
library(skimr)
library(caret)
library(glmnet)

train_data <- data.frame(read.csv(file = "C:/Users/bhava/OneDrive/Desktop/IDA/Homeworks/HW7/hm7-Train-r1.csv"))
test_data <- data.frame(read.csv(file = "C:/Users/bhava/OneDrive/Desktop/IDA/Homeworks/HW7/hm7-Test-r1.csv"))

dim(train_data)
glimpse(train_data)

lapply(train_data, function(x) sum(is.na(x))) #Missing values
lapply(test_data, function(x) sum(is.na(x)))

# unique(train_data$race)
# unique(train_data$time_in_hospital)
# unique(train_data$payer_code)
# unique(train_data$medical_specialty)
# unique(train_data$indicator_level)
# unique(train_data$num_lab_procedures)
# unique(train_data$diagnosis)

skim(train_data) #For Data Quality Report

train_data$patientID <- as.factor(train_data$patientID)
train_data$admission_type <- as.factor(train_data$admission_type)
train_data$discharge_disposition <- as.factor(train_data$discharge_disposition)
train_data$admission_source <- as.factor(train_data$admission_source)

test_data$patientID <- as.factor(test_data$patientID)
test_data$admission_type <- as.factor(test_data$admission_type)
test_data$discharge_disposition <- as.factor(test_data$discharge_disposition)
test_data$admission_source <- as.factor(test_data$admission_source)

train_data$payer_code <- NULL
train_data$medical_specialty <- NULL

test_data$payer_code <- NULL
test_data$medical_specialty <- NULL

dim(train_data)
dim(test_data)

unique(train_data$readmitted)

trCntl <- trainControl(method = "CV",number = 10)
logitMod_CV <- train(readmitted ~ race + gender + age + admission_type + discharge_disposition + admission_source + 
                       time_in_hospital + num_lab_procedures + num_procedures + num_medications + number_diagnoses + 
                       max_glu_serum + metformin + insulin + miglitol + diabetesMed , 
                     data = train_data, trControl = trCntl, method = "glm", family = "binomial")
