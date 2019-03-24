#######################################
###          loadData.r             ###
###           Ian, Fan              ###
###          2019.02.11             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/")


## read data file
trainData = read.table("iris-train.txt", head = TRUE)
testData = read.table("iris-test.txt", head = TRUE)


## bind data
IrisData = rbind(trainData, testData)
row_num = nrow(IrisData)



