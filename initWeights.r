#######################################
###         initWeights.r           ###
###           Ian, Fan              ###
###          2019.02.03             ###
#######################################

## Description
# This initWeights.r file generate weights with modifiable parameters.


## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/")


## import zeros function
# install.packages("optimbase")
library(optimbase)


## code starts from here
########################################################

## initWeights(input_size, output_size, range, bias_flag = TRUE)
## parameters: 
# vector: range
# numeric: input_size(without bias), output_size
# boolean: bias_flag
## return:
# matrix: weights (output_size * input_size+1)
initWeights = function(input_size, output_size, range, bias_flag = TRUE){
  
## get range 
  mi = range[1]
  ma = range[2]
  
  
## initialization
# remember that input is without bias 
# check bias flag to get weights input size
  weights_input_size = ifelse(bias_flag, input_size+ 1, input_size)
  
# generate zero matrix
  weights = zeros(output_size, weights_input_size)
  
  
## random weights
  for (i in c(1:output_size))
    weights[i,] = runif(weights_input_size, min = mi, max = ma)
  
  
## return 
  return(weights)
}
