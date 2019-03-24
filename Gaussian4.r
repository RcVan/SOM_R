#######################################
###         Gaussian_4SOM.r         ###
###           Ian, Fan              ###
###          2019.03.12             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop")


# source
source('initWeights.r')
source('SOM.r')
source('util.r')
library(MASS)           # generate gaussian
library(ggplot2)


sd = 1024
set.seed(sd)
########################################

## Parameters
# SOM output
som_output_width = 8
som_output_num = som_output_width ^ 2

# learning rate
learning_rate = 0.1

# SOM neighborhood width: decrease with time
neighborhood_width = som_output_width /2
sigma = neighborhood_width

# weight range
weight_range = c(-2, 10)



## generate data: 4 clusters of 2-d gaussion
data_num = 1000
data_dim = 2
covariance = 0.1
# 4 gaussion
cluster1 = mvrnorm(n = data_num, mu = c(0,0), Sigma = covariance * diag(data_dim))
cluster2 = mvrnorm(n = data_num, mu = c(0,7), Sigma = covariance * diag(data_dim))
cluster3 = mvrnorm(n = data_num, mu = c(7,0), Sigma = covariance * diag(data_dim))
cluster4 = mvrnorm(n = data_num, mu = c(7,7), Sigma = covariance * diag(data_dim))
# all data
trainSet = rbind(cluster1, cluster2, cluster3, cluster4)



## SOM weights
# initWeights(input_size, output_size, range, bias_flag = TRUE)
som_weights = initWeights(data_dim, som_output_num, weight_range, bias_flag = FALSE)
# som_weights
prototypes = som_weights



## train SOM
iter_num = 5000
for (iter in c(1:iter_num)) {
  
  ## update prototypes using data in trainSet, return nothing, update prototypes globally
  batch_order = sample(dim(trainSet)[1], dim(trainSet)[1])
  # updatePrototype(input, prototypes, som_output_width, sigma, learning_rate)
  apply(trainSet[batch_order,], 1, function(data){return(updatePrototype(data, prototypes, som_output_width, sigma, learning_rate))})
  
  
  ## neighborhood width(sigma) decays with time
  sigma = max(neighborhood_width * (iter_num -iter)/iter_num, 1)
  
  
  
  ## iteration info
  print(paste0("Finish Iteration ", iter))
  # plot to see SOM
  if (iter %% 100 == 0){
    data_center = data.frame(prototypes) # data.frame(som_weights)
    plt = plotSOM(data_center, som_output_width) + 
      labs(title=paste0("SOM for 2-D Gaussian in Iteration ", iter), x = "x", y = "y")
    print(plt)
  }
  
}





## plot trainData
data = data.frame(trainSet)
plt2 = plotPoint(data) + 
  labs(title="Data of 2-D gaussian", x = "x", y = "y")
plt2



## plot SOM
data_center = data.frame(prototypes) # data.frame(som_weights)
plt = plotSOM(data_center, som_output_width) + 
  labs(title="SOM for 2-D gaussian", x = "x", y = "y")
plt



