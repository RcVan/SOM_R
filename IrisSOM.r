#######################################
###           IrisSOM.r             ###
###           Ian, Fan              ###
###          2019.03.13             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop")


# source
source('initWeights.r')
source('SOM.r')
source('util.r')
library(ggplot2)


sd = 1024
set.seed(sd)
########################################

## Parameters
# SOM output
som_output_width = 10
som_output_num = som_output_width ^ 2

# learning rate
learning_rate = 0.2

# SOM neighborhood width: decrease with time
neighborhood_width = som_output_width 
sigma = neighborhood_width

# weight range
weight_range = c(-1, 2)



## get Iris data
source('loadData.r')

# data dim = 4
data_dim = 4
trainSet = IrisData[,seq(data_dim)]
# print(trainSet)



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
  sigma = max(neighborhood_width * (iter_num -iter) * 10 %/% iter_num / 10, 1)
  
  
  
  ## iteration info
  print(paste0("Finish Iteration ", iter))
  # plot to see SOM
  if (iter %% 500 == 0){
    fire_times = rep(0, dim(prototypes)[1])
    
    
    for(i in seq(1, dim(trainSet)[1])){
      
      ## get fire prototype index
      input_prototype_distance = apply(prototypes, 1, function(w){return(myEuclideanDistance(w, trainSet[i,]))})
      # print(input_prototype_distance)
      fire_index = which(input_prototype_distance == min(input_prototype_distance))
      # print(fire_index)
      
      ## fire times count: add one
      fire_times[fire_index] = fire_times[fire_index] + 1
    }
    
    # to matrix
    fire_times = matrix(fire_times, nrow = som_output_width)
    fire_times = melt(fire_times)
    
    ## plot
    plt = ggplot(data = fire_times) + geom_tile(aes(x = Var1, y = Var2, fill = value)) + 
      theme_classic() +
      theme(axis.ticks = element_blank(), axis.line = element_blank()) +
      scale_fill_gradient2('fire times', low = 'black', high = 'red') +
      labs(title="Prototypes Fire Times", x = "x", y = "y")
    
    print(plt)
  }
  
}

