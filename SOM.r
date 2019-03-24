#######################################
###             SOM.r               ###
###           Ian, Fan              ###
###          2019.03.04             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop")


# source
source('util.r')


########################################

## calculateIndexDist(current, target, grid_width = 10)
## parameters: 
# numeric: current, target, grid_width
## return:
# numeric: index_distance
calculateIndexDist = function(current, target, grid_width = 10){
  
  # location of current index
  x1 = (current-1) %/% grid_width
  y1 = (current-1) %% grid_width
  
  
  # location of target index
  x2 = (target-1) %/% grid_width
  y2 = (target-1) %% grid_width
  
  # test
  # print(sprintf('(%d, %d)--> (%d, %d)', x1,y1,x2,y2))
  
  return( myEuclideanDistance(c(x1,y1), c(x2,y2)) )
}



## SOM neighborhood function(index_distance, neighborhood_width_sigma)
## parameters: 
# numeric: index_distance(in grid), neighborhood_width_sigma
## return:
# numeric: neighborhood_factor
neighborhood_func = function(index_distance, neighborhood_width_sigma){
  
  return( exp(-index_distance^2 / (2 * neighborhood_width_sigma^2) ))
}



## updatePrototype(input, prototypes, som_output_width, sigma, learning_rate)
## parameters: 
# numeric: som_output_width, sigma, learning_rate
# vector: input(N*1)
# matrix: prototypes(M*N)
## return:
# NULL
## update global:
# matrix: prototypes
updatePrototype = function(input, prototypes, som_output_width, sigma, learning_rate){
  
  ## get fire prototype index
  input_prototype_distance = apply(prototypes, 1, function(w){return(myEuclideanDistance(w, input))})
  # print(input_prototype_distance)
  fire_index = which(input_prototype_distance == min(input_prototype_distance))
  # print(fire_index)
  
  ## calculate neighborhood factor
  neighborhood_factor = apply(matrix(c(1:som_output_width^2)), 1, 
                              function(p_i){return(neighborhood_func(calculateIndexDist(fire_index, p_i, som_output_width), sigma))})
  
  ## update prototypes
  # w_(t+1) = w_t + eta * neighborhood_factor * (x - w_t)
  prototypes <<- prototypes + learning_rate * neighborhood_factor * (t(ones(size(prototypes)[2], size(prototypes)[1])*input) - prototypes)
  # prototypes

}


