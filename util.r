#######################################
###            util.r               ###
###           Ian, Fan              ###
###          2019.03.04             ###
#######################################

## clear variables and set work directory
# rm(list = ls())


# source
library(reshape2)


########################################

## myEuclideanDistance(vector1, vector2)
## parameters: 
# vector: vector1, vector2
## return:
# numeric: distance
myEuclideanDistance = function(vector1, vector2){
  
  return(sqrt( sum((vector1 - vector2)^2) ) )
}



## plotPoint(df)
## parameters: 
# dataframe: df
## return:
# plot: plt
plotPoint = function(df){
  
  colnames(df) = c("x", "y")
  plt = ggplot() + geom_point(aes(x=df$x, y=df$y))
  
  return(plt)
  
}



## plotSOM
## parameters: 
# matrix: prototypes
# numeric: som_output_width
## return:
# plot: plt
plotSOM = function(prototypes, som_output_width = 10){
  
  ## get data
  data = data.frame(prototypes)
  colnames(data) = c("x", "y")
  
  
  ## plot point
  plt = ggplot() + geom_point(aes(x=data$x, y=data$y, color = 'SOM Prototypes'))

  
  ## path 1
  path1 = data
  for(i in seq(2, som_output_width, by = 2)){
    path1[seq(i*som_output_width, (i-1)*som_output_width + 1), ] = path1[seq((i-1)*som_output_width + 1, i*som_output_width), ]
  }
  # plot path 1
  path1 = data.frame(path1)
  colnames(path1) = c("x", "y")
  plt = plt + geom_path(aes(x=path1$x, y=path1$y))

    
  ## path 2
  path2_index = matrix(seq(som_output_width^2), ncol = som_output_width)
  path2_index = matrix(t(path2_index))
  for(i in seq(2, som_output_width, by = 2)){
    path2_index[seq(i*som_output_width, (i-1)*som_output_width + 1), ] = path2_index[seq((i-1)*som_output_width + 1, i*som_output_width), ]
  }
  
  # plot path 2
  path2 = data.frame(data[path2_index,])
  colnames(path2) = c("x", "y")
  plt = plt + geom_path(aes(x=path2$x, y=path2$y))

  
  return(plt)
  
}







