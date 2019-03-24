#######################################
###         MapDataToSOM.r          ###
###           Ian, Fan              ###
###          2019.03.13             ###
#######################################

library(reshape2)

######################################
##    map data to SOM
######################################

## plot gaussian data
c1 = data.frame(cluster1)
c2 = data.frame(cluster2)
c3 = data.frame(cluster3)
c4 = data.frame(cluster4)
plt = ggplot()
plt = plt + geom_point(aes(x=c1$X1, y=c1$X2, color='Cluster1'))
plt = plt + geom_point(aes(x=c2$X1, y=c2$X2, color='Cluster2'))
plt = plt + geom_point(aes(x=c3$X1, y=c3$X2, color='Cluster3'))
plt = plt + geom_point(aes(x=c4$X1, y=c4$X2, color='Cluster4'))


## get prototypes data
data = data.frame(prototypes)
colnames(data) = c("x", "y")


## plot point
plt = plt + geom_point(aes(x=data$x, y=data$y, color = 'SOM Prototypes'))


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


## plot 
plt = plt +
  labs(title="Mapping 2-D gaussian Data to SOM", x = "x", y = "y")
plt



######################################
## plot fire times of prototypes (Heat plot)
######################################


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
  scale_fill_gradient2('fire times', low = 'white', high = 'red') +
  labs(title="Prototypes Fire Times", x = "x", y = "y")
plt
