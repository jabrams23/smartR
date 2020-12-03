# So the general idea here follows the logic from the paper I shared with you.
# We first select a starting point
# Then we select a random neighbor cell to go to from t=1 until t = whatever limit we set
# we select the random cell according to which has the highest transition probability
# calculate the patrol reward

# then we do this 1000 times (or however many) according to the algorithm above
# keep the top 10% of these routes
# use them to update the transition matrix

# then we repeat that until we reach convergence

library(reshape2)
library(raster)
library(foreach)
library(doParallel)

##### FUNCTIONS

calculate_reward <- function(reward_input, cell){
  reward <- reward_input[cell]
  return(reward)
}

calculate_cost <- function(costlayer,prev,cur){
  tmp_cost <- abs(costlayer[prev]-costlayer[cur])
  return(tmp_cost)
}

select_next_cell <- function(cost_layer,cell, transition_mat, covered){
  potential_cells <<- adjacent(cost_layer, cell, directions=8, pairs=FALSE, target=NULL, sorted=FALSE, 
                              include=FALSE, id=FALSE) 
  potential_cells <<- potential_cells[!potential_cells %in% covered]
  if(length(potential_cells)==0) {
    potential_cells <<- adjacent(cost_layer, cell, directions=8, pairs=FALSE, target=NULL, sorted=FALSE, 
                                 include=FALSE, id=FALSE) 
  }
  
  if(max(transition_mat[potential_cells])==0) {
    potential_cells <<- sample(x = adjacent(cost_layer, cell, directions=8, pairs=FALSE, target=NULL, sorted=FALSE, 
                                            include=FALSE, id=FALSE),
                               size=1)
  }
  if(length(potential_cells)==1) {
    next_cell <- potential_cells
  } else {
    next_cell <- sample(x=potential_cells,
                        size=1,
                        prob=transition_mat[potential_cells])
  }
  #next_cell <- potential_cells[which.min(cost_layer[potential_cells])]
  return(next_cell)
}

generate_route <- function(start_cell,trans_mat,cost_layer,reward_layer,t){
  library(reshape2)
  library(raster)
  total_cost <- 0
  total_reward <- 0
  current_cell <- start_cell
  cells_covered <- start_cell
  while (total_cost < t){
    previous_cell <- current_cell
    current_cell <- select_next_cell(cost_layer,current_cell,trans_mat,cells_covered)
    total_reward <- total_reward + calculate_reward(reward_input=reward_layer,
                                                    cell=current_cell)
    cost <- calculate_cost(cost_layer,previous_cell,current_cell)
    total_cost <- total_cost + cost #cost_layer[current_cell]
    cells_covered <- cbind(cells_covered,current_cell)
  }
  tmp_return <- list(cells_covered,total_reward)
  return(tmp_return)
  #return(total_reward)
}

ce <- function(numb,mat,cost,reward,time) {
  #setup parallel backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-2) #not to overload your computer
  registerDoParallel(cl)
  
  rew <- rep(0,numb)
  route <- vector(mode = "list", length = numb)
  #for (i in 1:numb) 
  #foreach(i = 1:numb) %dopar% {
    #print(i)
  #  tmp <- generate_route(start_cell=1,
  #                        trans_mat=mat,
  #                        cost_layer=cost,
  #                        reward_layer=reward,
  #                        t=time)
  #  rew[i]<-tmp[[2]]
  #  route[[i]]<-as.vector(tmp[[1]])
  #}
  
  tmp_test <- foreach(i = 1:numb, .export=c("calculate_cost","calculate_reward", "generate_route","select_next_cell")) %dopar% {
    print(i)
    tmp <- generate_route(start_cell=1,
                          trans_mat=mat,
                          cost_layer=cost,
                          reward_layer=reward,
                          t=time)
    #rew[i]<-tmp[[2]]
    #route[[i]]<-as.vector(tmp[[1]])
  }
  
  stopCluster(cl)
  gc()
  for (k in 1:numb) {
    rew[k]<-as.vector(tmp_test[[k]][2])
    route[k]<-as.vector(tmp_test[[k]][1])
  }
  ten_percent <- 0.1 * numb
  rew <- unlist(rew)
  tmp2 <- sort(rew, decreasing = TRUE)
  tmp_10 <- tmp2[ten_percent]
  selected_routes <- route[which(rew>=tmp_10)]
  return(selected_routes)
}

update_transition <- function(cost_matrix,tran_mat,select,numbr){
  tmp1 <- unlist(select)
  tmp3 <- as.data.frame(table(tmp1))
  tran_mat@data@values[!is.na(tran_mat@data@values)] <- 0
  tran_mat@data@values[tmp3$tmp1] <- as.vector(tmp3$Freq)/(numbr*0.1)
  tran_mat@data@values <- ifelse(tran_mat@data@values>1, 1, tran_mat@data@values)
  return(tran_mat)
}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}


########## TESTING


road_map <- raster(ncol=10, nrow=10)
values(road_map) <- 1:ncell(road_map)
reward_layer <- road_map
values(reward_layer) <- sample(seq(0,100),100,replace=TRUE)
cost_layer <- road_map
values(cost_layer) <- sample(seq(0,10),100,replace=TRUE)

values(cost_layer) <- range01(values(cost_layer))

transition_mat <- cost_layer
#values(transition_mat) <- 0.0

numb<-1000
reps<-1000
for (i in 1:reps) {
  #transition_mat <- (0.5*(1-cost_layer))+(0.5*transition_mat)
  test <- ce(numb,transition_mat,cost_layer,reward_layer,20)
  transition_mat <- update_transition(cost_layer,transition_mat,test,numb)
  plot(transition_mat)
}



