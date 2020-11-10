# So the general idea here follows the logic from the paper I shared with you.
# We first select a starting point
# Then we select a random neighbor cell to go to from t=1 until t = whatever limit we set
# we select the random cell according to which has the highest transition probability
# calculate the patrol reward

# then we do this 1000 times (or however many) according to the algorithm above
# keep the top 10% of these routes
# use them to update the transition matrix

# then we repeat that until we reach convergence


calculate_reward <- function(reward_input, cell){
  reward <- reward_input[cell]
  return(reward)
}

select_next_cell <- function(cell, transition_mat){
  potential_cells <- adjacent(cost_layer, cell, directions=8, pairs=FALSE, target=NULL, sorted=FALSE, 
                              include=FALSE, id=FALSE) 
  next_cell <- potential_cells[which.min(cost_layer[potential_cells])]
  #next_cell <- pick cell from 8 neighbors with highest transition probability
  return(next_cell)
}

generate_route <- function(start_cell,trans_mat,cost_layer,reward_layer,t){
  current_cell <- start_cell
  cells_covered <- start_cell
  while (total_cost < t){
    current_cell <- select_next_cell(current_cell,trans_mat)
    total_reward <- total_reward + calculate_reward(reward_input=reward_layer,
                                                    cell=current_cell)
    total_cost <- total_cost + cost_layer[current_cell]
    cells_covered <- cbind(cells_covered,current_cell)
  }
  tmp_return <- list(cells_covered,total_reward)
  #return(tmp_return)
  return(total_reward)
}

update_transition <- function(transition_mat){
  return(new_tran_mat)
}


total_cost <- 0
total_reward <- 0

library(raster)

road_map <- raster(ncol=10, nrow=10)
values(road_map) <- 1:ncell(road_map)
reward_layer <- road_map
values(reward_layer) <- sample(seq(0,1000),100)
cost_layer <- road_map
values(cost_layer) <- sample(seq(0,10),100,replace=TRUE)
transition_mat <- matrix(data=0,nrow=10,ncol=10)


test <- generate_route(start_cell=1,
                       trans_mat=transition_mat,
                       cost_layer=cost_layer,
                       reward_layer=reward_layer,
                       t=100)


library(CEoptim)
mu0 <- c(5); sigma0 <- c(100000)
ctsDefaults<- list(mean=NULL, sd=NULL, conMat=NULL, conVec=NULL,smoothMean=1, smoothSd=1, sdThr=0.001)

p0 <- list()
for (i in 1:1) {
  p0 <- c(p0, list(rep(0.5, 2)))
}
p0[[1]] <- c(0, 1)


maximum <- CEoptim(f=generate_route,
                   f.arg=list(start_cell=1,
                              trans_mat=transition_mat,
                              cost_layer=cost_layer,
                              reward_layer=reward_layer,
                              t=100), 
                   verbose = TRUE,
                   maximize=TRUE,
                   N=10L)






maximum <- CEoptim(f=generate_route,
                   
                   maximize=TRUE,
                   N=10)



#cross_entropy(run_route_selection(transition_mat))

#reward_layer <- hotspot layer
#road_map <- possible roads layer
#cost_layer <- least cost path layer


run_route_selection <- function(transition_mat){
  cost <- 0
  total_reward <- 0
  current_cell <- starting point
  for (r in 1:1000){
    generatre_route(current_cell,transition_mat)
  }
  selected_routes <- select top 10% of routes
  new_transition <- update_transition
}


