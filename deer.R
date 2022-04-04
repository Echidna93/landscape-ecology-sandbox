# TODO add comments to each function to document
library(tidyverse)
library(plotrix)
# make a square matrix for now
make_landscape_matrix <- function(row, col){
   matrix(sample(c(0,1), replace=TRUE, size=row*col), nrow=row)
}
make_infection_matrix<-function(row,col){
   matrix(sample(c(0), replace=TRUE, size=row*col), nrow=row)
}
get_infected_deer<-function(data_frame){
  for(i in 1:nrow(data_frame)){
    if(data_frame[i,]$status == "I"){
      infected_ind<-data_frame[i,]
    }
  }
  infected_ind
}
# TODO use the single parameter used in white's code for the location of the deer
#' Updates individual values within dataframe
#' @param data_frame holds data about deer
#' @param infection_matrix holds data about current infectivity levels of landscape
#' @export infectivity_threshold some amount of disease where infection automatically happens (definitely subject to change)

update_infection_statuses<-function(data_frame, infection_matrix, infectivity_threshold){
  inf_ind<-get_infected_deer(data_frame)
  # TODO break this off into it's own function, maybe is_same_location
  for(i in 1:nrow(data_frame)){
    if((data_frame[i,]$xloc==inf_ind$xloc & data_frame[i,]$yloc==inf_ind$yloc & data_frame[i,]$status=="S") |
       (is_cell_inf_val_above_threshold(infection_matrix[data_frame[i,]$xloc + data_frame[i,]$yloc],infectivity_threshold))){
      data_frame[i,]$status = "I"
    }
  }
  data_frame
}

# want a helper function that checks the infectivity value of the
# currently occupied cell, if the cell is above a certain threshold
# deer becomes infected

is_cell_inf_val_above_threshold<-function(cell_value, infectivity_threshold){
  cell_value>=infectivity_threshold
}

update_infection_matrix<-function(inf_matrix, data_frame){
  for(i in 1:nrow(data_frame)){
    if(data_frame[i,]$status == "I")
      inf_matrix[data_frame[i,]$xloc, data_frame[i,]$yloc]<-inf_matrix[data_frame[i,]$xloc,data_frame[i,]$yloc] + 1
  }
  inf_matrix
}
move<-function(data_frame){
  for(i in 1:nrow(data_frame)){
    nbs<-get_neighbors(c(data_frame[i,]$xloc,data_frame[i,]$yloc), num_row, num_col)
    new_loc<-nbs[[round(runif(1,1,length(nbs)))]]
    data_frame[i,]$xloc<-new_loc[[1]]
    data_frame[i,]$yloc<-new_loc[[2]]
  }
  data_frame
}
# want function to create dataframe of deer
make_deer <- function(n.initial,dim, nI){
  id<-1:n.initial
  xloc<-round(runif(n.initial, min=1, max=dim))
  yloc<-round(runif(n.initial, min=1, max=dim))
  # vec<-Cmatrix(yloc,xloc,dim)
  I<-sample(1:n.initial, nI)
  status<-rep("S", times=n.initial)
  status[I]<-"I"
  inds <- data.frame(id = id, xloc=xloc, yloc=yloc, status=status, stringsAsFactors=FALSE) 
  inds
}
# takes in vector(s), matrix points
# @param matrix
# returns neighborhood of cells as a list of vectors
get_neighbors<-function(loc, nrow, ncol){
  # list iterator
  k=1
  l<-list()
  # check if either x,y element of loc is greater than
  # the dimension of the landscape matrix
  for(i in -1:1){
    for(j in -1:1){
      # case 1 on left or right edge of matrix
      if(!(loc[1]+i < 1 | loc[1]+i > nrow) & !(loc[2]+j < 1 | loc[2]+j > ncol)){
        l[[k]] <- c(loc[1] + i, loc[2] + j)
        k<-k+1
      }
    }
  }
  l
}
# Main simulation
# TODO make main simulation loop, using functional? (lapply)

# CONSTANTS
# TODO look into making these changeable by a user in an x11() window?
num_row=5
infectivity_threshold=2
num_col=5
landscape<-make_landscape_matrix(5,5) 
infection_matrix<-make_infection_matrix(5,5)
deer<-make_deer(8,num_row,1)
inf_ind<-get_infected_deer(deer)
# deer<-deer %>% add_row(id=nrow(), xloc=inf_ind$xloc, yloc=inf_ind$yloc, status="S")
deer
inf_ind<-get_infected_deer(deer)
for(i in 1:10){
  deer<-update_infection_statuses(deer, infection_matrix, infectivity_threshold)
  infection_matrix<-update_infection_matrix(infection_matrix, deer)
  print(infection_matrix )
  deer<-move(deer)
  print(deer)
  # add a column with the extreme values (-1,1) to calculate
  # the colors, then drop the extra column in the result
  
  # code for movement map
  # matplot(deer$xloc, deer$yloc, type=,pch=1)
  # code for "heatmap"
  cellcol<-color.scale(cbind(infection_matrix,
                             c(8, rep(1,4))), c(0,1), 0, c(1,0))[,1:5]
  color2D.matplot(infection_matrix,cellcolors=cellcol,
                  main="Landscape Infectivity Heatmap")
  # do the legend call separately to get the full range
  color.legend(0,-4,10,-3,legend=c(0,1,2,3,4,5,6,7,8,9,10),
               rect.col=color.scale(c(0:8),c(0,1),0,c(1,0)),align="rb")
  
  Sys.sleep(2)
}
color.scale