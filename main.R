# library(tidyverse)
library(plotrix)
library('plot.matrix')

#' initiates a landscape matrix of vectors of random 0's and 1's
#' @param nrow number of rows in matrix
#' @param ncol number of columns in matrix
#' @export
make_landscape_matrix <- function(numrow, numcol, binary=TRUE){
   if(!binary){
     # TODO replace later with something better
     # matrix(sample(c(runif(2,0,1)), replace=TRUE, size=5*5), nrow=5)
     matrix(runif(numrow*numcol,0,1),ncol=numcol)
   }
   else{
    matrix(sample(c(1,2,3), replace=TRUE, size=numrow*numcol), nrow=numrow)
  }
}
#' initiates a matrix to track density at each cell
#' @param nrow number of rows in matrix
#' @param ncol number of columns in matrix
#' @export
make_density_matrix <- function(numrow, numcol, data_frame){
  d_mat<-matrix(sample(c(0), replace=TRUE, size=numrow*numcol), nrow=numrow)
  for(i in 1:nrow(data_frame)){
    # temp mat holds number of deer want density
    d_mat[data_frame[i,]$xloc,][data_frame[i,]$yloc] <- d_mat[data_frame[i,]$xloc,][data_frame[i,]$yloc] + 1
  }
  for(i in 1:numrow){
    for(j in 1:numcol){
        d_mat[i,][j] <- d_mat[i,][j] / nrow(data_frame)
      }
  }
  d_mat
}

#' initiates an infection matrix of all 0's
#' @param nrow number of rows in matrix
#' @param ncol number of columns in matrix
#' @export
make_infection_matrix<-function(nrow,ncol){
   matrix(sample(c(0), replace=TRUE, size=nrow*ncol), nrow=nrow)
}

#' Helper function
#' want it to return a list of infected deer
#' @param data_frame holds data about deer
#' @export
get_infected_deer<-function(data_frame){
  # iterator for empty infected_inds array
  j<-1
  infected_inds<-rep()
  for(i in 1:nrow(data_frame)){
    if(data_frame[i,]$status == "I"){
      infected_inds[[j]]<-c(data_frame[i,]$id,
                            data_frame[i,]$xloc,
                            data_frame[i,]$yloc
                            )
      j<-j+1
    }
  }
  infected_inds
}

# TODO use the single parameter used in white's code for the location of the deer

#' Updates individual values within dataframe
#' @param data_frame holds data about deer
#' @param infection_matrix holds data about current infectivity levels of landscape
#' @param infectivity_threshold some amount of disease where infection automatically happens (definitely subject to change)
#' @export
update_infection_statuses<-function(data_frame, infection_matrix, infectivity_threshold){
  inf_inds<-get_infected_deer(data_frame)
  # TODO break this off into it's own function, maybe is_same_location
  for(i in 1:nrow(data_frame)){
    if(data_frame[i,]$status=="S"){
      if(is_cell_inf_val_above_threshold(infection_matrix[data_frame[i,]$xloc + data_frame[i,]$yloc],infectivity_threshold)){
        data_frame[i,]$status = "I"
      }
      else{
        for(j in 1:length(inf_inds)) {
          if(data_frame[i,]$xloc==inf_inds[[j]][2] & 
              data_frame[i,]$yloc==inf_inds[[j]][3] &
              !(data_frame[i,]$id==inf_inds[[j]][1])){
           data_frame[i,]$status = "I"
          }
        }
      }
    }
  }
  data_frame
}

# new function 
# check locations of each deer
# check_locations<-function(ind, data_frame){
#   for(i in data_frame)
# }

#' Helper function
#' Determines if current cell value is above acceptable "infection" threshold
#' @param cell_value current cell coordinate(s)
#' @param infection_matrix holds data about current infectivity levels of landscape
#' @param infectivity_threshold some amount of disease where infection automatically happens (definitely subject to change)
#' @export
is_cell_inf_val_above_threshold<-function(cell_value, infectivity_threshold){
  cell_value>=infectivity_threshold
}

#' Updates the matrix that holds infection levels for the landscape
#' @param data_frame holds data about deer
#' @param inf_matrix holds data about current infectivity levels of landscape
#' @export
update_infection_matrix<-function(inf_matrix, data_frame){
  for(i in 1:nrow(data_frame)){
    if(data_frame[i,]$status == "I")
      inf_matrix[data_frame[i,]$xloc, data_frame[i,]$yloc]<-inf_matrix[data_frame[i,]$xloc,data_frame[i,]$yloc] + 1
  }
  max_val<-max(inf_matrix)
  apply(inf_matrix, 2, divide_by_max, max=max_val)
}

# helper function
# divides by max value of matrix
divide_by_max<-function(x, max){
  x / max
}
#' Updates individual locations
#' @param data_frame holds data about deer
#' @export
move<-function(data_frame, landscape, nrow, ncol){
  for(i in 1:nrow(data_frame)){
    d_mat<-make_density_matrix(nrow,ncol,data_frame)
    nbrs<-get_neighbors(c(data_frame[i,]$xloc,data_frame[i,]$yloc), num_row, num_col, landscape)
    # new_loc<-nbrs[[round(runif(1,1,length(nbrs)))]]
    new_loc<-make_decision(landscape=landscape, d_mat=d_mat, nbrs=nbrs)
    data_frame[i,]$xloc<-new_loc[[1]]
    data_frame[i,]$yloc<-new_loc[[2]]
  }
  data_frame
}

#' Chooses best possible landscape component to move to
#' TODO alter in the case of an make_decision being fed an empty list, make 
#' else case
#' TODO implement sorting function
#' @param 
#' @export
make_decision<-function(landscape, d_mat, nbrs){
  # assign decision to be the first element by default--make comparison
  decision_vec<-c(nbrs[[1]][1],nbrs[[1]][2])
  decision_val<-landscape[nbrs[[1]][1],][nbrs[[1]][2]] - d_mat[nbrs[[1]][1],][nbrs[[1]][2]]
    for(i in 1:length(nbrs)){
      # recalculate value of landscape matrix to account for conspecific dens.
      crnt_adjstd_val <- (landscape[nbrs[[i]][1],][nbrs[[i]][2]] - d_mat[nbrs[[i]][1],][nbrs[[i]][2]])
      if(crnt_adjstd_val > decision_val){
        # want decision to take into account conspecific density
        # want list of values from the landscape matrix
        decision_vec<-c(nbrs[[i]][1], nbrs[[i]][2])
      }
    }
  decision_vec
}

#' initiates a data frame of deer
#' @param n.initial # deer
#' @param dim dimension of a vector for random assignment of location
#' @param nI # infected individuals to start
#' @export
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

#' Helper function
#' returns neighborhood of cells as a list of vectors
#' @param loc current coordinates of individual
#' @param nrow # of rows in landscape matrix
#' @param ncol # columns in landscape matrix
#' @export
get_neighbors<-function(loc, nrow, ncol, landscape){
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
check_inds_locs<-function(ind, data_frame){
  for(i in 1:nrow(data_frame)){
    if(is_same_location(ind, data_frame[i,]) & is_not_same_id(ind, data_frame[i,])){
      
    }
  }
}

#helper function is location the same
is_same_location<-function(ind1, ind2){
  (ind1$xloc == ind1$yloc) & (ind1$yloc == ind2$yloc)
}

#helper checks if two ids are the same
is_not_same_id<-function(ind1, ind2){
  !(ind1$id == ind2$id)
}

# CONSTANTS
# TODO look into making these changeable by a user in an x11() window?
num_row=5
infectivity_threshold=2
num_col=5
landscape<-make_landscape_matrix(5,5, TRUE)
landscape
infection_matrix<-make_infection_matrix(5,5)
deer<-make_deer(16,num_row,1)
inf_ind<-get_infected_deer(deer)
for(i in 1:50){
  print(i)
  deer<-update_infection_statuses(deer, infection_matrix, infectivity_threshold)
  infection_matrix<-update_infection_matrix(infection_matrix, deer)
  print(infection_matrix)
  deer<-move(deer, landscape, nrow(landscape), ncol(landscape))
  print(deer)
  d_mat<-make_density_matrix(5,5,deer)
  print(d_mat)
  print(landscape)
  # add a column with the extreme values (-1,1) to calculate
  # the colors, then drop the extra column in the result

  # code for movement map
  # matplot(deer$xloc, deer$yloc, type=,pch=1)
  # code for "heatmap"
  # cellcol<-color.scale(cbind(infection_matrix,
  #                            c(0:1)))
  # color2D.matplot(infection_matrix,cellcolors=cellcol,
  #                 main="Landscape Infectivity Heatmap")
  # # do the legend call separately to get the full range
  # color.legend(0,-4,10,-3,legend=c(0,1,2,3,4,5,6,7,8,9,10),
  #              rect.col=color.scale(c(0:8),c(0,1),0,c(1,0)),align="rb")
  par(mar=c(5.1,4.1,4.1,4.1))
  plot(infection_matrix)
  Sys.sleep(2)
}

color.scale
