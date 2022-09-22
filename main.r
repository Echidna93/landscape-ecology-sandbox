# library(tidyverse)
library(plotrix)
library('plot.matrix')
source("deer.R")

percep <- 2
num_row=5
infectivity_threshold=2
num_col<-5
is_binary = FALSE



#' Create neutral landscape maps
#' 
#' Use standard methods to generate fractal maps. Binary and continuous surfaces may be produced.
#' 
#' @param k integer. The extent of the map (2^k+1)^2 pixels
#' @param h numeric. Level of aggregation in the map.
#' @param p numeric (0,1). The proportion of map in habitat=1
#' @param binary logical. If TRUE, a 0/1 categorical landscape is produced.
#' @author Shannon Pittman, James Forester, modified by Lauren White
#' @export
#' @example examples/neutral.landscape_example.R
fracland_mod <- function(k, h, p, binary = TRUE) {
  ## Function for creating neutral landscapes Shannon Pittman University of Minnesota May, 2013 k = the extent of the map (2^k+1)^2 pixels h =
  ## how clumped the map should be (ranging from ?? to ??) -- weird behavior at higher values p = proportion of map in habitat 1 binary =
  ## plotflag == if TRUE will plot a filled contour version of the matrix
  
  ## function call: testmap=land(6,1,.5,FALSE,TRUE)
  A <- 2^k + 1  # Scalar-determines length of landscape matrix
  
  #Right now, as written (1-p) represents the amount of habitat listed as "1"
  B <- matrix(0, A, A)  # Creates landscape matrix
  
  B[1, 1] <- 0
  B[1, A] <- 0
  B[A, 1] <- 0
  B[A, A] <- 0
  
  
  iter <- 1
  for (iter in 1:k) {
    scalef <- (0.5 + (1 - h)/2)^(iter)
    
    d <- 2^(k - iter)
    
    # ALL SQUARE STEPS#
    for (i in seq(d + 1, A - d, 2 * d)) {
      for (j in seq(d + 1, A - d, 2 * d)) {
        B[i, j] <- mean(c(B[i - d, j - d], B[i - d, j + d], B[i + d, j - d], B[i + d, j + d])) + scalef * rnorm(n = 1)
      }
    }
    
    # OUTSIDE DIAMOND STEP#
    for (j in seq(d + 1, A - d, 2 * d)) {
      B[1, j] <- mean(c(B[1, j - d], B[1, j + d], B[1 + d, j])) + scalef * rnorm(n = 1)
      B[A, j] <- mean(c(B[A, j - d], B[A, j + d], B[A - d, j])) + scalef * rnorm(n = 1)
    }
    
    for (i in seq(d + 1, A - d, 2 * d)) {
      B[i, 1] <- mean(c(B[i - d, 1], B[i + d, 1], B[i, 1 + d])) + scalef * rnorm(n = 1)
      B[i, A] <- mean(c(B[i - d, A], B[i + d, A], B[i, A - d])) + scalef * rnorm(n = 1)
    }
    
    # INSIDE DIAMOND STEP#
    if (2 * d + 1 <= A - 2 * d) {
      for (i in seq(d + 1, A - d, 2 * d)) {
        for (j in seq(2 * d + 1, A - 2 * d, 2 * d)) {
          B[i, j] <- mean(c(B[i - d, j], B[i + d, j], B[i, j - d], B[i, j + d])) + scalef * rnorm(n = 1)
        }
      }
      
      for (i in seq(2 * d + 1, A - 2 * d, 2 * d)) {
        for (j in seq(d + 1, A - d, 2 * d)) {
          B[i, j] <- mean(c(B[i - d, j], B[i + d, j], B[i, j - d], B[i, j + d])) + scalef * rnorm(n = 1)
        }
      }
    }
    
    iter <- iter + 1
  }
  
  if (binary) {
    R <- sort(B)
    PosR <- (1 - p) * length(R)  #larger values become habitat, designated as 1
    pval <- R[PosR]
    T1 <- which(B > pval)
    T2 <- which(B <= pval)
    B[T1] <- 1  #habitat is 1
    B[T2] <- 0
  } 
  return(B)
}



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
make_density_matrix <- function(numrow, numcol, inds){
  d_mat<-matrix(sample(c(0), replace=TRUE, size=numrow*numcol), nrow=numrow)
  for(i in 1:length(inds)){
    # temp mat holds number of deer want density
    d_mat[inds[[i]]$xloc,][inds[[i]]$yloc] <- d_mat[inds[[i]]$xloc,][inds[[i]]$yloc] + 1
  }
  for(i in 1:numrow){
    for(j in 1:numcol){
        d_mat[i,][j] <- d_mat[i,][j] / length(inds)
      }
  }
  d_mat
}

#' initiates an infection matrix of all 0's
#' @param k number of rows in matrix
#' @export
make_infection_matrix<-function(k){
   A <- 2^k + 1
   matrix(0,A,A)
}

#' Helper function
#' want it to return a list of infected deer
#' @param data_frame holds data about deer
#' @export
get_infected_deer<-function(inds){
  infected_inds<-rep()
  for(i in 1:length(inds)){
    print(inds[[i]]$status)
    if(inds[[i]]$status == "I"){
      print('here')
      infected_inds<-append(infected_inds, i)
      print(infected_inds)
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
update_infection_statuses<-function(inds, infection_matrix, infectivity_threshold){
  inf_ind_indx<-get_infected_deer(inds)
  # TODO break this off into it's own function, maybe is_same_location
  for(i in 1:length(inds)){
    # only check for uninfected susceptible individuals
    if(inds[[i]]$status=="S"){
      # case 1 cell is above some designated threshold where infection is guaranteed
      # dynamics here may change
      if(is_cell_inf_val_above_threshold(infection_matrix[inds[[i]]$xloc + inds[[i]]$yloc],
                                         infectivity_threshold)){
        inds[[i]]$status <- "I"
      }
      # case 2 infected and uninfected individual are occupying same cell
      # automatically make uninfected individual infected
      else{
        for(j in 1:length(inf_ind_indx)) {
          print(inf_ind_indx[j])
          if((inds[[i]]$xloc==inds[[inf_ind_indx[j]]]$xloc & 
              inds[[i]]$yloc==inds[[inf_ind_indx[j]]]$yloc) &
              !(inds[[i]]$id==inf_ind_indx[j])){ # verify these aren't the same
           inds[[i]]$status <- "I"
          }
        }
      }
    }
  }
  inds
}

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
update_infection_matrix<-function(inf_matrix, inds){
  for(i in 1:length(inds)){
    if(inds[[i]]$status == "I")
      inf_matrix[inds[[i]]$xloc,inds[[i]]$yloc]<-inf_matrix[inds[[i]]$xloc,inds[[i]]$yloc] + 1
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
#' @param sim_iter simulation iterator, current time step in
#' @export
move<-function(inds, landscape, nrow, ncol, binary, sim_iter){
  for(i in 1:length(inds)){
    d_mat<-make_density_matrix(nrow,ncol,inds)
    nbrs<-get_neighbors(c(inds[[i]]$xloc,inds[[i]]$yloc), num_row, num_col)
    # new_loc<-nbrs[[round(runif(1,1,length(nbrs)))]]
    new_loc<-make_decision(landscape=landscape, d_mat=d_mat, nbrs=nbrs, binary)
    inds[[i]]$push_location(sim_iter)
    inds[[i]]$xloc<-new_loc[[1]]
    inds[[i]]$yloc<-new_loc[[2]]
  }
}

#' Chooses best possible landscape component to move to
#' TODO alter in the case of an make_decision being fed an empty list, make
#' else case
#' TODO implement sorting function
#' @param
#' @export
make_decision<-function(landscape, d_mat, nbrs, binary){
  # assign decision to be the first element by default--make comparison
  decision_vec<-c(nbrs[[1]][1],nbrs[[1]][2])
  decision_val<-landscape[nbrs[[1]][1],][nbrs[[1]][2]] - d_mat[nbrs[[1]][1],][nbrs[[1]][2]]
  # recalculate value of landscape matrix to account for conspecific dens.
      if(binary){
        i<-1
        while(!is_habitat(landscape, nbrs[[i]][1], nbrs[[i]][2])){
            #i<-round(runif(1, min=1, max=length(nbrs[1,])))
            i<-i+1
            print(i)
          }
        }
      else{
        for(j in 1:length(nbrs)){
          crnt_adjstd_val <- (landscape[nbrs[[j]][1],][nbrs[[j]][2]] - d_mat[nbrs[[j]][1],][nbrs[[j]][2]])
          if(crnt_adjstd_val > decision_val){
            # want decision to take into account conspecific density
            # want list of values from the landscape matrix
            decision_vec<-c(nbrs[[j]][1], nbrs[[j]][2])
          }
        }
      }
  decision_vec
}

#' helper function
#' creates xloc and yloc for initialization of deer
#' ensures that deer are only placed on habitat in the binary case
#' 
is_habitat <- function(landscape, xloc, yloc){
  (landscape[xloc,][yloc] == 1)
}

#' initiates a data frame of deer
#' @param n.initial # deer
#' @param dim dimension of a vector for random assignment of location
#' @param nI # infected individuals to start
#' @export
make_deer <- function(n_initial, dim, nI, binary, landscape){
  I<-sample(1:n_initial, nI)
  status<-rep("S", times=n_initial)
  sex = c("M", "F")
  status[I]<-"I"
  print(status)
  inds <- list()
  # create desire num of individuals n_initial
  # id = NA, sex = NA, xloc = NA, yloc = NA, status = NA, past_locs = list()
  for(i in 1:n_initial){
    inds <- append(inds, Deer$new(i,
                                  sex = sex[round(runif(1, min=1, max=2))], 
                                  xloc = round(runif(1, min=1, max=dim)),
                                  yloc = round(runif(1, min=1, max=dim)),
                                  status=status[i]))
  }
  #inds <- data.frame(id = id, xloc=xloc, yloc=yloc, status=status, stringsAsFactors=FALSE) 
  inds
}

#' Helper function
#' returns neighborhood of cells as a list of vectors
#' @param loc current coordinates of individual
#' @param nrow # of rows in landscape matrix
#' @param ncol # columns in landscape matrix
#' @export
get_neighbors<-function(loc, nrow, ncol){
  k=1
  l<-list()
  # check if either x,y element of loc is greater than
  # the dimension of the landscape matrix
  for(i in -percep:percep){
    for(j in -percep:percep){
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
check_inds_locs<-function(ind, inds){
  for(i in 1:length(inds)){
    if(is_same_location(ind, inds[[i]]) & is_not_same_id(ind, inds[[i]])){
      
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

# recover_inds<-function(data_frame, gamma){
#   infected<-which(data_frame$status=="I" ) #which individuals are currently infected?
#   if(length(infected>0)){
#     rec.prob<-runif(length(infected), min=0, max=1)
#     for (i in 1:length(infected)){
#       if(rec.prob[i]<= gamma){
#         data_frame$status[infected[i]]<-"R"
#       }
#     }
#   }
#   return(data_frame)
# }


# CONSTANTS
# TODO look into making these changeable by a user in an x11() window?

landscape <-fracland_mod(k=5,h=0.5,p=0.5,binary=is_binary)
landscape
infection_matrix<-make_infection_matrix(5)
deer<-make_deer(16,num_row,1,is_binary, landscape)
deer
gamma=0.1 # recovery rate
for(iter in 1:50){
  deer<-update_infection_statuses(deer, infection_matrix, infectivity_threshold)
  infection_matrix<-update_infection_matrix(infection_matrix, deer)
  move(deer, landscape, nrow(landscape), ncol(landscape), is_binary, iter)
  d_mat<-make_density_matrix(5,5,deer)
  # print(deer[[1]]$past_locs)
  par(mfrow=c(1,2))
  plot(infection_matrix,
            axis.col=NULL,
            axis.row=NULL,
            xlab="",
            ylab=""
       )
  plot(landscape,
            col=c("green", "white"),
            axis.col=NULL,
            axis.row=NULL,
            xlab="",
            ylab="")
  Sys.sleep(2)
}

color.scale
