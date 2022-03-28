# make a square matrix for now
make_landscape_matrix <- function(row, col){
   matrix(sample(c(0,1), replace=TRUE, size=row*col), nrow=row)
}
make_infection_matrix<-function(row,col){
   matrix(sample(c(0), replace=TRUE, size=row*col), nrow=row)
}
update_infection_matrix<-function(inf_matrix, data_frame){
  for(i in 1:nrow(data_frame)){
    if(data_frame[i,]$status == "I")
      inf_matrix[data_frame[i,]$xloc][data_frame[i,]$yloc]<-inf_matrix[data_frame[i,]$xloc][data_frame[i,]$yloc] + 1
  }
  inf_matrix
}
move<-function(data_frame){
  for(i in 1:nrow(data_frame)){
    nbs<-get_neighbors(c(data_frame[i,]$xloc,data_frame[i,]$yloc), num_row, num_col)
    new_loc<-nbs[[round(runif(1,1,length(nbs)))]]
    dframe[i,]$xloc<-new_loc[[1]]
    dframe[i,]$yloc<-new_loc[[2]]
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
      if(!(loc[1]+i < 1 | loc[1]+i > nrow) | (loc[1]+j < 1 | loc[1]+j > ncol)){
        l[[k]] <- c(loc[1] + i, loc[2] + j)
        k<-k+1
      }
    }
  }
  l 
}

num_row=5
num_col=5
deer<-make_deer(3,num_row,1)
deer
landscape<-make_landscape_matrix(5,5)
landscape[5]
infection_matrix<-make_infection_matrix(5,5)
# infection_matrix<-update_infection_matrix(infection_matrix, deer)
#infection_matrix[deer[1,]$xloc][deer[1,]$yloc]
deer[1,]$xloc
# infection_matrix[
# deer<-move(deer)