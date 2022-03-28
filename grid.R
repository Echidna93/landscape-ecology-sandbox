
make.inds <- function(n.initial,dim, nI){
  id<-1:n.initial
  xloc<-round(runif(n.initial, min=1, max=dim))
  yloc<-round(runif(n.initial, min=1, max=dim))
  vec<-Cmatrix(yloc,xloc,dim)
  I<-sample(1:n.initial, nI)
  status<-rep("S", times=n.initial)
  status[I]<-"I"
  inds <- data.frame(id = id, xloc=xloc, yloc=yloc, vec=vec, status=status, stringsAsFactors=FALSE) 
  inds
}

calc.dens<-function(hab1, newloc.vec, n.initial, inds){
  consdens<-hab1*0
  idens<-hab1*0
  for(i in 1:length(newloc.vec)) {
    consdens[newloc.vec[i]]=consdens[newloc.vec[i]]+1
    if(inds$status[i]=="I"){
      idens[newloc.vec[i]]=idens[newloc.vec[i]]+1
    }
  }
  dens<-consdens/max(consdens) #normalize to 1 by dividiing conspecific density by maximum number of individuals found in a single cell in the landscape
  Num<-list(consdens,idens,dens) #returns (as list): number of conspecifcs per cell, number of infected conspecifcs per cell, and normalized density
  return(Num)
}

Cmatrix<-function (row, col, nrows) #y,x
{
  vector_loc <- row + (col - 1) * nrows
  return(vector_loc)
}
Rmatrix<-function (vector_loc, nrows) 
{
  col <- floor(vector_loc/nrows) + 1
  row <- vector_loc - (col - 1) * nrows + 1
  return(list(col = col, row = row))
}

get.neighbors <- function(loc, mapdim, rowcol.delta=NULL, n.offset, torus=TRUE, na.val=NA, ...){
  
  if(is.null(rowcol.delta)){
    rowcol.delta<- expand.grid(-n.offset:n.offset,-n.offset:n.offset)
  }
  
  neighborhood<-nrow(rowcol.delta) #e.g. an offset of 1, yields 9 neighbors
  outmat<-matrix(NA,nrow(loc),neighborhood) #rows= number of inds./col.= size of neighborhood
  
  for(i in 1:neighborhood){
    newloc=cbind(loc[,1]+rowcol.delta[i,1], loc[,2]+rowcol.delta[i,2])
    
    if(torus){
      ##Wrap map to torus
      newloc[newloc[,1]<=0,1]=mapdim[1]+newloc[newloc[,1]<=0,1]
      newloc[newloc[,2]<=0,2]=mapdim[2]+newloc[newloc[,2]<=0,2]
      newloc[newloc[,1]>mapdim[1],1] = newloc[newloc[,1]>mapdim[1],1]-mapdim[1]
      newloc[newloc[,2]>mapdim[2],2] = newloc[newloc[,2]>mapdim[2],2]-mapdim[2]
    }else{
      newloc[newloc[,1]<=0,1]=na.val
      newloc[newloc[,2]<=0,2]=na.val
      newloc[newloc[,1]>mapdim[1],1] = na.val
      newloc[newloc[,2]>mapdim[2],2] = na.val
    }
    
    outmat[,i]=Cmatrix(newloc[,2],newloc[,1],nrows=mapdim[1]) #row, col, nrows
  }
  
  return(outmat)
}

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
  
  if (binary == T) {
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
inds<-make.inds(3,10,1)
inds
startloc<-inds$vec
startloc-1
inf_prob=0.2
rec_rate=0.1
p=0.5
H=0.5
beta1=0
beta2=2
beta3=-0.5
percep=3
k=5
density=0.5
dim<-2^(k+1)
n.initial<-round(density*dim^2)
#landscape is returned as a 1D array where
# in a 3x3 matrix landscape[3] = landscape[3][1]
# landscape[9]=landscape[3][3]

hab1<-fracland_mod(k=k, h=H, p=p, binary=TRUE)
hab1
newloc<-as.data.frame(Rmatrix(startloc-1, nrow(landscape1)))

# maxtime=10000, nsim=1, k=5, density=0.5,
# inf_prob=0.2, rec_rate=0.1, p=0.5, H=0.5,
# beta1=0, beta2=2, beta3=-0.5, percep=3

Num<-calc.dens(hab1=hab1,newloc.vec=newloc.vec, n.initial, inds=inds)
#Num
#num<-as.numeric(Num[[1]])
linear.pred<-(beta1*hab1+beta2*as.numeric(Num[[1]])+ beta3*as.numeric(Num[[1]])^2)
linear.pred
nbs<-get.neighbors(loc=newloc, mapdim=dim(linear.pred), n.offset=percep)
nbs