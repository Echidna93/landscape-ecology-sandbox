library(R6)

Deer <- R6Class("Deer", list(
  id = NULL,
  sex = NULL,
  xloc = NULL,
  yloc = NULL,
  status = NULL,
  initialize = function(id, sex, xloc, yloc, status){
    self$id <- id
    self$sex <- sex
    self$xloc <- xloc
    self$yloc <- yloc
    self$status <- status
  })
)

d<-Deer$new(1, "M", 1, 2, "I")
d