library(R6)

Deer <- R6Class("Deer", public = list(
  id = NULL,
  sex = NULL,
  xloc = NULL,
  yloc = NULL,
  status = NULL,
  past_locs = NULL,
  initialize = function(id = NA, sex = NA, xloc = NA, yloc = NA, status = NA, past_locs = list()){
    self$id <- id
    self$sex <- sex
    self$xloc <- xloc
    self$yloc <- yloc
    self$status <- status
    self$past_locs <- past_locs
  })
)
# 
# d<-Deer$new(1, "M", 1, 2, "I")
# d
