library(R6)
source('terrain.r')

Water <- R6Class("Water",
                  inherit = Terrain, 
                  list(
                  type = 3,
                  score = 0.1)
)
w<-Water$new()