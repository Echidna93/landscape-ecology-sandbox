library(R6)
source('terrain.r')

Forest <- R6Class("Forest",
  inherit = Terrain,
  list(
  type = 1,
  score = 0.5)
)
# x<-Forest$new()
# x