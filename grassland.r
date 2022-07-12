library(R6)
source('terrain.r')

Grassland <- R6Class("Grassland",
             inherit = Terrain,
             list(
             type = 2,
             score = 0.5
             )
)
