library(ggplot2)
library(gganimate)
#world <- fortify(countriesLow)
# ts <- runif(5, min=0, max=25)
# tx <- runif(5, min=0, max=25)
# ty <- runif(5, min=0, max=25)
# deer <- data.frame(ts=ts, xs=tx, ys=ty)
df <- data.frame(
  x = c(1,2,3,4,5,6,7,8,9,10), 
  y = c(2,3,4,5,6,7,8,9,10,1),
  deer = c(1,1,2,3,1,3,1,3,2,1),
  time = 1:10 )
plot<-ggplot(df, aes(x,y)) +
  geom_point() + 
  transition_states(time)
animate(plot,renderer=gifski_renderer())
#ggplot(data = particle) + geom_point(aes(x = xs, y = ys), col = 'red') + # Generate the plot theme_tufte() + # Make ... labs(x = 'x', y = 'y') + # ... it ... scale_y_continuous(limits = c(-2, 2)) + # ... look ... guides(col = FALSE) + # ... pretty. transition_time(ts) + # And animate! ease_aes('linear')

# {r, echo=F, message=F, warning=F, comment=F, fig.width=10, fig.height=10, fig.align = "left"}
# inputPanel(
#   selectInput("soort", label = "species:",
#               choices = unique(birds$Biotaxon.naam), selected = "Larus fuscus"),
#   radioButtons("type", label = "Type:",
#                c("Original points" = "point", "Hexagonal grid" = "hex", "Rectangular grid" = "rect")),
#   sliderInput("resolution", label = "Resolution:",
#               min = 0.05, max = 1, value = 0.2, step = 0.05)
# )

# renderPlot({
#   birds_soort <- subset(birds, birds$Biotaxon.naam == input$soort)
#   map = map + coord_equal(, xlim = xxlim, ylim = yylim)
#   if(input$type == "rect")  {
#     map = map + geom_bin2d(data = birds_soort, aes(Longitude, Latitude, fill = (..count..)), binwidth = as.numeric(c(input$resolution,input$resolution)))
#   }
#   if(input$type == "hex")  {
#     map = map +stat_bin_hex(data = birds_soort, aes(Longitude, Latitude, fill = (..count..)), binwidth = as.numeric(c(input$resolution,input$resolution)))}
#   if(input$type == "point")  {
#     map = map + geom_point(data = birds_soort, aes(Longitude, Latitude, fill = log10(Numeriekewaarde)), alpha = 0.4, shape = 21, color = "white", size = 2)
#   }
#   map = map + ggtitle(paste("")) +
#     scale_fill_gradient(low = "blue", high = "red") +  # limits=c(0,50), breaks=seq(0, 40, by=10),
#     scale_color_gradient(low = "blue", high = "red") + 
#     theme(axis.text = element_blank(),
#           axis.title = element_blank(),
#           axis.line = element_blank(),
#           axis.ticks = element_blank())
#   map
# })