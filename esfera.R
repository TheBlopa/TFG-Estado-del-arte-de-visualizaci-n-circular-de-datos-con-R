temp <- read.table("amaps.txt", skip=1, header = TRUE, na.strings="9999.0000")

breaks <- c(-5.1, -4, -2, -1, -.5, -.2, .2, .5, 1, 2, 4, 11.1)
lesCouleurs <- cbind(
  val = levels(cut(range(breaks), breaks = breaks)),
  col = c("#8600FF", "#3F94FE", "#77CAFD", "#99EEFF", "#D9FFD9", "#FFFFFF",
          "#FFFF4C", "#FFCC00", "#FF7E00", "#FF0000", "#5E0000")
)
lesCouleurs <- data.frame(lesCouleurs, stringsAsFactors = FALSE)
colnames(lesCouleurs) <- list("val", "col")
# Colours in 8 digits, the last two digits are for transparency
lesCouleurs$col <- paste(lesCouleurs$col,"FF", sep = "")

temp$interval <- cut(temp$array.i.j, breaks = breaks)

library(rworldmap)
library(dplyr)
library(ggplot2)
library(geosphere)
library(gpclib)

worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

rotate_map <- function(angle = 0){
  ggplot() + 
    geom_tile(data = temp, aes(x = lon, y = lat, fill = interval), alpha = 0.8) +
    scale_fill_manual("intervalos\n 2019/20", breaks = lesCouleurs$val, values = lesCouleurs$col) +
    geom_path(data = world.df, aes(x = long, y = lat, group = group)) +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45) +
    coord_map("ortho", orientation=c(25, angle, 0)) +
    theme_minimal() +
    ggtitle("Cambio de Temperatura en Asia") +
    theme(plot.title = element_text(hjust = 0.7))
}

rotate_map(80)



#donut#######################################################
library(ggplot2)

# Create test data.
data <- data.frame(
  table(mtcars$carb)
)

# Compute percentages
data$fraction <- data$Freq / sum(data$Freq)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$Freq)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette="Set1") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) + labs(fill = "N Carburadores") +
  theme_void() + labs(title="Coches\npor Carburadores") +
  theme(legend.position="bottom", plot.title = element_text(vjust = - 55,hjust = 0.5)) 

##############################################
require(ggplot2)
require(ggiraphExtra)
require(plyr)
ggDonut(browsers,aes(donuts=version,count=share), interactive = TRUE)
