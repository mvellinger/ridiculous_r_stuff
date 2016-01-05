# libraries
library(ggplot2)
library(dplyr)
library(grid)
library(scales)
library(ggmap)
library(maptools)
library(rgeos)
library(rworldmap)
library(grid)
library(useful)
library(Cairo)


#windowsFonts(Arial=windowsFont("TT Arial"))

# optional live data retrieval
 data <- CountryCodeSpread("2016-01-04", "2016-01-05")
 data <- as.data.frame(data)
 names(data) <- c("issuing_country_code", "result")
 write.csv(data, "countrydata.csv", row.names = FALSE)

#----- data wrangling -----#

# Load dataset
data <- read.csv("countrydata.csv", stringsAsFactors = F)

# Germany has the wrong country code in our data, only "D" instead of ISO "DEU"
# so we're just going to fix that.
data$issuing_country_code[data$issuing_country_code == "D"] <- "DEU"

# Fix the names so they match the map
names(data) <- c("id", "count")

# we need to create some bins, or our map will be awful due to the vastly higher number
# of UK evidence

data$bins <- cut(data$count, 
                 breaks = c(0, 10, 50, 100, 200, Inf), 
                 labels = c("1-10","10-50", "50-100", "100-200", "200+"))




#----- map wrangling -----#

# Read shapefile
map <- getMap(resolution = "low")
map <- subset(map, continent != "Antarctica")

# Fortify into data frame
map.f <- fortify(map, region = "ISO3.1")

# Merge with data set
merge.map.f <- merge(map.f, data, by = "id", all.x = TRUE)

# make sure the order was not ruined
final.plot <- merge.map.f[order(merge.map.f$order), ]

#get mean lat/long for each polygon
centroids <- as.data.frame(coordinates(map))
names(centroids) <- c("long", "lat")  #more sensible column names
centroids$id <- map$ISO3.1
labels <- merge(centroids, data, by = "id", all.x = T)
annotations <- labels[is.na(labels$bin) == FALSE, ]

#------ theme section -----#

# define theme
dark_map_theme <- function() {
  theme(
    axis.text.x = element_text(angle = 90, size = 7, family = "Montserrat", colour = "grey90", hjust = 1),
    axis.text.y = element_text(size = 7, family = "Montserrat", colour = "grey90", hjust = 1),
    plot.title = element_text(colour = "grey90", size = 12, family = "Montserrat", hjust = 1),
    axis.title = element_text(colour = "grey90", size = 8, family = "Montserrat", hjust = 0),
    plot.background = element_rect(fill = "grey20", colour = "grey25"),
    panel.background = element_rect(fill = "grey20"),
    panel.grid.major.x = element_line(colour = "grey80", linetype = "dotted", size = 0.2),
    panel.grid.minor.x = element_line(colour = "grey60", linetype = "dotted", size = 0.2),
    panel.grid.major.y = element_line(colour = "grey80", linetype = "dotted", size = 0.2),
    panel.grid.minor.y = element_line(colour = "grey60", linetype = "dotted", size = 0.2),
    panel.border = element_blank(),
    legend.justification=c(0,0),
    legend.position=c(0,0),
    legend.background = element_rect(fill = "grey20", colour = "grey25"),
    legend.title = element_text(size = 12,
                                family = "Montserrat",
                                colour = "grey90",
                                hjust = 1,
                                face = "plain"),
    legend.text = element_text(size = 8,
                               family = "Montserrat",
                               colour = "grey90",
                               hjust = 1),
    legend.key = element_blank()
  )
}


#----- make plot ------#
# make plot

P <- ggplot() +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = bins), 
               alpha = 0.5, 
               size = 0.125
               ) +
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group), 
               fill = NA, 
               size = 0.125, 
               colour = "grey95", 
               show_guide = FALSE
               ) +
  coord_map() +
  labs(title = "EVIDENCE BY COUNTRY DEC 2015") +
  labs(fill = "CUSTOMERS") +
  scale_x_continuous(breaks = seq(-180,180,10), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-60,80,10), limits = c(-60,75), expand = c(0,0)) +
  xlab("LONGITUDE") +
  ylab("LATITUDE") + 
  scale_fill_manual(values=c("grey40", "grey60", "grey80", "yellow", "red")) +  
  dark_map_theme() + 
  geom_point(data = annotations, aes(long, lat), size = 3.5, colour = "red") +
  geom_text(data = annotations, aes(long, lat, label = count), size = 1.2, colour = "grey95", hjust = 0.5)


#Grid

# Open new file device
#png(filename="test.png", width = 1920, height = 1080, units = "px")
png(filename = "test.png", width = 15, height = 10, units = "in", res = 300)
# Open new grid page
grid.newpage()

# Layout grid
# 108 rows, 192 columns means each cell is 10px * 10px
pushViewport(viewport(layout = grid.layout(108, 192)))
# Add background rectangle
grid.rect(gp = gpar(fill = "grey20", col = "grey10"))
print(P, vp = vplayout(2 :107, 2:191))
dev.off()