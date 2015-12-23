# Libraries
library(ggplot2)
library(scales)
library(grid)
library(lubridate)
library(dplyr)
library(useful)
library(reshape2)
library(igraph)

set.seed(2545) # Feel free!

#------data section------#

# This is just some dummy data for now, once the proof of
# concept is done, I could replace it with actual data
# plot 4 has percentile type data

names <- seq(0,23,1)
plot1.data <- data.frame(t(round(rnorm(24, mean = 400, sd = 50))))
plot2.data <- data.frame(t(round(rnorm(24, mean = 400, sd = 30))))
plot3.data <- data.frame(t(round(rnorm(24, mean = 400, sd = 90))))
plot4.data <- data.frame(t(rnorm(24, mean = 0, sd = 0.3)))

names(plot1.data) <- names
names(plot2.data) <- names
names(plot3.data) <- names
names(plot4.data) <- names

# use melt() from reshape 2 to make an easy to use dataset
plot1.data <- melt(plot1.data)
plot2.data <- melt(plot2.data)
plot3.data <- melt(plot3.data)
plot4.data <- melt(plot4.data)

# Don't want factors in plot 4
plot4.data$variable <- as.numeric(plot4.data$variable)

# Combined data for density graph
plots.combined <- data.frame(
  "METRIC_A" = plot1.data$value,
  "METRIC_B" = plot2.data$value,
  "METRIC_C" = plot3.data$value
  )
plots.combined <- melt(plots.combined)

# Combined data for density graph
plots.combined.means <- data.frame(
  "METRIC_A" = mean(plot1.data$value),
  "METRIC_B" = mean(plot2.data$value),
  "METRIC_C" = mean(plot3.data$value)
  )
plots.combined.means <- melt(plots.combined.means)

# Lets make a little heatmap data
heatmap <- data.frame(
  "A" = c(0,0,1,1,0,0,0),
  "B" = c(0,1,1,1,2,1,0),
  "C" = c(1,1,1,2,3,2,1),
  "D" = c(1,2,2,3,4,3,2),
  "E" = c(1,1,1,2,3,2,1),
  "F" = c(0,1,1,1,2,1,0),
  "G" = c(1,2,1,1,1,0,0),
  "H" = c(2,3,1,0,0,0,0)
)
heatmap$coord <- seq(0,6,1)
heatmap <- melt(heatmap, id.vars = "coord")


# mini bar graphs
minibar <- data.frame(replicate(6,sample(0:100,3,rep=TRUE)))
names(minibar) <- c("A", "B", "C", "D", "E", "F")
minibar$group <- c("ALEPH", "BETH", "GIMEL")
minibar <- melt(minibar, id.vars="group")

# Technobabble, because why not?
message <- ">There needs to be compressed causes within the weak traced streamline.\n>The matrix network seems to be not reset.\n>They must boost the transfer stream server near the virtual area.\n>I have to calibrate the datas!\n>The re-routed access point node next to the virtual region subnet node.\n>Expanding digital domains modulates the critical networks.\n>The mainframe network seems to be re-routed.\n>They have to invert the audio filter, because the access protocol appears\n>to be within the transfer stream socket.\n>Expanding matrix, overloading network.\n"

noise <- data.frame(replicate(36,sample(0:9,36,rep=TRUE)))
noise$Y <- seq(0,35,1)
noisemelt <- melt(noise, id.vars = "Y")


#------theme section------#

nice_red <- "#942828"

dark_bar_theme <- function() {
    theme(
        axis.text.x = element_text(angle = 90, size = 7, family = "Montserrat", colour = "grey90", hjust = 1),

        axis.text.y = element_text(size = 7, family = "Montserrat", colour = "grey90", hjust = 1),

        plot.title = element_text(colour = "grey90", size = 12, family = "Montserrat", hjust = 1),

        axis.title = element_text(colour = "grey90", size = 8, family = "Montserrat", hjust = 0),

        plot.background = element_rect(fill = "grey20", colour = "grey25"),
        panel.background = element_rect(fill = "grey20"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none"
    )
}

dark_density_theme <- function() {
    theme(
        axis.text.x = element_text(angle = 90, size = 7, family = "Montserrat", colour = "grey90", hjust = 1),

        axis.text.y = element_text(size = 7, family = "Montserrat", colour = "grey90", hjust = 1),

        plot.title = element_text(colour = "grey90", size = 12, family = "Montserrat"),

        axis.title = element_text(colour = "grey90", size = 8, family = "Montserrat", hjust = 0),

        plot.background = element_rect(fill = "grey20", colour = "grey25"),
        panel.background = element_rect(fill = "grey20"),
        panel.grid.major.x = element_line(size=0.5, colour="grey40"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),

        legend.justification=c(1,1),
        legend.position=c(1,1),
        legend.background = element_blank(),
        legend.title = element_text(size = 9,
                                    family = "Montserrat",
                                    colour = "grey90",
                                    hjust = 1),
        legend.text = element_text(size = 5,
                                   family = "Montserrat",
                                   colour = "grey90",
                                   hjust = 1),
        legend.key = element_blank(),
        legend.key.size = unit(20, "native")
    )
}

light_bar_theme <- function() {
    theme(
        axis.text.x = element_text(angle = 90, size = 7, family = "Montserrat", colour = "grey30", hjust = 1),

        axis.text.y = element_text(size = 7, family = "Montserrat", colour = "grey30", hjust = 1),

        plot.title = element_text(colour = "grey30", size = 12, family = "Montserrat", hjust = 1),

        axis.title = element_text(colour = "grey30", size = 8, family = "Montserrat", hjust = 0),

        plot.background = element_rect(fill = "grey95", colour = "grey90"),
        panel.background = element_rect(fill = "grey95"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none"
    )
}


#------gggraph function------#

# pretty neat network graphing function found at:
# http://recology.info/2011/03/basic-ggplot2-network-graphs/
# by Scott Chamberlin
# it still used opts() which is of course deprecated, so I updated
# the function to use theme(), needed to fiddle with it anyways.

# ggplot network graphics functions
# g = an igraph graph object, any igraph graph object
# vplace = type of vertex placement assignment, one of rnorm, runif, etc.

gggraph <- function(g, vplace = rnorm) {

    require(ggplot2)

    g_ <- get.edgelist(g)
    g_df <- as.data.frame(g_)
    g_df$id <- 1:length(g_df[,1])
    g_df$id <- factor(g_df$id)
    g_df <- melt(g_df, id=3)
    xy_s <- data.frame(value = unique(g_df$value),
                       x = vplace(length(unique(g_df$value))),
                       y = vplace(length(unique(g_df$value))))
    g_df2 <- merge(g_df, xy_s, by = "value")

    p <- ggplot(g_df2, aes(x, y)) +
        geom_point(colour = nice_red, size = 5) +
        geom_line(size = 1, aes(group = id, linetype = id), colour = nice_red) +
        light_bar_theme() +
        ggtitle("NODE_CONN")

    p

}

# let's use one of the examples from the webpage:
g <- grg.game(10, 4.5, torus=TRUE)
P9 <- gggraph(g, rnorm)

#------plots section------#

# Dark barplot 1, let's give this one it's y-grid back for visual interest
P1 <- ggplot(data = plot1.data, aes(x=variable, y=value)) +
      geom_bar(stat = "identity", colour = "grey80", fill = "grey60", width = 0.5, size = 0.5) +
      geom_hline(aes(yintercept=mean(plot1.data$value)), colour=nice_red, linetype="dashed") +
      dark_bar_theme() +
      ggtitle("METRIC A") +
      xlab("HOUR") +
      ylab("VALUE") +
      theme(panel.grid.major.x = element_line(colour = "grey60", linetype = 3))

# Dark barplot 2, let's give this one it's y-grid back for visual interest
P2 <- ggplot(data = plot2.data, aes(x=variable, y=value)) +
      geom_bar(stat = "identity", colour = "grey80", fill = "grey60", width = 0.5) +
      geom_hline(aes(yintercept=mean(plot2.data$value)), colour=nice_red, linetype="dashed") +
      dark_bar_theme() +
      ggtitle("METRIC B") +
      xlab("HOUR") +
      ylab("VALUE") +
  theme(panel.grid.major.x = element_line(colour = "grey60", linetype = 3))

# Dark barplot 3, let's give this one it's y-grid back for visual interest
P3 <- ggplot(data = plot3.data, aes(x=variable, y=value)) +
      geom_bar(stat = "identity", colour = "grey80", fill = "grey60", width = 0.5) +
      geom_hline(aes(yintercept=mean(plot3.data$value)), colour=nice_red, linetype="dashed") +
      dark_bar_theme() +
      ggtitle("METRIC C") +
      xlab("HOUR") +
      ylab("VALUE") +
      theme(panel.grid.major.x = element_line(colour = "grey60", linetype = 3))

# Dark dot-and-line graph
P4 <- ggplot(data = plot4.data, aes(x=variable, y=value)) +
      geom_point(stat = "identity", colour = "grey90",  size = 4) +
      geom_point(stat = "identity", colour = "grey60",  size = 3) +
      geom_line(stat= "identity", colour = "grey60", linetype = 2) +
      geom_hline(y = 0, colour = "grey40") +
      dark_bar_theme() +
      ggtitle("METRIC D") +
      xlab("HOUR") +
      ylab("VALUE")

# Dark density graph
P5 <- ggplot(data = plots.combined, aes(x=value, colour = variable)) +
      geom_line(stat = "density", adjust = .25 ) +
      dark_density_theme() +
      xlab("") +
      ylab("DENSITY") +
      scale_colour_manual(values = c("grey30", "grey60", "grey90"), name="VAR_001") +
      ggtitle("VAR_001 DENSITY")

# Dark radial plot, which is utterly pointless
P6 <- ggplot(data=plots.combined.means,
      aes(x=variable, y=value, fill=variable)) +
      geom_bar(stat = "identity", alpha = 0.5) +
      coord_polar(theta = "x") +
      dark_bar_theme()  +
      theme(panel.grid.major.y = element_line(color = "grey95", linetype = 2),
            axis.text.x = element_blank(),
            plot.background = element_rect(fill = "grey20", colour = "grey20")) +
      geom_text(aes(color = variable, y=0.7*max(plots.combined.means$value),
      label = round(plots.combined.means$value), digits = 2),
      family = "Montserrat", size = 4) +
      geom_text(aes(y=0.3*max(plots.combined.means$value),
      label = c("A", "B", "C")),
      family = "Montserrat",
      color = "grey90", size = 4) +
      scale_fill_manual(values = c("grey30", "grey60", "grey90")) +
      scale_colour_manual(values = c("grey90", "grey90", nice_red)) +
      xlab("") +
      ylab("") +
      ggtitle("VAR_001_MEAN")

# Tiny heatmap
P7 <- ggplot(data = heatmap, aes(x = variable, y = coord, fill = value)) +
      geom_tile(colour = "grey95") +
      dark_bar_theme() +
      scale_fill_gradient(low = "grey30", high = "grey95") +
      scale_y_continuous(breaks=seq(0,7,1)) +
      ggtitle("COORD_GRID") +
      xlab("LONG") +
      ylab("LAT")

# Running out of ideas here
P8 <- ggplot(data = plot4.data, aes(x = value)) +
      geom_line(stat = "density", adjust = .25, colour = nice_red ) +
      dark_density_theme() +
      xlab("") +
      ylab("DENSITY") +
      scale_colour_manual(values = c("grey30", "grey60", "grey90"), name="VAR_001") +
      ggtitle("VAR_004 DENSITY") +
      theme(panel.grid.major.x = element_line(size = 0.5,
                                            colour = "grey40",
                                          linetype = "dotted"),
            panel.grid.major.y = element_line(size = 0.5,
                                            colour = "grey40",
                                          linetype = "dotted"),
            axis.text.x = element_text(angle = 45),
            plot.background = element_rect(fill = "grey20", colour = "grey20")) +
  coord_polar(theta = "x", start = 0)


P10 <- ggplot(data = minibar, aes(x=group, y=value, fill=group)) +
      geom_bar(stat="identity", width = 0.3) +
      facet_grid(.~variable) +
      light_bar_theme() +
      scale_fill_manual(values = c("#2a5e55", "#9e2b38", "grey30")) +
      xlab("") +
      ylab("") +
      ggtitle("MANIFOLD_LEVEL")


P11 <- ggplot(data = noisemelt, aes(x = variable, y = Y)) +
      geom_tile(fill = "grey90", colour = "grey95") +
      light_bar_theme() +
      geom_text(aes(label = value), size = 2, colour="grey30") +
      coord_fixed() +
  xlab("") +
  ylab("") +
  ggtitle("SIG_BLOCK") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

#------layout section------#

# Open new file device
png(filename="test.png", width = 1400, height = 900, units = "px")

# Open new grid page
grid.newpage()

# Layout grid
# 18 rows, 28 columns means each cell is 10px * 10px
pushViewport(viewport(layout = grid.layout(90, 140)))

# Add background rectangle
grid.rect(gp = gpar(fill = "grey20", col = "grey10"))

# Add container rectangle
grid.rect(gp = gpar(fill = "grey95", col = "grey80"), vp = vplayout(2:89, 2:139))

# Add dark section to the right
grid.rect(gp = gpar(fill = "grey20", col = "grey10"), vp = vplayout(2:89, 50:139))

# Add in dark wide plots
print(P1, vp = vplayout(2 :23, 50:120))
print(P2, vp = vplayout(25:45, 50:120))
print(P3, vp = vplayout(47:67, 50:120))
print(P4, vp = vplayout(69:89, 50:120))

# Add in dark narrow plots
print(P5, vp = vplayout(2 :23, 121:139))

# we need to put a little rectangle under plot 6 to retain the
# border, because coord_polar enforces the aspect ratio, long story.
grid.rect(gp = gpar(fill = "grey20", col = "grey25"), vp = vplayout(25:45, 121:139))

print(P6, vp = vplayout(24:46, 121:139))
print(P7, vp = vplayout(47:67, 121:139))

# same trick as for P6
grid.rect(gp = gpar(fill = "grey20", col = "grey25"), vp = vplayout(69:89, 121:139))

print(P8, vp = vplayout(69:89, 121:139))

print(P9, vp = vplayout(3 :44, 3:48))
print(P10, vp = vplayout(45:64, 3:48))
print(P11, vp = vplayout(65:88, 3:24))
print(P11, vp = vplayout(65:88, 25:48))
# complete .png file
dev.off()
