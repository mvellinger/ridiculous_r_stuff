# Discord theme ----------------------------------------------------------------

bg_color   = "#36393E"
grid_color = "#3E4146"
text_color = "#D6D7D8"
axis_color = "#717E82"
fg_color1  = "#C74343"
fg_color2  = "#1BB9C2"
fg_color3  = "#5A3D6E"

theme_discord <- function(x) {
  theme(
    panel.background = element_rect(fill = bg_color, color = grid_color, size = 0.5),
    plot.background = element_rect(fill = bg_color, color = grid_color, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = grid_color, size = 0.5),
    panel.grid.major.y = element_line(color = grid_color, size = 0.5),
    text = element_text(family = "OpenSans", color = text_color),
    axis.text = element_text(family = "OpenSans", color = axis_color),
    axis.line = element_line(color = axis_color, size = 0.5),
    axis.ticks = element_line(color = axis_color, size = 0.5),
    legend.background = element_rect(fill = bg_color, color = axis_color, size = 0.5),
    legend.position = "bottom",
    legend.key = element_rect(fill = bg_color, color = bg_color)
  )
}

# Scraped data ------------------------------------------------------------------
dataset  <- read.csv("./sfvshoto.csv", stringsAsFactors = FALSE)
colnames <- tolower(names(dataset))
colnames <- gsub(pattern = "[.]", x = colnames, replacement = "_", )
colnames[colnames == "move_name_"] <- "move_name"
names(dataset) <- colnames



movename <- "crouch HK"

move_plot <- function(movename) {
 
   plotdata <- filter(dataset, move_name == movename) %>% select(character, startup, active, on_block, recovery, vtc_on_block) %>% gather(key, value, -character)
  plotdata$value <- as.integer(plotdata$value)
  plotdata$key <- factor(plotdata$key, levels = c("startup", "active", "recovery", "on_block", "vtc_on_block"), ordered = TRUE)
  
  return(
    ggplot(plotdata, aes(x = key, y = value, fill = character, label = value)) + 
      geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.5) + 
      geom_text(stat = "identity", color = text_color, family = "OpenSans" , position = position_dodge(0.8), aes(vjust = ifelse(value > 0, -0.2, 1.2))) + 
      theme_discord() + 
      labs(x = "PHASE", y = "FRAME ADVANTAGE", fill = "CHARACTER") + 
      ggtitle(paste("DATA FOR MOVE:", movename)) + 
      scale_fill_manual(values = c(fg_color3, fg_color1, text_color))
  )
}
