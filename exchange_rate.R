library(ggplot2)
library(scales)
library(jsonlite)
library(dplyr)

# Set plot theme
currency_theme = function(base_size = 10) {
  bg_color = "#f4f4f4"
  bg_rect = element_rect(fill = bg_color, color = bg_color)
  
  theme_bw(base_size) +
    theme(text = element_text(family = "Open Sans"),
          plot.title = element_text(family = "Open Sans"),
          plot.background = element_rect(fill=bg_color),
          panel.background = bg_rect,
          panel.border = element_rect(colour = "grey60", size = 0.35),
          legend.background = bg_rect,
          panel.grid.major = element_line(colour = "grey70", size = 0.25, linetype = 3),
          panel.grid.minor = element_line(colour = "grey70", size = 0.25, linetype = 3),
          legend.key.width = unit(1.5, "line"),
          legend.key = element_blank(),
          legend.position="bottom")
}

# Data collection
# First we'll run a small loop to get historical data, and then we will fetch
# the latest value and append that.
#
# currency exchange data supplied by http://fixer.io/ please be courteous and
# do not moles their API too much.

# Set the date range for our historical data, you can change this to show more
# or less history, 180 days seemed like a reasonable period to show change 
# over time.
daterange <- seq(Sys.Date() - 180, Sys.Date()-1, 1)

# Empty model to stick our data into
dataset <- data.frame()

# Get historical values
for (i in seq_along(daterange)) {
  
  # The base currency for this graph is set to "GBP" and I am showing the rate
  # vs. EUR and USD, you can change this as you see fit, see the fixit.io API 
  # documentation for more information.
  data <- fromJSON(paste("http://api.fixer.io/", daterange[i], "?base=GBP",sep = ""))
  
  api_data <- data.frame("date" = daterange[i],
                         "USD"  = data$rates$USD,
                         "EUR"  = data$rate$EUR)
  
  dataset <- rbind(dataset, api_data)
}

# Get most recent data
api_data_current <- fromJSON("http://api.fixer.io/latest?base=GBP")
current_values <-  data.frame("date" = Sys.Date(),
                              "USD"  = api_data_current$rates$USD,
                              "EUR"  = api_data_current$rate$EUR)

# Append current data to dataset
dataset <- rbind(dataset, current_values)

# Convert to long form for plot
plot_data <- gather(dataset, variable, value, -date)

# Label data
label_data <- gather(current_values, variable, value, -date)



P1 <- ggplot(data = plot_data, 
             aes(x     = date,
                 y     = value,
                 color = variable)) + 
  geom_line(size = 0.5) + 
  geom_point(data = label_data,
             aes(x     = date,
                 y     = value,
                 color = variable), 
             size = 1) + 
  geom_text(data = label_data, 
            aes(x     = date, 
                y     = value, 
                color = variable, 
                label = paste(variable, "\n", value, sep ="")),
            size = 2.5, 
            nudge_x = 8, 
            show.legend = FALSE) +
  currency_theme() + 
  scale_color_manual(values = c("steelblue", "red")) + 
  scale_y_continuous(breaks = pretty_breaks(n = 6), 
                     limits = c(min(plot_data$value) - 0.1, max(plot_data$value) + 0.1)) +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%B", 
               expand = c(0.06,0)) + 
  labs(x     = "Date", 
       y     = "Exchange rate", 
       title = "GBP exchange rates", 
       color = "Currency") 

