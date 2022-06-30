library(dplyr)
library(ggplot2)
library(ggiraph)

donut_data <- data.frame(type = c("Gray", "Green"), value = c(44, 73)) %>%
  mutate(
    percentage = value / sum(value),
    hover_text = paste0(type, ": ", value)
  ) %>%
  mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))

donut_plot <- ggplot(donut_data, aes(y = value, fill = type)) +
  geom_bar_interactive(
    aes(x = 1, tooltip = hover_text),
    width = 0.3,
    stat = "identity",
    show.legend = FALSE
  ) +
  annotate(
    geom = "text",
    x = 0,
    y = 0,
    label = donut_data[["percentage_label"]][donut_data[["type"]] == "Green"],
    size = 20,
    color = "#2C3E50"
  ) +
  scale_fill_manual(values = c(Gray = "lightgray", Green = "#2C3E50")) +
  coord_polar(theta = "y") +
  theme_void()

ggiraph(ggobj = donut_plot)

#formattable::percent

#
# ejpru<- df_players %>% 
#   filter(Player == "Rudy Gobert") %>%
#   filter(Season == c("18-19","19-20")) %>%
#   group_by(Player) %>%
#   summarise(across(everything(), list(mean))) %>%
#   dplyr::select(Player, MPG_1, PER_1, BPM_1, WS_1) %>%
#   rename(MPG = MPG_1, PER = PER_1, BPM = BPM_1, WS = WS_1) %>%
#   mutate(MPG_diff = MPG - 16,
#          PER_diff = PER - 14.3,
#          BPM_diff = BPM - 0,
#          WS_diff = WS - 1.5) %>%
#   mutate(percMPG = 0.43,
#          percPER = 0.32,
#          percBPM = 0.2,
#          percWS = 0.67)
# 

donutDatamake <- function(){
  donut_data <- percPlayer_data()
  donut_data[2,]$percMPG <- 1-donut_data[1,]$percMPG
  donut_data$Player <- as.factor(donut_data$Player)

  donut_data

}