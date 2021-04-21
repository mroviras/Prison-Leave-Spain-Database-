###
title: "Prison Leaves Graph + Interactive Graph"
author: "Marti Rovira"
date: "21/04/2021"
output: html_document
###
  
### Libraries

library(tidyverse)
library(plotly)
library(htmlwidgets)

### Transforming the dataset into the right format

leaves_dbs <- read_csv("Evolution_prison_leaves_Spain.csv") # Opening the file

leaves_dbs <- leaves_dbs %>%
  mutate(
    `SGIP Ratio` = `SGIP leaves` / `SGIP convicted inmates`, 
    # Creating the ratio of leaves x prisoner for SGIP
    `CAT Ratio` = `CAT leaves` / `CAT convicted inmates`
    # Creating the ratio of leaves x prisoner for the Catalan Administration
  ) 

leaves_dbs_long <- leaves_dbs %>%
  select(`Year`,
         `CAT no returns`,
         `SGIP no returns`,
         `CAT Ratio`,
         `SGIP Ratio`) %>% 
  # Only selecting those variables of interest
  pivot_longer(cols = !`Year`, names_to = "Variable") %>% 
  # Changing the database from wider to longer
  mutate(Admin = if_else(str_detect(Variable, '^SGIP'), "SGIP", "CAT")) %>% 
  # Creating a variable to diff SGIP from CAT
  mutate(Type = if_else(str_detect(Variable, 'Ratio$'), "Ratio", "No Return")) 
# Creating a variable to diff indicators

# Plot
plot <- leaves_dbs_long %>%
  ggplot(
    aes(
      x = `Year`,
      y = `value`,
      group = `Variable`, # This and subsequent codes allow to modify graph characteristics
      alpha = `Type`,
      linetype = `Type`,
      color = `Admin`,
      text = `value`
    )
  ) +
  geom_line() +
  geom_point() +
  theme_light() +
  scale_color_manual(values = c('#999999', 'darkred')) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  scale_alpha_discrete(range = c(0.5, 1))

ggsave("plot.jpg")
# Saving the file as jpg

# Interactive Plot

plot_int <- ggplotly(plot, tooltip = "text") %>%
  layout(legend = list(
    orientation = "h",
    y = -0.15,
    x = 0.2
  ))

htmlwidgets::saveWidget(plot_int, "plot_int.html")
# Saving the file as html

