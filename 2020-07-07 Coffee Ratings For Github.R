## Tidytuesday 07.07.2020
## Coffee Ratings
## A. Waldmann, July 2020



## Load packages, data, set theme. 
library(tidyverse)
library(tidytuesdayR)
library(grid)
library(ggplotify)

theme_set(theme_classic())
tuesdata <- tt_load('2020-07-07')

df <- tuesdata$coffee_ratings


## Inspect data 
View(df)
head(df)


## Data Wrangling Part 1 (Sensoric profile)
sens_arabica <- df %>% 
    filter(total_cup_points > 0 & species == "Arabica" & is.na(country_of_origin) == FALSE) %>% 
    summarize(aroma = median(aroma, na.rm = TRUE),
              flavor = median(flavor, na.rm = TRUE),
              aftertaste = median(aftertaste, na.rm = TRUE),
              acidity = median(acidity, na.rm = TRUE), 
              body = median(body, na.rm = TRUE), 
              balance = median(balance, na.rm = TRUE), 
              uniformity = median(uniformity, na.rm = TRUE), 
              clean_cup = median(clean_cup, na.rm = TRUE), 
              sweetness = median(sweetness, na.rm = TRUE), 
              cupper_points = median(cupper_points, na.rm = TRUE), 
              moisture = median(moisture, na.rm = TRUE)) 
              
    
sens_long <- sens_arabica %>% 
    pivot_longer(aroma:moisture, names_to ="sensoric", values_to = "median_value")



## Draw Plots p1 (Lolliop) and p2 (Radarchart)

sens_long$sensoric <- factor(sens_long$sensoric, levels = c("aroma", "flavor", "aftertaste", "acidity", "body", "balance",
                                                            "uniformity", "clean_cup", "sweetness", "cupper_points", "moisture"))

p1 <- ggplot(sens_long, aes(x = sensoric, y = median_value )) +
    geom_segment(aes(x = sensoric, xend = sensoric, y = 0, yend = median_value), color="chocolate4") +
    geom_point( color="burlywood3", size = 8) +
    geom_text(label = sens_long$median_value, size = 3) +
    theme_light() +
    coord_flip() +
    labs(x = 'Sensoric profile',
         y = 'Rating on a scale from 0 (lowest) to 10 (highest)',
         title = 'Sensoric profile of Arabica coffee',
         subtitle = 'Median ratings') + 
    theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
    )

p1


# Add min and max for radarchart to data frame
sens_radar <-  rbind(rep(10,11) , rep(0,11), sens_arabica)

p2 <- as.ggplot(~radarchart(sens_radar, 
                 axistype = 4 , 
                 
                 #custom polygon
                 pfcol = rgb(222, 184, 135, max = 255 , alpha = 110),
                 pcol = rgb(139, 69, 19, max = 255),
                 #pfcol= '#DEB887' , # fill color
                 #pcol = '#D2691E' , # line color
                 plwd = 1 , # line width
                 
                 #custom the grid
                 cglcol = "grey", # color of the net
                 cglty = 3, # net line type 
                 axislabcol = "grey", # color of axis labels
                 caxislabels = seq(0,10,2.5), # vector of axis labels to display
                 cglwd = 1, # net width
                 
                 #custom labels
                 vlcex = 1 ,# group labels size
                 
                 # title
                 #title = "Sensoric profile of Arabica coffee (Median ratings)"
                 ))

p2


## Cowplot Sensoric profile

p5  <- cowplot::plot_grid(
    p1 ,
    p2,
    ncol = 2, align = "h")

p5


## Save plot
ggsave(filename = "YOUR PATHFILE/2020-07-07 Coffee Ratings.png", 
       plot = p5, 
       device = "png", 
       scale = 0.5,
       units = "in", 
       width = 16, 
       height = 9)



