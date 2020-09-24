# TidyTuesday 22.9.2020
#
# AW, September 2020
###########################################################################
###########################################################################

## path to library
.libPaths('XXX/R/win-library/3.6')


## Create vector with package names
packages <- c("tidyverse",
              "magrittr",
              "ggplot2",
              "tidytuesdayR",
              "scales",
              "patchwork")


## check installation status
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}


## load packages
invisible(lapply(packages, library, character.only = TRUE))




## set path
tmpDir <- 'XXX/tidytues/2020-09-22 Expeditions'


## load data
tuesdata <- tidytuesdayR::tt_load('2020-09-22')


## content of tuesdata
tuesdata

peaks <- tuesdata$peaks  
glimpse(peaks) 
View(peaks)

members <- tuesdata$members
glimpse(members) 
View(members)

expeditions <- tuesdata$expeditions  
glimpse(expeditions) 
View(expeditions)




### I decided to plat "Top 10" bar charts for multiple questions:
### Which are the highest (climbed/unblimbed peaks)?
### What are the most frequent reseasons for termination?
### Which were the biggest expeditions?
### Which peakswere climbed most often (by groups and solo climbers)?
### How many people died while trying to climb which peaks?




## Data Wrangling and TOP 10 Creating Plots

theme_set(theme_minimal ())


## Group 1; Highest peaks (climbed/unclimbed), most frequent reasons for termination.

# Highest Altitude - total = climbed, unclimbed
Top10_peaks_climbed <- peaks %>% 
  filter(climbing_status == 'Climbed') %>% 
  top_n(10, wt = height_metres) %>% 
  arrange(desc(height_metres)) %>% 
  mutate(rank = rank(desc(height_metres))) %>% 
  mutate(label_y = paste(peak_name)) %>% 
  select(2, 4, 6, 9, 10)%>% 
  mutate(label_y = paste(peak_name, '-', first_ascent_year))

p1 <- ggplot(data = Top10_peaks_climbed, 
             aes(y = reorder(label_y, height_metres), x = height_metres, 
                 label = paste('Year of 1. Ascent:', first_ascent_year), 
                 fill = rank)) +
  geom_col() +
  geom_text(aes(label = height_metres), 
            size = 3, color = "white", hjust = 1.2) + 
  theme(legend.position = 'none') +
  scale_x_continuous(  
    limits = c(-25, 9000),  
    breaks = c(0, 2000, 4000, 6000, 8000)) +
  labs(x = 'Height in Meters',
       y = '',
       title = 'Top 10 of Peaks According to Altitude',
       caption = 'Displayed on y-axis: Name of Peak and Year of First Ascent')

p1




Top10_peaks_unclimbed <- peaks %>% 
  filter(climbing_status == 'Unclimbed') %>% 
  top_n(10, wt = height_metres) %>% 
  arrange(desc(height_metres)) %>% 
  mutate(rank = rank(desc(height_metres))) %>% 
  mutate(label_y = paste(peak_name)) %>% 
  select(2, 4, 9, 10) %>% 
  mutate(label_y = paste(peak_name))

p2 <- ggplot(data = Top10_peaks_unclimbed, 
             aes(y = reorder(label_y, height_metres), x = height_metres, 
                 fill = rank)) +
  geom_col() +
  geom_text(aes(label = height_metres), 
            size = 3, color = "white", hjust = 1.2) + 
  theme(legend.position = 'none') +
  scale_x_continuous(  
    limits = c(-25, 9000),  
    breaks = c(0, 2000, 4000, 6000, 8000)) +
  labs(x = 'Height in Meters',
       y = '',
       title = 'Top 10 of Peaks Yet Unclimbed',
       caption = '')

p2



# Reasons for termination
Top10_reasons_end <- expeditions %>% 
  filter(str_detect(termination_reason, 'Success') == FALSE) %>% 
  group_by(termination_reason) %>% 
  summarize(freq = n()) %>% 
  top_n(10, wt = freq) %>% 
  arrange(desc(freq)) %>% 
  mutate(rank = rank(freq)) %>% 
  mutate(label_y = paste(termination_reason))

Top10_reasons_end$label_y <- ifelse(Top10_reasons_end$label_y == 'Bad conditions (deep snow, avalanching, falling ice, or rock)', 'Bad conditions (Snow, ice, etc.)', Top10_reasons_end$label_y)
Top10_reasons_end$label_y <- ifelse(Top10_reasons_end$label_y == 'Route technically too difficult, lack of experience, strength, or motivation', 'Route too difficult, lack of experience', Top10_reasons_end$label_y)


p3 <- ggplot(data = Top10_reasons_end, 
             aes(y = reorder(label_y, freq), x = freq, 
                 fill = -rank)) +
  geom_col() +
  geom_text(aes(label = freq), 
            size = 3, color = "white", hjust = 1.2) + 
  theme(legend.position = 'none') +
  scale_x_continuous(  
    limits = c(-25, 1500),  
    breaks = c(0, 250, 500, 750, 1000, 1250)) +
  labs(x = 'Frequency of Reason for Termination',
       y = '',
       title = 'Top 10 of Reasons for the Termination of an Expedition',
       caption = '')

p3




## Group 2: Expeditions

# Most expeditions
Top10_most <- expeditions %>% 
  group_by(peak_name) %>% 
  summarise(freq = n_distinct(expedition_id)) %>% 
  top_n(10, wt = freq) %>% 
  arrange(desc(freq)) %>% 
  mutate(rank = rank(-freq)) %>% 
  mutate(label_y = paste(peak_name))

p4 <- ggplot(data = Top10_most, 
             aes(y = reorder(label_y, freq), x = freq, 
                 fill = rank)) +
  geom_col() +
  geom_text(aes(label = freq), 
            size = 3, color = "white", hjust = 1.2) + 
  theme(legend.position = 'none') +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  labs(x = 'Total Amount of Expeditions',
       y = '',
       title = 'Top 10 of Most Climbed Peaks',
       caption = '')

p4



# largest expeditions
Top10_big <- expeditions %>% 
  mutate(total = members + hired_staff) %>% 
  top_n(10, wt = total) %>% 
  arrange(desc(total)) %>% 
  select(3, 17, 4) %>% 
  mutate(rank = rank(-total)) %>% 
  mutate(label_y = paste(peak_name, '-', year))

p5 <- ggplot(data = Top10_big, 
             aes(y = reorder(label_y, total), x = total, 
                 fill = rank)) +
  geom_col() +
  geom_text(aes(label = total), 
            size = 3, color = "white", hjust = 1.2) + 
  theme(legend.position = 'none') +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  labs(x = 'Total Size of Expedition (Climbers and Hired Staff)',
       y = '',
       title = 'Top 10 of Largest Expedition',
       caption = 'Displayed on y-axis: Name of Peak and Year of Expedition')

p5



# Solo expeditions
Top10_solo <- expeditions %>% 
  filter(members == 1) %>% 
  group_by(peak_name) %>% 
  summarize(freq = n()) %>% 
  top_n(10, wt = freq) %>% 
  arrange(desc(freq)) %>% 
  mutate(rank = rank(-freq)) %>% 
  mutate(label_y = paste(peak_name))

p6 <- ggplot(data = Top10_solo, 
             aes(y = reorder(label_y, freq), x = freq, 
                 fill = rank)) +
  geom_col() +
  geom_text(aes(label = freq), 
            size = 3, color = "white", hjust = 1.2) + 
  theme(legend.position = 'none') +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  labs(x = 'Total Amount of Solo Expeditions',
       y = '',
       title = 'Top 10 of Peaks with Most Solo Expeditions',
       caption = '')

p6







## Group 3: Fatal Accidents / Deaths

# most deaths (total)
Top10_death <- expeditions %>% 
  mutate(deaths_total = member_deaths + hired_staff_deaths,
         total = members + hired_staff) %>% 
  arrange(desc(deaths_total)) %>% 
  slice(1:10) %>% 
  select(3, 17, 18, 4) %>% 
  mutate(rank = rank(-deaths_total, ties.method = "first")) %>% 
  mutate(label_y = paste(peak_name, '-', year))

p7 <- ggplot(data = Top10_death, 
             aes(y = reorder(label_y, deaths_total), x = deaths_total, 
                 fill = rank)) +
  geom_col() +
  geom_text(aes(label = deaths_total), 
            size = 3, color = "white", hjust = 1.2) + 
  theme(legend.position = 'none') +
  scale_x_continuous(  
    limits = c(-0.15, 20),  
    breaks = c(0, 10, 15, 20)) +
  labs(x = 'Number of Deaths (Climbers & Hired Staff)',
       y = '',
       title = 'Top 10 of Peaks with Most Fatal Accidents',
       caption = 'Displayed on y-axis: Name of Peak and Year of Expedition')

p7




# most deaths - members/climbers
Top10_death_m <- expeditions %>% 
  top_n(10, wt = member_deaths) %>% 
  arrange(desc(member_deaths)) %>% 
  select(3, 12, 11, 4) %>% 
  mutate(rank = rank(-member_deaths, ties.method = "first")) %>% 
  mutate(label_y = paste(peak_name, '-', year))

p8 <- ggplot(data = Top10_death_m, 
             aes(y = reorder(label_y, member_deaths), x = member_deaths, 
                 fill = rank)) +
  geom_col() +
  geom_text(aes(label = member_deaths), 
            size = 3, color = "white", hjust = 1.2) + 
  theme(legend.position = 'none') +
  scale_x_continuous(  
    limits = c(-0.15, 20),  
    breaks = c(0, 10, 15, 20)) +
  labs(x = 'Number of Deaths Among Climbers',
       y = '',
       title = 'Top 10 of Peaks - Fatal Accidents (Climbers)',
       caption = 'Displayed on y-axis: Name of Peak and Year of Expedition')

p8


# most deaths - staff (sherpas)
Top10_death_s <- expeditions %>% 
  top_n(10, wt = hired_staff_deaths) %>% 
  arrange(desc(hired_staff_deaths)) %>% 
  select(3, 14, 13, 4) %>% 
  mutate(rank = rank(-hired_staff_deaths, ties.method = "first")) %>% 
  mutate(label_y = paste(peak_name, '-', year))



p9 <- ggplot(data = Top10_death_s, 
             aes(y = reorder(label_y, hired_staff_deaths), x = hired_staff_deaths, 
                 fill = rank)) +
  geom_col() +
  geom_text(aes(label = hired_staff_deaths), 
            size = 3, color = "white", hjust = 1.2) + 
  theme(legend.position = 'none') +
  scale_x_continuous(  
    limits = c(-0.15, 20),  
    breaks = c(0, 10, 15, 20)) +
  labs(x = 'Total Amount of Deaths Among Hired Staff',
       y = '',
       title = 'Top 10 of Peaks - Fatal Accidents (Staff)',
       caption = 'Displayed on y-axis: Name of Peak and Year of Expedition')

p9





## Combine PPlot and save

p <- (p1 + p4 + p7) / (p2 + p6 + p8) / (p3 + p5 + p9) +
plot_annotation(
  title = "Top 10 Charts for the Himalayan",
  subtitle = "Mt. Everest - the highest peak in the world - a place of many people's desire. \nWith more than 2,000 ascents being documented in the Himalayan Database (years: 1905-2019) Mt. Everest is the most climbed mountain in the Himalayan. \n9 out of the 10 largest expeditions and more than 450 solo-expeditions aimed to climb Mt. Everst.  Unfortunately, not all expeditions were successfull and lucky. \n3 out of 10 expeditions with the most fatal accidents happend on Mt. Everest.",
  caption = "Data source: The Himalayan Database\nVisualisation: @annika_waldmann",
  theme = theme(plot.title = element_text(size = 24, color = "dodgerblue4"),
                plot.subtitle = element_text(size = 16, color = "dodgerblue4"),
                plot.caption = element_text(size = 12, color = "dodgerblue4"),
                plot.background = element_rect(fill = "gainsboro"))
)

p



ggsave(filename = paste0(tmpDir,"/2020-09-22 Top 10 Charts.png"), 
       plot = p, 
       device = "png", 
       scale = 1,
       units = "in", 
       width = 16, 
       height = 9)


