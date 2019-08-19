library(tidyverse)
library(lubridate)

# Read in the data
emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

# Format the data for plotting
emp_long <- emperors %>%
  mutate(start = ymd(reign_start),
         end = ymd(reign_end)) %>%
  select(dynasty, start, end, era) %>%
  gather(date_type, date, -c(dynasty, era)) %>%
  arrange(date, date_type) %>%
  mutate(
    dynasty = factor(dynasty, levels=rev(unique(dynasty)), ordered=TRUE),
    era = factor(era, levels = c('Principate', 'Dominate'), ordered = TRUE)
  )


# Chart Theme
theme_emperor <- function(background_color = 'cornsilk') {
 
  theme(
    axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
    panel.background = element_rect(fill= background_color, colour=NA),
    panel.border = element_blank(), axis.line=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major = element_line(size=0.5, colour="grey80"),
    axis.ticks=element_blank(),
    legend.position="bottom", 
    legend.key = element_blank(),
    legend.background = element_rect(fill = background_color),
    plot.background = element_rect(fill = background_color)
  )
}

# Chart
ggplot(emp_long, aes(x=dynasty, y=date)) +
  geom_line(color = 'dodgerblue', size=6) + 
  labs(
    title = 'Time of Rule',
    subtitle = 'Roman Emperor Dynasties',
    x='Dynasty', y='Year'
  ) +
  geom_hline(yintercept = as.Date('0284-11-20'), size = 1.5) +
  annotate(geom = 'text', label ='Dominate', size = 4.5, color = 'red', x = 8, y = as.Date('0290-11-20'), hjust = 0, vjust = 1.1) +
  annotate(geom = 'text', label ='Era', size = 4.5, color = 'red', x = 8, y = as.Date('0290-11-20'), hjust = 0, vjust = 2.1) +
  annotate(geom = 'text', label ='Principate', size = 4.5, color = 'red', x = 8, y = as.Date('0200-11-20'), hjust = 0, vjust = 1.1) +
  annotate(geom = 'text', label ='Era', size = 4.5, color = 'red', x = 8, y = as.Date('0200-11-20'), hjust = 0, vjust = 2.1) +
  coord_flip() +
  theme_emperor() 

