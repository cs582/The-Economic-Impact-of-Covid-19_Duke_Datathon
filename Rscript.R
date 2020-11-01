#bunch of libraries I use from other proj, not sure which dependancy does what so I called them all

#visualisation dependendancies
library(ggplot2)
library(reshape2)
library(lubridate)
library(gridExtra)
library(plyr)

#sentiment analysis depenendancies
library(tidytext)
library(tidyverse)
library(dplyr)
library(tidyr)
library(magrittr)
library(wordcloud)

#n-grams and correlations
library(igraph)
library(ggraph)
library(widyr)

#more knitting
library(knitr)


data <- read.csv("datasets/final_report_data.csv", stringsAsFactors=FALSE, encoding = 'UTF-8')

View(data)

data$Date <- as.Date(data$date,'%Y-%m-%d')

data$instantaneous_deathsbypop <- data$deaths/data$population

data$instanteous_affectedness <- data$instantaneous_deathsbypop * data$rfgrp

data %>%
  group_by(sub_region_2) %>%
  summarise(Total = n(), affectedness = Total/rfgrp)


data %>%
  group_by(sub_region_2) %>%
  summarise(Total = n(), total_deaths = sum(deaths))

View(
  data %>% 
    group_by(county.size) %>%
    summarise(Date, overall_inst_affectedness = sum(instanteous_affectedness))
)


data %>% 
  group_by(county.size) %>%
  summarise(Date, instanteous_affectedness) %>%
  ggplot(aes(Date, instanteous_affectedness, color = county.size)) + 
  geom_line(size = 1) +
  ggtitle("Instantaneous Affectedness Score For Different County Sizes Over Time") +
  xlab("Time") +
  ylab("Instanteous Affectedness")+
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 


#the worst affected counties adjusted for the varying impact of covid on rural, metropolitan, and other county sizes
county_data <- data %>%
  group_by(county.size) %>%
  summarise(sub_region_2, Date, instanteous_affectedness) %>%
  distinct(sub_region_2, instanteous_affectedness) %>%
  group_by(sub_region_2) %>%
  summarise(Affectedness_Score = max(instanteous_affectedness))

grid.table(
  county_data[order(-county_data$Affectedness_Score),] %>%
    top_n(10) )

county_data[order(-county_data$Affectedness_Score),] %>%
  top_n(10) %>%
  ggplot(aes(reorder(as.factor(sub_region_2), -Affectedness_Score), Affectedness_Score))+
  geom_col() +
  ylab('Affectedness Score') + 
  xlab("County Name") + 
  ggtitle("Worst Affected Counties Adjusted For Their Size/Population")+ 
  scale_fill_discrete(drop=FALSE) +
  scale_x_discrete(drop=FALSE) +
  coord_flip()

write.csv(county_data[order(-county_data$Affectedness_Score),],"PREDICTIONS.csv", row.names = FALSE)

#counties with highest risk factor scores from highest to lowest
View(data[order(-data$rfgrp),] %>%
       distinct(sub_region_2))
     