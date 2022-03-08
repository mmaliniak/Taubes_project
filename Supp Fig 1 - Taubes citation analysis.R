#Taubes - citation histogram
#Created 2/7/2022


library(tidyverse)
library(ggpubr)

#Import publish or perish data
popdat <- read.csv(file="PoPCites_clean.csv")


table(popdat$Year, useNA="ifany")
popdat$Year <- as.numeric(popdat$Year)
popdat <- filter(popdat, !is.na(Year)) #remove 47 citations without years

plot_dat <- popdat  %>%  
  group_by(Year) %>% 
  summarise(citation_count = length(Year)) #Count the number of dates that occur in each year
plot_dat

fig1 <- ggbarplot(plot_dat, x = "Year", y = "citation_count",
          fill = "gray", 
          xlab = "Year", ylab = "Citation count",
          x.text.angle = 45,  # x axis text rotation angle
          label = plot_dat$citation_count
)
fig1

# popdat  %>%  
#   group_by(Year) %>% 
#   summarise(date_count = length(Year)) %>%  # Count the number of dates that occur in each year
#   ggplot(aes(x = Year, y = date_count)) +
#   geom_col(colour = 'black', fill = 'blue') # plot as a column graph


