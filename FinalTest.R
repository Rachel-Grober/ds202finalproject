library(tidyverse)
library(ggcorrplot)
library(rvest)

df <- read.csv("../usa_00002.csv")

unique(df$STATEICP)

#Web Scrape this
#https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696

colnames(df)
#=========================================================Web Scraping===============================================================================

url <- "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696"
html <- read_html(url)
tables <- html_table(html)
str(tables)
tables[18]


#===================================================How does race impact income?======================================================================

#Average is way skewed so we used median
df2 <- df %>% group_by(RACE) %>% summarize(Income = median(FTOTINC))

raceDF <- data.frame(c(1,2,3,4,5,6,7,8,9), c("White", "Black/African American/Negro", "American Indian or Alaska Native", 
                                             "Chinese", "Japanese", "Other Asian or Pacific Islander", "Other Race, nec", "Two major races", 
                                             "Three or more major races"))
colnames(raceDF) <- c("Race ID", "Race")

df2 <- left_join(df2, raceDF, by = c("RACE" = "Race ID"))
df2 <- df2 %>% select(Race, Income)

ggplot(df2, aes(reorder(Race,-Income), Income, fill = Race)) + geom_bar(stat = "identity") + theme(axis.text.x = element_blank()) + 
  xlab("Race") + ylab("Income") + labs(title = "Median Income by Race")

#=====================================================================Income===========================================================================

incomeDF <- df %>% select()




