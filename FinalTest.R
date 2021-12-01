library(tidyverse)
library(ggcorrplot)
library(rvest)
library(plotly)

df <- read.csv("Final Project Folder DS202/usa_00002.csv")
statesdf <- read.csv("Final Project Folder DS202/usa_00004.csv")
statesdf <- unique(statesdf) %>% select(STATEICP, STATEFIP)
df <- left_join(df, statesdf, by = "STATEICP")

#Web Scrape this
#https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696

colnames(df)
#=========================================================Web Scraping===============================================================================

url <- "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696"
html <- read_html(url)
tables <- html_table(html)
str(tables)
stateCodes <- data.frame(tables[19])
statesClean <- stateCodes %>% slice(-c(1,2)) %>% select(X1, X2, X3)
colnames(statesClean) <- statesClean[1,]
statesClean <- statesClean %>% slice(-1)
statesClean$FIPS <- as.integer(statesClean$FIPS)


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

#=====================================================================State===========================================================================

incomeDF <- df %>% group_by(STATEFIP) %>% summarise(Income = median(FTOTINC)) %>% left_join(statesClean, by = c("STATEFIP" = "FIPS")) %>% 
  select(Name, Income) %>% drop_na()

ggplot(incomeDF, aes(reorder(Name, -Income), Income, fill = Name)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, size = 15), legend.position = "none") + xlab("State") + ylab("Income") + 
  labs(title = "Median Income by State")


#=========================================================================Birthplace===================================================================

birthDF <- df %>% group_by(BPL) %>% summarise(Income = median(FTOTINC)) %>% arrange(Income)
top5 <- birthDF %>% slice_head(n = 5) %>% mutate(Group = "Bottom 5")
bottom5 <- birthDF %>% slice_tail(n = 5) %>% mutate(Group = "Top 5")
bplDF <- rbind(top5, bottom5)

#Manually gathered from data codebook
birthplaceName <- data.frame(c(544, 950, 200, 110, 210, 700, 516, 521,413,543),
                             c("North Yemen Arab Republic", "Other", "Mexico", "Puerto Rico", "Central America / Carribean", 
                               "Australia / New Zealand", "Singapore", "India", "United Kingdom", "United Arab Emirates"))
colnames(birthplaceName) <- c("Code", "Name")

bplDF <- left_join(bplDF, birthplaceName, by = c("BPL" = "Code")) 


ggplot(bplDF, aes(x = reorder(Group, -Income), Income, fill = reorder(Name, -Income))) + geom_bar(position = "dodge", stat = "identity") + 
  xlab("Grouping") + ylab("Median Income") + labs(title = "Median Income by Birthplace", fill = "Birthplace")

#=====================================================================Migration Status=================================================================

migrationDF <- df %>% group_by(MIGRATE1) %>% summarize(Income = median(FTOTINC))

statusCodes <- data.frame(c(1, 2, 3, 4, 9), 
                          c("Same House", "Moved Within State", "Moved States", "Abroad", "Unknown"))
colnames(statusCodes) <- c("Code", "MigrationStatus")

migrationDF <- migrationDF %>% filter(MIGRATE1 != 0) %>% left_join(statusCodes, by = c("MIGRATE1" = "Code"))

ggplot(migrationDF, aes(10, reorder(MigrationStatus, Income), size = Income, color = MigrationStatus)) + geom_point() + scale_size(range = c(18,38)) + 
  xlab(element_blank()) + ylab(element_blank()) + labs(title = "Income by Migration Status") + 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.ticks= element_blank(), axis.text = element_blank(), 
        panel.background = element_blank()) + geom_text(aes(label = MigrationStatus, color = "white"), size = 6, nudge_y = .25) + 
  geom_text(aes(label = Income, color = "purple"), size = 6) + scale_color_manual(values = 
                                                                                    c("green", "purple", "pink", "black", "red", "black", "black"))

#========================================================================City=========================================================================

citypopDF <- df %>% select(CITYPOP, FTOTINC) %>% group_by(CITYPOP) %>% summarise(Income = median(FTOTINC)) %>% 
  mutate(`log(City Population)` = log(CITYPOP))
p <- ggplot(citypopDF, aes(`log(City Population)`, Income)) + geom_point() + labs(title = "Income by log(City Population)") + 
  geom_smooth(method = 'lm')
ggplotly(p)

#=======================================================================Famsize========================================================================

famsizeDF <- df %>% select(FAMSIZE, FTOTINC) %>% group_by(FAMSIZE) %>% summarise(Income = median(FTOTINC), count = n())
ggplot(famsizeDF, aes(FAMSIZE, Income)) + geom_line() + labs(title = "Income by Family Size") + theme(axis.title.x = element_text("Family Size")) + 
  geom_smooth(method = 'lm')

famsizeDF

#====================================================================Correlation Matrix================================================================

correlationDF <- df <- read.csv("Final Project Folder DS202/usa_00005.csv")
correlation <- cor(correlationDF)

  
  