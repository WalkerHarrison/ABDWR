####### Chapter 1 #######
library(tidyverse)
library(retrosheet)

#setwd("/Users/walkerharrison/PROJECT_R/ABDWR/Chapters/Chapter1")

## Section 1.2.2
temp <- tempfile()
download.file("http://seanlahman.com/files/database/baseballdatabank-2017.1.zip", temp)
unzip(temp)

## Section 1.2.3

Master <- read.csv("baseballdatabank-2017.1/core/Master.csv")
Master %>% 
  filter(playerID == "aaronha01")

## Section 1.2.4
Batting <- read.csv("baseballdatabank-2017.1/core/Batting.csv")
Batting %>% 
  filter(playerID == "brocklo01", yearID == 1964)
Batting %>% 
  filter(playerID == "ruthba01")

Batting %>% 
  filter(playerID == "ruthba01", yearID == 1919) %>%
  transmute(AVG = round(H/AB, 3))

## Section 1.2.5
Pitching <- read.csv("baseballdatabank-2017.1/core/Pitching.csv")
Pitching %>% 
  filter(playerID == "ruthba01")

## Section 1.2.6
Fielding <- read.csv("baseballdatabank-2017.1/core/Fielding.csv")
Fielding %>% 
  filter(playerID == "ruthba01")

Fielding %>% 
  filter(playerID == "ruthba01",
         POS == "OF"
         ) %>%
  group_by(yearID) %>%
  summarize(RF = round((PO + A)/G, 2)) %>% 
  pull()

## Section 1.2.7
Teams <- read.csv("baseballdatabank-2017.1/core/Teams.csv")
Teams %>% 
  filter(yearID == 1927,
         teamID == "NYA")

## Section 1.2.8
Teams %>% 
  mutate(decade = floor(yearID/10)*10,
         HRpG = HR/G,
         SOpG = SO/G) %>% 
  group_by(decade) %>% 
  summarize(HRpG = mean(HRpG*2, na.rm = T),
            SOpG = mean(SOpG*2, na.rm = T))

Teams %>%
  filter(lgID %in% c("AL", "NL")) %>%
  group_by(yearID, lgID) %>%
  summarize(RpG = mean(R/G)) %>%
  spread(lgID, RpG) %>%
  filter(!is.na(AL)) %>%
  mutate(diff = AL-NL) %>%
  ggplot(aes(yearID, diff)) + geom_point()


Pitching %>%
  mutate(decade = floor(yearID/10)*10) %>%
  filter(decade %in% c(1900, 2000)) %>%
  group_by(decade) %>%
  summarize(GS = sum(GS),
            CG = sum(CG)) %>%
  mutate(perc.finish = CG/GS)

## Section 1.3

games80_17 <- do.call(rbind,
                      lapply(1980:2017, 
                             function(year) getRetrosheet("game", year)))

games80_17 %>% 
  mutate(month = month(ymd(Date))) %>% 
  group_by(month) %>%
  summarize(HRpG = mean(HmHR + VisHR))

games80_17 %>% 
  group_by(ParkID) %>%
  summarize(HRpG = mean(HmHR + VisHR))

games80_17 %>% 
  group_by(UmpHNm) %>%
  summarize(RpG = mean(HmRuns + VisRuns),
            total_g = n()) %>%
  filter(total_g > 400) %>% View()

games80_17 %>% 
  mutate(day = wday(ymd(Date), label = T)) %>% 
  group_by(day) %>%
  summarize(avg_att = mean(Attendance, na.rm = T)) %>% View()


## Section 1.3

teams_17 <- getTeamIDs(2017)

full_17 <- lapply(teams_17, function(team) getRetrosheet("play", 2017, team))

teams_98

getRetrosheet("play", 2017, "NYA")
