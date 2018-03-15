## Chapter 1

library(tidyverse)
library(retrosheet)
setwd("/Users/walkerharrison/Downloads/Analyzing Baseball Data With R/baseballdatabank-2017.1/core")

## Section 1.2

Master <- read.csv("Master.csv")
Master %>% filter(playerID == "aaronha01")

Batting <- read.csv("Batting.csv")
Batting %>% filter(playerID == "brocklo01") %>% View()

Pitching <- read.csv("Pitching.csv")
Pitching %>% filter(playerID == "ruthba01") %>% View()

Fielding <- read.csv("Fielding.csv")
Fielding %>% filter(playerID == "jeterde01") %>% View()

Teams <- read.csv("Teams.csv")
Teams %>% filter(yearID == 2016) %>% View()

Teams %>% 
  mutate(decade = floor(yearID/10)*10,
         HRpG = HR/G,
         KpG = SO/G) %>% 
  group_by(decade) %>% 
  summarize(mean(HRpG*2, na.rm = T),
            mean(KpG*2, na.rm = T)) %>% 
  View()

Teams %>%
  filter(lgID %in% c("AL", "NL")) %>%
  group_by(yearID, lgID) %>%
  summarize(RpG = mean(R/G)) %>%
  spread(lgID, RpG) %>%
  mutate(diff = AL-NL) %>%
  ggplot(aes(yearID, diff)) + geom_point()

Pitching %>%
  filter(yearID %in% c(1900:1910, 2000:2010)) %>%
  mutate(decade = round(yearID, -2)) %>%
  group_by(decade) %>%
  summarize(GS = sum(GS),
            CG = sum(CG)) %>%
  mutate(perc.finish = CG/GS) %>%
  View()

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
