####### Chapter 1 #######
library(tidyverse)
library(retrosheet)
library(lubridate)
setwd("/Users/walkerharrison/PROJECT_R/ABDWR/Data")

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

## Section 1.3.5

games95 <- getRetrosheet("game", 1995)
games95 %>% filter(Date == 19950906, 
                   HmTm == "BAL")

## Section 1.3.6
games80_17 <- do.call(rbind,
                      lapply(1980:2017, 
                             function(year) getRetrosheet("game", year)))
games80_17 %>% 
  mutate(month = month(ymd(Date), label = TRUE)) %>% 
  group_by(month) %>%
  summarize(HRpG = mean(HmHR + VisHR))

Parks <- read_csv("baseballdatabank-2017.1/core/Parks.csv")

games80_17 %>% 
  inner_join(Parks, by = c("ParkID" = "park.key")) %>%
  group_by(park.name) %>%
  summarize(games = n(),
            HRpG = mean(HmHR + VisHR)) %>%
  filter(games > 100) %>%
  arrange(desc(HRpG))

games80_17 %>% 
  group_by(UmpHNm) %>%
  summarize(games = n(),
            RpG = mean(HmRuns + VisRuns)) %>%
  filter(games > 400) %>%
  arrange(desc(RpG))

games80_17 %>% 
  mutate(day = wday(ymd(Date), label = TRUE)) %>% 
  group_by(day) %>%
  summarize(avg_att = mean(Attendance, na.rm = TRUE))


## Section 1.4
setwd('chadwick-0.6.5')


NYY01 <- getRetrosheet("play", 2001, "NYA")
NYY01 %>% head()

parse.retrosheet2.pbp(2001)
data2001 <- read_csv('download.folder/unzipped/all2001.csv', col_names = FALSE)
fields <- read_csv('download.folder/unzipped/fields.csv')
names(data2001) <- fields %>% pull(Header)
data2001 %>% 
  filter(GAME_ID == "OAK200110130")

data2001 %>% 
  filter(AWAY_TEAM_ID == "NYA",
         PIT_ID == "mussm001") %>%
  pull(GAME_ID) %>% unique()


event_key <- read_csv('download.folder/unzipped/event_key.csv')
data2017 <- data2017 %>% inner_join(event_key, by = "EVENT_CD")

setwd('chadwick-0.6.5')
parse.retrosheet2.pbp(2017)

data2017 <- read_csv('download.folder/unzipped/all2017.csv')
fields <- read_csv('download.folder/unzipped/fields.csv')
names(data2017) <- fields %>% pull(Header)
event_key <- read_csv('download.folder/unzipped/event_key.csv')
data2017 <- data2017 %>% inner_join(event_key, by = "EVENT_CD")

judge17 <- data2017 %>%
  filter(BAT_ID == 'judga001')



### Section1.4.3

parse.retrosheet2.pbp(1998)
data1998 <- read_csv('download.folder/unzipped/all1998.csv', col_names = FALSE)
names(data1998) <- fields %>% pull(Header)

data1998 %>%
  inner_join(event_key, by = "EVENT_CD") %>%
  filter(BAT_ID %in% c('mcgwm001', 'sosas001'),
         BAT_EVENT_FL == TRUE,
         (!is.na(BASE1_RUN_ID) | !is.na(BASE2_RUN_ID) | !is.na(BASE3_RUN_ID))) %>%
  count(BAT_ID, RESULT) %>%
  group_by(BAT_ID) %>%
  mutate(n/sum(n)) %>%
  filter(RESULT == "Home run")

data1998 %>%
  inner_join(event_key, by = "EVENT_CD") %>%
  filter(BAT_ID %in% c('mcgwm001', 'sosas001'),
         BAT_EVENT_FL == TRUE,
         (!is.na(BASE1_RUN_ID) | !is.na(BASE2_RUN_ID) | !is.na(BASE3_RUN_ID))) %>%
  count(BAT_ID, RESULT) %>%
  filter(!RESULT %in% c("Walk", "Intentional walk", "Hit by pitch")) %>%
  group_by(BAT_ID) %>%
  mutate(n/sum(n)) %>%
  filter(RESULT == "Home run")

parse.retrosheet2.pbp(2001)
data2001 <- read_csv('download.folder/unzipped/all2001.csv', col_names = FALSE)
names(data2001) <- fields %>% pull(Header)

data2001 %>%
  inner_join(event_key, by = "EVENT_CD") %>%
  filter(BAT_ID == "bondb001",
         RESULT == "Intentional walk") %>%
  group_by(FIRST = !is.na(BASE1_RUN_ID), 
           SECOND = !is.na(BASE2_RUN_ID), 
           THIRD = !is.na(BASE3_RUN_ID)) %>%
  summarize(IBB = n())

parse.retrosheet2.pbp(2011)
data2001 <- read_csv('download.folder/unzipped/all2011.csv', col_names = FALSE)
names(data2011) <- fields %>% pull(Header)

data2011  %>%
  filter(PITCH_SEQ_TX == "BBX",
         AB_FL == TRUE) %>%
  summarize(mean(H_FL>0))

teams_17 <- getTeamIDs(2017)

full_17 <- lapply(teams_17, function(team) getRetrosheet("play", 2017, team))

teams_98

getRetrosheet("play", 2017, "NYA")


teams_98 <- getTeamIDs(1998)

full_17 <- lapply(teams_17, function(team) getRetrosheet("play", 2017, team))
