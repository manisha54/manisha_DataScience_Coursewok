
library(tidyverse)




#---------------house rank------------------------------------------------------------------------------
Towns = read_csv("Towns.csv")%>% 
  select(shortPostcode, Town, District, County)

House_price = read_csv("housePricesclean.csv") %>% 
  na.omit()


#house rank
Houseprice= House_price %>% 
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()
housePrices=Houseprice  %>% 
  filter(Year=="2020") %>% 
  group_by(Town) %>% 
  summarise(Price=mean(Price)) %>% 
  arrange(Price) %>% 
  mutate(HouseScore=10-(Price/120000)) %>% 
  select(Town,HouseScore)
housePrices


#download rank

speed_downloads = read_csv("clean_broadband_speed.csv") %>% 
  na.omit()

Speed_Download = speed_downloads %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()
colnames(speed_downloads)=c("1","shortPostcode","AverageDownload","MinDownload","Averageupload","Minupload","ID")

download_speed=Speed_Download %>% 
  group_by(Town) %>% 
  summarise(downloadSpeed=AverageDownload) %>% 
  arrange(-downloadSpeed) %>% 
  mutate(DownloadScore=10-(downloadSpeed/1200)) %>% 
  select(Town,DownloadScore) 
download_speed




#crime score rank
crime_score=read_csv("cleanCrime.csv") %>% 
  rename("CrimeCount"="n")
crime_rank = crime_score %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()


crime_rank=crime_rank%>% 
  group_by(Town) %>% 
  summarise(score=mean(CrimeCount)) %>% 
  arrange(desc(score)) %>% 
  mutate(score=10-(score/1200)) %>% 
  rename("crimescore"="score") %>% 
  select(Town,crimescore) 
crime_rank



#school rank

school_score=read_csv("NewcleanSchool.csv")
school_rank = school_score %>%
  rename(shortPostcode=shortPostCode) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()

school_rank=school_rank%>% 
  group_by(SchoolName,Town) %>% 
  summarise(score=mean(Attainment8Score)) %>% 
  arrange(score) %>% 
  mutate(score=10-(score/100)) %>% 
  select(SchoolName,score,Town)
school_rank


# OVERALL RANKING

RankingMerge = housePrices%>% 
  left_join(download_speed, by = "Town") %>% 
  left_join(crime_rank, by = "Town") %>% 
  left_join(school_rank, by = "Town") %>% 
  na.omit()

overallRank = RankingMerge %>% 
  group_by(HouseScore, score,DownloadScore,crimescore) %>%
  mutate(overallScore = (HouseScore + score + DownloadScore +crimescore)/4) %>% 
  arrange(-overallScore)

overallRank




