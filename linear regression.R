#Linear Modeling
library(tidyverse)



#--------------------liner regression -----------------------------------------------
Towns = read_csv("Towns.csv")%>% 
  select(shortPostcode, Town, District, County)
prices = read_csv("housePricesclean.csv") %>% 
  na.omit()

speeds = read_csv("clean_broadband_speed.csv") %>% 
  na.omit()  

crime=read_csv("cleanCrime.csv") %>% 
  rename("DrugCount"="n") %>% 
  na.omit()

schools=read_csv("NewcleanSchool.csv") %>% 
  na.omit()




#------------------------------House prices vs Download Speed----------------------------------------


options(scipen=999)

unclean_house_prices = prices %>%
  filter(Year=="2020") %>% 
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))

Unclean_broadband_speed = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  summarise(AverageDownload=mean(AverageDownload))

lm_res = unclean_house_prices %>% left_join(Unclean_broadband_speed,by="Town")
model = lm(data= lm_res, Price~AverageDownload)
summary(model)

color= c("LANCASHIRE" = "red", "LEICESTERSHIRE " = "blue")

ggplot(lm_res,aes(x=AverageDownload,y=Price)) +
  geom_point(data = filter(lm_res,County.x=="LEICESTERSHIRE"),aes(color="Leicestershire"))+
  geom_point(data = filter(lm_res,County.x=="LANCASHIRE"), aes(color="lancashire")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Download Speed (Mbit/s)",y="Price (£)",title="House Prices vs Download Speed",color="County")




#----------------------------------House price and drug offence--------------------------------------------------


unclean_house_prices = prices %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))


Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  tally() %>% 
  na.omit()

#colnames(Drugs)=c("Town","County","DrugCount")

lm_res1 = unclean_house_prices %>% left_join(Drugs ,by="Town") %>%
  # group_by(Town,County.x,Price) %>% 
  na.omit()

model1 = lm(data= lm_res1, Price~n)
summary(model1)

color= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")

ggplot(lm_res6,aes(x=n,y=Price)) +
  geom_point(data = filter(lm_res1,County.x=="LEICESTERSHIRE"),aes(color="Leicestershirer"))+
  geom_point(data = filter(lm_res1,County.x=="LANCASHIRE"), aes(color="lancashire")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="count",y="price(?)",title="House Prices vs Drug",color="County")






#-----------------------house price and school------------------------------------------
school_lm= schools %>%
  left_join(Towns,by=c("shortPostCode"="shortPostcode") )%>%  
  group_by(Town,County,District) %>%
  summarise(score=mean(Attainment8Score)) %>% 
  na.omit()

HousePrices = prices %>%
  filter(Year=="2020") %>%
  left_join(Towns,by="shortPostcode") %>%  
  group_by(Town,County) %>%
  summarise(Price=mean(Price))


lm_res4 = HousePrices %>% left_join(school_lm,by="County") %>% 
  na.omit()
model4 = lm(data= lm_res4, Price~score)
summary(model4)

colors1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Pink")

ggplot(lm_res4,aes(x=score,y=Price)) +
  geom_point(data = filter(lm_res4,County=="LEICESTERSHIRE"),aes(color=" Leicestershirer"))+
  geom_point(data = filter(lm_res4,County=="LANCASHIRE"), aes(color="lancashire")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Score",y="Average Download",title="score vs AverageDownload",color="County")




#-----------------------download speed and drugs---------------------------------


BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County,District) %>%
  summarise(AverageDownload=mean(AverageDownload))


Drugs = crime %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  filter(CrimeType=="Drugs") %>% 
  na.omit()

lm_res3 = BroardbandSpeeds %>% left_join(Drugs ,by="Town") %>% 
  na.omit()
model3 = lm(data= lm_res3, AverageDownload~DrugCount)
summary(model3)

colors1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")


ggplot(lm_res3,aes(x=DrugCount,y=AverageDownload)) +
  geom_point(data = filter(lm_res3,County.x=="LEICESTERSHIRE "),aes(color="Leicestershire "))+
  geom_point(data = filter(lm_res3,County.x=="LANCASHIRE"), aes(color="lancashire")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Drug count",y="Average Download",title="AVergae download speed vs Drug",color="County")



#-----------------------average download and school-------------------------
BroardbandSpeeds = speeds %>%
  left_join(Towns,by="shortPostcode") %>%
  group_by(Town,County) %>%
  summarise(AverageDownload=mean(AverageDownload))


school_lm= schools %>%
  left_join(Towns,by=c("shortPostCode"="shortPostcode") )%>%  
  group_by(Town,County,District) %>%
  summarise(score=mean(Attainment8Score)) %>% 
  na.omit()



lm_res4 = BroardbandSpeeds %>% left_join(school_lm,by="County") %>% 
  na.omit()
model4 = lm(data= lm_res4, AverageDownload~score)
summary(model4)

colors1= c("LANCASHIRE" = "yellow", "LEICESTERSHIRE" = "Green")

ggplot(lm_res4,aes(x=score,y=AverageDownload)) +
  geom_point(data = filter(lm_res4,County=="LEICESTERSHIRE"),aes(color=" Leicestershire"))+
  geom_point(data = filter(lm_res4,County=="LANCASHIRE"), aes(color="lancashire")) +
  geom_smooth(method=lm,se=FALSE,color="black")+
  labs(x="Score",y="Average Download",title="score vs AverageDownload",color="County")








