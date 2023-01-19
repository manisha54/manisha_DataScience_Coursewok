
library(tidyverse)
library(dplyr)

library(scales)
library(fmsb)
library(ggrepel)

euro <- dollar_format(prefix = "\u20ac", big.mark = ",")



#---------------graph of Average house price --------------------------------------------------------------------------------------------------------------

Towns = read_csv("Towns.csv")
HousePricesclean=read_csv("HousePricesclean.csv")

HousePricesclean=HousePricesclean %>% 
  left_join(Towns, by ="shortPostcode") %>% 
  na.omit()


House_town = unclean_house_prices %>%
  filter(County =="LANCASHIRE" | County=="LEICESTERSHIRE") %>% 
  group_by(Town,District,County) %>% 
  summarise(AveragePrice= mean(Price)) %>% 
  ungroup(Town,District,County) %>%
  arrange(County) %>% 
  na.omit()




# BOXPLOT Average house prices by district (2019-2021)
House_town %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = AveragePrice)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title=" Average house prices from 2019-2021 by district")



# BOXPLOT Average house prices by district (2019-2021)
HousePricesclean %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="Average house prices from 2019-2021 by district")





#LINEGRAPH Average house prices by year (2019-2021)
HousePricesclean %>% 
  group_by(Year) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = Year, y = AveragePrice)) +
  geom_line(size = 1.5, 
            color = "blue") +
  
  scale_x_continuous(breaks = 2019:2021) +
  
  geom_point(size = 2, 
             color = "black")+
  labs(title = "2019-2021 Average house prices by year")



#--------------------------------------------------------------------------

# BOXPLOT Average house prices by district (2021)
HousePricesclean %>% 
  filter(Year == 2021) %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2021 Average house prices by district")




# BARGRAPH house prices by district (2021)
HousePricesclean %>% 
  filter(Year == 2021) %>% 
  group_by(District) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = District, y = AveragePrice)) +
  geom_bar(position = "stack",stat = "identity", fill = "light green") +
  
  labs(title = "2021 Average house prices by district") +
  coord_flip()


options(scipen = 10000)



#---------------end of house price --------------------------------------------------------------------------------------------------------------







#-------------------------graph of broadband speed---------------------
Towns = read_csv("Towns.csv") %>% 
  select(shortPostcode, Town, District, County)

clean_broadband_speed=read_csv("clean_broadband_speed.csv")


broadband=clean_broadband_speed %>% 
  
  left_join(Towns, by = "shortPostcode") %>% 
  select(shortPostcode,AverageDownload,MinDownload,AverageUpload,MinUpload,Town, District, County ) %>% 
  na.omit()
broadband <- broadband[!duplicated(broadband), ]





#---------------LANCASHIRE Broadband Speeds--------------------------------------------------------------------------

broadband %>% 
  filter(County == "LANCASHIRE") %>% 
  ggplot(aes(y=Town)) + 
  labs(title = "Average and Minimum download internet broadband speed of Lancashire", x = "Intenet Speed (Mbit/s)", y = "Towns") + 
  geom_bar(aes(x=AverageDownload, fill = "Average"), stat = "Identity") + 
  geom_bar(aes(x=MinDownload, fill = "Minimum"), stat = "Identity") + 
  guides(fill=guide_legend("Download Speeds"))



#---------------LEICESTERSHIRE Broadband Speeds---------------------------------------------------

broadband %>% 
  filter(County == "LEICESTERSHIRE") %>% 
  ggplot(aes(y=Town)) + 
  labs(title = "Average and Minimum download internet broadband speed of Leicestershire", x = "Intenet Speed (Mbit/s)", y = "Towns") + 
  geom_bar(aes(x=AverageDownload, fill = "Average"), stat = "Identity") + 
  geom_bar(aes(x=MinDownload, fill = "Minimum"), stat = "Identity") + 
  guides(fill=guide_legend("Download Speeds"))




#---------------Average download speed of both county------------------------------------------------------

broadband %>% 
  group_by(District) %>% 
  ggplot(aes(x=District, y=AverageDownload)) + 
  labs(title = "Average Download internet speed of Lancashire and Leicestershire", x = "Districts", y="Average Download Speed (Mbits/s)") +
  geom_boxplot(outlier.colour="green") + coord_flip()






#---------------------------end of broadband speed-----------------------------------------------------------







#-----------crime graph-----------------------------------------------------------------------------


crime_Data = read_csv("cleanCrime.csv")

crimeData = crime_Data %>% 
  left_join(Towns, by = "shortPostcode") %>% 
  na.omit()


# Pie chart for 2021 Robbery by District
RobberyData = crimeData %>% 
  filter(CrimeType=="Robbery", Year == 2021) %>%
  group_by(Town) %>%
  mutate(sumCount = sum(n)) %>%+ 
  ungroup() %>%
  mutate(perc =sumCount / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(Town, sumCount, perc, labels) %>% 
  select(Town, sumCount, perc, labels)


RobberyData %>% 
  ggplot(aes(x = "", y = perc, fill =Town)) +
  geom_col(color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = .5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  theme_void()+
  labs(title="2021 Robbery by Town")


new_row = c("Town" = "min", "sumCount" = 3, "perc" = 0.005405405,"labels"=0.54)
new_row1 = c("Town" = "max", "sumCount" = 307, "perc" = 	
               0.553153153153153,"labels"=55.32)

data_robbery<-rbind(RobberyData,new_row,new_row1) 
select(sumCount,labels,perc)

df_robbery<-data_robbery[c("max","min","WIGAN"),]
radarchart(data_robbery)


#------------------------------- BOXPLOT drugs-----------------------------------------------------
crimeData %>% 
  filter(CrimeType == "Drugs", Year == 2021) %>% 
  ggplot(aes(x=District, y=n, color=CrimeType)) + 
  geom_boxplot() +
  labs(title="Drugs Count in Different District (2021)")+
  coord_flip()









#-------------graph of school---------------------------------------------------------------------------------



# Line graph Average Attainment8Score by year
schoolData %>% 
  group_by(Year) %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = Year, y = AverageAttainment)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = AverageAttainment), 
            vjust = -0.85) +
  scale_x_continuous(breaks = 2017:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "Average Attainment8Score by year")




# Box plot of Leicester shire year 2017-2021
schoolData %>% 
  filter(County == "LEICESTERSHIRE") %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2017-2021 Attainment8Score of Schools")




# Box plot of Lancashire year 2017-2021
schoolData %>% 
  filter(County == "LANCASHIRE") %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2017-2021 Attainment8Score of Schools")















