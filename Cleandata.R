library(tidyverse)
library(dplyr)



# uncleaned house price 

unclean_house_2019 = read_csv("houseprice2019.csv")
colnames(unclean_house_2019) = c("ID" , "Price", "Date", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street name",
                                 "Locality", "Town" , "District", "County", "Type1", "Type2" );

unclean_house_2020 = read_csv("houseprice2020.csv")
colnames(unclean_house_2020) = c("ID" , "Price", "Date", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street name",
                                 "Locality", "Town" , "District", "County", "Type1", "Type2" );


unclean_house_2021 = read_csv("houseprice2021.csv")
colnames(unclean_house_2021) = c("ID" , "Price", "Date", "PostCode" , "PAON", "SAON", "FL", "House Num", "Flat", "Street name",
                                 "Locality", "Town" , "District", "County", "Type1", "Type2" );




#--------- complete unclean house prices------------------------------------------------------------------------------------------------------
unclean_house_prices = unclean_house_2021 %>% 
  add_row(unclean_house_2020) %>% 
  add_row(unclean_house_2019)



#-----data clean of house price--------------------------------------------------------------------------------------------------------------

CleaningHp = unclean_house_prices %>%
  filter(County =="LANCASHIRE" | County=="LEICESTERSHIRE") %>% 
  mutate( ID = row_number()) %>% 
  na.omit()

#-----export house clean data---------------------------------------------------------------------------------------------------------------
write.csv(CleaningHp, "cleanData_price.csv")



cleanHousePrices = unclean_house_prices %>%
  filter(County =="LANCASHIRE" | County=="LEICESTERSHIRE")  %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>% 
  mutate(Year=substring(Date,1,4)) %>% 
  arrange(County) %>% 
  select(PostCode,shortPostcode,Price,Year,PAON) %>% 
  na.omit()


#exporting file
write.csv(cleanHousePrices, "HousePricesclean.csv")



#----------population mutate with house price--------------------------------------------------------------------------------------------------
PopulationData = read_csv("population.csv")

PopulationData = PopulationData %>%  mutate(shortPostcode = str_trim(substring(Postcode, 1,4)))  %>% 
  group_by(shortPostcode) %>% 
  summarise_at(vars(Population),list(Population2011 = sum)) %>% 
  mutate(Population2012= (1.00695353132322269 * Population2011)) %>% 
  mutate(Population2013= (1.00669740535540783 * Population2012)) %>%
  mutate(Population2014= (1.00736463978721671 * Population2013)) %>%
  mutate(Population2015= (1.00792367505802859 * Population2014)) %>%
  mutate(Population2016= (1.00757874492811929 * Population2015)) %>%
  mutate(Population2017= (1.00679374473924223 * Population2016)) %>%
  mutate(Population2018= (1.00605929132212552 * Population2017)) %>%
  mutate(Population2019= (1.00561255390388033 * Population2018)) %>%
  mutate(Population2020= (1.00561255390388033 * Population2019)) %>% 
  mutate(Population2021= (1.00561255390388033 * Population2020)) %>%
  mutate(Population2022= (1.00561255390388033 * Population2021)) %>% 
  
  select(shortPostcode,Population2018,Population2019,Population2020,Population2021,Population2022)





Towns = unclean_house_prices %>%
  filter(County =="LANCASHIRE" | County=="LEICESTERSHIRE") %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>% 
  left_join(PopulationData,by = "shortPostcode") %>% 
  select(shortPostcode,Town,District,County,Population2018,Population2019,Population2020,
         Population2021,Population2022) %>% 
  group_by(shortPostcode) %>% 
  filter(row_number()==1) %>% 
  arrange(County) %>% 
  na.omit()




#export towns data
write.csv(Towns, "Towns.csv")



#-----------------------------------Broadband/internet speed------------------------------------------------------------




# uncleaned broadband/internet speed

Unclean_broadband_speed = read_csv("BroadBand Speed.csv")

#remove not available data
Unclean_broadband_speed = replace(Unclean_broadband_speed,is.na(Unclean_broadband_speed), 0)

# saving raw unclean data of internet speed
write.csv(Unclean_broadband_speed, "Unclean_broadband_speed.csv")

#clean data 

Unclean_broadband_speed = read_csv("Unclean_broadband_speed.csv")

clean_broadband_speed = Unclean_broadband_speed %>% 
  mutate(shortPostcode = str_trim(substring(postcode_space, 1,4))) %>% 
  group_by(shortPostcode) %>% 
  summarise_at(vars("Average download speed (Mbit/s)","Minimum download speed (Mbit/s)","Average upload speed (Mbit/s)",
                    "Minimum upload speed (Mbit/s)"),
               list(name = mean)) %>% 
  left_join(Towns,by="shortPostcode") %>%  
  filter(County =="LANCASHIRE" | County=="LEICESTERSHIRE") %>% 
  arrange(County) %>% 
  select(-County,-Town,-District,-Population2018,-Population2019,-Population2020,
         -Population2021,-Population2022) %>% 
  rename("AverageDownload"="Average download speed (Mbit/s)_name","MinDownload"="Minimum download speed (Mbit/s)_name",
         "AverageUpload"="Average upload speed (Mbit/s)_name","MinUpload"="Minimum upload speed (Mbit/s)_name") %>% 
  na.omit()

#need to write again
write.csv(clean_broadband_speed,"clean_broadband_speed.csv")








#---------------------------data clean of crime-----------------------------------------------------------



Crime_2020_01_lancashire = read.csv('crime_file/2020-01/2020-01-lancashire-street.csv')
Crime_2020_01_leicestershire = read.csv('crime_file/2020-01/2020-01-leicestershire-street.csv')

Crime_2020_02_lancashire = read.csv('crime_file/2020-02/2020-02-lancashire-street.csv')
Crime_2020_02_leicestershire = read.csv('crime_file/2020-02/2020-02-leicestershire-street.csv')

Crime_2020_03_lancashire = read.csv('crime_file/2020-03/2020-03-lancashire-street.csv')
Crime_2020_03_leicestershire = read.csv('crime_file/2020-03/2020-03-leicestershire-street.csv')

Crime_2020_04_lancashire = read.csv('crime_file/2020-04/2020-04-lancashire-street.csv')
Crime_2020_04_leicestershire = read.csv('crime_file/2020-04/2020-04-leicestershire-street.csv')

Crime_2020_05_lancashire = read.csv('crime_file/2020-05/2020-05-lancashire-street.csv')
Crime_2020_05_leicestershire = read.csv('crime_file/2020-05/2020-05-leicestershire-street.csv')

Crime_2020_06_lancashire = read.csv('crime_file/2020-06/2020-06-lancashire-street.csv')
Crime_2020_06_leicestershire = read.csv('crime_file/2020-06/2020-06-leicestershire-street.csv')

Crime_2020_07_lancashire = read.csv('crime_file/2020-07/2020-07-lancashire-street.csv')
Crime_2020_07_leicestershire = read.csv('crime_file/2020-07/2020-07-leicestershire-street.csv')

Crime_2020_08_lancashire = read.csv('crime_file/2020-08/2020-08-lancashire-street.csv')
Crime_2020_08_leicestershire = read.csv('crime_file/2020-08/2020-08-leicestershire-street.csv')

Crime_2020_09_lancashire = read.csv('crime_file/2020-09/2020-09-lancashire-street.csv')
Crime_2020_09_leicestershire = read.csv('crime_file/2020-09/2020-09-leicestershire-street.csv')

Crime_2020_10_lancashire = read.csv('crime_file/2020-10/2020-10-lancashire-street.csv')
Crime_2020_10_leicestershire = read.csv('crime_file/2020-10/2020-10-leicestershire-street.csv')

Crime_2020_11_lancashire = read.csv('crime_file/2020-11/2020-11-lancashire-street.csv')
Crime_2020_11_leicestershire = read.csv('crime_file/2020-11/2020-11-leicestershire-street.csv')

Crime_2020_12_lancashire = read.csv('crime_file/2020-12/2020-12-lancashire-street.csv')
Crime_2020_12_leicestershire = read.csv('crime_file/2020-12/2020-12-leicestershire-street.csv')

Crime_2021_01_lancashire = read.csv('crime_file/2021-01/2021-01-lancashire-street.csv')
Crime_2021_01_leicestershire = read.csv('crime_file/2021-01/2021-01-leicestershire-street.csv')

Crime_2021_02_lancashire = read.csv('crime_file/2021-02/2021-02-lancashire-street.csv')
Crime_2021_02_leicestershire = read.csv('crime_file/2021-02/2021-02-leicestershire-street.csv')

Crime_2021_03_lancashire = read.csv('crime_file/2021-03/2021-03-lancashire-street.csv')
Crime_2021_03_leicestershire = read.csv('crime_file/2021-03/2021-03-leicestershire-street.csv')

Crime_2021_04_lancashire = read.csv('crime_file/2021-04/2021-04-lancashire-street.csv')
Crime_2021_04_leicestershire = read.csv('crime_file/2021-04/2021-04-leicestershire-street.csv')

Crime_2021_05_lancashire = read.csv('crime_file/2021-05/2021-05-lancashire-street.csv')
Crime_2021_05_leicestershire = read.csv('crime_file/2021-05/2021-05-leicestershire-street.csv')

Crime_2021_06_lancashire = read.csv('crime_file/2021-06/2021-06-lancashire-street.csv')
Crime_2021_06_leicestershire = read.csv('crime_file/2021-06/2021-06-leicestershire-street.csv')

Crime_2021_07_lancashire = read.csv('crime_file/2021-07/2021-07-lancashire-street.csv')
Crime_2021_07_leicestershire = read.csv('crime_file/2021-07/2021-07-leicestershire-street.csv')

Crime_2021_08_lancashire = read.csv('crime_file/2021-08/2021-08-lancashire-street.csv')
Crime_2021_08_leicestershire = read.csv('crime_file/2021-08/2021-08-leicestershire-street.csv')

Crime_2021_09_lancashire = read.csv('crime_file/2021-09/2021-09-lancashire-street.csv')
Crime_2021_09_leicestershire = read.csv('crime_file/2021-09/2021-09-leicestershire-street.csv')

Crime_2021_10_lancashire = read.csv('crime_file/2021-10/2021-10-lancashire-street.csv')
Crime_2021_10_leicestershire = read.csv('crime_file/2021-10/2021-10-leicestershire-street.csv')

Crime_2021_11_lancashire = read.csv('crime_file/2021-11/2021-11-lancashire-street.csv')
Crime_2021_11_leicestershire = read.csv('crime_file/2021-11/2021-11-leicestershire-street.csv')

Crime_2021_12_lancashire = read.csv('crime_file/2021-12/2021-12-lancashire-street.csv')
Crime_2021_12_leicestershire = read.csv('crime_file/2021-12/2021-12-leicestershire-street.csv')


Crime_2022_01_lancashire = read.csv('crime_file/2022-01/2022-01-lancashire-street.csv')
Crime_2022_01_leicestershire = read.csv('crime_file/2022-01/2022-01-leicestershire-street.csv')

Crime_2022_02_lancashire = read.csv('crime_file/2022-02/2022-02-lancashire-street.csv')
Crime_2022_02_leicestershire = read.csv('crime_file/2022-02/2022-02-leicestershire-street.csv')

Crime_2022_03_lancashire = read.csv('crime_file/2022-03/2022-03-lancashire-street.csv')
Crime_2022_03_leicestershire = read.csv('crime_file/2022-03/2022-03-leicestershire-street.csv')

Crime_2022_04_lancashire = read.csv('crime_file/2022-04/2022-04-lancashire-street.csv')
Crime_2022_04_leicestershire = read.csv('crime_file/2022-04/2022-04-leicestershire-street.csv')

Crime_2022_05_lancashire = read.csv('crime_file/2022-05/2022-05-lancashire-street.csv')
Crime_2022_05_leicestershire = read.csv('crime_file/2022-05/2022-05-leicestershire-street.csv')

Crime_2022_06_lancashire = read.csv('crime_file/2022-06/2022-06-lancashire-street.csv')
Crime_2022_06_leicestershire = read.csv('crime_file/2022-06/2022-06-leicestershire-street.csv')

Crime_2022_07_lancashire = read.csv('crime_file/2022-07/2022-07-lancashire-street.csv')
Crime_2022_07_leicestershire = read.csv('crime_file/2022-07/2022-07-leicestershire-street.csv')

Crime_2022_08_lancashire = read.csv('crime_file/2022-08/2022-08-lancashire-street.csv')
Crime_2022_08_leicestershire = read.csv('crime_file/2022-08/2022-08-leicestershire-street.csv')

Crime_2022_09_lancashire = read.csv('crime_file/2022-09/2022-09-lancashire-street.csv')
Crime_2022_09_leicestershire = read.csv('crime_file/2022-09/2022-09-leicestershire-street.csv')

Crime_2022_10_lancashire = read.csv('crime_file/2022-10/2022-10-lancashire-street.csv')
Crime_2022_10_leicestershire = read.csv('crime_file/2022-10/2022-10-leicestershire-street.csv')


Crime_2022_11_lancashire = read.csv('crime_file/2022-11/2022-11-lancashire-street.csv',show_col_types = FALSE)
Crime_2022_11_leicestershire = read.csv('crime_file/2022-11/2022-11-leicestershire-street.csv')


crimedata= Crime_2020_01_lancashire %>%
  add_row(Crime_2020_01_leicestershire) %>% 
  
  add_row(Crime_2020_02_lancashire) %>%
  add_row(Crime_2020_02_leicestershire) %>%
  
  add_row(Crime_2020_03_lancashire) %>%
  add_row(Crime_2020_03_leicestershire) %>%
  
  add_row(Crime_2020_04_lancashire) %>%
  add_row(Crime_2020_04_leicestershire) %>%
  
  add_row(Crime_2020_05_lancashire) %>%
  add_row(Crime_2020_05_leicestershire) %>%
  
  add_row(Crime_2020_06_lancashire) %>%
  add_row(Crime_2020_06_leicestershire) %>%
  
  add_row(Crime_2020_07_lancashire) %>%
  add_row(Crime_2020_07_leicestershire) %>%
  
  add_row(Crime_2020_08_lancashire) %>%
  add_row(Crime_2020_08_leicestershire) %>%
  
  add_row(Crime_2020_09_lancashire) %>%
  add_row(Crime_2020_09_leicestershire) %>%
  
  add_row(Crime_2020_10_lancashire) %>%
  add_row(Crime_2020_10_leicestershire) %>%
  
  add_row(Crime_2020_11_lancashire) %>%
  add_row(Crime_2020_11_leicestershire) %>%
  
  add_row(Crime_2020_12_lancashire) %>%
  add_row(Crime_2020_12_leicestershire) %>%
  
  
  add_row(Crime_2021_01_lancashire) %>%
  add_row(Crime_2021_01_leicestershire) %>%
  
  add_row(Crime_2021_02_lancashire) %>%
  add_row(Crime_2021_02_leicestershire) %>%
  
  add_row(Crime_2021_03_lancashire) %>%
  add_row(Crime_2021_03_leicestershire) %>%
  
  add_row(Crime_2021_04_lancashire) %>%
  add_row(Crime_2021_04_leicestershire) %>%
  
  add_row(Crime_2021_05_lancashire) %>%
  add_row(Crime_2021_05_leicestershire) %>%
  
  add_row(Crime_2021_06_lancashire) %>%
  add_row(Crime_2021_06_leicestershire) %>%
  
  add_row(Crime_2021_07_lancashire) %>%
  add_row(Crime_2021_07_leicestershire) %>%
  
  add_row(Crime_2021_08_lancashire) %>%
  add_row(Crime_2021_08_leicestershire) %>%
  
  add_row(Crime_2021_09_lancashire) %>%
  add_row(Crime_2021_09_leicestershire) %>%
  
  add_row(Crime_2021_10_lancashire) %>%
  add_row(Crime_2021_10_leicestershire) %>%
  
  add_row(Crime_2021_11_lancashire) %>%
  add_row(Crime_2021_11_leicestershire) %>%
  
  add_row(Crime_2021_12_lancashire) %>%
  add_row(Crime_2021_12_leicestershire) %>%
  
  
  add_row(Crime_2022_01_lancashire) %>%
  add_row(Crime_2022_01_leicestershire) %>%
  
  add_row(Crime_2022_02_lancashire) %>%
  add_row(Crime_2022_02_leicestershire) %>%
  
  add_row(Crime_2022_03_lancashire) %>%
  add_row(Crime_2022_03_leicestershire) %>%
  
  add_row(Crime_2022_04_lancashire) %>%
  add_row(Crime_2022_04_leicestershire) %>%
  
  add_row(Crime_2022_05_lancashire) %>%
  add_row(Crime_2022_05_leicestershire) %>%
  
  add_row(Crime_2022_06_lancashire) %>%
  add_row(Crime_2022_06_leicestershire) %>%
  
  add_row(Crime_2022_07_lancashire) %>%
  add_row(Crime_2022_07_leicestershire) %>%
  
  add_row(Crime_2022_08_lancashire) %>%
  add_row(Crime_2022_08_leicestershire) %>%
  
  add_row(Crime_2022_09_lancashire) %>%
  add_row(Crime_2022_09_leicestershire) %>%
  
  add_row(Crime_2022_10_lancashire) %>%
  add_row(Crime_2022_10_leicestershire)  %>% 
  
  mutate(Year=substring(Month, 1,4)) %>% 
  rename(lsoa11cd="LSOA.code",CrimeType="Crime.type") %>% 
  select(lsoa11cd,Year,CrimeType) %>% 
  distinct()


Towns = read_csv("Towns.csv")
lsoa = read_csv("lsoa.csv")

lsoa  = lsoa %>% 
  mutate(shortPostcode = str_trim(substring(pcds, 1,4))) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  filter(County =="LANCASHIRE" | County=="LEICESTERSHIRE")  %>%  
  group_by(lsoa11cd) %>% 
  filter(row_number()==1) %>% 
  select(lsoa11cd,shortPostcode,Town,District,County)  
lsoa

#remove duplicate data  of lsoa
lsoa[!duplicated(lsoa), ]


cleanCrimes = crimedata %>% 
  left_join(lsoa,by="lsoa11cd")%>% 
  group_by(shortPostcode,Year,CrimeType)  %>% 
  select(shortPostcode,Year,CrimeType) %>% 
  tally()

write.csv(cleanCrimes, "cleanCrime.csv")

#---------------------------end of broadband speed-----------------------------------------------------------



Crime_2020_01_lancashire = read.csv('crime_file/2020-01/2020-01-lancashire-street.csv')
Crime_2020_01_leicestershire = read.csv('crime_file/2020-01/2020-01-leicestershire-street.csv')

Crime_2020_02_lancashire = read.csv('crime_file/2020-02/2020-02-lancashire-street.csv')
Crime_2020_02_leicestershire = read.csv('crime_file/2020-02/2020-02-leicestershire-street.csv')

Crime_2020_03_lancashire = read.csv('crime_file/2020-03/2020-03-lancashire-street.csv')
Crime_2020_03_leicestershire = read.csv('crime_file/2020-03/2020-03-leicestershire-street.csv')

Crime_2020_04_lancashire = read.csv('crime_file/2020-04/2020-04-lancashire-street.csv')
Crime_2020_04_leicestershire = read.csv('crime_file/2020-04/2020-04-leicestershire-street.csv')

Crime_2020_05_lancashire = read.csv('crime_file/2020-05/2020-05-lancashire-street.csv')
Crime_2020_05_leicestershire = read.csv('crime_file/2020-05/2020-05-leicestershire-street.csv')

Crime_2020_06_lancashire = read.csv('crime_file/2020-06/2020-06-lancashire-street.csv')
Crime_2020_06_leicestershire = read.csv('crime_file/2020-06/2020-06-leicestershire-street.csv')

Crime_2020_07_lancashire = read.csv('crime_file/2020-07/2020-07-lancashire-street.csv')
Crime_2020_07_leicestershire = read.csv('crime_file/2020-07/2020-07-leicestershire-street.csv')

Crime_2020_08_lancashire = read.csv('crime_file/2020-08/2020-08-lancashire-street.csv')
Crime_2020_08_leicestershire = read.csv('crime_file/2020-08/2020-08-leicestershire-street.csv')

Crime_2020_09_lancashire = read.csv('crime_file/2020-09/2020-09-lancashire-street.csv')
Crime_2020_09_leicestershire = read.csv('crime_file/2020-09/2020-09-leicestershire-street.csv')

Crime_2020_10_lancashire = read.csv('crime_file/2020-10/2020-10-lancashire-street.csv')
Crime_2020_10_leicestershire = read.csv('crime_file/2020-10/2020-10-leicestershire-street.csv')

Crime_2020_11_lancashire = read.csv('crime_file/2020-11/2020-11-lancashire-street.csv')
Crime_2020_11_leicestershire = read.csv('crime_file/2020-11/2020-11-leicestershire-street.csv')

Crime_2020_12_lancashire = read.csv('crime_file/2020-12/2020-12-lancashire-street.csv')
Crime_2020_12_leicestershire = read.csv('crime_file/2020-12/2020-12-leicestershire-street.csv')

Crime_2021_01_lancashire = read.csv('crime_file/2021-01/2021-01-lancashire-street.csv')
Crime_2021_01_leicestershire = read.csv('crime_file/2021-01/2021-01-leicestershire-street.csv')

Crime_2021_02_lancashire = read.csv('crime_file/2021-02/2021-02-lancashire-street.csv')
Crime_2021_02_leicestershire = read.csv('crime_file/2021-02/2021-02-leicestershire-street.csv')

Crime_2021_03_lancashire = read.csv('crime_file/2021-03/2021-03-lancashire-street.csv')
Crime_2021_03_leicestershire = read.csv('crime_file/2021-03/2021-03-leicestershire-street.csv')

Crime_2021_04_lancashire = read.csv('crime_file/2021-04/2021-04-lancashire-street.csv')
Crime_2021_04_leicestershire = read.csv('crime_file/2021-04/2021-04-leicestershire-street.csv')

Crime_2021_05_lancashire = read.csv('crime_file/2021-05/2021-05-lancashire-street.csv')
Crime_2021_05_leicestershire = read.csv('crime_file/2021-05/2021-05-leicestershire-street.csv')

Crime_2021_06_lancashire = read.csv('crime_file/2021-06/2021-06-lancashire-street.csv')
Crime_2021_06_leicestershire = read.csv('crime_file/2021-06/2021-06-leicestershire-street.csv')

Crime_2021_07_lancashire = read.csv('crime_file/2021-07/2021-07-lancashire-street.csv')
Crime_2021_07_leicestershire = read.csv('crime_file/2021-07/2021-07-leicestershire-street.csv')

Crime_2021_08_lancashire = read.csv('crime_file/2021-08/2021-08-lancashire-street.csv')
Crime_2021_08_leicestershire = read.csv('crime_file/2021-08/2021-08-leicestershire-street.csv')

Crime_2021_09_lancashire = read.csv('crime_file/2021-09/2021-09-lancashire-street.csv')
Crime_2021_09_leicestershire = read.csv('crime_file/2021-09/2021-09-leicestershire-street.csv')

Crime_2021_10_lancashire = read.csv('crime_file/2021-10/2021-10-lancashire-street.csv')
Crime_2021_10_leicestershire = read.csv('crime_file/2021-10/2021-10-leicestershire-street.csv')

Crime_2021_11_lancashire = read.csv('crime_file/2021-11/2021-11-lancashire-street.csv')
Crime_2021_11_leicestershire = read.csv('crime_file/2021-11/2021-11-leicestershire-street.csv')

Crime_2021_12_lancashire = read.csv('crime_file/2021-12/2021-12-lancashire-street.csv')
Crime_2021_12_leicestershire = read.csv('crime_file/2021-12/2021-12-leicestershire-street.csv')


Crime_2022_01_lancashire = read.csv('crime_file/2022-01/2022-01-lancashire-street.csv')
Crime_2022_01_leicestershire = read.csv('crime_file/2022-01/2022-01-leicestershire-street.csv')

Crime_2022_02_lancashire = read.csv('crime_file/2022-02/2022-02-lancashire-street.csv')
Crime_2022_02_leicestershire = read.csv('crime_file/2022-02/2022-02-leicestershire-street.csv')

Crime_2022_03_lancashire = read.csv('crime_file/2022-03/2022-03-lancashire-street.csv')
Crime_2022_03_leicestershire = read.csv('crime_file/2022-03/2022-03-leicestershire-street.csv')

Crime_2022_04_lancashire = read.csv('crime_file/2022-04/2022-04-lancashire-street.csv')
Crime_2022_04_leicestershire = read.csv('crime_file/2022-04/2022-04-leicestershire-street.csv')

Crime_2022_05_lancashire = read.csv('crime_file/2022-05/2022-05-lancashire-street.csv')
Crime_2022_05_leicestershire = read.csv('crime_file/2022-05/2022-05-leicestershire-street.csv')

Crime_2022_06_lancashire = read.csv('crime_file/2022-06/2022-06-lancashire-street.csv')
Crime_2022_06_leicestershire = read.csv('crime_file/2022-06/2022-06-leicestershire-street.csv')

Crime_2022_07_lancashire = read.csv('crime_file/2022-07/2022-07-lancashire-street.csv')
Crime_2022_07_leicestershire = read.csv('crime_file/2022-07/2022-07-leicestershire-street.csv')

Crime_2022_08_lancashire = read.csv('crime_file/2022-08/2022-08-lancashire-street.csv')
Crime_2022_08_leicestershire = read.csv('crime_file/2022-08/2022-08-leicestershire-street.csv')

Crime_2022_09_lancashire = read.csv('crime_file/2022-09/2022-09-lancashire-street.csv')
Crime_2022_09_leicestershire = read.csv('crime_file/2022-09/2022-09-leicestershire-street.csv')

Crime_2022_10_lancashire = read.csv('crime_file/2022-10/2022-10-lancashire-street.csv')
Crime_2022_10_leicestershire = read.csv('crime_file/2022-10/2022-10-leicestershire-street.csv')


Crime_2022_11_lancashire = read.csv('crime_file/2022-11/2022-11-lancashire-street.csv',show_col_types = FALSE)
Crime_2022_11_leicestershire = read.csv('crime_file/2022-11/2022-11-leicestershire-street.csv')


crimedata= Crime_2020_01_lancashire %>%
  add_row(Crime_2020_01_leicestershire) %>% 
  
  add_row(Crime_2020_02_lancashire) %>%
  add_row(Crime_2020_02_leicestershire) %>%
  
  add_row(Crime_2020_03_lancashire) %>%
  add_row(Crime_2020_03_leicestershire) %>%
  
  add_row(Crime_2020_04_lancashire) %>%
  add_row(Crime_2020_04_leicestershire) %>%
  
  add_row(Crime_2020_05_lancashire) %>%
  add_row(Crime_2020_05_leicestershire) %>%
  
  add_row(Crime_2020_06_lancashire) %>%
  add_row(Crime_2020_06_leicestershire) %>%
  
  add_row(Crime_2020_07_lancashire) %>%
  add_row(Crime_2020_07_leicestershire) %>%
  
  add_row(Crime_2020_08_lancashire) %>%
  add_row(Crime_2020_08_leicestershire) %>%
  
  add_row(Crime_2020_09_lancashire) %>%
  add_row(Crime_2020_09_leicestershire) %>%
  
  add_row(Crime_2020_10_lancashire) %>%
  add_row(Crime_2020_10_leicestershire) %>%
  
  add_row(Crime_2020_11_lancashire) %>%
  add_row(Crime_2020_11_leicestershire) %>%
  
  add_row(Crime_2020_12_lancashire) %>%
  add_row(Crime_2020_12_leicestershire) %>%
  
  
  add_row(Crime_2021_01_lancashire) %>%
  add_row(Crime_2021_01_leicestershire) %>%
  
  add_row(Crime_2021_02_lancashire) %>%
  add_row(Crime_2021_02_leicestershire) %>%
  
  add_row(Crime_2021_03_lancashire) %>%
  add_row(Crime_2021_03_leicestershire) %>%
  
  add_row(Crime_2021_04_lancashire) %>%
  add_row(Crime_2021_04_leicestershire) %>%
  
  add_row(Crime_2021_05_lancashire) %>%
  add_row(Crime_2021_05_leicestershire) %>%
  
  add_row(Crime_2021_06_lancashire) %>%
  add_row(Crime_2021_06_leicestershire) %>%
  
  add_row(Crime_2021_07_lancashire) %>%
  add_row(Crime_2021_07_leicestershire) %>%
  
  add_row(Crime_2021_08_lancashire) %>%
  add_row(Crime_2021_08_leicestershire) %>%
  
  add_row(Crime_2021_09_lancashire) %>%
  add_row(Crime_2021_09_leicestershire) %>%
  
  add_row(Crime_2021_10_lancashire) %>%
  add_row(Crime_2021_10_leicestershire) %>%
  
  add_row(Crime_2021_11_lancashire) %>%
  add_row(Crime_2021_11_leicestershire) %>%
  
  add_row(Crime_2021_12_lancashire) %>%
  add_row(Crime_2021_12_leicestershire) %>%
  
  
  add_row(Crime_2022_01_lancashire) %>%
  add_row(Crime_2022_01_leicestershire) %>%
  
  add_row(Crime_2022_02_lancashire) %>%
  add_row(Crime_2022_02_leicestershire) %>%
  
  add_row(Crime_2022_03_lancashire) %>%
  add_row(Crime_2022_03_leicestershire) %>%
  
  add_row(Crime_2022_04_lancashire) %>%
  add_row(Crime_2022_04_leicestershire) %>%
  
  add_row(Crime_2022_05_lancashire) %>%
  add_row(Crime_2022_05_leicestershire) %>%
  
  add_row(Crime_2022_06_lancashire) %>%
  add_row(Crime_2022_06_leicestershire) %>%
  
  add_row(Crime_2022_07_lancashire) %>%
  add_row(Crime_2022_07_leicestershire) %>%
  
  add_row(Crime_2022_08_lancashire) %>%
  add_row(Crime_2022_08_leicestershire) %>%
  
  add_row(Crime_2022_09_lancashire) %>%
  add_row(Crime_2022_09_leicestershire) %>%
  
  add_row(Crime_2022_10_lancashire) %>%
  add_row(Crime_2022_10_leicestershire)  %>% 
  
  mutate(Year=substring(Month, 1,4)) %>% 
  rename(lsoa11cd="LSOA.code",CrimeType="Crime.type") %>% 
  select(lsoa11cd,Year,CrimeType) %>% 
  distinct()


Towns = read_csv("Towns.csv")
lsoa = read_csv("lsoa.csv")

lsoa  = lsoa %>% 
  mutate(shortPostcode = str_trim(substring(pcds, 1,4))) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  filter(County =="LANCASHIRE" | County=="LEICESTERSHIRE")  %>%  
  group_by(lsoa11cd) %>% 
  filter(row_number()==1) %>% 
  select(lsoa11cd,shortPostcode,Town,District,County)  
lsoa

#remove duplicate data  of lsoa
lsoa[!duplicated(lsoa), ]


cleanCrimes = crimedata %>% 
  left_join(lsoa,by="lsoa11cd")%>% 
  group_by(shortPostcode,Year,CrimeType)  %>% 
  select(shortPostcode,Year,CrimeType) %>% 
  tally()

write.csv(cleanCrimes, "cleanCrime.csv")

#---------------------------end of broadband speed-----------------------------------------------------------



Crime_2020_01_lancashire = read.csv('crime_file/2020-01/2020-01-lancashire-street.csv')
Crime_2020_01_leicestershire = read.csv('crime_file/2020-01/2020-01-leicestershire-street.csv')

Crime_2020_02_lancashire = read.csv('crime_file/2020-02/2020-02-lancashire-street.csv')
Crime_2020_02_leicestershire = read.csv('crime_file/2020-02/2020-02-leicestershire-street.csv')

Crime_2020_03_lancashire = read.csv('crime_file/2020-03/2020-03-lancashire-street.csv')
Crime_2020_03_leicestershire = read.csv('crime_file/2020-03/2020-03-leicestershire-street.csv')

Crime_2020_04_lancashire = read.csv('crime_file/2020-04/2020-04-lancashire-street.csv')
Crime_2020_04_leicestershire = read.csv('crime_file/2020-04/2020-04-leicestershire-street.csv')

Crime_2020_05_lancashire = read.csv('crime_file/2020-05/2020-05-lancashire-street.csv')
Crime_2020_05_leicestershire = read.csv('crime_file/2020-05/2020-05-leicestershire-street.csv')

Crime_2020_06_lancashire = read.csv('crime_file/2020-06/2020-06-lancashire-street.csv')
Crime_2020_06_leicestershire = read.csv('crime_file/2020-06/2020-06-leicestershire-street.csv')

Crime_2020_07_lancashire = read.csv('crime_file/2020-07/2020-07-lancashire-street.csv')
Crime_2020_07_leicestershire = read.csv('crime_file/2020-07/2020-07-leicestershire-street.csv')

Crime_2020_08_lancashire = read.csv('crime_file/2020-08/2020-08-lancashire-street.csv')
Crime_2020_08_leicestershire = read.csv('crime_file/2020-08/2020-08-leicestershire-street.csv')

Crime_2020_09_lancashire = read.csv('crime_file/2020-09/2020-09-lancashire-street.csv')
Crime_2020_09_leicestershire = read.csv('crime_file/2020-09/2020-09-leicestershire-street.csv')

Crime_2020_10_lancashire = read.csv('crime_file/2020-10/2020-10-lancashire-street.csv')
Crime_2020_10_leicestershire = read.csv('crime_file/2020-10/2020-10-leicestershire-street.csv')

Crime_2020_11_lancashire = read.csv('crime_file/2020-11/2020-11-lancashire-street.csv')
Crime_2020_11_leicestershire = read.csv('crime_file/2020-11/2020-11-leicestershire-street.csv')

Crime_2020_12_lancashire = read.csv('crime_file/2020-12/2020-12-lancashire-street.csv')
Crime_2020_12_leicestershire = read.csv('crime_file/2020-12/2020-12-leicestershire-street.csv')

Crime_2021_01_lancashire = read.csv('crime_file/2021-01/2021-01-lancashire-street.csv')
Crime_2021_01_leicestershire = read.csv('crime_file/2021-01/2021-01-leicestershire-street.csv')

Crime_2021_02_lancashire = read.csv('crime_file/2021-02/2021-02-lancashire-street.csv')
Crime_2021_02_leicestershire = read.csv('crime_file/2021-02/2021-02-leicestershire-street.csv')

Crime_2021_03_lancashire = read.csv('crime_file/2021-03/2021-03-lancashire-street.csv')
Crime_2021_03_leicestershire = read.csv('crime_file/2021-03/2021-03-leicestershire-street.csv')

Crime_2021_04_lancashire = read.csv('crime_file/2021-04/2021-04-lancashire-street.csv')
Crime_2021_04_leicestershire = read.csv('crime_file/2021-04/2021-04-leicestershire-street.csv')

Crime_2021_05_lancashire = read.csv('crime_file/2021-05/2021-05-lancashire-street.csv')
Crime_2021_05_leicestershire = read.csv('crime_file/2021-05/2021-05-leicestershire-street.csv')

Crime_2021_06_lancashire = read.csv('crime_file/2021-06/2021-06-lancashire-street.csv')
Crime_2021_06_leicestershire = read.csv('crime_file/2021-06/2021-06-leicestershire-street.csv')

Crime_2021_07_lancashire = read.csv('crime_file/2021-07/2021-07-lancashire-street.csv')
Crime_2021_07_leicestershire = read.csv('crime_file/2021-07/2021-07-leicestershire-street.csv')

Crime_2021_08_lancashire = read.csv('crime_file/2021-08/2021-08-lancashire-street.csv')
Crime_2021_08_leicestershire = read.csv('crime_file/2021-08/2021-08-leicestershire-street.csv')

Crime_2021_09_lancashire = read.csv('crime_file/2021-09/2021-09-lancashire-street.csv')
Crime_2021_09_leicestershire = read.csv('crime_file/2021-09/2021-09-leicestershire-street.csv')

Crime_2021_10_lancashire = read.csv('crime_file/2021-10/2021-10-lancashire-street.csv')
Crime_2021_10_leicestershire = read.csv('crime_file/2021-10/2021-10-leicestershire-street.csv')

Crime_2021_11_lancashire = read.csv('crime_file/2021-11/2021-11-lancashire-street.csv')
Crime_2021_11_leicestershire = read.csv('crime_file/2021-11/2021-11-leicestershire-street.csv')

Crime_2021_12_lancashire = read.csv('crime_file/2021-12/2021-12-lancashire-street.csv')
Crime_2021_12_leicestershire = read.csv('crime_file/2021-12/2021-12-leicestershire-street.csv')


Crime_2022_01_lancashire = read.csv('crime_file/2022-01/2022-01-lancashire-street.csv')
Crime_2022_01_leicestershire = read.csv('crime_file/2022-01/2022-01-leicestershire-street.csv')

Crime_2022_02_lancashire = read.csv('crime_file/2022-02/2022-02-lancashire-street.csv')
Crime_2022_02_leicestershire = read.csv('crime_file/2022-02/2022-02-leicestershire-street.csv')

Crime_2022_03_lancashire = read.csv('crime_file/2022-03/2022-03-lancashire-street.csv')
Crime_2022_03_leicestershire = read.csv('crime_file/2022-03/2022-03-leicestershire-street.csv')

Crime_2022_04_lancashire = read.csv('crime_file/2022-04/2022-04-lancashire-street.csv')
Crime_2022_04_leicestershire = read.csv('crime_file/2022-04/2022-04-leicestershire-street.csv')

Crime_2022_05_lancashire = read.csv('crime_file/2022-05/2022-05-lancashire-street.csv')
Crime_2022_05_leicestershire = read.csv('crime_file/2022-05/2022-05-leicestershire-street.csv')

Crime_2022_06_lancashire = read.csv('crime_file/2022-06/2022-06-lancashire-street.csv')
Crime_2022_06_leicestershire = read.csv('crime_file/2022-06/2022-06-leicestershire-street.csv')

Crime_2022_07_lancashire = read.csv('crime_file/2022-07/2022-07-lancashire-street.csv')
Crime_2022_07_leicestershire = read.csv('crime_file/2022-07/2022-07-leicestershire-street.csv')

Crime_2022_08_lancashire = read.csv('crime_file/2022-08/2022-08-lancashire-street.csv')
Crime_2022_08_leicestershire = read.csv('crime_file/2022-08/2022-08-leicestershire-street.csv')

Crime_2022_09_lancashire = read.csv('crime_file/2022-09/2022-09-lancashire-street.csv')
Crime_2022_09_leicestershire = read.csv('crime_file/2022-09/2022-09-leicestershire-street.csv')

Crime_2022_10_lancashire = read.csv('crime_file/2022-10/2022-10-lancashire-street.csv')
Crime_2022_10_leicestershire = read.csv('crime_file/2022-10/2022-10-leicestershire-street.csv')


Crime_2022_11_lancashire = read.csv('crime_file/2022-11/2022-11-lancashire-street.csv',show_col_types = FALSE)
Crime_2022_11_leicestershire = read.csv('crime_file/2022-11/2022-11-leicestershire-street.csv')


crimedata= Crime_2020_01_lancashire %>%
  add_row(Crime_2020_01_leicestershire) %>% 
  
  add_row(Crime_2020_02_lancashire) %>%
  add_row(Crime_2020_02_leicestershire) %>%
  
  add_row(Crime_2020_03_lancashire) %>%
  add_row(Crime_2020_03_leicestershire) %>%
  
  add_row(Crime_2020_04_lancashire) %>%
  add_row(Crime_2020_04_leicestershire) %>%
  
  add_row(Crime_2020_05_lancashire) %>%
  add_row(Crime_2020_05_leicestershire) %>%
  
  add_row(Crime_2020_06_lancashire) %>%
  add_row(Crime_2020_06_leicestershire) %>%
  
  add_row(Crime_2020_07_lancashire) %>%
  add_row(Crime_2020_07_leicestershire) %>%
  
  add_row(Crime_2020_08_lancashire) %>%
  add_row(Crime_2020_08_leicestershire) %>%
  
  add_row(Crime_2020_09_lancashire) %>%
  add_row(Crime_2020_09_leicestershire) %>%
  
  add_row(Crime_2020_10_lancashire) %>%
  add_row(Crime_2020_10_leicestershire) %>%
  
  add_row(Crime_2020_11_lancashire) %>%
  add_row(Crime_2020_11_leicestershire) %>%
  
  add_row(Crime_2020_12_lancashire) %>%
  add_row(Crime_2020_12_leicestershire) %>%
  
  
  add_row(Crime_2021_01_lancashire) %>%
  add_row(Crime_2021_01_leicestershire) %>%
  
  add_row(Crime_2021_02_lancashire) %>%
  add_row(Crime_2021_02_leicestershire) %>%
  
  add_row(Crime_2021_03_lancashire) %>%
  add_row(Crime_2021_03_leicestershire) %>%
  
  add_row(Crime_2021_04_lancashire) %>%
  add_row(Crime_2021_04_leicestershire) %>%
  
  add_row(Crime_2021_05_lancashire) %>%
  add_row(Crime_2021_05_leicestershire) %>%
  
  add_row(Crime_2021_06_lancashire) %>%
  add_row(Crime_2021_06_leicestershire) %>%
  
  add_row(Crime_2021_07_lancashire) %>%
  add_row(Crime_2021_07_leicestershire) %>%
  
  add_row(Crime_2021_08_lancashire) %>%
  add_row(Crime_2021_08_leicestershire) %>%
  
  add_row(Crime_2021_09_lancashire) %>%
  add_row(Crime_2021_09_leicestershire) %>%
  
  add_row(Crime_2021_10_lancashire) %>%
  add_row(Crime_2021_10_leicestershire) %>%
  
  add_row(Crime_2021_11_lancashire) %>%
  add_row(Crime_2021_11_leicestershire) %>%
  
  add_row(Crime_2021_12_lancashire) %>%
  add_row(Crime_2021_12_leicestershire) %>%
  
  
  add_row(Crime_2022_01_lancashire) %>%
  add_row(Crime_2022_01_leicestershire) %>%
  
  add_row(Crime_2022_02_lancashire) %>%
  add_row(Crime_2022_02_leicestershire) %>%
  
  add_row(Crime_2022_03_lancashire) %>%
  add_row(Crime_2022_03_leicestershire) %>%
  
  add_row(Crime_2022_04_lancashire) %>%
  add_row(Crime_2022_04_leicestershire) %>%
  
  add_row(Crime_2022_05_lancashire) %>%
  add_row(Crime_2022_05_leicestershire) %>%
  
  add_row(Crime_2022_06_lancashire) %>%
  add_row(Crime_2022_06_leicestershire) %>%
  
  add_row(Crime_2022_07_lancashire) %>%
  add_row(Crime_2022_07_leicestershire) %>%
  
  add_row(Crime_2022_08_lancashire) %>%
  add_row(Crime_2022_08_leicestershire) %>%
  
  add_row(Crime_2022_09_lancashire) %>%
  add_row(Crime_2022_09_leicestershire) %>%
  
  add_row(Crime_2022_10_lancashire) %>%
  add_row(Crime_2022_10_leicestershire)  %>% 
  
  mutate(Year=substring(Month, 1,4)) %>% 
  rename(lsoa11cd="LSOA.code",CrimeType="Crime.type") %>% 
  select(lsoa11cd,Year,CrimeType) %>% 
  distinct()


Towns = read_csv("Towns.csv")
lsoa = read_csv("lsoa.csv")

lsoa  = lsoa %>% 
  mutate(shortPostcode = str_trim(substring(pcds, 1,4))) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  filter(County =="LANCASHIRE" | County=="LEICESTERSHIRE")  %>%  
  group_by(lsoa11cd) %>% 
  filter(row_number()==1) %>% 
  select(lsoa11cd,shortPostcode,Town,District,County)  
lsoa

#remove duplicate data  of lsoa
lsoa[!duplicated(lsoa), ]


cleanCrimes = crimedata %>% 
  left_join(lsoa,by="lsoa11cd")%>% 
  group_by(shortPostcode,Year,CrimeType)  %>% 
  select(shortPostcode,Year,CrimeType) %>% 
  tally()

write.csv(cleanCrimes, "cleanCrime.csv")





#---------------------------school data cleaning------------------------------------------------------------------------------------

LancashireSchool16 = read.csv('school_file/2016-2017_888_ks4final.csv') %>% 
  mutate(Year=2016)
LancashireSchool17 = read.csv('school_file/2017-2018_888_ks4final.csv') %>% 
  mutate(Year=2017)
LancashireSchool18 = read.csv('school_file/2018-2019_888_ks4final.csv') %>% 
  mutate(Year=2018)
LancashireSchool21 = read.csv('school_file/2021-2022_888_ks4final.csv') %>% 
  mutate(Year=2021)

LeicestershireSchool16 = read.csv('school_file/2016-2017_855_ks4final.csv') %>% 
  mutate(Year=2016)
LeicestershireSchool17 = read.csv('school_file/2017-2018_855_ks4final.csv') %>% 
  mutate(Year=2017)
LeicestershireSchool18 = read.csv('school_file/2018-2019_855_ks4final.csv') %>% 
  mutate(Year=2018)
LeicestershireSchool21 = read.csv('school_file/2021-2022_855_ks4final.csv') %>% 
  mutate(Year=2021)

LancashireSchool16 = select(LancashireSchool17, Year, PCODE, SCHNAME, ATT8SCR)
LancashireSchool17 = select(LancashireSchool17, Year, PCODE, SCHNAME, ATT8SCR)
LancashireSchool18 = select(LancashireSchool18, Year, PCODE, SCHNAME, ATT8SCR)
LancashireSchool21 = select(LancashireSchool21, Year, PCODE, SCHNAME, ATT8SCR)
LancashireSchool17 = select(LancashireSchool17, Year, PCODE, SCHNAME, ATT8SCR)

LeicestershireSchool16 = select(LancashireSchool17, Year, PCODE, SCHNAME, ATT8SCR)
LeicestershireSchool17 = select(LancashireSchool17, Year, PCODE, SCHNAME, ATT8SCR)
LeicestershireSchool18 = select(LancashireSchool18, Year, PCODE, SCHNAME, ATT8SCR)
LeicestershireSchool21 = select(LancashireSchool21, Year, PCODE, SCHNAME, ATT8SCR)




schoolData = LancashireSchool21 %>%  
  add_row(LancashireSchool18) %>% 
  add_row(LancashireSchool17) %>% 
  add_row(LancashireSchool16) %>%
  add_row(LeicestershireSchool21) %>% 
  add_row(LeicestershireSchool18) %>% 
  add_row(LeicestershireSchool17) %>% 
  add_row(LeicestershireSchool16) %>% 
  mutate(shortPostCode = str_trim(substring(PCODE,1,4))) %>% 
  filter(ATT8SCR != "SUPP" & ATT8SCR != "NE") %>%  
  mutate(ID = row_number()) %>%  
  select(ID, Year, PCODE, shortPostCode, SCHNAME, ATT8SCR) %>% 
  na.omit()
colnames(schoolData) = c("ID", "Year", "PostCode", "shortPostCode", "SchoolName", "Attainment8Score")


#save csv file of cleaned school data
write.csv(schoolData, "NewcleanSchool.csv") 





