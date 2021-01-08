
###########  Libraries ########### 
library(dplyr)
library(caret)
library(mice)
library(caTools)
library(tidyr)
library(nnet)
library(car)
library(ggmap)

###########  Data Tranformation ########### 

data = read.csv('/Users/Demi/Downloads/R_Summer_2020/HW/Crimes_Data_trimmed.csv')
# Drop unnecessary columns
cols.dont.want = c('ID','Case.Number','IUCR','Location')
data = data[, ! names(data) %in% cols.dont.want, drop = F]
# Transform date columns from factor to date
data$Date = strptime(as.character.Date(data$Date), format='%m/%d/%Y %r')
data$Updated.On = strptime(as.character.Date(data$Updated.On), format='%m/%d/%Y %r')
# Transform factor columns to string
cols.factor.to.str = c('Block','Description','Location.Description')
data[cols.factor.to.str] <- lapply(data[cols.factor.to.str],function(x) as.character(x))
# Transform int to factor
cols.int = c('Beat','District','Ward','Community.Area')

# 36 total levels of criminal types, we regroup those 36 levels to 8 new categories.
data$newgrouped_type[data$Primary.Type == 'THEFT' | data$Primary.Type == 'MOTOR VEHICLE THEFT'] <- 'Theft'
data$newgrouped_type[data$Primary.Type == 'BATTERY' | data$Primary.Type == 'DOMESTIC VIOLENCE'] <- 'Battery'
data$newgrouped_type[data$Primary.Type == 'CRIMINAL DAMAGE'] <- 'Criminal Damage'
data$newgrouped_type[data$Primary.Type == 'NARCOTICS' | data$Primary.Type == 'OTHER NARCOTIC VIOLATION'] <- 'Narcotics'
data$newgrouped_type[data$Primary.Type == 'ASSAULT' | data$Primary.Type == "CRIM SEXUAL ASSAULT" | data$Primary.Type == "CRIMINAL SEXUAL ASSAULT"] <- 'Assault'
data$newgrouped_type[data$Primary.Type == 'BURGLARY'] <- 'Burglary'
data$newgrouped_type[data$Primary.Type == 'ROBBERY'] <- 'Robbery'
data$newgrouped_type[data$Primary.Type == 'ARSON' | data$Primary.Type == 'CONCEALED CARRY LICENSE VIOLATION' | data$Primary.Type == 'CRIMINAL TRESPASS' | data$Primary.Type == 'DECEPTIVE PRACTICE' | data$Primary.Type == 'GAMBLING' | data$Primary.Type == 'HOMICIDE' | data$Primary.Type == 'HUMAN TRAFFICKING' | data$Primary.Type == 'INTERFERENCE WITH PUBLIC OFFICER' | data$Primary.Type == 'INTIMIDATION' | data$Primary.Type == 'KIDNAPPING' | data$Primary.Type == 'LIQUOR LAW VIOLATION' | data$Primary.Type == 'NON - CRIMINAL' | data$Primary.Type == 'NON-CRIMINAL' | data$Primary.Type == 'NON-CRIMINAL (SUBJECT SPECIFIED)'| data$Primary.Type == 'OBSCENITY' | data$Primary.Type == 'OFFENSE INVOLVING CHILDREN'|data$Primary.Type == 'OTHER OFFENSE'|data$Primary.Type == 'PROSTITUTION'|data$Primary.Type =='PUBLIC INDECENCY'| data$Primary.Type == 'PUBLIC PEACE VIOLATION' | data$Primary.Type== 'RITUALISM'|data$Primary.Type == 'SEX OFFENSE'| data$Primary.Type == 'STALKING' |data$Primary.Type == "WEAPONS VIOLATION"] <- 'Others'        
data$newgrouped_type = as.factor(data$newgrouped_type)

# our raw data has 169 levels of location description and we regroup them to 9 categories
data$newgrouped_location[data$Location.Description == 'ABANDONED BUILDING' | data$Location.Description == 'AIRPORT BUILDING NON-TERMINAL - NON-SECURE AREA' | data$Location.Description == 'AIRPORT BUILDING NON-TERMINAL - SECURE AREA' |data$Location.Description == 'AIRPORT EXTERIOR - NON-SECURE AREA' | data$Location.Description == 'AIRPORT EXTERIOR - SECURE AREA' |data$Location.Description == 'AIRPORT TERMINAL LOWER LEVEL - NON-SECURE AREA' |data$Location.Description == 'AIRPORT TERMINAL LOWER LEVEL - SECURE AREA' | data$Location.Description == 'AIRPORT TERMINAL MEZZANINE - NON-SECURE AREA' |data$Location.Description == 'AIRPORT TERMINAL UPPER LEVEL - NON-SECURE AREA' | data$Location.Description == 'AIRPORT TERMINAL UPPER LEVEL - SECURE AREA' |data$Location.Description == 'AIRPORT VENDING ESTABLISHMENT' | data$Location.Description == 'ANIMAL HOSPITAL' |data$Location.Description == 'ATHLETIC CLUB' | data$Location.Description == 'CEMETARY' |data$Location.Description == 'CHURCH' | data$Location.Description == 'CHURCH / SYNAGOGUE / PLACE OF WORSHIP' | 
                           data$Location.Description == 'CHURCH/SYNAGOGUE/PLACE OF WORSHIP' | data$Location.Description == 'COIN OPERATED MACHINE' |data$Location.Description == 'COMMERCIAL / BUSINESS OFFICE' | data$Location.Description == 'CONSTRUCTION SITE' |data$Location.Description == 'FACTORY / MANUFACTURING BUILDING' | data$Location.Description == 'FACTORY/MANUFACTURING BUILDING' | data$Location.Description == 'FARM' | data$Location.Description == 'FEDERAL BUILDING' |data$Location.Description == 'FOREST PRESERVE' | data$Location.Description == 'GOVERNMENT BUILDING / PROPERTY' |data$Location.Description == 'GOVERNMENT BUILDING/PROPERTY' | data$Location.Description == 'HOSPITAL BUILDING / GROUNDS'| data$Location.Description == 'HOSPITAL BUILDING/GROUNDS' |data$Location.Description == 'JAIL / LOCK-UP FACILITY' |data$Location.Description == 'LAKEFRONT / WATERFRONT / RIVERBANK' |data$Location.Description == 'LAKEFRONT/WATERFRONT/RIVERBANK' |data$Location.Description == 'LIBRARY' |data$Location.Description == 'MEDICAL / DENTAL OFFICE'|
                           data$Location.Description == 'MEDICAL/DENTAL OFFICE' |data$Location.Description == 'MOVIE HOUSE/THEATER' |data$Location.Description == 'OFFICE' |data$Location.Description == 'OTHER' |data$Location.Description == 'OTHER (SPECIFY)' |data$Location.Description == 'POOL ROOM' |data$Location.Description == 'PRAIRIE' |data$Location.Description == 'RIVER' |data$Location.Description == 'RIVER BANK' |data$Location.Description == 'SAVINGS AND LOAN' |data$Location.Description == 'SPORTS ARENA / STADIUM' |data$Location.Description == 'STAIRWELL' |data$Location.Description == 'SPORTS ARENA/STADIUM' |data$Location.Description == 'YARD' 
                         ]<- 'Others'
data$newgrouped_location[data$Location.Description == 'COLLEGE / UNIVERSITY - GROUNDS' |data$Location.Description == 'COLLEGE/UNIVERSITY GROUNDS' |data$Location.Description == 'COLLEGE/UNIVERSITY RESIDENCE HALL' |data$Location.Description == 'SCHOOL - PRIVATE BUILDING' |data$Location.Description == 'SCHOOL - PRIVATE GROUNDS'|data$Location.Description == 'SCHOOL - PUBLIC BUILDING' |data$Location.Description == 'SCHOOL - PUBLIC GROUNDS' |data$Location.Description == 'SCHOOL YARD' |data$Location.Description == 'SCHOOL, PRIVATE, BUILDING' |data$Location.Description == 'SCHOOL, PRIVATE, GROUNDS' |data$Location.Description == 'SCHOOL, PUBLIC, BUILDING' |data$Location.Description == 'SCHOOL, PUBLIC, GROUNDS' ] <- 'School'
data$newgrouped_location[data$Location.Description == 'ALLEY'|data$Location.Description == 'BOWLING ALLEY'] <- 'Alley'
data$newgrouped_location[data$Location.Description == 'APPLIANCE STORE' |data$Location.Description == 'AUTO / BOAT / RV DEALERSHIP' |data$Location.Description == 'BANK' |data$Location.Description == 'BARBER SHOP/BEAUTY SALON' |data$Location.Description == 'BARBERSHOP'|data$Location.Description == 'CAR WASH' |data$Location.Description == 'CLEANING STORE' |data$Location.Description == 'CONVENIENCE STORE' |data$Location.Description == 'CREDIT UNION' |
                           data$Location.Description == 'CTA GARAGE / OTHER PROPERTY' |data$Location.Description == 'CURRENCY EXCHANGE' |data$Location.Description == 'DAY CARE CENTER' |data$Location.Description == 'DEPARTMENT STORE' |data$Location.Description == 'DRUG STORE' |data$Location.Description == 'GROCERY FOOD STORE' |data$Location.Description == 'LIQUOR STORE' |data$Location.Description == 'NEWSSTAND' |data$Location.Description == 'PAWN SHOP' |data$Location.Description == 'RETAIL STORE' |data$Location.Description == 'SMALL RETAIL STORE' |data$Location.Description == 'TAVERN / LIQUOR STORE' |
                           data$Location.Description == 'TAVERN/LIQUOR STORE' |data$Location.Description == 'WAREHOUSE'
                         ]<- 'Merchandise stores/shops'
data$newgrouped_location[data$Location.Description == 'ATM (AUTOMATIC TELLER MACHINE)' |data$Location.Description == 'BRIDGE' |data$Location.Description == 'CTA BUS STOP' |data$Location.Description == 'CTA STATION' |data$Location.Description == 'FIRE STATION' | data$Location.Description == 'GANGWAY' |data$Location.Description == 'GAS STATION' |data$Location.Description == 'GAS STATION DRIVE/PROP.' |data$Location.Description == 'HIGHWAY / EXPRESSWAY' |data$Location.Description == 'HIGHWAY/EXPRESSWAY' 
                         |data$Location.Description == 'SIDEWALK' |data$Location.Description == 'STREET' ] <- 'Street'
data$newgrouped_location[data$Location.Description == 'BAR OR TAVERN'|data$Location.Description == 'CLUB' |data$Location.Description == 'HOTEL' |data$Location.Description == 'HOTEL / MOTEL' |data$Location.Description == 'HOTEL/MOTEL' |data$Location.Description == 'MOTEL'|data$Location.Description == 'RESTAURANT' |data$Location.Description == 'TAVERN' ] <- 'Hotel/restaurant/Club'
data$newgrouped_location[data$Location.Description == 'AIRPORT PARKING LOT' | data$Location.Description == 'CHA PARKING LOT' | data$Location.Description == 'CHA PARKING LOT / GROUNDS' |data$Location.Description == 'CHA PARKING LOT/GROUNDS' | data$Location.Description == 'CTA PARKING LOT / GARAGE / OTHER PROPERTY' |data$Location.Description == 'GARAGE' | data$Location.Description == 'GARAGE/AUTO REPAIR' |data$Location.Description == 'PARK PROPERTY'|
                           data$Location.Description == 'PARKING LOT' |data$Location.Description == 'PARKING LOT / GARAGE (NON RESIDENTIAL)' |data$Location.Description == 'PARKING LOT/GARAGE(NON.RESID.)' |data$Location.Description == 'POLICE FACILITY / VEHICLE PARKING LOT' |data$Location.Description == 'POLICE FACILITY/VEH PARKING LOT' |data$Location.Description == 'VACANT LOT' |data$Location.Description == 'VACANT LOT / LAND' |data$Location.Description == 'VACANT LOT/LAND' 
                         ] <- 'Parking Lot'
data$newgrouped_location[data$Location.Description == 'AIRCRAFT' | data$Location.Description == 'AIRPORT TRANSPORTATION SYSTEM (ATS)' | data$Location.Description == 'AIRPORT/AIRCRAFT' |data$Location.Description == 'AUTO' | data$Location.Description == 'BOAT/WATERCRAFT'| data$Location.Description == 'CTA BUS' |data$Location.Description == 'CTA PLATFORM' |data$Location.Description == 'CTA TRACKS - RIGHT OF WAY' |data$Location.Description == 'CTA TRAIN'  |data$Location.Description == 'DELIVERY TRUCK' |data$Location.Description == 'OTHER COMMERCIAL TRANSPORTATION' |data$Location.Description == 'OTHER RAILROAD PROP / TRAIN DEPOT' |data$Location.Description == 'OTHER RAILROAD PROPERTY / TRAIN DEPOT'|
                           data$Location.Description == 'RAILROAD PROPERTY' |data$Location.Description == 'TAXI CAB' |data$Location.Description == 'TAXICAB' |data$Location.Description == 'TRAILER' |data$Location.Description == 'VEHICLE - COMMERCIAL'|data$Location.Description == 'VEHICLE - DELIVERY TRUCK' |data$Location.Description == 'VEHICLE - OTHER RIDE SERVICE' |data$Location.Description == 'VEHICLE - OTHER RIDE SHARE SERVICE (E.G., UBER, LYFT)' |data$Location.Description == 'VEHICLE - OTHER RIDE SHARE SERVICE (LYFT, UBER, ETC.)' |data$Location.Description == 'VEHICLE NON-COMMERCIAL' |data$Location.Description == 'VEHICLE-COMMERCIAL'
                         ] <- 'transportation services'
data$newgrouped_location[data$Location.Description == 'BASEMENT'|data$Location.Description == 'APARTMENT' |data$Location.Description == 'CHA APARTMENT' |data$Location.Description == 'CHA GROUNDS' |data$Location.Description == 'CHA HALLWAY'|data$Location.Description == 'CHA HALLWAY / STAIRWELL / ELEVATOR' |data$Location.Description == 'CHA HALLWAY/STAIRWELL/ELEVATOR' |data$Location.Description == 'CHA LOBBY' |data$Location.Description == 'DRIVEWAY - RESIDENTIAL' |data$Location.Description == 'HALLWAY' |data$Location.Description == 'HOUSE' |data$Location.Description == 'NURSING / RETIREMENT HOME' |data$Location.Description == 'NURSING HOME/RETIREMENT HOME' |data$Location.Description == 'PORCH'|  
                           data$Location.Description == 'RESIDENCE' |data$Location.Description == 'RESIDENCE - GARAGE' |data$Location.Description == 'RESIDENCE - PORCH / HALLWAY' |data$Location.Description == 'RESIDENCE - YARD (FRONT / BACK)' |data$Location.Description == 'RESIDENCE PORCH/HALLWAY' |data$Location.Description == 'RESIDENCE-GARAGE' |data$Location.Description == 'RESIDENTIAL YARD (FRONT/BACK)' |data$Location.Description == 'VESTIBULE' 
                         ] <- 'Residence/Apartments'

data$newgrouped_location[is.na(data$newgrouped_location)] <- 'Others'
data$newgrouped_location = as.factor(data$newgrouped_location)
                           
# Check NA
factorcol = c(colnames(data)[sapply(data,is.factor)])
strcol = c(colnames(data)[sapply(data,is.character)])
na_in_factorcol = c(colnames(data)[colSums(is.na(data[factorcol])) > 0])
na_in_strcol = c(colnames(data)[colSums(is.na(data[strcol])) > 0])
na_in_data = c(colnames(data)[colSums(is.na(data)) > 0])
  # The results show that no factor/string columns contain NA, all NA in numeric cols

data1 = na.omit(data)

###########  Analysis of time and crime_numbers & arrest rate ########### 

#Further clean Date Column and seperate it to Year, Month, Date, and Hour as data2
str(data1)

data1$Date = as.character(data1$Date)
data1$Updated.On = as.character(data1$Updated.On)

data1 = data1 %>% separate(Date, 
                c("Year", "Month",'Date','Hour'))
data1$Year = as.integer(data1$Year)
data1$Month = as.integer(data1$Month)
data1$Date = as.integer(data1$Date)
data1$Hour = as.integer(data1$Hour)
data1$Hour[data1$Hour== 00] = 24

data2 = data1[, ! names(data1) %in% c('Updated.On'), drop = F]

str(data2)

# Analyze Hour & arrest
Hour_data = data2 %>% 
  group_by(Hour,Arrest) %>%
  summarise(n())

Hour_sum = aggregate(Hour_data$`n()`, by=list(Hour=Hour_data$Hour), FUN=sum)
Hour_data = inner_join(Hour_data,Hour_sum)
Hour_data$Percentage = Hour_data$`n()`/Hour_data$x
ggplot(data = Hour_data, aes(Hour, Percentage))+
  geom_col()+
  facet_grid(cols= vars(Arrest))
  # The result shows that crimes in 11am-11pm on average got arrested, Hourc column can be used to predict arrest or not

# Analyze Month & arrest
Month_data = data2 %>% 
  group_by(Month,Arrest) %>%
  summarise(n())

Month_sum = aggregate(Month_data$`n()`, by=list(Month=Month_data$Month), FUN=sum)
Month_data = inner_join(Month_data,Month_sum)
Month_data$Percentage = Month_data$`n()`/Month_data$x
ggplot(data = Month_data, aes(Month, Percentage))+
  geom_col()+
  facet_grid(cols= vars(Arrest))
  # The result shows that the arrest rates do not vary much by months

# Analyze Date & arrest
Date_data = data2 %>% 
  group_by(Date,Arrest) %>%
  summarise(n())

Date_sum = aggregate(Date_data$`n()`, by=list(Date=Date_data$Date), FUN=sum)
Date_data = inner_join(Date_data,Date_sum)
Date_data$Percentage = Date_data$`n()`/Date_data$x

ggplot(data = Date_data, aes(Date, Percentage))+
  geom_col()+
  facet_grid(cols= vars(Arrest))
  #  The result shows that crimes on 1st and 31st are significantly harder to be arrested.

# Graph of Date, Month, Hour crime numbers
gplot(Date_data, aes(Date, x))+
  geom_col()
ggplot(Month_data, aes(Month, x))+
  geom_col()
ggplot(Hour_data, aes(Hour, x))+
  geom_col()

#According to the analysis based on "Year", "Month",'Date','Hour' columns, create new columns for analysis
data2$Date_is_1_or_31[data2$Date == 1 | data2$Date == 31] = TRUE
data2$Date_is_1_or_31[!(data2$Date == 1 | data2$Date == 31)] = FALSE
data2$Date_is_1_or_31= as.factor(data2$Date_is_1_or_31)

data2$Hour_in_11am_11pm[data2$Hour >= 11 & data2$Hour <= 23] = TRUE
data2$Hour_in_11am_11pm[!(data2$Hour >= 11 & data2$Hour <= 23)] = FALSE
data2$Hour_in_11am_11pm = as.factor(data2$Hour_in_11am_11pm)

########### Analysis of crime and arrest rate by areas ########### 

##### By ward (too specific, no income data)
data_crime_by_ward = data2 %>% 
  group_by(Ward) %>%
  summarise(n())
data_crime_by_ward
ggplot(data=data_crime_by_ward, aes(x= Ward, y= `n()`))+
  geom_col()
##### Crime By area
data_crime_by_area = data2 %>% 
  group_by(Community.Area) %>%
  summarise(n())
ggplot(data=data_crime_by_area, aes(x= Community.Area, y= `n()`))+
  geom_col()  

  # Find the worst 10 areas
data2 %>%
  group_by(Community.Area) %>%
  summarise(n()) %>%
  arrange(desc(`n()`)) %>%
  top_n(n=10)
##### Connect income with crime numbers 
income = read.csv('/Users/Demi/Downloads/R_Summer_2020/HW/Census_Data_Chicago_by_Area.csv')
area_income_crime = inner_join(data_crime_by_area, income, by = c('Community.Area' ='Community.Area.Number' ))
names(area_income_crime)[names(area_income_crime) == "n()"] =  "no.crime"
area_income_crime$POPULATION = as.character(area_income_crime$POPULATION)
area_income_crime$POPULATION = as.integer(gsub(",", "", paste(area_income_crime$POPULATION)))
area_income_crime$no.crime.by.pop = area_income_crime$no.crime / area_income_crime$POPULATION
cor(area_income_crime$no.crime.by.pop,area_income_crime$PER.CAPITA.INCOME)
scatterplot(no.crime.by.pop ~ PER.CAPITA.INCOME, data=area_income_crime)
  # cor = -0.3 higher income areas tend to have less crime

##### Arrest rate by area
data2$Arrest_int = as.integer(data2$Arrest)
data2$Arrest_int[data2$Arrest_int == 1] = 0
data2$Arrest_int[data2$Arrest_int == 2] = 1
data_arrest_by_area = data2 %>%
  select(Arrest_int,Community.Area) %>%
  filter(Arrest_int == 1) %>%
  group_by(Community.Area) %>%
  summarise(n())
names(data_arrest_by_area)[names(data_arrest_by_area) == "n()"] = "no.arrest"
data_arrest_rate_by_area = inner_join(data_arrest_by_area,data_crime_by_area)
data_arrest_rate_by_area$arrest.rate = data_arrest_rate_by_area$no.arrest/data_arrest_rate_by_area$`n()`
data_arrest_rate_by_area
ggplot(data=data_arrest_rate_by_area, aes(x= Community.Area, y= arrest.rate))+
  geom_col()
  #get a graph about arrest rate by area.

data_arrest_rate_by_area %>%
  arrange(desc(arrest.rate)) %>%
  top_n(n=10)

data_arrest_rate_by_area %>%
  arrange(arrest.rate) %>%
  top_n(n=10)
  # Compared with the chart we found about crime numbers in each area, we found most communities with more crimes happening tend to have higher arrest rates.

##### Connect income with arrest rates 

area_income_crime = inner_join(area_income_crime, data_arrest_rate_by_area, by = c('Community.Area' ='Community.Area' ))
cor(area_income_crime$arrest.rate,area_income_crime$PER.CAPITA.INCOME)
scatterplot(arrest.rate ~ PER.CAPITA.INCOME, data=area_income_crime)

  #correlation is -0.36, higher income, lower arrest rate, which is interesting.

    #income should be one of the indicator of whether the arrest can be made, so decided to add income to the data
area_income = area_income_crime %>%
  select (Community.Area,PER.CAPITA.INCOME)

data3 = left_join(data2,area_income)

########### Spacial analysis with Google Map ########### 

register_google(key = 'AIzaSyD9lBYJK5te3xz2JUBsACq4iYRBfZZNtr4')

chi_bb <- c(left = -87.936287,
            bottom = 41.679835,
            right = -87.447052,
            top = 42.000835)

chicago_map <- get_stamenmap(bbox = chi_bb,
                                zoom = 11)
ggmap(chicago_map) +
  geom_point(data = data3,
             aes(x = Longitude,
                 y = Latitude),
             size = .05,
             alpha = .01)


# Turn True or False to 0 or 1. 
data1$Arrest_int = as.numeric(data1$Arrest)-1

###########  location distribution ########### 
data1$Count <- 1
aggr1 <- aggregate(Count ~ newgrouped_location, data = data1, FUN = sum)
aggr1$newgrouped_location <- factor(aggr1$newgrouped_location, levels = aggr1$newgrouped_location[order(-aggr1$Count)])
ggplot(aggr1, aes(x = newgrouped_location, y = Count)) + theme_minimal() + geom_bar(stat="identity", width=0.5, fill = "steelblue") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x = "Location", y = "Number of crimes")+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())

  #crime type evolution by time, we saw a decreasing trend
aggr2 <- aggregate(Count ~ newgrouped_type + Year, data = data1, FUN = sum)
ggplot(data=aggr2, aes(x=Year, y=Count, group = newgrouped_type, colour = newgrouped_type)) +
  geom_line() + geom_point() + theme_minimal() + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())+ theme(legend.title=element_blank())

 #Heatmap (Location versus crime type)
aggr3 <- aggregate(Count ~  newgrouped_type + newgrouped_location, data = data1, FUN = sum)
p2 <- ggplot(data = aggr3, aes(x = newgrouped_location, y = newgrouped_type)) + geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "white", high = "red") 
p2+ theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size= 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) 


########### Model to predict Arrest ########### 

##### linear regression 
  #data preparation before using linear regression
data3 = left_join(data2,area_income)
data3$Date_is_1_or_31_int = as.numeric(data3$Date_is_1_or_31) -1
data3$Hour_in_11am_11pm_int = as.numeric(data3$Hour_in_11am_11pm) -1
data3$Domestic_int = as.numeric(data3$Domestic) -1
data3 = inner_join(data3,data_arrest_rate_by_area)
  #Arrest rate by location
data_arrest_by_location = data2 %>%
  select(Arrest_int,newgrouped_location) %>%
  filter(Arrest_int == 1) %>%
  group_by(newgrouped_location) %>%
  summarise(n())
data_crime_by_location = data2 %>% 
  group_by(newgrouped_location) %>%
  summarise(n())
names(data_arrest_by_location)[names(data_arrest_by_location) == "n()"] = "no.arrest"
data_arrest_rate_by_location = inner_join(data_arrest_by_location,data_crime_by_location)
data_arrest_rate_by_location$arrest.rate_by_location = data_arrest_rate_by_location$no.arrest/data_arrest_rate_by_location$`n()`
data_arrest_rate_by_location <- data_arrest_rate_by_location[-2]
data_arrest_rate_by_location <- data_arrest_rate_by_location[-2]
data4 = left_join(data3,data_arrest_rate_by_location)
  #Arrest rate by type
data_arrest_by_type = data2 %>%
  select(Arrest_int,newgrouped_type) %>%
  filter(Arrest_int == 1) %>%
  group_by(newgrouped_type) %>%
  summarise(n())
data_crime_by_type = data2 %>% 
  group_by(newgrouped_type) %>%
  summarise(n())
names(data_arrest_by_type)[names(data_arrest_by_type) == "n()"] = "no.arrest"
data_arrest_rate_by_type = inner_join(data_arrest_by_type,data_crime_by_type)
data_arrest_rate_by_type$arrest.rate_by_type = data_arrest_rate_by_type$no.arrest/data_arrest_rate_by_type$`n()`
data_arrest_rate_by_type
data_arrest_rate_by_type <- data_arrest_rate_by_type[-2]
data_arrest_rate_by_type <- data_arrest_rate_by_type[-2]
data4 = left_join(data4,data_arrest_rate_by_type)
  #clear NAs
data5= na.omit(data4)

  #Linear Regression Modedl
set.seed(666)
split = sample.split(data5$Arrest,SplitRatio = 0.7)
train1 = data5[split,]
test1 = data5[!split,]
model_lm = lm(Arrest_int ~ Date_is_1_or_31_int +Hour_in_11am_11pm_int+Domestic_int+arrest.rate+arrest.rate_by_type+arrest.rate_by_location+PER.CAPITA.INCOME, data = train1)
model_lm
summary(model_lm)
pred_lm =predict(model_lm, newdata = test1)
RMSE(pred_lm,test1$Arrest_int)
ct1 = table(Arrest = test1$Arrest, predictions = as.numeric(pred_lm>0.5))
ct1
accuracy1 = sum(ct1[1,1],ct1[2,2])/nrow(test1)
accuracy1

    # Linear regression Accuracy 0.8285868

##### Logistic Regression
set.seed(666)
split = sample.split(data3$Arrest,SplitRatio = 0.7)
train = data3[split,]
test = data3[!split,]
model_glm = glm(Arrest~Date_is_1_or_31+Hour_in_11am_11pm+newgrouped_type+newgrouped_location,PER.CAPITA.INCOME,
                   data=train,
                   family='binomial')
summary(model_glm)
pred = predict(model_glm,type='response', newdata = test)
ct = table(Arrest = test$Arrest,
           predictions = as.numeric(pred>0.5))
ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(test)
accuracy

##### Neural Network model
set.seed(666)
model_nnet = nnet(Arrest~
                    Date_is_1_or_31+Hour_in_11am_11pm+newgrouped_type+newgrouped_location,PER.CAPITA.INCOME,
                  data=train,size=5,
              decay=0.1,
              MaxNWts=10000,
              maxit=100) 
pred_nnet = predict(model_nnet,newdata=test,type='class')
confusionMatrix(factor(pred_nnet),test$Arrest)

