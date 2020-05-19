library(readr)
library(here)
library(tidyverse)
library(bizdays)
library(lubridate)
library(plotly)
Fact_table <- read_csv("san_francisco//Fact_table.csv")
View(Fact_table)

#total trip during the period 
Fact_table %>% 
  summarize(total_trip = sum(n_trip))

#number of station
Fact_table %>% 
  n_distinct(ID_station)

Fact_table %>% 
  group_by(ID_month) %>% 
  summarise(n = sum(n_trip))





#explication des variables

#X1 c'est du noise elle sert à rien 
#ID_station c'est le numéro de la station de vélo
#n_available c'est le nombres de vélos qui étaient dispo à ce moment
#mean_duration c'est la duréée moyenn des trajets dans l'heure la, pour cette borne
#n_trip c'est le nombre de trajet dans l'heure à cette borne
#le reste c'est assez clair

#desfois ça à l'air un peu brouillon en gros j'ai fusionner 4 tables en une
#c'est pas facile mais c'est ce qu'il demande
#je me suis basé sur les ID_hour,month, etc comme clé étrangère....

#à partir de la on peut analyser et faire quelqeus graphs ahahah
#un truc que je penses mtn on peut aussi dire si c'est lundi mardi mercredi etc..
#comme ça on analyse la diff avec les weekends. 



#Partie Gaëtan

#We drop the NA, we delete the columns "X1_1" and "X1" and we filter only the observation whose number of trip (n_trip) is larger than 0. 
Fact_table_filtered <- Fact_table %>% filter(n_trip>0) %>% drop_na() %>% select(-X1_1, -X1)

#We need to know the working days

library(RQuantLib)
load_quantlib_calendars(ql_calendars = "UnitedStates", from='2013-01-01', to='2015-12-31')

Fact_table_filtered <-Fact_table_filtered %>% mutate(workingday=is.bizday(ID_date, "QuantLib/UnitedStates"))
Fact_table_filtered$workingday <- as.numeric(Fact_table_filtered$workingday)

Fact_table_filtered$ID_date <- ymd(Fact_table_filtered$ID_date) %>% as.POSIXct()

#Remplacer 0 pour minuit en 24
Fact_table_filtered$ID_hour[Fact_table_filtered$ID_hour == 0] <- 24

#Ajouter colonne count pour nombre de location par heure 
Fact_table_filtered_cumul <- Fact_table_filtered %>% arrange(ID_date, ID_hour) %>% group_by(ID_date, ID_hour)%>% mutate(count= sum(n_trip))

#on sélectionne les colonnes que l'on souhaite
Fact_table_filtered_cumul <-
  Fact_table_filtered_cumul %>% select(
    ID_date,ID_year,
    ID_hour,
    workingday,
    count,
    mean_temperature,
    mean_wind_speed,
    mean_visibility,
    mean_humidity,
    mean_precipitation_mm
  )

#on enlève les lignes en doublons 
bestdata<-unique(Fact_table_filtered_cumul)

#On ajoute une ligne cumulative pour avoir le nombre cumulé de location par jour
bestdata <- bestdata %>% arrange(ID_date, ID_hour) %>% group_by(ID_date)%>% mutate(cumul = cumsum(count))

#On rajoute les jours de la semaine
bestdata<- bestdata %>% mutate(weekday = wday(ID_date, label = TRUE))

#Changer l'ordre des colonnes
bestdata <- bestdata[, c(1, 2, 3, 12, 4, 5, 11, 6,7,8, 9, 10 )]  
#renommer les colonnes
bestdata <- bestdata %>% rename("date"=ID_date,"year"= ID_year , "hour"=ID_hour)
bestdata$date<- format(as.POSIXct(bestdata$date,format='%Y/%m/%d %H:%M:%S'),format='%Y/%m/%d')

  
 
#Ensuite, on va comparer les semaines, les weekend et les jours de pluie vs les jours de beau temps. 
#Est-ce que le temps d'utilisation du vélo dépend du vent 

library(bizdays)
#Year analysis: is there any trend (incresing trend through the years)
#On a les date de août 2013 à août 2015: seul l'année 2014 est complète
#year2013<- Fact_table_filtered %>% filter(ID_year == 2013)

#year2014<- Fact_table_filtered %>% filter(ID_year == 2014)

#year2015<- Fact_table_filtered %>% filter(ID_year == 2015)


#number of rentals per day
#perdayyear2013 <- year2013%>% group_by(ID_date) %>% summarise(total_trip_per_day=sum(n_trip)) 
#perdayyear2014 <- year2014%>% group_by(ID_date) %>% summarise(total_trip_per_day=sum(n_trip)) 
#perdayyear2015 <- year2015%>% group_by(ID_date) %>% summarise(total_trip_per_day=sum(n_trip)) 

#on peut changer les dates pour analyser telle ou telle période 

bestdata  %>% summarise(sum = sum(count)) %>% 
  filter(date >= ymd("2013-08-29") &
           date < ymd("2013-09-07")) %>% 
  ggplot() + 
  geom_line(aes(date, sum, group = 1)) + 
  labs(x = "Date", y = "Number of rentals", title = "Rental curve")

#année 2013
bestdata %>% filter(year == 2013) %>% summarise(sum = sum(count)) %>%   
  ggplot() + 
  geom_line(aes(date, sum, group = 1)) + 
  labs(x = "Date", y = "Number of rentals", title = "Rental curve")

#année 2014
bestdata %>% filter(year == 2014) %>% summarise(sum = sum(count)) %>%   
  ggplot() + 
  geom_line(aes(date, sum, group = 1)) + 
  labs(x = "Date", y = "Number of rentals", title = "Rental curve")

#année 2015
bestdata %>% filter(year == 2015) %>% summarise(sum = sum(count)) %>%   
  ggplot() + 
  geom_line(aes(date, sum, group = 1)) + 
  labs(x = "Date", y = "Number of rentals", title = "Rental curve")

#On peut voir une certaine saisonnalité 

bestdata %>% 
  filter(date>= ymd("2014-01-01") &
           date < ymd("2014-01-31"))  %>% 
  transmute(days_week = date(date), workingday, count) %>% 
  group_by(days_week) %>% 
  summarise(sum = sum(count)) %>% 
  ggplot(aes(x=days_week, y=sum))+
  geom_line() +
  scale_x_date(date_labels = "%a", date_breaks = "1 day")+
  labs(title = "Daily rental for January 2014",   
       subtitle = str_wrap(""))+
  scale_y_continuous(breaks = seq(0 , 6000 , by = 500), labels = function(x) format(x, big.mark = ",",
                                                                                    scientific = FALSE))+
  theme_bw() +
  theme(panel.grid.major = element_line(color="grey", size = 0.1)) 


#By analyzing the period from september to december 2013, we can see that Saturday is the weaks day in terms of rentals. Tuesday is in majority the best rental day. 



#Plot the average number of total rentals per hour using different colors for different week days as a line chart.

bestdata  %>% 
  group_by(weekday, hour) %>% 
  summarize(avg_count = mean(count)) %>%
  ggplot() +
  geom_line(aes(hour, avg_count, color = weekday)) + 
  labs(
    x = "Hour",
    y = "Average number of rentals",
    title = "Rental curves depending on the day of the week",
    color = "Day of week"
  )

#Plot the same chart as in __3.d__, but use different colors depending on whether the day is working or not.

bestdata$workingday[bestdata$workingday == 1] <- "TRUE"
bestdata$workingday[bestdata$workingday == 0] <- "FALSE"

#bestdata$workingday1[is.na(bestdata$workingday1)] <- "FALSE"

bestdata  %>% 
  group_by(workingday, hour) %>%
  summarize(avg_count = mean(count)) %>%
  ggplot() + 
  geom_line(aes(hour, avg_count, color = workingday)) + 
  labs(
    x = "Hour",
    y = "Average number of rentals",
    title = "Rental curves depending on the day of the week",
    color = "Working day"
  )





#Number of trips by hour, across the year¶ CORRECT

 bestdata2 <- bestdata %>% arrange(hour)%>% group_by(hour) %>% summarise(totalperhour = sum(count))
 bestdata2 <- reshape2::melt(bestdata2$totalperhour)
 bestdata2 <-  bestdata2 %>% mutate(hour=(1:24))
 
 bestdata2 %>%
   ggplot(aes(hour, value)) +
   geom_bar(stat = "identity") +
   labs (
     x = "Hour",
     y = "Total number of bicycle trips",
     title = "Number of trips by hour, across the year
",
     subtitle = str_wrap(
       ""
     )
   ) +
   theme(
     axis.text.x = element_text(
       face = "bold",
       color = "black",
       size = 12,
       angle = 45
     ),
     axis.text.y = element_text(face = "bold", size = 10),
     panel.grid.minor = element_line(color = "grey", size = 0.1)
   ) +
   theme(panel.grid.major = element_line(color = "grey", size = 0.1)) +
   ggpubr::rotate_x_text(angle = 360, hjust = 0.5)
 
 
 #Number of trips by hour and by quarter¶ #CORRECT
 bestdata <- bestdata%>%  mutate(quarter(date))
 bestdata3 <- bestdata %>% arrange(hour, quarter)%>% group_by(hour, quarter) %>% summarise(totalperhour = sum(count))

 bestdata3 %>%
   ggplot(aes(hour, totalperhour)) +
   geom_bar(stat = "identity") +
   labs (
     x = "Hour",
     y = "Total number of bicycle trips",
     title = "Number of trips by hour, across the year
",
     subtitle = str_wrap(
       ""
     )
   ) +
   theme(
     axis.text.x = element_text(
       face = "bold",
       color = "black",
       size = 12,
       angle = 45
     ),
     axis.text.y = element_text(face = "bold", size = 10),
     panel.grid.minor = element_line(color = "grey", size = 0.1)
   ) +
   theme(panel.grid.major = element_line(color = "grey", size = 0.1)) +   scale_x_continuous(breaks = seq(2, 24 , by = 2), labels = function(x) format(x, big.mark = ",", scientific = FALSE))+

   ggpubr::rotate_x_text(angle = 360, hjust = 0.5)+   facet_wrap(~quarter)


#Trip duration CORRECT

 Fact_table_filtered <- Fact_table_filtered %>% mutate(duration_min = (mean_duration / 60)) 
 distribution <- Fact_table_filtered %>% group_by(duration_min) %>% summarise(total = sum(n_trip)) %>%  filter(duration_min <=60) 
 distribution <- distribution %>% mutate(minute=trunc(duration_min )) %>% group_by(minute) %>% summarise(total = sum(total))

 distribution %>%
   ggplot(aes(minute, total)) +
   geom_bar(stat = "identity") +
   labs (
     x = "Trip duration [min]",
     y = "Total number of bicycle trips",
     title = "Distribution of bike trip duration in minute
",
     subtitle = str_wrap(
       ""
     )
   ) +
   theme(
     axis.text.x = element_text(
       face = "bold",
       color = "black",
       size = 12,
       angle = 45
     ),
     axis.text.y = element_text(face = "bold", size = 10),
     panel.grid.minor = element_line(color = "grey", size = 0.1)
   ) +
   theme(panel.grid.major = element_line(color = "grey", size = 0.1)) +   scale_x_continuous(breaks = seq(1, 60 , by = 2), labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
   
   ggpubr::rotate_x_text(angle = 360, hjust = 0.5)
 
  
#Number of bike trips per month  #correct
bestdata4 <- bestdata %>% group_by(month=month(date)) %>% summarise(totalpermonth = sum(count))

bestdata4 %>% ggplot(aes(month, totalpermonth)) +
  geom_bar(stat = "identity") +
  labs (
    x = "Month",
    y = "Total number of bicycle trips",
    title = "Number of trips by month
",
    subtitle = str_wrap(
      ""
    )
  ) +
  theme(
    axis.text.x = element_text(
      face = "bold",
      color = "black",
      size = 12,
      angle = 45
    ),
    axis.text.y = element_text(face = "bold", size = 10),
    panel.grid.minor = element_line(color = "grey", size = 0.1)
  ) +
  theme(panel.grid.major = element_line(color = "grey", size = 0.1)) +
  ggpubr::rotate_x_text(angle = 360, hjust = 0.5)+
  scale_x_continuous(breaks = seq(1, 12 ), labels = function(x) format(x, big.mark = ",", scientific = FALSE))


#Total number of trips by day of the week¶
bestdata5 <- bestdata %>% group_by(weekday) %>% summarise(totalperday = sum(count))

bestdata5 %>% ggplot(aes(weekday, totalperday)) +
  geom_bar(stat = "identity") +
  labs (
    x = "Day of the week",
    y = "Total number of bicycle trips",
    title = "Number of trips by weekday
",
    subtitle = str_wrap(
      ""
    )
  ) +
  theme(
    axis.text.x = element_text(
      face = "bold",
      color = "black",
      size = 12,
      angle = 45
    ),
    axis.text.y = element_text(face = "bold", size = 10),
    panel.grid.minor = element_line(color = "grey", size = 0.1)
  ) +
  theme(panel.grid.major = element_line(color = "grey", size = 0.1)) +
  ggpubr::rotate_x_text(angle = 360, hjust = 0.5)

#Total number of trips by day of the month¶
bestdata6 <- bestdata %>% group_by(day=day(date)) %>% summarise(totalperdayofmonth = sum(count))

bestdata6 %>% ggplot(aes(day, totalperdayofmonth)) +
  geom_bar(stat = "identity") +
  labs (
    x = "Day",
    y = "Total number of bicycle trips",
    title = "Number of trips by day of month
",
    subtitle = str_wrap(
      ""
    )
  ) +
  theme(
    axis.text.x = element_text(
      face = "bold",
      color = "black",
      size = 12,
      angle = 45
    ),
    axis.text.y = element_text(face = "bold", size = 10),
    panel.grid.minor = element_line(color = "grey", size = 0.1)
  ) +
  theme(panel.grid.major = element_line(color = "grey", size = 0.1)) +
  ggpubr::rotate_x_text(angle = 360, hjust = 0.5)





#Ensuite, on va comparer les semaines, les weekend et les jours de pluie vs les jours de beau temps. 
#Est-ce que le temps d'utilisation du vélo dépend du vent 
#Bike available distribution 
#Which route is the most popoular 
#Which stations are the most popular 

#Modeling part 

  




