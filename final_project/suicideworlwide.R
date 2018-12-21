
library(tidyverse)
library(readr)
library(leaflet)
library(maps)
library(sf)




suicide <- read_csv("master.csv") 

names(suicide)[7]<-"suicide_per_onek_pop"
names(suicide)[10]<-"gdp_for_year_dollars"
names(suicide)[11]<-"gdp_per_capita_dollars"
View(suicide)



#plot all suicides by age and gender in two differnt years 
#how to change the y scale?
type_levels <- c("5-14 years", "15-24 years", "25-34 years", "35-54 years", 
                 "55-74 years", "75+ years")

suicide_00_15 <- suicide %>% 
  mutate(age = factor(age, levels = type_levels)) %>% 
  filter(year == 2000 | year==2010 | year==2015)%>%
  group_by(year,age,sex)%>%
  summarise(sum=sum(suicides_no))

ggplot(suicide_00_15, aes(x= age, y=sum, fill=sex))+
  geom_col(position = "dodge") +
  facet_wrap(~year)+
  coord_flip()+
  labs(fill= "Gender", x="Number of Suicides", y="Age of Suicide Victim", 
       title="Number of Worldwide Suicides in 2000, 2010, and 2015 by Age ")

# graph 2 compare between gdp and suicide rate 
suicide_0015 <- suicide %>% 
  filter(year == 2000 | year==2015 | year==2010)%>% 
  group_by(year, sex, country, gdp_per_capita_dollars )%>%
  summarise(sum=sum(suicide_per_onek_pop))

ggplot(suicide_0015, aes(y=sum, x= gdp_per_capita_dollars, color=sex))+
  geom_point()+ 
  facet_wrap(~year~sex, ncol=2, scale="free")+
  geom_smooth(method = "lm", se = FALSE, color="black")+
  guides(color=FALSE)+
  labs(x= "GDP per Capita ($)", y="Number of Suicides per 100K Population", 
       title= "GDP vs. Suicides Number by Gender Across Decades")


#grapgh 3 average no suicide in each age by gender 
suicide_avg_age <- suicide %>%
  group_by(sex, age, year)%>%
  summarise(mean=mean(suicides_no))
  
ggplot(suicide_avg_age, aes(x=year, y=mean, color=age))+
  geom_line()+
  facet_wrap(~sex)

#grapgh 4 suicide average in each generation by gender 

suicide_avg_gen <- suicide %>%
  filter(year == 2000 | year==2015 | year==2010)%>% 
  group_by(sex, generation, year)%>%
  summarise(mean=mean(suicides_no))



ggplot(suicide_avg_gen, aes(x=generation, y=mean))+
  geom_boxplot()+
  facet_wrap(~sex, scale="free")

#graph 4 
countries <- read_csv("world-country-boundaries.csv")
names(countries)[2]<-"country"

suicide_numbers <- suicide %>%
  filter(year == 2000 | year==2015 | year==2010) %>%
  group_by(country, sex, year)%>%
  summarise(sum=sum(suicides_no)) %>% 
  inner_join(countries, by="country") %>% 
  group_by(REGION, sex, year) %>%
  summarise(total_suicides=sum(sum))%>%
  arrange(total_suicides)

ggplot(suicide_numbers, aes(x= REGION, y=total_suicides, fill=sex))+
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~year)+
  labs(fill= "Gender", x="World Region", y="Number of People Committed Suicide", 
       title="Number of Worldwide Suicides in 2000, 2010, and 2015 by Gender ")



#Mapping: 




suicide_avg_gen <- suicide %>%
  group_by(sex, generation, year)%>%
  summarise(mean=mean(suicides_no)) 
  

ggplot(suicide_avg_gen, aes(x=generation, y=mean))+
  geom_boxplot()+
  facet_wrap(~sex, scale="free") +
  labs( x="World Region", y="Number of People Committed Suicide", 
       title="Number of Worldwide Suicides in 2000, 2010, and 2015 by Gender ")



  
