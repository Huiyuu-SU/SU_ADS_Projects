dev.off()
cat('\014')
rm(list=ls())
work_dir <- "/Users/Huiyu/Desktop/IST719/Rfile"
setwd(work_dir)
getwd()

library(tidyverse)
library(lubridate)
library(dplyr)
##install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
library(broom)
library(httr)
#install.packages("rgdal")
library(rgdal)
library(geojsonio)

###Data Cleaning
nyc_vcrash<-read_csv("Motor_Vehicle_Collisions_Crashes.csv")
nyc_vcrash$`CRASH DATE`<-mdy(nyc_vcrash$`CRASH DATE`)
nyc_vcrash$crash_time_h <- hour(strptime(nyc_vcrash$`CRASH TIME`, "%H:%M"))
nyc_vcrash$`Year` <- year(nyc_vcrash$`CRASH DATE`)
nyc_vcrash$`Month` <- month(nyc_vcrash$`CRASH DATE`)
unique(nyc_vcrash$`Year`)
nyc_mcrash <- nyc_vcrash %>% filter(`Year` %in% c(2019:2022))
unique(nyc_mcrash$`Year`)
names(nyc_mcrash) <- tolower(names(nyc_mcrash))

nyc_mcrash$borough[is.na(nyc_mcrash$borough)]<- "Notin_Borough" ##replace blank space in column borough
sum(is.na(nyc_mcrash$borough))

##cleaned dataset
view(head(nyc_mcrash,1000))
nrow(nyc_mcrash)
ncol(nyc_mcrash)
sum(is.na(nyc_mcrash$collision_id))


##analysis
crash_hurt<- nyc_mcrash %>% select(borough,year,month,collision_id,`number of persons injured`:`number of motorist killed`) %>% pivot_longer(`number of persons injured`:`number of motorist killed`,names_to="category",values_to="count")


crash_hurt$category <- gsub("number of ", "", crash_hurt$category)

crash_hurt <- crash_hurt %>% group_by(collision_id) %>% arrange(collision_id,-count) 


###plot 1 how many crashes happened by month in each year trend
crash_hurt %>% group_by(year,month) %>% summarise(total_crashes = sum(count,na.rm=T)) %>% arrange(year,month) %>% ggplot() + geom_line(aes(month,total_crashes,color=year))  +
  facet_wrap(~year) + 
  scale_x_continuous(breaks=seq(1,12,by=1))+
  ggtitle("Motor vehicle crashes happened by month between 2019-2022 in NYC")
ggsave("fp_strory_plot1.pdf")

###plot 2 how many crashes happened by month in each year at each borough
crash_hurt %>% group_by(borough,year,month) %>% summarise(total = sum(count,na.rm=T)) %>% arrange(month) %>% ggplot() + geom_line(aes(month,total,color=borough))  +
  facet_wrap(~year)+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  ggtitle("Motor vehicle crashes happened by month in different borough between 2019-2022 in NYC")
ggsave("fp_strory_plot2.pdf")

##--only borough
crash_hurt %>% filter(borough!="Notin_Borough") %>% group_by(borough,year,month) %>% summarise(total = sum(count,na.rm=T)) %>% arrange(month) %>% ggplot() + geom_line(aes(month,total,color=borough))  +
  facet_wrap(~year)+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  ggtitle("Motor vehicle crashes happened by month in different borough between 2019-2022 in NYC")
ggsave("fp_strory_plot2.1.pdf")




###plot 3 Total injured by type of motor vehicle crashes happened in different borough between 2020-2022 in NYC
crash_hurt %>% filter(str_detect(category,"injured")) %>%
  group_by(category) %>% summarise(total = sum(count,na.rm=T))  %>% 
  mutate(prop = total/sum(total)) %>%
  ggplot(aes(category,prop)) +  geom_bar(stat="identity") +
  theme_minimal(base_size = 11)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))+
  ggtitle("2019-2022 Proportion of type of people got injured by motor vehicle crashes in NYC")
ggsave("fp_strory_plot3.1.pdf",width = 10)

crash_hurt %>% filter(str_detect(category,"injured")) %>%
  group_by(borough,category) %>% summarise(total = sum(count,na.rm=T)) %>%
  mutate(prop = total/sum(total)) %>%
  ggplot(aes(category,prop,fill=borough)) +  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal(base_size = 11)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))+
  ggtitle("2019-2022 Proportion type of total injured by motor vehicle crashes in different borough in NYC")
ggsave("fp_strory_plot3.pdf",width = 10)

#####----------------update -----------------##

crash_hurt %>% filter(str_detect(category,"injured"),borough!="Notin_Borough") %>%
  group_by(borough) %>% summarise(total = sum(count,na.rm=T)) %>%
  mutate(prop = total/sum(total)) %>%
  ggplot(aes(borough,prop,fill=borough)) +  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal(base_size = 11)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))+
  ggtitle("2019-2022 Proportion of people injured by motor vehicle crashes in different borough in NYC")
ggsave("fp_strory_plot3.3.pdf",width =8)



###plot 5 Total killed by type of motor vehicle crashes happened in different borough between 2020-2022 in NYC
crash_hurt %>% filter(str_detect(category,"killed")) %>%
  group_by(category) %>% summarise(total = sum(count,na.rm=T))  %>% 
  mutate(prop = total/sum(total)) %>%
  ggplot(aes(category,prop)) +  geom_bar(stat="identity") +
  theme_minimal(base_size = 11)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))+
  ggtitle("2019-2022 Proportion of type of people got killed by motor vehicle crashes in NYC")
ggsave("fp_strory_plot4.1.pdf",width = 10)



crash_hurt %>% filter(str_detect(category,"killed")) %>%
  group_by(borough,category) %>% summarise(total = sum(count,na.rm=T)) %>%
  mutate(prop = total/sum(total)) %>%
  ggplot(aes(category,prop,fill=borough)) +  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal(base_size = 11)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))+
  ggtitle("2019-2022 Proportion type of total killed by motor vehicle crashes in different borough in NYC")
ggsave("fp_strory_plot4.pdf",width = 10)

#####----------------update -----------------##

crash_hurt %>% filter(str_detect(category,"killed"),borough!="Notin_Borough") %>%
  group_by(borough) %>% summarise(total = sum(count,na.rm=T)) %>%
  mutate(prop = total/sum(total)) %>%
  ggplot(aes(borough,prop,fill=borough)) +  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal(base_size = 11)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))+
  ggtitle("2019-2022 Proportion of people killed by motor vehicle crashes in different borough in NYC")
ggsave("fp_strory_plot3.4.pdf",width = 8)


------#type of people killed 
  crash_hurt %>% filter(str_detect(category,"killed"),borough!="Notin_Borough") %>%
  group_by(borough,category) %>% summarise(total = sum(count,na.rm=T)) %>%
  mutate(prop = total/sum(total)) %>%
  ggplot(aes(borough,prop,fill=category)) +  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal(base_size = 11)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))+
  ggtitle("2019-2022 Proportion of types of people killed by motor vehicle crashes in different borough in NYC")
ggsave("fp_strory_plot3.5.pdf",width = 8)



------#type of people injured 
  crash_hurt %>% filter(str_detect(category,"injured"),borough!="Notin_Borough") %>%
  group_by(borough,category) %>% summarise(total = sum(count,na.rm=T)) %>%
  mutate(prop = total/sum(total)) %>%
  ggplot(aes(borough,prop,fill=category)) +  geom_bar(stat="identity", position=position_dodge()) +
  theme_minimal(base_size = 11)+
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5))+
  ggtitle("2019-2022 Proportion of types of people injured by motor vehicle crashes in different borough in NYC")
ggsave("fp_strory_plot3.6.pdf",width = 8)

### vehicles involved in a crash
nyc_mcrash$vehicle1 <- ifelse(is.na(nyc_mcrash$`vehicle type code 1`),0,1)
nyc_mcrash$vehicle2 <- ifelse(is.na(nyc_mcrash$`vehicle type code 2`),0,2)
nyc_mcrash$vehicle3 <- ifelse(is.na(nyc_mcrash$`vehicle type code 3`),0,3)
nyc_mcrash$vehicle4 <- ifelse(is.na(nyc_mcrash$`vehicle type code 4`),0,4)
nyc_mcrash$vehicle5 <- ifelse(is.na(nyc_mcrash$`vehicle type code 5`),0,5)
nyc_mcrash

crash_vehicle<- nyc_mcrash %>% select(borough,year,month,collision_id,vehicle1:vehicle5) %>% pivot_longer(vehicle1:vehicle5,names_to="num_car",values_to="if_car")

vehicles_carsh <- crash_vehicle %>% group_by(collision_id) %>% arrange(collision_id,-if_car) %>%top_n(1,if_car) 
vehicles_carsh

vehicles_carsh <- vehicles_carsh %>% group_by(borough,year,month,collision_id,num_car) %>% summarise(with_oth_vehicles = sum(if_car))
vehicles_carsh

##plot5
vehicles_carsh %>% group_by(year,num_car) %>% summarise(total = n()) %>%
  ggplot(aes(year,total,fill=num_car)) + geom_bar(stat = "identity",position=position_dodge())+
  ggtitle("2019-2022 Vehicles invovled in crashes amount in NYC ")

ggsave("fp_strory_plot5.pdf",width = 10)

waf <- vehicles_carsh %>% group_by(year,num_car) %>% summarise(total = n())
waf

ggplot(waf,aes(fill=num_car,values=total))+
  geom_waffle(n_rows = 7, size =1, colour = "black",
              make_proportional = TRUE
  )+
  coord_equal() +
  theme_void()+
  facet_wrap(~year)+
  scale_fill_manual(
    values = c("#f8766d", alpha("#00ba38", 1/3), alpha("#619cff", 1/3), 
               alpha("#FFB233", 1/3), alpha("#C3EE14", 1/3))
  )+
  ggtitle("2019-2022 Vehicles invovled in crashes amount in NYC")
ggsave("fp_strory_plot5.1.pdf",width = 10)


##plot6
vehicles_carsh %>% group_by(year,month,num_car) %>% summarise(total = n()) %>%ggplot(aes(month,total,color=num_car)) + geom_line()+facet_wrap(~year)+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  theme(axis.text.x = element_text(angle = 10, hjust = 0.5, vjust = 0.5))+
  ggtitle("2019-2022 Vehicles invovled in crashes amount by months in NYC ")
ggsave("fp_strory_plot6.pdf")

vehicles_carsh
### injured & killed correlation with happened time:

crash_hurt_t<- nyc_mcrash %>% select(year,month,collision_id,`crash time`,crash_time_h,borough,`number of persons injured`:`number of motorist killed`) %>% pivot_longer(`number of persons injured`:`number of motorist killed`,names_to="category",values_to="count")


crash_injured <- crash_hurt_t %>% 
  group_by(year,crash_time_h,category) %>% summarise(tota_p=sum(count,na.rm=T)) %>%
  filter(str_detect(category,"injured")) %>% arrange(year)



crash_injured %>% group_by(crash_time_h) %>% summarise(cnt =tota_p) %>%
  ggplot(aes(x=crash_time_h, y=cnt)) + geom_point()+
  scale_x_continuous(breaks=seq(0,24,by=1))


crash_injured %>% group_by(crash_time_h,category) %>% summarise(cnt = tota_p) %>%
  ggplot(aes(x=crash_time_h, y=cnt,color=category)) + geom_point()+
  scale_x_continuous(breaks=seq(0,24,by=1))+
  theme(axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5))+
  ggtitle("2019-2022 number of people injured related by motor vehicles by hours in NYC ")

ggsave("fp_strory_plot7.pdf",width = 10)



##kill
crash_killed <- crash_hurt_t %>% 
  group_by(year,`crash time`,crash_time_h,category) %>% summarise(tota_p=sum(count,na.rm=T)) %>%
  filter(str_detect(category,"killed")) %>% arrange(year)

crash_killed %>% group_by(crash_time_h) %>% summarise(cnt = tota_p) %>%
  ggplot(aes(x=crash_time_h, y=cnt)) + geom_point()+
  scale_x_continuous(breaks=seq(0,24,by=1))


crash_killed %>% group_by(year,crash_time_h,category) %>% summarise(cnt = tota_p) %>%
  ggplot(aes(x=crash_time_h, y=cnt,color=category)) + geom_point()+
  scale_x_continuous(breaks=seq(0,24,by=1))+
  facet_wrap(~year)+
  ggtitle("2019-2022 number of people killed related by motor vehicles by hours in NYC ")

ggsave("fp_strory_plot8.pdf",width = 15)


#####----------------update relationship between accident cnt and people injured by hour -------##

crash_injured_h <- crash_hurt_t %>% filter(str_detect(category,"injured")) %>% group_by(year,crash_time_h) %>%summarise(accident_cnt = n_distinct(collision_id),total_p = sum(count,na.rm = T)) 

ggplot(crash_injured_h) + geom_bar(aes(crash_time_h,accident_cnt),stat="identity",fill="lightblue",colour="#006000") + geom_line(aes(crash_time_h,total_p), stat = "identity",color="orange",size=1)+
  scale_x_continuous(breaks=seq(0,24,by=1))+
  facet_wrap(~year)+
  scale_y_continuous(sec.axis=sec_axis(~.*1,name="people_injured"))+
  ggtitle("2019-2022 number of accidents and injured people by hours and years in NYC ")
ggsave("fp_strory_plot9.1.pdf",width = 11)


injured_p_h <- crash_hurt_t %>% filter(str_detect(category,"injured")) %>% group_by(crash_time_h) %>%summarise(accident_cnt = n_distinct(collision_id),total_p = sum(count,na.rm = T)) 



  ggplot(injured_p_h) + geom_bar(aes(crash_time_h,accident_cnt),stat="identity",fill="lightblue",colour="#006000") + geom_line(aes(crash_time_h,total_p), stat = "identity",color="red",size=1)+
  scale_x_continuous(breaks=seq(0,24,by=1))+
    scale_y_continuous(sec.axis=sec_axis(~.*1,name="people_injured"))+
    ggtitle("2019-2022 number of accidents and injured people by hours in NYC ")
  ggsave("fp_strory_plot9.2.pdf",width = 11)





#-------------------------------------------------------------------------#

#####people injured and killed relations between vehicles 
vehicles_carsh
crash_hurt

collision_vl <- crash_hurt %>% left_join(vehicles_carsh, by=c("collision_id" = "collision_id","borough" = "borough","year"="year","month"="month"))

collision_df <- separate(collision_vl, col=category, into=c('people_type', 'crash_status'), sep=' ')

collision_df

##------------number of vehicles involved and accident cnt---------------##
collision_df$count[is.na(collision_df$count)] <- 0
sum(is.na(collision_df))

collision_df %>% group_by(crash_status,num_car,count) %>% summarise(acci_cnt = n_distinct(collision_id))
collision_df %>% 
  ggplot(aes(people_type,count,color=num_car)) + geom_point() +
  facet_wrap(~crash_status)

ggsave("fp_strory_plot10.2.pdf",width = 8)


##-----------reasons of collisions happened----------------##

vehicles_carsh
collision_df
view(head(nyc_mcrash,1000))

Rea_crash <- collision_df %>% left_join(nyc_mcrash, by=c("collision_id"="collision_id")) %>%
  select(borough.x:with_oth_vehicles,`contributing factor vehicle 1`:`contributing factor vehicle 5`)


Rea_crash$`contributing factor vehicle 1`[is.na(Rea_crash$`contributing factor vehicle 1`)] <-  "unknown"
Rea_crash$`contributing factor vehicle 2`[is.na(Rea_crash$`contributing factor vehicle 2`)] <-  "unknown"
Rea_crash$`contributing factor vehicle 3`[is.na(Rea_crash$`contributing factor vehicle 3`)] <-  "unknown"
Rea_crash$`contributing factor vehicle 4`[is.na(Rea_crash$`contributing factor vehicle 4`)] <-  "unknown"
Rea_crash$`contributing factor vehicle 5`[is.na(Rea_crash$`contributing factor vehicle 5`)] <-  "unknown"

sum(is.na(Rea_crash$`contributing factor vehicle 1`))



Fac_crash <- Rea_crash %>% group_by(`contributing factor vehicle 1`,`contributing factor vehicle 2`,num_car) %>% summarise(crash_cnt = n_distinct(collision_id),total_p = sum(count)) 


ve_1 <- Rea_crash %>% group_by(`contributing factor vehicle 1`)%>% filter(!`contributing factor vehicle 1` %in% c("unknown","Unspecified"))%>% summarise(crash_cnt = n_distinct(collision_id),total_p = sum(count))%>% arrange(-crash_cnt) %>% top_n(5) %>% mutate(fac_reasons = `contributing factor vehicle 1`,vehicle_type = 1)



ve_2 <- Rea_crash %>% group_by(`contributing factor vehicle 2`) %>% filter(!`contributing factor vehicle 2` %in% c("unknown","Unspecified"))%>% summarise(crash_cnt = n_distinct(collision_id),total_p = sum(count))%>% arrange(-crash_cnt) %>% top_n(5)%>% mutate(fac_reasons = `contributing factor vehicle 2`,vehicle_type = 2)

ve_3 <-Rea_crash %>% group_by(`contributing factor vehicle 3`) %>% filter(!`contributing factor vehicle 3` %in% c("unknown","Unspecified"))%>% summarise(crash_cnt = n_distinct(collision_id),total_p = sum(count))%>% arrange(-crash_cnt) %>% top_n(5)%>% mutate(fac_reasons = `contributing factor vehicle 3`,vehicle_type = 3)

ve_4 <-Rea_crash %>% group_by(`contributing factor vehicle 4`) %>% filter(!`contributing factor vehicle 4` %in% c("unknown","Unspecified"))%>% summarise(crash_cnt = n_distinct(collision_id),total_p = sum(count))%>% arrange(-crash_cnt) %>% top_n(5)%>% mutate(fac_reasons = `contributing factor vehicle 4`,vehicle_type = 4)

ve_5 <-Rea_crash %>% group_by(`contributing factor vehicle 5`) %>% filter(!`contributing factor vehicle 5` %in% c("unknown","Unspecified"))%>% summarise(crash_cnt = n_distinct(collision_id),total_p = sum(count))%>% arrange(-crash_cnt) %>% top_n(5)%>% mutate(fac_reasons = `contributing factor vehicle 5`,vehicle_type = 5)


ve_fac <- ve_1 %>% full_join(ve_2) %>% full_join(ve_3) %>% full_join(ve_4) %>% full_join(ve_5) 
ve_fac <- ve_fac %>% mutate(injured_p = round(total_p/crash_cnt,3)) %>% 
  select(vehicle_type,fac_reasons,crash_cnt,total_p,injured_p) 

ve_fac$vehicle_type <- as.character(ve_fac$vehicle_type)

ve_fac %>% filter(fac_reasons != "Driver Inexperience") %>% 
  ggplot() + geom_tile(aes(vehicle_type,fac_reasons, fill=injured_p))+scale_fill_viridis_c() +
  ggtitle("Top5 Collision Factors of average injured people by vehicle involved in NYC")
ggsave("fp_strory_plot10.3.pdf",width = 7)



##NYC map

#NYC_Zip: https://data.beta.nyc/dataset/nyc-zip-code-tabulation-areas
spdf_file <- geojson_read(  # Read the geojson file
  "zip_code_040114.geojson",
  what = "sp"
)
stats_df <- as.data.frame(spdf_file)  # Export the census statistics in another data frame variable
spdf_file <- tidy(  # Convert it to a spatial data frame, with zip code as index
  spdf_file,
  region="ZIPCODE"  # Use ZIPCODE variable as index, the index will be named "id"
)
nyc_map <- stats_df %>%
  inner_join(spdf_file, c("ZIPCODE"="id"))
view(head(nyc_map,1000))

#NYC_Borough: https://data.beta.nyc/dataset/pediacities-nyc-neighborhood
nyc_zip<-read_csv("nyc_zip_borough_neighborhoods_pop.csv")
nyc_zip$borough <- toupper(nyc_zip$borough)
nyc_zip$zip <- as.character(nyc_zip$zip)
nyc_zip


nyc_mapplot <-nyc_map %>% inner_join(nyc_zip,by=c("ZIPCODE" = "zip")) %>% select (ZIPCODE,long,lat,borough,STATE,group)

borough_injuredf <-nyc_mcrash %>% select(borough,longitude,latitude,year,month,collision_id,`number of persons injured`:`number of motorist killed`) %>% pivot_longer(`number of persons injured`:`number of motorist killed`,names_to="category",values_to="count")%>%filter(str_detect(category,"injured"),borough!="Notin_Borough")

boroughmonth_injured <- borough_injuredf %>% group_by(borough,year) %>% summarise(crash_cnt = n_distinct(collision_id), injure_p=sum(count))
boroughmonth_injured


#######map plot
map_plot <- boroughmonth_injured%>% inner_join(nyc_mapplot,by=c("borough"="borough"))
map_plot %>% group_by(borough,year,crash_cnt,injure_p,ZIPCODE,long,lat,group,STATE) %>% summarise(n())

map_plot

#----crash
ggplot(map_plot)+geom_polygon(
             aes(x=long,
                 y=lat,
                 group=group,
                 fill=crash_cnt),
             color="white")+scale_fill_viridis_c()+
   theme_void() +
  coord_map() +
  ggtitle("2019-2022 Total collisions happened by borough in NYC")
ggsave("fp_strory_plot11.pdf",width = 8)


###---

ggplot(map_plot)+geom_polygon(
  aes(x=long,
      y=lat,
      group=group,
      fill=crash_cnt),
  color="white")+scale_fill_viridis_c()+
  facet_wrap(~year)+ theme_void() +
  coord_map() +
  ggtitle("2019-2022 Total collisions happened by borough in NYC")
ggsave("fp_strory_plot11.1.pdf",width = 8)


map_plot$injure_p
###--------crash and injured
ggplot(map_plot)+geom_polygon(
  aes(x=long,
      y=lat,
      group=group,
      fill=crash_cnt),
  color="white")+scale_fill_viridis_c()+
  
  geom_point(
             aes(x=long,
                 y=lat,fill=injure_p,color=injure_p),
             alpha=0.3,
             size=0.05,
             shape=0.5)+ theme_void() +
  coord_map() +
  ggtitle("2019-2022 Total collisions happened by borough in NYC")
ggsave("fp_strory_plot12.pdf",width = 8)

###--------injured rate
map_rt <- map_plot %>% mutate(rate = injure_p/crash_cnt)
map_rt

ggplot(map_rt)+geom_polygon(
  aes(x=long,
      y=lat,
      group=group,
      fill=crash_cnt),
  color="white")+scale_fill_viridis_c(option = "plasma")+
  
  geom_point(
    aes(x=long,
        y=lat,color=rate),
    alpha=0.5,
    size=0.1,
    shape=0.5)+
  theme_void() +
  coord_map() +
  ggtitle("2019-2022 Total collisions happened by borough in NYC")
ggsave("fp_strory_plot12.1.pdf",width = 8)





