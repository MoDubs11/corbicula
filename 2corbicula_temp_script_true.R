#Input Data
##Input data in long format, one singular column
temp_long<-read.csv("tempdata_long.csv", header=T, stringsAsFactors = F)
##Input data in horizontal format 
temp_horiz<-read.csv("tempdata_horiz.csv", header=T, stringsAsFactors = F)

str(temp_horiz)
######
#Convert Excel date/time to R date/time
##load required package
library(janitor)
library("tibble")
library(openxlsx)
library(plyr)
library(dplyr)
library(ggplot2) #for all your plotting needs! It's GGPLOT2!
library(lubridate) #allows easy manipulation of date data
temp_horiz$date_time2R<-convertToDateTime(temp_horiz$date_time, origin = "1900-01-01")
temp_horiz$hour<-hour(temp_horiz$time)
temp_horiz$time<-excel_numeric_to_date(as.numeric(as.character(temp_horiz$date_time), date_system = "modern"))

byday_summary$dt<-as.Date(byday_summary$mmddyy)  

#This works
byday_summary$dt<-paste(byday_summary$mm, byday_summary$dd, byday_summary$year, sep = "/", collapse = NULL)
byday_summary$dt1 <- as.Date(byday_summary$dt, "%m /%d /%Y") 
temp_horiz$dt <- as.Date(temp_horiz$date_time,"%m /%d /%y") 


##########
#Differential Calculations
temp_horiz$LB_diff_H2<-temp_horiz$LB_T6HOT-((temp_horiz$LB_T6C+temp_horiz$LB_T1)/2)#Laneborough differential
temp_horiz$LB_diff_H3<-temp_horiz$LB_T3-((temp_horiz$LB_T6C+temp_horiz$LB_T1)/2)#Laneborough differential
temp_horiz$SB_diff<-temp_horiz$SB_S2-((temp_horiz$SB_S4+temp_horiz$SB_S1+temp_horiz$SB_S3)/3) #Shannonbridge differential
temp_horiz$LB_diff_H2[temp_horiz$LB_diff_H2 < 0] <- 0 #Negative values are NA
temp_horiz$LB_diff_H3[temp_horiz$LB_diff_H3 < 0] <- 0 #Negative values are NA
temp_horiz$SB_diff[temp_horiz$SB_diff < 0] <- 0 #Negative values are NA


#######

byday_differential_summary <- ddply(temp_horiz, c("dt"), summarise,
                                    LB2N    = length(LB_diff_H2),
                                    LB3N    = length(LB_diff_H3),
                                    SBN    = length(SB_diff),
                                    LB2mean = mean(LB_diff_H2),
                                    LB2max  = max(LB_diff_H2),
                                    LB2min  = min(LB_diff_H2),
                                    LB3mean = mean(LB_diff_H3),
                                    LB3max  = max(LB_diff_H3),
                                    LB3min  = min(LB_diff_H3),
                                    SBmean = mean(SB_diff),
                                    SBmax  = max(SB_diff),
                                    SBmin  = min(SB_diff))


byday_summary <- ddply(temp_long, c("mm", "dd", "year", "transect_id", "site", "mmddyy"), summarise,
                       N    = length(temp),
                       mean = mean(temp,na.rm=T),
                       max  = max(temp,na.rm=T),
                       min  = min(temp,na.rm=T),
                       DDGrow= ((max+min)/2)-10,
                       DDLarv= ((max+min)/2)-15,
                       sd   = sd(temp,na.rm=T),
                       se   = sd / sqrt(N))

table1_paper <- ddply(byday_summary, c("transect_id"), summarise,
                      N    = length(mean),
                      cum_gdd = sum(DDGrow),
                      cum_ldd = sum(DDLarv),
                      growday = sum(growth1),
                      larvday = sum(larvae1))


byday_summary$growth1[byday_summary$mean >= 10 & byday_summary$mean <= 35] <- "1"
byday_summary$growth1[is.na(byday_summary$growth1)] <- 0

byday_summary$larvae1[byday_summary$mean >= 15 & byday_summary$mean <= 28] <- "1"
byday_summary$larvae1[is.na(byday_summary$larvae1)] <- 0


temp_long$growth1[temp_long$temp >= 10 & temp_long$temp <= 35] <- "1"
temp_long$growth1[is.na(temp_long$growth1)] <- 0
temp_long$toohot[temp_long$temp >= 28] <- "1"
temp_long$toohot[is.na(temp_long$toohot)] <- 0
temp_long$larvae1[temp_long$temp >= 15 & temp_long$temp <= 28] <- "1"
temp_long$larvae1[is.na(temp_long$larvae1)] <- 0

toohot_summary<- ddply(temp_long, c("transect_id"), summarise,
                       N    = length(toohot),#number of 10 minute intervals
                       toohotsum = sum(toohot=="1",na.rm=T), #number of larvae intervals
                       non = sum(toohot=="0"),#number of no larvae intervals
                       propoftime   = (toohotsum / N))


lanespalette <- c("#56B4E9","#F0E442", "#009E73", "#D55E00") #BASED ON CBP
shanbripalette<-c("#999999", "#D55E00", "#56B4E9", "#009E73") #BASED ON CBP

LanesTemp<-byday_summary %>%
  filter(site == "LB") %>% #filters single site
  ggplot(aes(x=dt1)) + #generate overall plot
  geom_point(aes(y=mean, colour = factor(transect_id))) + #add lines
  coord_cartesian(ylim=c(0,30))+ #adjust y axis
  guides(colour = guide_legend(override.aes = list(size=3)))+ #make lines in legend thicker
  scale_colour_manual(values=lanespalette, breaks=c("LB_T6C","LB_T1", "LB_T6H","LB_T3"), labels=c("LA2","LA3", "LH2","LH3"))+
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+ #center overall title
  labs(x="", y="Mean daily temperature (°C)", title="Lanesborough", colour="Stations") #labels

ShannonTemp<-byday_summary %>%
  filter(site == "SB") %>%
  ggplot(aes(x=dt1)) + #generate overall plot
  geom_point(aes(y=mean, colour = factor(transect_id))) + #add lines
  coord_cartesian(ylim=c(0,30))+ #adjust y axis
  guides(colour = guide_legend(override.aes = list(size=3)))+ #make lines in legend thicker
  scale_colour_manual(values=shanbripalette, breaks=c("SB_S4","SB_S3", "SB_S1","SB_S2"), labels=c("SA1","SA2", "SA3","SH3"))+
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+ #center overall title
  labs(x="", y="", colour="Stations", title="Shannonbridge") #labels


differential<-ggplot(byday_differential_summary, aes(x=dt)) + #generate overall plot
  geom_point(aes(y=SBmean, col="Shannonbridge")) +  #add line for T1
  geom_point(aes(y=LB2mean, col="Lanesborough H2")) + #add line for T3 Top
  geom_point(aes(y=LB3mean, col="Lanesborough H3")) + #add line for T3 Top
  coord_cartesian(ylim=c(0,20))+ #adjust y axis
  guides(colour = guide_legend(override.aes = list(size=3)))+ #make lines in legend thicker
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+ #center overall title
  labs(title="",  
       x="", y="Differential temperature (°C)", colour="Location") #labels


library(cowplot)
top_row<-plot_grid(LanesTemp,
                   ShannonTemp,
                   align = 'vh',
                   labels = c("a", "b"),
                   hjust = -1,
                   nrow = 1
)

final<-plot_grid(top_row,
                 differential + theme(legend.position="right"),
                 labels = c("", "c"),
                 hjust = -1,
                 nrow = 2
)

final



#############
#STUFF NOT TO USE BECAUSE IT IS bad.
#generate general plots for lanesborough and shannonbridge

theme_set(theme_bw())#removes background colours on ggplot
#lanesborough colour palatte generation
lbPalette<-c("#009E73", "#F0E442", "#D55E00", "#CC79A7")

lanesborough<-ggplot(temp_horiz, aes(x=date_time_R)) + #generate overall plot
  geom_line(aes(y=LB_T1, col="T1")) +  #add line for T1
  geom_line(aes(y=LB_T3, col="T3")) + #add line for T3 Top
  geom_line(aes(y=LB_T6HOT, col="T6 Hot")) + #add line for T6 Cold
  geom_line(aes(y=LB_T6C, col="T6 Cold")) + #add line for T6 Hot
  scale_colour_manual(values=lbPalette)+ #change line colours
  coord_cartesian(ylim=c(0,30))+ #adjust y axis
  guides(colour = guide_legend(override.aes = list(size=3)))+ #make lines in legend thicker
  theme(plot.title = element_text(hjust = 0.5))+ #center overall title
  labs(title="Lanesborough Temperature",  
       x="", y="Temperature °C", colour="Logger") #labels

#shannonbridge colour palatte generation
sbPalette <- c("#000000", "#E69F00", "#56B4E9", "#FF3333")

shannonbridge<-ggplot(temp_horiz, aes(x=date_time_R)) + #generate overall plot
  geom_line(aes(y=SB_S1, col="S1")) + #add line for s1
  geom_line(aes(y=SB_S2, col="S2")) + #add line for s2
  geom_line(aes(y=SB_S3, col="S3")) + #add line for s3
  geom_line(aes(y=SB_S4, col="S4")) + #add line for s4
  scale_colour_manual(values=sbPalette)+ #change line colours
  coord_cartesian(ylim=c(0,30))+ #adjust y axis
  scale_x_date( limits= c(as.Date("2017/07/15"), as.Date("2018/03/18")))+#adjust x axis
  guides(colour = guide_legend(override.aes = list(size=3)))+ #make lines in legend thicker
  theme(plot.title = element_text(hjust = 0.5))+ #center overall title
  labs(title="Shannonbridge Temperature",  
       x="", y="Temperature °C", colour="Logger") #labels

multiplot(lanesborough,shannonbridge, cols=1) #print plots on same page

byday_summary$date <- as.Date(with(byday_summary, paste(year, mm, dd,sep="-")), "%Y-%m-%d")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(byday_summary, aes(x=mmddyy)) + #generate overall plot
  geom_point(aes(y=mean, colour = factor(transect_id))) + #add lines
  geom_smooth(aes(y=mean, colour = factor(transect_id))) +
  coord_cartesian(ylim=c(0,30))+ #adjust y axis
  scale_colour_manual(values=cbbPalette, labels=c("LA3", "LH3", "LA2", "LH2","SA3","SH3", "SA2", "SA1"))+
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid.major = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+ #center overall title
  labs(x="", y="Mean Daily Temperature °C", colour="Site") #labels


temp_long$growth1[temp_long$temp >= 10 & temp_long$temp <= 35] <- "1"
temp_long$growth1[is.na(temp_long$growth1)] <- 0

temp_long$toohot[temp_long$temp >= 28] <- "1"

temp_long$toohot[is.na(temp_long$toohot)] <- 0

temp_long$larvae1[temp_long$temp >= 15 & temp_long$temp <= 28] <- "1"
temp_long$larvae1[is.na(temp_long$larvae1)] <- 0

toohot_summary<- ddply(temp_long, c("transect_id"), summarise,
                       N    = length(toohot),#number of 10 minute intervals
                       toohotsum = sum(toohot=="1",na.rm=T), #number of larvae intervals
                       non = sum(toohot=="0"),#number of no larvae intervals
                       propoftime   = (toohotsum / N))

larvae_summary<- ddply(temp_long, c("transect_id"), summarise,
                       N    = length(larvae),#number of 10 minute intervals
                       larvae = sum(larvae=="1",na.rm=T), #number of larvae intervals
                       non = sum(larvae=="0"),#number of no larvae intervals
                       propoftime   = (larvae / N)) 

growth_summary<- ddply(temp_long, c("transect_id"), summarise,
                       N    = length(growth1),#number of 10 minute intervals
                       growth = sum(growth1=="1",na.rm=T), #number of larvae intervals
                       notgrowth   = sum(growth1=="0",na.rm=T),#number of no larvae intervals
                       check = sum(growth+notgrowth),
                       propoftime   = growth / N) 

super_summary<-ddply(temp_long, c("transect_id"), summarise,
                     N    = length(mmddyy),#number of intervals per day
                     growth_sum = sum(growth1=="1",na.rm=T), #number of growth optimum intervals
                     larvae_sum   = sum(larvae1=="1",na.rm=T),#number of larval release intervals
                     measuredminutes = N*10, #minutes measured in the transect
                     growth_minutes = growth_sum*10, #minutes at growth optimum
                     larvae_minutes = larvae_sum*10, #minutes for potential larval release
                     growth_prop = growth_minutes/measuredminutes,
                     larvae_prop = larvae_minutes/measuredminutes
)

daily_summary<-ddply(temp_long, c("mmddyy","transect_id"), summarise,
                     N    = length(mmddyy),#number of intervals per day
                     growth_sum = sum(growth1=="1",na.rm=T), #number of growth optimum intervals
                     larvae_sum   = sum(larvae1=="1",na.rm=T))#number of larval release intervals

daily_summary$growth_binary[daily_summary$growth_sum>=1] <- 1
daily_summary$growth_binary[is.na(daily_summary$growth_binary)] <- 0
daily_summary$larvae_binary[daily_summary$larvae_sum>=1] <- 1
daily_summary$larvae_binary[is.na(daily_summary$larvae_binary)] <- 0

day_totals<-ddply(daily_summary, c("transect_id"), summarise,
                  N    = length(mmddyy),#number of intervals per day
                  larval_days = sum(larvae_binary==1,na.rm=T),
                  growth_days = sum(growth_binary==1,na.rm=T)
) #number of growth optimum intervals



########
#Summary Stats for the two areas 
N_LB<-length(temp_horiz$LB_diff) #Number of points
mean_LB<- mean(temp_horiz$LB_diff,na.rm=T) #Lanesborough Mean
sd_LB<- sd(temp_horiz$LB_diff,na.rm=T) #Lanesborough Standard Deviation
se_LB<- sd_LB / sqrt(N_LB) #Lanesborough Standard Error

N_SB<-length(temp_horiz$SB_diff) #Number of points
mean_SB<- mean(temp_horiz$SB_diff,na.rm=T) #Shannonbridge Mean
sd_SB<- sd(temp_horiz$SB_diff,na.rm=T) #Shannonbridge Standard Deviation
se_SB<- sd_SB / sqrt(N_SB) #shannonbridge Standard Error


library(chillR)
byhour_summary <- ddply(temp_long, c("mm", "dd", "year","hour","transect_id"), summarise,
                        N    = length(temp),
                        mean = mean(temp,na.rm=T),
                        sd   = sd(temp,na.rm=T),
                        se   = sd / sqrt(N))
t6hot_gdd<-GDD(byhour_summary$mean[byhour_summary$transect_id=="LB_T6H"],summ=FALSE,Tbase=16)
t6hot_gdd[t6hot_gdd < 0] <- 0 
sum(t6hot_gdd)
t6cold_gdd<-GDD(byhour_summary$mean[byhour_summary$transect_id=="LB_T6C"],summ=FALSE,Tbase=16)
t6cold_gdd[t6cold_gdd < 0] <- 0 
sum(t6cold_gdd)



