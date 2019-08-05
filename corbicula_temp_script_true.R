#Input Data
##Input data in long format, one singular column
temp_long<-read.csv("tempdata_long.csv", header=T, stringsAsFactors = F)
##Input data in horizontal format 
temp_horiz<-read.csv("tempdata_horiz.csv", header=T, stringsAsFactors = F)

######
#Convert Excel date/time to R date/time
##load required package
library(janitor)
temp_horiz$date_time<-as.numeric(temp_horiz$date_time)
temp_horiz$date_time_R<-excel_numeric_to_date(temp_horiz$date_time)
temp_long$date_time_R<-excel_numeric_to_date(temp_long$date_time)
#double check with a quick plot
plot(temp_horiz$LB_T1~temp_horiz$date_time_R)
plot(temp_long$temp~temp_long$date_time_R)


#############
#generate general plots for lanesborough and shannonbridge
library(ggplot2) #for all your plotting needs! It's GGPLOT2!
library(lubridate) #allows easy manipulation of date data
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


###########
#Prep for analyses
library(plyr)
library(dplyr)
#generate a summary of daily mean temps and variances
byday_summary <- ddply(temp_long, c("mm", "dd", "year", "transect_id"), summarise,
                       N    = length(temp),
                       mean = mean(temp,na.rm=T),
                       sd   = sd(temp,na.rm=T),
                       se   = sd / sqrt(N))
byday_summary$date <- as.Date(with(byday_summary, paste(year, mm, dd,sep="-")), "%Y-%m-%d")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

with_smooths<-ggplot(byday_summary, aes(x=date)) + #generate overall plot
  geom_point(aes(y=mean, colour = factor(transect_id))) + #add lines
  geom_smooth(aes(y=mean, colour = factor(transect_id))) +
  coord_cartesian(ylim=c(0,30))+ #adjust y axis
  scale_colour_manual(values=cbbPalette, labels=c("LA3", "LH3", "LA2", "LH2","SA3","SH3", "SA2", "SA1"))+
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+ #center overall title
  labs(x="", y="Mean Daily Temperature °C", colour="Site") #labels

means_only<-ggplot(byday_summary, aes(x=date)) + #generate overall plot
  geom_point(aes(y=mean, colour = factor(transect_id))) + #add lines
  coord_cartesian(ylim=c(0,30))+ #adjust y axis
  scale_colour_manual(values=cbbPalette, labels=c("LA3", "LH3", "LA2", "LH2","SA3","SH3", "SA2", "SA1"))+
  theme(axis.text=element_text(size=14, colour="black"),
        axis.title=element_text(size=14, colour="black"),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+ #center overall title
  labs(x="", y="Mean Daily Temperature °C", colour="Site") #labels


differential<-ggplot(temp_horiz, aes(x=date_time_R)) + #generate overall plot
  geom_smooth(aes(y=SB_diff, col="Shannonbridge")) +  #add line for T1
  geom_smooth(aes(y=LB_diff, col="Lanesborough")) + #add line for T3 Top
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
       x="", y="Differential Temperature °C", colour="Site") #labels


#Generate extra variables
##generate new column, growth which is between 10 and 16 degrees, 1 is yes, 0 is no
temp_long$growth1[temp_long$temp > 10 & temp_long$temp <= 15] <- "1"
temp_long$growth1[is.na(temp_long$growth1)] <- 0

temp_long$larvae1[temp_long$temp > 16 & temp_long$temp <= 28] <- "1"
temp_long$larvae1[is.na(temp_long$larvae1)] <- 0

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

##########
#Differential Calculations
temp_horiz$LB_diff<-temp_horiz$LB_T6HOT-temp_horiz$LB_T6C #Laneborough differential
temp_horiz$SB_diff<-temp_horiz$SB_S2-temp_horiz$SB_S4 #Shannonbridge differential
temp_horiz$LB_diff[temp_horiz$LB_diff < 0] <- NA #Negative values are NA
temp_horiz$SB_diff[temp_horiz$SB_diff < 0] <- NA #Negative values are NA

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


#######
##Final Plot Script
library(cowplot)
plot_grid(with_smooths + theme(legend.position="right"),
                 differential + theme(legend.position="right"),
                 align = 'vh',
                 labels = c("a", "b"),
                 hjust = -1,
                 nrow = 2
)
