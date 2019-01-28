library('dplyr'); 
library('ggplot2')

setwd("C://Users//Julia Michaels//Google Drive//Dissertation Chapter 2//Data//Chapter_2_Analysis//")




# Load data ---------------------------------------------------------------

data_loggers_2018<-read.csv('2018_Levelloggers.csv')
staff_gauge_2018<-read.csv('2017-2018 Staff Gauges All.csv') 
pools<-read.csv("2018 Inundation_Stopping 2018-03-19.csv") %>% 
  filter(Pool.ID !="D5.13")

# format data -------------------------------------------------------------


data_loggers_2018$Date<-as.Date(data_loggers_2018$Date)#convert date to numeric
staff_gauge_2018$Date<-as.Date(staff_gauge_2018$Date)#convert date to numeric
comparing_freq<-read.csv('comparing_sg_sampling_frequency.csv')




dl_selection<- data_loggers_2018 %>%  #filter out all dates after 3-19 (stopped consistent SG monitoring)
  filter(Date<='2018-03-19') %>%      #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median))

sg_selection<-staff_gauge_2018 %>%  #filter out all dates after 3-19 (stopped consistent SG monitoring)
  filter(Date<='2018-03-19')

all_pools<-full_join(dl_selection, sg_selection, "Date")

#Calculate days of inundation from staff gauges
sg_weeks_inundation<-c()
for(i in 2:ncol(sg_selection)){
  sg_weeks_inundation[i]<-sum(sg_selection[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
} 
total_days_sg_selection<-tibble(weeks=sg_weeks_inundation, Pool.ID=colnames(sg_selection)) %>% 
  mutate(sg_days=weeks * 7)

dl_days_inundation<-c()
for(i in 2:ncol(dl_selection)){
  dl_days_inundation[i]<-sum(dl_selection[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
} 

total_days_dl_selection<-tibble(dl_days=dl_days_inundation, Pool.ID=colnames(dl_selection))

compare<-full_join(total_days_dl_selection, total_days_sg_selection) %>% 
  mutate(difference=dl_days-sg_days)  
###write.csv("2018 Inundation Master List_Stopping 2018-03-19_copy.csv")
  #don't run this again, added in the calibrated pools manually


##Compare staff gauge to dl in pools that had both
compare_weekly<-compare %>% 
  filter(!is.na(difference)) %>% 
  filter(! Pool.ID=="D5.13")

fit<-lm(dl_days ~ sg_days, data = compare_weekly)

weekly<-ggplot(data=compare_weekly, aes(x=dl_days,y=sg_days))+
  geom_point()+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE)+
  geom_label(aes(x = 40, y = 70), hjust = 0, 
             label = paste("Calbirated Staff Gauge",
                           "\nAdj R2 =" ,signif(summary(fit)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit$coef[[1]],5 ),
                           " \nSlope =",signif(fit$coef[[2]], 5),
                           " \nP =",signif(summary(fit)$coef[2,4], 5)))

###Now compare SG weekly sampling to sampling every 2 and 3 weeks

#every week
every_week<-total_days_sg_selection

#every 2 weeks
every_2<-sg_selection%>% 
  filter(row_number() %% 2 == 0)
every_2_inundation<-c()
for(i in 2:ncol(every_2)){
  every_2_inundation[i]<-sum(every_2[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
} 
every_2_weeks<-tibble(weeks2=every_2_inundation, Pool.ID=colnames(every_2)) %>% 
  mutate(days_2=weeks2 * 14)

#every 3 weeks
every_3<-sg_selection%>% 
  filter(row_number() %% 4 == 0)
every_3_inundation<-c()
for(i in 3:ncol(every_3)){
  every_3_inundation[i]<-sum(every_3[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
} 
every_3_weeks<-tibble(weeks3=every_3_inundation, Pool.ID=colnames(every_3)) %>% 
  mutate(days_3=weeks3 * 28)

compare_frequency<-full_join(total_days_dl_selection, every_week) %>% 
  full_join(., every_2_weeks) %>% 
  full_join(., every_3_weeks) #%>% 
 #write.csv("compare_frequency.csv")#made some changes to this, called it "comparing_sg_sampling_frequency"


fit_weekly<-lm(DL_Days ~ SG_Days_Adjusted, data = comparing_freq[comparing_freq$Method=='Every_Week',])
fit_2_weeks<-lm(DL_Days ~ SG_Days_Adjusted, data = comparing_freq[comparing_freq$Method=='Every_2_Weeks',])
fit_3_weeks<-lm(DL_Days ~ SG_Days_Adjusted, data = comparing_freq[comparing_freq$Method=='Every_3_Weeks',])
fit_4_weeks<-lm(DL_Days ~ SG_Days_Adjusted, data = comparing_freq[comparing_freq$Method=='Every_4_Weeks',])

labels <- c(Stable = "Stable Inundation (<4 dry down periods)", Variable = "Variable Inundation (>4 dry down periods)")



ggplot(data=comparing_freq, aes(x=DL_Days,y=SG_Days_Adjusted))+
  geom_point(aes(color = Method))+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, aes(color=Method))+
 # facet_wrap(~Inundation.Type, labeller=labeller(Inundation.Type = labels))+
  labs(y="Staff Gauge Days of Inundation", x="Data Logger Days of Inundation")+
  theme(plot.title=element_text(size=20, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=20))+
  theme(strip.text =element_text(size=20))+
  scale_color_manual(name="Staff Gauge Measurement Frequency", 
                       labels = c("Weekly", 
                                  "Every Two Weeks", 
                                  "Every Three Weeks", 
                                  "Monthly"), 
                       values = c("Every_Week"="blue", 
                                  "Every_2_Weeks"="pink", 
                                  "Every_3_Weeks"="turquoise", 
                                  "Every_4_Weeks"="purple"))



###extra labeling section###

+
  geom_label(aes(x = 0, y = 50), hjust = 0, 
             label = paste("Weekly Sampling",
                           "\nAdj R2 =" ,signif(summary(fit_weekly)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit_weekly$coef[[1]],5 ),
                           " \nSlope =",signif(fit_weekly$coef[[2]], 5),
                           " \nP =",signif(summary(fit_2_weeks)$coef[2,4], 5)))+
  geom_label(aes(x = 20, y = 50), hjust = 0, 
             label = paste("Every 2 Weeks",
                           "\nAdj R2 =" ,signif(summary(fit_2_weeks)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit_2_weeks$coef[[1]],5 ),
                           " \nSlope =",signif(fit_2_weeks$coef[[2]], 5),
                           " \nP =",signif(summary(fit_2_weeks)$coef[2,4], 5)))+
  geom_label(aes(x = 40, y = 50), hjust = 0, 
             label = paste("Every 3 Weeks",
                           "\nAdj R2 =" ,signif(summary(fit_3_weeks)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit_3_weeks$coef[[1]],5 ),
                           " \nSlope =",signif(fit_3_weeks$coef[[2]], 5),
                           " \nP =",signif(summary(fit_3_weeks)$coef[2,4], 5)))+
  geom_label(aes(x = 60, y = 50), hjust = 0, 
             label = paste("Every 4 Weeks",
                           "\nAdj R2 =" ,signif(summary(fit_4_weeks)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit_4_weeks$coef[[1]],5 ),
                           " \nSlope =",signif(fit_4_weeks$coef[[2]], 5),
                           " \nP =",signif(summary(fit_4_weeks)$coef[2,4], 5)))
####

###Comparing calibrated fit to uncalibrated fit

fit1<-lm(Calibrated.Days ~ SG.Days, data = pools[pools$Method=='Calibrated Staff Gauge',])
fit2<-lm(Calibrated.Days ~ SG.Days, data = pools[pools$Method=='Data Logger',])

ggplot(data=pools, aes(x=Calibrated.Days,y=SG.Days))+
  geom_point(aes(color = Method), size=3)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, aes(color=Method))+
  geom_label(aes(x = 0, y = 70), hjust = 0, size=7, 
             label = paste("Calbirated Staff Gauge",
                           "\nAdj R2 =" ,signif(summary(fit1)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit1$coef[[1]],5 ),
                           " \nSlope =",signif(fit1$coef[[2]], 5),
                           " \nP =",signif(summary(fit1)$coef[2,4], 5)))+
  geom_label(aes(x = 20, y = 70), hjust = 0, size=7, 
             label = paste("Data Logger",
                           "\nAdj R2 =" ,signif(summary(fit2)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit1$coef[[1]],5 ),
                           " \nSlope =",signif(fit1$coef[[2]], 5),
                           " \nP =",signif(summary(fit1)$coef[2,4], 5)))+
  labs(title="2017-2018 Inundation Days Measured by Data Loggers vs Calibrated Staff Gauge", y="Staff Gauge", x="Data Logger")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=20))


# Calibrate 2018 hydro data --------------------------------------------------------------------

DL<-data_loggers_2018%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 

SG<-staff_gauge_2018                   #Load staff gauge data

joined<-full_join(DL, SG, "Date")      #Join data logger and staff gaugue

sg_days_inundation<-c()
for(i in 2:ncol(SG)){
  sg_days_inundation[i]<-sum(SG[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}

dl_days_inundation<-c()
for(i in 2:ncol(DL)){
  dl_days_inundation[i]<-sum(DL[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}

total_days_sg<-tibble(weeks=sg_days_inundation, Pool.ID=colnames(SG)) %>% 
  mutate(sg_days=weeks * 7)

total_days_dl<-tibble(dl_days=dl_days_inundation, Pool.ID=colnames(DL))

compare<-full_join(total_days_sg, total_days_dl) %>% 
  mutate(difference=dl_days-sg_days)

##The data logger totals are consistently higher than the staff gauge (except for D5-01)
#Now the problem is that we still have a lot of staff gauge pools that didn't have data
#loggers, so we graph each staff gauge pool against all the data loggers to find a pool that 
#behaves similarly
ggplot(joined, mapping=aes(joined$Date, joined$C2.10.x, group=1))+
  #geom_line()+
  #geom_line(mapping=aes(joined$Date, joined$C2.14.x))+
  #geom_line(mapping=aes(joined$Date, joined$C2.17.x))+
  #geom_line(mapping=aes(joined$Date, joined$C2.17.x))+
  #geom_line(mapping=aes(joined$Date, joined$C2.18.x))+
  #geom_line(mapping=aes(joined$Date, joined$C2.19))+
  #geom_line(mapping=aes(joined$Date, joined$C3.12.x))+#shallow
  #geom_line(mapping=aes(joined$Date, joined$C3.13.x))+ #shallow
  #geom_line(mapping=aes(joined$Date, joined$C3.16.x))+ #shallow
  #geom_line(mapping=aes(joined$Date, joined$D5.01.x))+
  #geom_line(mapping=aes(joined$Date, joined$D5.02.x))+#shallow
#geom_line(mapping=aes(joined$Date, joined$D5.03.x))+
geom_line(mapping=aes(joined$Date, joined$D5.08.x))+ #shallow
  #geom_line(mapping=aes(joined$Date, joined$D5.10.x))+
  ####geom_line(mapping=aes(joined$Date, joined$D5.11.x))+ #shallow
  #geom_line(mapping=aes(joined$Date, joined$D5.13.x))+#no water DL FAILED DON'T USE
  #geom_line(mapping=aes(joined$Date, joined$D5.14.x))+
  #geom_line(mapping=aes(joined$Date, joined$D5.15.x))+ #shallow
  #geom_line(mapping=aes(joined$Date, joined$D5.16.x))+#shall0w
  #geom_line(mapping=aes(joined$Date, joined$D5.17.x))+#shall0w
  #geom_line(mapping=aes(joined$Date, joined$D5.22.x))+
  #geom_line(mapping=aes(joined$Date, joined$D5.25.x, color="red"))+ #shallow
  #geom_line(mapping=aes(joined$Date, joined$D5.29.x))+ #shallow
  #geom_line(mapping=aes(joined$Date, joined$D5.30.x))+ #shallow
#geom_line(mapping=aes(joined$Date, joined$D5.39.x))+#shallow
#geom_line(mapping=aes(joined$Date, joined$D6.37.x))+ #shallow
#geom_line(mapping=aes(joined$Date, joined$D5.16.x))+
#geom_line(mapping=aes(joined$Date, joined$D6.40.x))+
#geom_line(mapping=aes(joined$Date, joined$D6.61.x, , color="red"))+
#geom_line(mapping=aes(joined$Date, joined$E5.04.x))+
#geom_line(mapping=aes(joined$Date, joined$E5.05.x, color="red"))+ #shallow
#geom_line(mapping=aes(joined$Date, joined$E5.27.x))+ #deep
##geom_line(mapping=aes(joined$Date, joined$E5.29.x))+#shallow
###geom_line(mapping=aes(joined$Date, joined$E5.30.x))+#shallow
#geom_line(mapping=aes(joined$Date, joined$E5.31.x))+
geom_point(mapping=aes(joined$Date, joined$D5.08.y, color="red"))


##Example hydrograph for Calibration paper: D5-08
ggplot(joined, mapping=aes(joined$Date, joined$D5.08.x, group=1))+
  geom_line( mapping=aes(linetype="Data Logger",group=1), size=1.5)+
  geom_point(mapping=aes(joined$Date, joined$D5.08.y, color="Staff Gauge"), size=4)+
geom_hline(yintercept=0, color='grey50', linetype='dashed')+
  theme(plot.title=element_text(hjust=.5))+
  theme(axis.text.y=element_text(size=10))+
  theme(axis.text.x = element_text(angle=25, size=10))+
  labs(title="Sample Pool Depth, 2017-2018", y="Pool Depth (cm)", x="")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title = element_blank())+
  theme(legend.text =element_text(size=30))+
  theme(axis.text.x =element_text(size=20))+
  theme(axis.text.y =element_text(size=20))+
  scale_linetype_manual(name="", 
                     values=c("Data Logger"="solid"))
 
  


