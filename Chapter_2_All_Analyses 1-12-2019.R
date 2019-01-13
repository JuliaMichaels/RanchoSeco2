
# Install and Load Packages --------------------------------------------------------

install.packages('tidyverse'); install.packages('vegan')
install.packages('MASS');install.packages("devtools"); devtools::install_github("gavinsimpson/ggvegan")
install.packages("ggrepel"); install.packages('ggplot2');install.packages('dplyr'); install.packages('scales')
install.packages('viridis')


library('ggvegan'); library('tidyverse'); 
library('MASS');library('ggplot2');  library('scales')
library('viridis')

setwd("C:/Users/Julia Michaels/Google Drive/Dissertation Chapter 2/")



# Calibration method ------------------------------------------------------


#Step 1: Prove that calibration method works
calibration<-read.csv("2018 Inundation_Stopping 2018-03-19.csv")
fit_DL<-lm(Calibrated.Days ~ SG.Days, data = calibration[calibration$Method=='Data Logger',])
fit_Cal<-lm(Calibrated.Days ~ SG.Days, data = calibration[calibration$Method=='Calibrated Staff Gauge',])


ggplot(data=calibration, aes(x=Calibrated.Days, y=SG.Days))+
  geom_point(aes(color = Method))+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, aes(color=Method))+
  geom_label(aes(x = 0, y = 70), hjust = 0, 
             label = paste("Data Logger",
                           "\nAdj R2 =" ,signif(summary(fit_DL)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit_DL$coef[[1]],5 ),
                           " \nSlope =",signif(fit_DL$coef[[2]], 5),
                           " \nP =",signif(summary(fit_DL)$coef[2,4], 5)))+
  geom_label(aes(x = 30, y = 70), hjust = 0, 
             label = paste("Staff Gauge Calibrated",
                           "\nAdj R2 =" ,signif(summary(fit_Cal)$adj.r.squared, 5),
                           "\nIntercept =",signif(fit_Cal$coef[[1]],5 ),
                           " \nSlope =",signif(fit_Cal$coef[[2]], 5),
                           " \nP =",signif(summary(fit_Cal)$coef[2,4], 5)))



# Calibrate 2018 hydro data --------------------------------------------------------------------


data_loggers_2018<-read.csv('2018_Levelloggers.csv')
staff_gauge_2018<-read.csv('2017-2018 Staff Gauges All.csv') 


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
  geom_line(mapping=aes(joined$Date, joined$C2.14.x))+
  #geom_line(mapping=aes(joined$Date, joined$C2.17.x))+
  #geom_line(mapping=aes(joined$Date, joined$C2.17.x))+
  #geom_line(mapping=aes(joined$Date, joined$C2.18.x))+
  #geom_line(mapping=aes(joined$Date, joined$C2.19))+
  #geom_line(mapping=aes(joined$Date, joined$C3.12.x))+#shallow
  #geom_line(mapping=aes(joined$Date, joined$C3.13.x, color="red"))+ #shallow
  #geom_line(mapping=aes(joined$Date, joined$C3.16.x))+ #shallow
  #geom_line(mapping=aes(joined$Date, joined$D5.01.x))+
  #geom_line(mapping=aes(joined$Date, joined$D5.02.x))+#shallow
  #geom_line(mapping=aes(joined$Date, joined$D5.03.x))+
  #geom_line(mapping=aes(joined$Date, joined$D5.08.x))+ #shallow
#geom_line(mapping=aes(joined$Date, joined$D5.10.x))+
#geom_line(mapping=aes(joined$Date, joined$D5.11.x))+ #shallow
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
#geom_line(mapping=aes(joined$Date, joined$E5.29.x))+#shallow
#geom_line(mapping=aes(joined$Date, joined$E5.30.x))+#shallow
#geom_line(mapping=aes(joined$Date, joined$E5.31.x))+
geom_point(mapping=aes(joined$Date, joined$C2.14.y, color="red"))

#to check, plot 


#Here is the list of equivalent pool comparisons
##AKA "Calibrated Total Days"
#C2-16=D5.39
#E5-38=C2.10
#C2-11=E5-27
#B3-92=C2-10
#D5-21=D5.15
#D5.35=E5.31
#D6.58=C2.10
#D6.59=C3-12
#D6.60=D5-14
#D6.63=C3.12
#E5-02=C2-18
#E5-03=C3-13
#E5.23=D5.08
#F5.10=D6.61
#B3-91=C2-19
#B4-27-C2.10
#B4-28-D5-16
#C3-17-C3-13
#C3-19-C2-10
#D5-28-C2-18
#D5-29-C3-13
#D5-20-C2-18
#D5-18-E5-31
#E5-26-D5-08
#D5-26-C3-16
#B3-91-C2.19
#D5-12-Too hard to tell, only one point
#C2-15=Too hard to tell, only one point
#D5.05- 1 point only, not enough

#Is thia code necessary?
#veg<-read.csv("2017-2018 Vegetation Quadrats.csv")
#inundation<-read.csv("2018 Inundation Master List.csv") %>% 
#  select(Pool.ID, Calibrated.Total.Days)
#v<-right_join(veg, inundation) %>% 
#write.csv("Calibrated total days 2018.csv")

#Based on calibrated days of inundation, compare by grazing

days_2018<-read.csv("2018_Master_List.csv")%>% 
  filter(Pair.Paper.2 %in% c(1:17)) 

summary(aov(Calibrated.Days~Treatment, days_2018))

compare_2018<-ggplot(days_2018, mapping=aes(x=Treatment, y=Calibrated.Days))+
  geom_boxplot()



#graph average hydrograph by treatment

#divide into grazed/newly grazed/ungrazed
#ungrazed
DL<-read.csv("2018_Calibrated.csv")%>%
  filter(Pair.Paper.2 %in% c(1:17)) 
  group_by(Date) %>% 
  summarise_all(funs(median)) 

ungrazed <- DL[,colnames(DL) %in% days_2018$Pool.ID[days_2018$Treatment == 'Ungrazed']]
ungrazed_days<-cbind(Date=DL$Date, ungrazed)

#average each row
ug_days<-c()
for(i in 1:nrow(ungrazed_days)){
  ug_days[i]<-rowMeans(ungrazed_days[i,2:ncol(ungrazed_days)],na.rm = TRUE)
}

ungrazed<-mean<-tibble (
  Date=ungrazed_days$Date, 
  Level=ug_days)

#newly grazed
newgrazed <- DL[,colnames(DL) %in% days_2018$Pool.ID[days_2018$Treatment == 'New Grazed']]
newgrazed_days<-cbind(Date=DL$Date, newgrazed)

#average each row
ng_days<-c()
for(i in 1:nrow(newgrazed_days)){
  ng_days[i]<-rowMeans(newgrazed_days[i,2:ncol(newgrazed_days)], na.rm = TRUE)
}

newgrazed<-mean<-tibble (
  Date=newgrazed_days$Date, 
  Level=ng_days)


#grazed
grazed <- DL[,colnames(DL) %in% days_2018$Pool.ID[days_2018$Treatment == 'Grazed']]
grazed_days<-cbind(Date=DL$Date, grazed)

#average each row
g_days<-c()
for(i in 1:nrow(grazed_days)){
  g_days[i]<-rowMeans(grazed_days[i,2:ncol(grazed_days)])
}

grazed<-mean<-tibble (
  Date=grazed_days$Date, 
  Level=g_days)


#graph
ggplot(data=ungrazed, mapping=aes(x=Date, y=Level))+
  geom_line(aes(x=Date, y=Level, group=1), color='blue', size=.75)+
  geom_line(data=newgrazed, aes(x=Date, y=Level, group=1), color='turquoise', size=.75)+
  geom_line(data=grazed, aes(x=Date, y=Level, group=1), color='maroon', size=.75)+
  geom_hline(yintercept=0, color='grey63')+
  theme(plot.title=element_text(hjust=.5))+
  theme(axis.text.y=element_text(size=10))+
  scale_y_continuous(name="Level (cm)")+
  theme(axis.text.x = element_text(angle=25, size=10))+
  scale_x_discrete(breaks = ungrazed$Date[seq(1, length(ungrazed$Date), by = 30)])+
  labs(title="Average Vernal Pool Depth by Grazing, 2017-2018", y="Pool Depth", x="Date")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text.x =element_text(size=20))+
  theme(axis.text.y =element_text(size=20))


# Calibrate 2017 hydro data -----------------------------------------------

data_loggers_2017<-read.csv('2017_Levelloggers.csv')
staff_gauge_2017<-read.csv('2016-2017 Staff Gauges.csv') 


DL2017<-data_loggers_2017%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 

SG2017<-staff_gauge_2017                   #Load staff gauge data


joined2017<-full_join(DL2017, SG2017, "Date")      #Join data logger and staff gaugue


#Calculate days of inundation from staff gauges and data loggers
sg_days_inundation_2017<-c()
for(i in 2:ncol(SG2017)){
  sg_days_inundation_2017[i]<-sum(SG2017[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}

total_days_sg<-tibble(weeks=sg_days_inundation_2017, Pool.ID=colnames(SG2017)) %>% 
  mutate(sg_days=weeks * 7)#7 IS NOT CORRECT BECAUSE DIDNT SAMPLE EVERY WEEK, GOING TO UNDERESTIMATE

dl_days_inundation_2017<-c()
for(i in 2:ncol(DL2017)){
  dl_days_inundation_2017[i]<-sum(DL2017[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}
total_days_dl<-tibble(dl_days=dl_days_inundation_2017, Pool.ID=colnames(DL2017))

compare<-full_join(total_days_sg, total_days_dl) %>% 
  mutate(difference=dl_days-sg_days) %>% 
  write.csv("x.csv")

##The data logger totals are consistently higher than the staff gauge
ggplot(joined2017, mapping=aes(joined2017$Date, joined2017$C2.14.x, group=1))+
  #geom_line()+ #shallow
  #geom_line(mapping=aes(joined2017$Date, joined2017$C2.16.x))+ 
  #geom_line(mapping=aes(joined2017$Date, joined2017$C2.17.x))+
  #geom_line(mapping=aes(joined2017$Date, joined2017$C2.18.x))+
  #geom_line(mapping=aes(joined2017$Date, joined2017$C2.19.x))+
  #geom_line(mapping=aes(joined2017$Date, joined2017$C3.12.x))+
  #geom_line(mapping=aes(joined2017$Date, joined2017$C3.13.x))+ 
  #geom_line(mapping=aes(joined2017$Date, joined2017$C3.16.x))+ #shallow
  #geom_line(mapping=aes(joined2017$Date, joined2017$D5.01.x))+
  #geom_line(mapping=aes(joined2017$Date, joined2017$D5.02.x))+
  #geom_line(mapping=aes(joined2017$Date, joined2017$D5.03.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.08.x))+ #shallow
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.11.x))+ 
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.14.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.15.x))+ 
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.16.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.17.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.18.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.20.x))+ 
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.21.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.22.x))+ 
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.25.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.26.x))+ 
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.28.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.29.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D5.30.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D6.61.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$D6.63.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$E5.02.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$E5.03.x))+
#geom_line(mapping=aes(joined2017$Date, joined2017$E5.04.x))+ 
#geom_line(mapping=aes(joined2017$Date, joined2017$E5.26.x))+
geom_line(mapping=aes(joined2017$Date, joined2017$E5.27.x))+
  #geom_line(mapping=aes(joined2017$Date, joined2017$E5.29.x))+
  #geom_line(mapping=aes(joined2017$Date, joined2017$E5.30.x))+
  ##geom_line(mapping=aes(joined2017$Date, joined2017$E5.38.x))+
  geom_point(mapping=aes(joined2017$Date, joined2017$D5.10, color="red"))


days_2017<-read.csv("2017_Master_List.csv")

compare_2017<-ggplot(days_2017, mapping=aes(x=Treatment, y=Calibrated.Days))+
  geom_boxplot()
compare_2017
summary(aov(Calibrated.Days~Treatment, days_2017))



# Calibrate 2016 hyrdo data -----------------------------------------------


data_loggers_2016<-read.csv('2016_Levelloggers.csv')
staff_gauge_2016<-read.csv('2015-2016 Staff Gauges All.csv') 


DL2016<-data_loggers_2016%>%              #Average hourly datalogger data by day
  group_by(Date) %>% 
  summarise_all(funs(median)) 

SG2016<-staff_gauge_2016                   #Load staff gauge data

joined2016<-full_join(DL2016, SG2016, "Date")      #Join data logger and staff gaugue



#Plot staff gauge points against data logger (Can only look at one pool at a time)
ggplot(joined2016, mapping=aes(joined2016$Date, joined2016$C3.12.x, group=1))+ 
  geom_line()+
  geom_point(mapping=aes(joined2016$Date, joined2016$C3.12.y, color="red"))



#Calculate days of inundation from staff gauges and data loggers
sg_days_inundation_2016<-c()
for(i in 2:ncol(SG2016)){
  sg_days_inundation_2016[i]<-sum(SG2016[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}

total_days_sg<-tibble(weeks=sg_days_inundation_2016, Pool.ID=colnames(SG2016)) %>% 
  mutate(sg_days=weeks * 7)#7 IS NOT CORRECT BECAUSE DIDNT SAMPLE EVERY WEEK, GOING TO UNDERESTIMATE

dl_days_inundation_2016<-c()
for(i in 2:ncol(DL2016)){
  dl_days_inundation_2016[i]<-sum(DL2016[,i]>0, na.rm=TRUE)#took out na.rm=TRUE to look at ones where DL stops early, way after pools had dried
}
total_days_dl<-tibble(dl_days=dl_days_inundation_2016, Pool.ID=colnames(DL2016))

compare<-full_join(total_days_sg, total_days_dl) %>% 
  mutate(difference=dl_days-sg_days) %>% 
  write.csv("y.csv") 


ggplot(joined2016, mapping=aes(joined2016$Date, joined2016$B3.92.x, group=1))+
  #geom_line()+ #DONT USE
  geom_line(mapping=aes(joined2016$Date, joined2016$C2.11.x))+ #BROKEN?
  #geom_line(mapping=aes(joined2016$Date, joined2016$C2.14.x))+ #BROKEN?
  #geom_line(mapping=aes(joined2016$Date, joined2016$E5.30.x))+ #DONT USE
  #geom_line(mapping=aes(joined2016$Date, joined2016$F5.10.x))+ #DONT USE
  #geom_line(mapping=aes(joined2016$Date, joined2016$D6.59.x))+ #DONT USE
  #geom_line(mapping=aes(joined2016$Date, joined2016$D6.60.x))+ #DONT USE
  #geom_line(mapping=aes(joined2016$Date, joined2016$D6.63.x))+ #DONT USE
  #geom_line(mapping=aes(joined2016$Date, joined2016$D5.35.x))+ #DONT USE
  #geom_line(mapping=aes(joined2016$Date, joined2016$E5.03.x))+#DONT USE
  #geom_line(mapping=aes(joined2016$Date, joined2016$C2.10.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$C2.15.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$C2.17.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$C2.18.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$C3.12.x))+
##geom_line(mapping=aes(joined2016$Date, joined2016$C3.13.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$C3.17.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$C3.19.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$D5.28.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$D5.39.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$D6.40.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$D6.61.x))+
#geom_line(mapping=aes(joined2016$Date, joined2016$E5.27.x))+
# geom_point(mapping=aes(joined2016$Date, joined2016$B3.92, color="red"))


days_2016<-read.csv("2016_Master_List.csv")

compare_2016<-ggplot(days_2016, mapping=aes(x=Treatment, y=Calibrated.Days))+
  geom_boxplot()

summary(aov(Calibrated.Days~Treatment, days_2016))




# Compare calibrated inundation days over all years ----------------------------

all_years_combined<-read.csv("All_Years_Master_List.csv") 
all_years_combined$Year<-as.factor(all_years_combined$Year)
all_years<-all_years_combined%>%
  filter(Pair.Paper.2 %in% c(1:17)) %>% 
  group_by(Year, Treatment) %>% 
  summarize(days=mean(Calibrated.Days, na.rm = TRUE), sd=sd(Calibrated.Days, na.rm = TRUE), 
            sem = sd(Calibrated.Days, na.rm = TRUE)/sqrt(length(Calibrated.Days)))
all_years$Year<-as.factor(all_years$Year)

plot<-ggplot(data=all_years, aes(x=Year, y=days, group=Treatment))+
  geom_line(aes(color=Treatment), size=1.5)+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_errorbar(aes(ymin=days-sem, ymax=days+sem), width=.025)+
  geom_point(size=2)+
  scale_x_discrete(labels=c("2015-2016", "2016-2017", "2017-2018"))+
  labs(title="Average Days of Innundation by Treatment, 2015-2018", y="Days of Inundation", x="Year")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

plot

#Inundation period by grazing and year
anova<-aov(Calibrated.Days~Treatment*Year, all_years_combined)
summary(anova)
TukeyHSD(anova)

inundation_days <- lm(Calibrated.Days ~ Treatment+Year+Size+Soil.Type, data=all_years_combined) 
summary(inundation_days)
hist(residuals(inundation_days))



#2018 inundation period by grazing and other abiotic characteristics
qdata<-read.csv("2017-2018 Vegetation Quadrats.csv",fileEncoding="UTF-8-BOM")
qdata$Catchment<-as.character(qdata$Catchment)
qdata$Catchment<-as.numeric(qdata$Catchment)
specid<-read.csv("SpecID.csv", fileEncoding="UTF-8-BOM")
qdata_separate<-qdata[-39,] #remove 39 if lookng at samples separately

species<-dplyr::select(qdata_separate, -(Quadrat:Inundation.Type)) %>%  #average the three quadrat samples
  group_by(Pool.ID)%>%
  summarise_all(funs(mean))

pool_info<-qdata_separate %>% group_by(Pool.ID) %>% 
  filter(row_number()==1) %>% 
  dplyr::select(Pool.ID:Inundation.Type, -Quadrat)

qdata<-right_join(pool_info, species)#combine community data with pool characteristics


#RDM by grazing
ggplot(data=qdata, aes(x=Grazing,y=RDM, fill=Grazing))+
  geom_boxplot()+
  labs(title="Residual Dry Matter (RDM) by Grazing", y="RDM", x="Grazing")+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

summary(aov(RDM~Grazing, qdata))

#RDM by grazing
ggplot(data=qdata, aes(x=Grazing,y=Size, fill=Grazing))+
  geom_boxplot()+
  labs(title="Pool Size by Grazing", y="Size", x="Grazing")+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

#catchment area by grazingclass(qdata$Catchment)
qdata$Catchment<-as.character(qdata$Catchment)
qdata$Catchment<-as.numeric(qdata$Catchment)
ggplot(data=qdata, aes(x=Grazing,y=Catchment, fill=Grazing))+
  geom_boxplot()+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  labs(title="Catchment Size by Grazing", y="Catchment Size (m2)", x="Grazing")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


summary(aov(Catchment~Grazing, qdata))


inundation_days2 <- lm(Calibrated.Total.Days~Grazing+Size+Catchment+SoilType, data=qdata) 
summary(inundation_days2)
hist(residuals(inundation_days2))




# Transect data -----------------------------------------------------------

transect_data<-read.csv("2016-2018-Vegetation-Transects_Cleaned.csv") %>% 
  filter(Pair.P2 %in% c(1:11)) 
#Trans_2018<-filter(transect_data, Pair.P2 %in% c(1:12), Year=="2018")
#head(Trans_2018)

##########2016-2018 Transect Top Hit Only########
#2016 was top hit, 2017 and 2018 were multiple hits per point, so have to filter out first row

Transect_TopHit<-transect_data%>%
  group_by(Year, Point, Pool.ID, Transect., Zone)%>%
  #mutate(Speciesnew = first(Species))%>%
  filter(row_number()==1)%>%
  #select(-Point)%>%
  group_by(Species, Pool.ID, Grazing, Zone, Year, Transect.)%>%
  tally()%>%
  arrange(desc(Pool.ID))%>%
  group_by(Pool.ID, Zone, Year, Transect.)%>%
  mutate(trans_length=sum(n),
         Rel_abun=n/trans_length,#calculate relative abundance for each species per pool
         specrich=n_distinct(Species),#calculate species richness for each pool
         shannon=diversity(Rel_abun))%>%
  arrange(Pool.ID, Year, Zone, Transect.)

#calculate diversity metrics

Transect_diversity<-
  Transect_TopHit %>% 
  dplyr::select(-Species) %>% 
  group_by(Pool.ID, Grazing, Zone, Year) %>% 
  summarize_all(funs(mean)) #average the two transects
Transect_diversity$Year<-as.factor(Transect_diversity$Year)

#####ANOVAS for specrich and shannon
summary(aov(specrich~Grazing*Year, Transect_diversity))
summary(aov(shannon~Grazing*Year, Transect_diversity)) 


##Calculate native relative cover
specid<-read.csv("SpecID.csv", stringsAsFactors=FALSE)

Transect_TopHit_Summed<-Transect_TopHit %>% 
  dplyr::select(Species,Pool.ID, Transect., Year, Grazing, Zone, Rel_abun) %>% 
  group_by(Species,Pool.ID, Year, Zone, Grazing) %>% 
  summarize_all(funs(sum))

st<-t(Transect_TopHit_Summed)
colnames(st) <- as.character(unlist(st[1,]))
st1= st[-1, ]
stnative<-as.data.frame(st1[,colnames(st1) %in% specid$SpeciesIDCode[specid$Status == 'Native']])
stnative1<-t(stnative)
stnative2<-as.data.frame(stnative1, row.names = TRUE)

native_abundance<-stnative2%>%
  dplyr::select(-Transect.)


native_abundance$Rel_abun<-as.character(native_abundance$Rel_abun)
native_abundance$Rel_abun<-as.numeric(native_abundance$Rel_abun)

native_cover<-native_abundance%>% 
  group_by(Pool.ID, Zone, Grazing, Year) %>% 
  summarise_all(funs(sum)) %>% 
  mutate(native=Rel_abun/2)

###ANOVA for native cover
#####ANOVAS
summary(aov(native~Grazing*Year, native_cover))


############Plot species richness by grazing treatment
ggplot(data=Transect_diversity, aes(x=Year,y=specrich, fill=Grazing))+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  geom_boxplot()+
  facet_wrap(~Zone)+
  labs(title="Species Richness by Habitat Zone and Grazing Treatment", y="Species Richness", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(strip.text =element_text(size=30))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))


###########Plot shannon by grazing treatment
ggplot(data=Transect_diversity, aes(x=Year,y=shannon, fill=Grazing))+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  geom_boxplot()+
  facet_wrap(~Zone)+
  labs(title="Shannon Weiner Diversity by Habitat Zone and Grazing Treatment", y="Shannon Weiner DIversity", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(strip.text =element_text(size=30))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))



ggplot(data=plot2[plot2$Year==("2018"),], aes(x=Calibrated.Total.Days,y=shannon, colour=Grazing))+
  geom_jitter()+
  #geom_point(mapping=aes(group=cut_width(Calibrated.Total.Days, 15)))+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE)+
  facet_wrap(~Zone)+
  ggtitle("Shannon Weiner Diversity by Days of Inundation and Grazing")


##Plot native cover by grazing treatment


ggplot(data=native_cover, aes(x=Year,y=native, fill=Grazing))+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  geom_boxplot()+
  facet_wrap(~Zone)+
  labs(title="Native Cover by Habitat Zone and Grazing Treatment", y="Shannon Weiner DIversity", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(strip.text =element_text(size=30))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))




# 2018 Quadrats in Transition Zones ----------------------------------------------------------------

#Calculate diversity indices

#species richness
spec_rich<-c()
for(i in 1:nrow(qdata)){
  spec_rich[i]<-sum(qdata[i,-(1:15)]>0)
}

#shannon weiner
shannon <- c()
for(i in 1:nrow(qdata)){
  temp <- as.numeric(qdata[i,-(1:15)])
  shannon[i] <- diversity(temp)
}

#Calculate the relative cover of natives
natives <- qdata[,colnames(qdata) %in% specid$SpeciesIDCode[specid$Status == 'Native']]

total_cov_nat<-c()
for(i in 1:nrow(natives)){
  total_cov_nat[i]<-sum(natives[i,1:ncol(natives)])
}

total_cov <- c()
for(i in 1:nrow(qdata)){
  temp <- as.numeric(qdata[i,-(1:13)])
  total_cov[i] <- sum(temp)
}

rel_cov_nat<-c()
for(i in 1:nrow(qdata)){
  rel_cov_nat[i]<-total_cov_nat[i]/total_cov[i]
}


#create data frame with all community metrics
transition_diversity <- data.frame(qdata, spec_rich,shannon, rel_cov_nat)
write.csv("Transition_diversity.csv")

#Plot Species richness by grazing
ggplot(data=transition_diversity, aes(x=Grazing,y=spec_rich))+
  geom_boxplot()+
  
  ggtitle("Species Richness by Grazing")

summary(aov(spec_rich~Grazing*Calibrated.Total.Days, transition_diversity))

#Plot Species richness by days of inundation and treatment 
ggplot(data=transition_diversity, aes(x=Calibrated.Total.Days,y=spec_rich, colour=Grazing))+
  #geom_jitter()+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(mapping=aes(group=cut_width(Calibrated.Total.Days, 15)), size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Species Richness by Days of Inundation and Grazing", y="Species Richness", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))



#Plot shannon by grazing
ggplot(data=transition_diversity, aes(x=Grazing,y=shannon))+
  geom_boxplot()+
  ggtitle("Shannon Weiner Diversity by Grazing")

summary(aov(shannon~Grazing, transition_diversity))

#Plot Shannon by days of inundation and treatment 
ggplot(data=transition_diversity, aes(x=Calibrated.Total.Days,y=shannon, colour=Grazing))+
  #geom_jitter()+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(mapping=aes(group=cut_width(Calibrated.Total.Days, 15)), size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Shannon Weiner Diversity by Days of Inundation and Grazing", y="Shannon Weiner", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

#Plot native cover by grazing
ggplot(data=transition_diversity, aes(x=Grazing,y=rel_cov_nat))+
  geom_boxplot()+
  ggtitle("Relative Native Cover by Grazing")

summary(aov(rel_cov_nat~Grazing, transition_diversity))

#Plot native cover by days of inundation and treatment 
ggplot(data=transition_diversity, aes(x=Calibrated.Total.Days,y=rel_cov_nat, colour=Grazing))+
  #geom_jitter()+
  scale_color_manual(values=c("maroon", "turquoise", "blue"))+
  geom_point(mapping=aes(group=cut_width(Calibrated.Total.Days, 15)), size=4)+
  stat_smooth(method='lm', se=FALSE, fullrange = TRUE, size=2)+
  labs(title="Native Relative Cover by Days of Inundation and Grazing", y="Relative Cover Natives", x="Days of Inundation")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))




###Linear model: native cover by grazing and inundation (Staff gauge)
linearMod1 <- lm(rel_cov_nat ~ Calibrated.Total.Days*Grazing, data=transition_diversity) 
summary(linearMod1)
hist(residuals(linearMod1))


###Linear model: specrich by grazing and inundation

linearMod3 <- lm(spec_rich ~ Calibrated.Total.Days*Grazing, data=transition_diversity) 
summary(linearMod3)
hist(residuals(linearMod3))

linearMod4 <- lm(shannon ~ Calibrated.Total.Days*Grazing, data=transition_diversity) 
summary(linearMod4)
hist(residuals(linearMod4))



# 2018 Quadrat Ordination -------------------------------------------------


#####PCA#####

quad.pca<-rda(qdata1)
quad.pca
ordiplot(quad.pca)
biplot(quad.pca)
dim(qdata1)

quad.ca<-cca(qdata1[-93,])
quad.ca
chisq.test(qdata1/sum(qdata1))

plot(quad.ca, display=c('sites', 'sp')) #'sp' only displays labels, why can't both be displayed at the same time?
p0<-plot(quad.ca, choices = c(1, 2), display = c("sp", "wa"),
         scaling = 2)
identify(p0, "species")

p1<-plot(quad.ca, dis='sp', type='n')

mod<-decorana(qdata1)
stems<-colSums(qdata1)
plot(mod, dis='sp', type='n')
sel<-orditorp(mod, dis='sp', priority=stems, pcol='grey', pch='+' )


###fitting environmental variables####

env_quad1<-
  tibble(
    Pool.ID=qdata$Pool.ID,
    Grazing=qdata$Grazing,
    In_Days=qdata$Calibrated.Total.Days,
    HoofCount=qdata$Hoofprint,
    RDM=qdata$RDM,
    # Catchment=qdata$Catchment, can't do because of NAs
    Soil=qdata$SoilType,
    Size=qdata$Size
  )

#env_quad1<-full_join(env_quad1, pool_depth, by="Pool.ID")#stuck random values in for C3-17 and D5-08, measure soon
#env_quad<-env_quad[-c(112:nrow(env_quad)),]
ef<-envfit(quad.mds2, env_quad1, permu=999, na.rm=TRUE)

ef
plot(quad.mds2, display='sites')
plot(ef, p.max=0.1)

###CCA####
#install.packages('ggfortify'); library('ggfortify')
#install.packages('ggvegan', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('dplyr',  dependencies=TRUE, repos='http://cran.rstudio.com/')
#nstall.packages('tidyverse'); library('tidyverse')
#library('ggvegan')

quad<-cca(qdata1~In_Days+HoofCount+Grazing+Size+Soil+RDM, env_quad1)#will want to condition on year for transects
quad
plot(quad)
#the total inertia is decomposed into constrained and unconstrained components
#'proportion of inertia' doesn't really have a clear meaning in CCA (not lioke in RDA)

#test the significance of the model by permuting the data randomly and refitting the model
#when the constrained inertia in permutations is always lower than the oberved constrained inertia, the constraints are significant
anova(quad)
#don't pay attention to F ratio
#test significance of the variables
anova(quad, by='term', step=200)
anova(quad, by='margin', perm=500)
anova(quad, by='axis', perm=1000)# Only first axis is significant
##Try conditioning the CCA on days of inundation, looking just at grazing
quad2<-cca(qdata1~Grazing+ Condition(In_Days), env_quad1)
anova(quad2)
quad3<-cca(qdata1~Grazing, env_quad)#Grazing alone
anova(quad3)
#can sub in grazing treatment for hoofprint
#Grazing has an effect even when when variation due to inundation days is removed
#butttt, the variables of hoofprint and days of inundation are linearly dependent
with(env_quad, anova(quad, strata=In_Days))



#Plot CCA#
p1<-ordiplot(quad)
identify(p1, "species")
p1
p2<-autoplot(quad)+
  geom_point(data = cbind(subset(fmod, Score == "sites"), Grazing = env_quad$Grazing),
             aes(x = CCA1, y = CCA2, colour = Grazing), size = 2)


fmod <- fortify(quad)
size <- 1.8


p4<-ggplot(fmod, aes(x = CCA1, y = CCA2, label=Label)) +
  geom_text(data = subset(fmod, Score == "species"),
            colour = 'grey66', size = 3) +
  geom_point(data = cbind(subset(fmod, Score == "sites"), Grazing = env_quad1$Grazing),
             aes(colour = Grazing), size = 2) +
  scale_colour_brewer("Score", palette = "Set2") +
  coord_fixed() +
  theme(legend.position = "top") +
  geom_segment(data = subset(fmod, Score == "biplot")[c(1:2,5:7),],
               aes(x = 0, y = 0, xend = CCA1 * 3, yend = CCA2 * 3), arrow = arrow(length = unit(1/2, 'picas')), colour = "gray24", size = 1) +
  geom_text(data = subset(fmod, Score == "biplot")[c(1:2,5:7),], 
            aes(x=CCA1 * 3,y=CCA2 *3,label=c("Hydroperiod", "Hoofprints",  "Size", "Soil Type", "RDM")), size=4, colour = "gray24") + xlab("CCA1") + ylab("CCA2") +
  geom_segment(data = subset(fmod, Score == "biplot")[3:4,],
               aes(x = 0, y = 0, xend = CCA1, yend = CCA2), arrow = arrow(length = unit(1/2, 'picas')), colour = "gray24", size = 1) +
  geom_text(data = subset(fmod, Score == "biplot")[3:4,], 
            aes(x=CCA1,y=CCA2,label=c("New Grazed", "Ungrazed")), size=4, colour = "gray24") + xlab("CCA1") + ylab("CCA2")+
  geom_segment(data = subset(fmod, Score == "centroids")[1,],
               aes(x = 0, y = 0, xend = CCA1, yend = CCA2), arrow = arrow(length = unit(1/2, 'picas')), colour = "gray24", size = 1) +
  geom_text(data = subset(fmod, Score == "centroids")[1,], 
            aes(x=CCA1,y=CCA2,label="Grazed"), size=4, colour = "gray24") + xlab("CCA1") + ylab("CCA2")
p4





































































