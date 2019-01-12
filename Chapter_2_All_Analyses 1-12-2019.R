install.packages('tidyverse'); install.packages('vegan')
install.packages('MASS');install.packages("devtools"); devtools::install_github("gavinsimpson/ggvegan")
install.packages("ggrepel"); install.packages('ggplot2');install.packages('dplyr'); install.packages('scales')
install.packages('viridis')


library('ggvegan'); library('tidyverse'); 
library('MASS');library('ggplot2');  library('scales')
library('viridis')

setwd("C:/Users/Julia Michaels/Google Drive/Dissertation Chapter 2/Data/Chapter_2_Analysis/")



# calibration method ------------------------------------------------------


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

#Inundation differences in 2018 (All 17 pairs)
inundation<-read.csv("2018_Master_List.csv")
inundation1<-inundation%>% 
  filter(Pair.Paper.2 %in% c(1:17)) %>% 
  ggplot(aes(x=Treatment,y=Calibrated.Days))+
  geom_boxplot()
inundation1

library(dplyr)
#Inundation differences in 2018 (11 quadrat pairs only)
inundation2<-inundation %>% 
  filter(Pair.Paper.2 %in% c(1:17)) 
inundation2%>% 
  ggplot(aes(x=Treatment,y=Calibrated.Days))+
  geom_boxplot()


#Look at distribution of inundation timing by grazing
inundation3<-inundation %>% 
  filter(Mid.season.dry.period %in% c("Longdry", "Middry", "Shortdry"))



ggplot(data=inundation3, aes(Mid.season.dry.period, fill=Treatment))+
  geom_bar()+
  scale_fill_manual(values=c("maroon", "turquoise", "blue"))+
  labs(title="2018 Mid Season Dry Period by Grazing Treatment", y="Count", x="Mid season dry period")+
  theme(plot.title=element_text(size=30, hjust=.5))+
  theme(axis.title.x =element_text(size=30))+
  theme(axis.title.y =element_text(size=30))+
  theme(legend.title =element_text(size=30))+
  theme(legend.text =element_text(size=30))+
  theme(axis.text =element_text(size=30))

# Quadrats ----------------------------------------------------------------


#2018 Quadrats in Transition Zones
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



attempt1 <- lm(Calibrated.Total.Days~Grazing+Size+Catchment+SoilType, data=qdata) 
summary(attempt1)
hist(residuals(attempt1))

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

###################################Multivariate Analysis#############
#NMDS
qdata1<-qdata[,-c(1:15)] 

quadist<-vegdist(qdata1)
quad.mds<-isoMDS(quadist)
stressplot(quad.mds, quadist)

ordiplot(quad.mds, type='t')
quad.mds2<-metaMDS(qdata1, trace=FALSE)
quad.mds2
plot(quad.mds2, type='t')


###NDMS Plot in ggplot##


#calculate total cover
total_cover<-qdata1 %>%
  summarise_all(funs(sum))
total_cover<-gather(total_cover, "Species", "Cover")
total_cover<-as.data.frame(total_cover)

total_cover<-gather(total_cover, "Species", "Cover")
total_cover<-as.data.frame(total_cover)
# What level do we want to plot?
data_adonis = qdata

# Runs NMDS with 2 dimensions
nmds_run = metaMDS(data_adonis[,16:(ncol(data_adonis) - 1)],
                   try = 50,
                   trymax = 100,
                   trace = FALSE,
                   distance = "bray",
                   wascores = TRUE
)

# Sets the group variable to factor
data_adonis$group = as.factor(data_adonis$Grazing)

# Adds group column to dataframe of NMDS points
NMDS = bind_cols(data.frame(nmds_run$points), group = as.factor(c(data_adonis$group)))

# Calculates the midpoint of each group
NMDS.mean=aggregate(NMDS[,1:2], list(group=NMDS$group),mean)

# Calculating the shape of the ellipses
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100){
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

# Creating blank dataframe to store ellipses
df_ell <- data.frame()
for(g in levels(NMDS$group)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g,],
                                                   veganCovEllipse(cov.wt(cbind(MDS1,MDS2),
                                                                          wt=rep(1/length(MDS1),length(MDS1)))$cov,
                                                                   center=c(mean(MDS1),mean(MDS2)))))
                                ,group=g))
}

# Adding other details for facetting
NMDS = right_join(NMDS, bind_cols(group = as.character(as.numeric(data_adonis$group)), data_adonis[,2:4]),
                  by = "group")


NMDS.mean = inner_join(NMDS.mean, bind_cols(group = as.character(as.numeric(data_adonis$group)), data_adonis[,2:4]),
                       by = "group")
NMDS.mean = NMDS.mean[!duplicated(NMDS.mean[,2:5]),]

df_ell = right_join(df_ell, bind_cols(group = as.character(as.numeric(data_adonis$group)), data_adonis[,2:4]),
                    by = "group")


#Creating dataframe of species scores to use in plotting vectors:   

#  Variables here are:  

# * MDS1 (Axis 1 score)
#* MDS2 (Axis 2 score)
#* Spec (species name)
#* dist (total distance from the center)
#* rank (ranking based on distance)
#*cover (ranking based on total cover at the site over all 3 years)


for_scores <-  data_adonis[,16:(ncol(data_adonis) - 2)]
for_scores <- for_scores[,colSums(for_scores) / nrow(for_scores) > 2]

NMDS.spec = data.frame(nmds_run$species)
NMDS.spec$spec = rownames(NMDS.spec)
NMDS.spec$dist = apply(NMDS.spec[,1:2], 1, function(x) as.numeric(sqrt(sum(x^2))))
NMDS.spec$rank = rank(-NMDS.spec$dist)
NMDS.spec$cover=rank(-total_cover$Cover)


# Dataframe of species scores
NMDS.spec

maxcover = 10
maxrank=30


distance1<-ggplot(data = NMDS, aes(MDS1, MDS2)) + 
  geom_point(aes(color = Grazing)) +
  scale_shape(solid = FALSE) + 
  geom_path(data = df_ell, aes(x=MDS1, y=MDS2, group = Grazing), size=1, linetype=1, alpha = .5) +
  geom_text(data = NMDS.mean, aes(label = NMDS.mean$Grazing, fontface="bold"), color="#0066CC") + 
  #facet_wrap(~Year) +
  #geom_segment(data = NMDS.spec[NMDS.spec$rank <= maxrank,], 
  #             aes(x = 0, y = 0, xend = MDS1, yend = MDS2),
  #                arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_text(data = NMDS.spec[NMDS.spec$rank <= maxrank,], 
            aes(x = MDS1, y = MDS2, label = spec), size=4, alpha=0.4) +
  theme_bw()

distance1


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
































































###################################Multivariate Analysis, Quadrats separate#############
#NMDS
qdata1<-qdata[,-c(1:15)] 
qdata1<-qdata1[-99,]

quadist<-vegdist(qdata1)
quad.mds<-isoMDS(quadist)
stressplot(quad.mds, quadist)

ordiplot(quad.mds, type='t')
quad.mds2<-metaMDS(qdata1, trace=FALSE)
quad.mds2
plot(quad.mds2, type='t')


###NDMS Plot in ggplot##


#calculate total cover
total_cover<-qdata1 %>%
  summarise_all(funs(sum))
total_cover<-gather(total_cover, "Species", "Cover")
total_cover<-as.data.frame(total_cover)


# What level do we want to plot?
data_adonis = qdata

# Runs NMDS with 2 dimensions
nmds_run = metaMDS(data_adonis[,1:(ncol(data_adonis))],
                   try = 50,
                   trymax = 100,
                   trace = FALSE,
                   distance = "bray",
                   wascores = TRUE
)


# Sets the group variable to factor
data_adonis$group = as.factor(data_adonis$Grazing)

# Adds group column to dataframe of NMDS points
NMDS = bind_cols(data.frame(nmds_run$points), group = as.factor(c(data_adonis$group)))

# Calculates the midpoint of each group
NMDS.mean=aggregate(NMDS[,1:2], list(group=NMDS$group),mean)

# Calculating the shape of the ellipses
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100){
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

# Creating blank dataframe to store ellipses
df_ell <- data.frame()
for(g in levels(NMDS$group)){
  df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g,],
                                                   veganCovEllipse(cov.wt(cbind(MDS1,MDS2),
                                                                          wt=rep(1/length(MDS1),length(MDS1)))$cov,
                                                                   center=c(mean(MDS1),mean(MDS2)))))
                                ,group=g))
}

# Adding other details for facetting
NMDS = right_join(NMDS, bind_cols(group = as.character(as.numeric(data_adonis$group)), data_adonis[,2:4]),
                  by = "group")

NMDS.mean = inner_join(NMDS.mean, bind_cols(group = as.character(as.numeric(data_adonis$group)), data_adonis[,2:4]),
                       by = "group")
NMDS.mean = NMDS.mean[!duplicated(NMDS.mean[,2:5]),]

df_ell = right_join(df_ell, bind_cols(group = as.character(as.numeric(data_adonis$group)), data_adonis[,2:4]),
                    by = "group")


#Creating dataframe of species scores to use in plotting vectors:   

#  Variables here are:  

# * MDS1 (Axis 1 score)
#* MDS2 (Axis 2 score)
#* Spec (species name)
#* dist (total distance from the center)
#* rank (ranking based on distance)
#*cover (ranking based on total cover at the site over all 3 years)


for_scores <-  data_adonis[,16:(ncol(data_adonis) - 2)]
for_scores <- for_scores[,colSums(for_scores) / nrow(for_scores) > 2]

NMDS.spec = data.frame(nmds_run$species)
NMDS.spec$spec = rownames(NMDS.spec)
NMDS.spec$dist = apply(NMDS.spec[,1:2], 1, function(x) as.numeric(sqrt(sum(x^2))))
NMDS.spec$rank = rank(-NMDS.spec$dist)
NMDS.spec$cover=rank(-total_cover$Cover)


# Dataframe of species scores
NMDS.spec

maxcover = 10
maxrank=30


distance1<-ggplot(data = NMDS, aes(MDS1, MDS2)) + 
  geom_point(aes(color = Grazing)) +
  scale_shape(solid = FALSE) + 
  geom_path(data = df_ell, aes(x=MDS1, y=MDS2, group = Grazing), size=1, linetype=1, alpha = .5) +
  geom_text(data = NMDS.mean, aes(label = NMDS.mean$Grazing, fontface="bold"), color="#0066CC") + 
  #facet_wrap(~Year) +
  #geom_segment(data = NMDS.spec[NMDS.spec$rank <= maxrank,], 
  #             aes(x = 0, y = 0, xend = MDS1, yend = MDS2),
  #                arrow = arrow(length = unit(0.03, "npc"))) + 
  geom_text(data = NMDS.spec[NMDS.spec$rank <= maxrank,], 
            aes(x = MDS1, y = MDS2, label = spec), size=4, alpha=0.4) +
  theme_bw()

distance1


###fitting environmental variables####

env_quad1<-
  tibble(
    Pool.ID=qdata$Pool.ID,
    Grazing=qdata$Grazing,
    In_Days=qdata$Calibrated.Total.Days,
    HoofCount=qdata$Hoofprint,
    Soil=qdata$SoilType,
    Size=qdata$Size, 
    Depth=qdata$Depth,
    RDM=qdata$RDM,
    Catchment=qdata$Catchment
  )

ef<-envfit(quad.mds2, env_quad1, permu=999, na.rm=TRUE)

ef
plot(quad.mds2, display='sites')
plot(ef, p.max=0.1)

###CCA####
#install.packages('ggfortify'); 
library('ggfortify')
#install.packages('ggvegan', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('dplyr',  dependencies=TRUE, repos='http://cran.rstudio.com/')
#nstall.packages('tidyverse'); library('tidyverse')
#library('ggvegan')

quad<-cca(qdata1~In_Days+HoofCount+Grazing+Size+Soil+Depth, env_quad1)#will want to condition on year for transects
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
quad3<-cca(qdata1~Grazing, env_quad1)#Grazing alone
anova(quad3)
#can sub in grazing treatment for hoofprint
#Grazing has an effect even when when variation due to inundation days is removed
#butttt, the variables of hoofprint and days of inundation are linearly dependent
with(env_quad1, anova(quad, strata=In_Days))



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
            colour = 'grey66', size = 7) +
  geom_point(data = cbind(subset(fmod, Score == "sites"), Grazing = env_quad1$Grazing),
             aes(colour = Grazing), size = 4) +
  scale_colour_manual("Treatment", values=c("maroon", "turquoise", "blue")) +
  coord_fixed() +
  theme(legend.position = "top") +
  geom_text(data = subset(fmod, Score == "biplot")[c(1:2,5:8),], 
            aes(x=CCA1 * 3,y=CCA2 *3,label=c("Hydroperiod", "Hoofprints",  "Size", "Redding Soil", "Depth", "Variable Inundation")), size=7, colour = "gray24") + xlab("CCA1") + ylab("CCA2")+ 
  geom_text(data = subset(fmod, Score == "centroids")[4,], 
            aes(x=CCA1,y=CCA2,label="Corning Soil"), size=7, colour = "gray34") + xlab("CCA1") + ylab("CCA2")+
  geom_segment(data = subset(fmod, Score == "biplot")[c(1:2,5:8),],
               aes(x = 0, y = 0, xend = CCA1 * 3, yend = CCA2 * 3), arrow = arrow(length = unit(1/2, 'picas')), colour = "gray36", size = 2) +
  geom_segment(data = subset(fmod, Score == "centroids")[6,],
               aes(x = 0, y = 0, xend = CCA1, yend = CCA2), arrow = arrow(length = unit(1/2, 'picas')), colour = "gray36", size = 2) +
  theme(axis.title.x =element_text(size=20))+
  theme(axis.title.y =element_text(size=20))+
  theme(legend.title =element_text(size=20))+
  theme(legend.text =element_text(size=20))+
  theme(axis.text =element_text(size=20))+
  xlim(-2.75, 2.75)+
  ylim(-2.75,2.75)

p4





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


