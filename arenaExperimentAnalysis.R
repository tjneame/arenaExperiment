################################\
#Arena experiment analysis
#TN and SR 2023
################################\

#Set up-------------------------
################################\
library(tidyverse)
library(mgcv)
library(googlesheets4)
library(DHARMa)

#read in data
aeDat<-read_sheet("https://docs.google.com/spreadsheets/d/1D2S6doOtfIzx281d_t6tGhyoMD6H7NlIElrQOyzsInU/edit?usp=sharing")
write_csv(aeDat, "arenaExperimentData.csv")
aeDat<-read_csv("arenaExperimentData.csv")

#make some characters into factors
aeDat<-aeDat%>%
  mutate(startDirection=as_factor(startDirection))%>%
  mutate(initialTwo=as_factor(initialTwo))%>%
  mutate(catSize=ordered(as_factor(catSize)))%>%
  mutate(catSize=fct_relevel(catSize,c("SS","SM","LM","LL")))%>%
  mutate(catSize=as.numeric(catSize))

#add some new columns and change some old ones
aeDat<-aeDat%>%
  mutate(startDirection=case_when(startDirection=='N'~0, 
                                  startDirection=='S'~180, 
                                  startDirection=='E'~90, 
                                  startDirection=='W'~270)) %>%
  mutate(catDeg=case_when(catSize==1~225, 
                          catSize==2~135,
                          catSize==3~45,
                          catSize==4~315)) %>%
  mutate(difDeg=startDirection-catDeg) %>%
  mutate(difDeg=as_factor(case_when(difDeg==-315~45, 
                          difDeg==-225~135, 
                          difDeg==225~135, 
                          difDeg==-135~135, 
                          difDeg==-45~45, 
                          difDeg==135~135,
                          difDeg==45~45)))
  
  
#Model the data-------------------
##################################\

aeForm1<-as.formula(catSize~s(elytraLength))
aeGAM1<-gam(aeForm1,
            family = ocat(R=4),
            data = aeDat,
            method = 'REML')

aeForm2<-as.formula(catSize~s(elytraLength)+difDeg)
aeGAM2<-gam(aeForm2,
            family = ocat(R=4),
            data = aeDat,
            method = 'REML')

#Test models ----------------------------------------------
##########################################################\
#Check that the models fit properly
gam.check(aeGAM1) #a little weird, try something else
plot(simulateResiduals(aeGAM1))#That looks better

gam.check(aeGAM2) #a little weird, try something else
plot(simulateResiduals(aeGAM2))#That looks better

#Let's look at the summaries
summary(aeGAM1)
#elytra length is minimally significant for the size of caterpillar that got eaten
summary(aeGAM2)
#elytra length is minimally significant for the size of caterpillar that got eaten
#AND the direction the beetle was facing did not affect the size of caterpillar that was eaten

AIC(aeGAM1, aeGAM2)
#aeGAM1 has a lower AIC, 
#and considering that the direction the beetle faced had no significant effect I will move forward with aeGAM1


#Visualize ------------------------------------------------
#Written by Sam Robinson
###########################################################\

#aeGAM1
expand.grid(elytraLength=seq(7.7,10.3,0.1)) %>%
  bind_cols(.,predict(aeGAM1,newdata=.,type='response')) %>%
  setNames(c('elytraLength','SS','SM','LM','LL')) %>%
  pivot_longer(c(-elytraLength),names_to = 'size',values_to='prob') %>%
  ggplot(aes(x=elytraLength,y=prob,col=size))+geom_line()

expand.grid(elytraLength=seq(7.6,10.3,0.1)) %>%
  cbind(predict(aeGAM1,newdata=.,type='response',se=TRUE)) %>%
  setNames(c('elytraLength','SS','SM','LM','LL',
             'SS_se','SM_se','LM_se','LL_se')) %>%
  pivot_longer(c(-elytraLength),names_to = 'size',values_to='prob') %>%
  mutate(stat=ifelse(grepl('_se',size),'SE','Mean')) %>%
  mutate(size=gsub('_se','',size)) %>%
  pivot_wider(names_from=stat,values_from=prob) %>%
  mutate(upr=Mean+SE*1.96,lwr=Mean-SE*1.96) %>%
  ggplot(aes(x=elytraLength))+
  geom_ribbon(aes(ymax=upr,ymin=lwr,fill=size),alpha=0.2)+
  geom_line(aes(y=Mean,col=size), linewidth=1.25, alpha = 0.8)+
  xlab("Elytra Length (mm)")+
  ylab("Mean Predation Probability")+
  geom_rug(data=aeDat, aes(x=elytraLength), alpha=0.3)+
  scale_colour_viridis_d(aesthetics = c('fill','colour'), option = "D", begin = 0.25, end = 0.85, 
                      labels=c("Largest", "Large", "Small", "Smallest"),
                      name='Size of Prey')+
  theme_bw()
ggsave('./figures/AEResults.png', width=10, height=5)
#aeGAM2
expand.grid(elytraLength=seq(7.7,10.3,0.1),difDeg='45') %>%
  bind_cols(.,predict(aeGAM2,newdata=.,type='response')) %>%
  setNames(c('elytraLength','difDeg','SS','SM','LM','LL')) %>%
  pivot_longer(c(-elytraLength,-difDeg),names_to = 'size',values_to='prob') %>%
  ggplot(aes(x=elytraLength,y=prob,col=size))+geom_line()

expand.grid(elytraLength=seq(7.6,10.3,0.1),difDeg='45') %>%
  cbind(predict(aeGAM2,newdata=.,type='response',se=TRUE)) %>%
  setNames(c('elytraLength','difDeg','SS','SM','LM','LL',
             'SS_se','SM_se','LM_se','LL_se')) %>%
  pivot_longer(c(-elytraLength,-difDeg),names_to = 'size',values_to='prob') %>%
  mutate(stat=ifelse(grepl('_se',size),'SE','Mean')) %>%
  mutate(size=gsub('_se','',size)) %>%
  pivot_wider(names_from=stat,values_from=prob) %>%
  mutate(upr=Mean+SE*1.96,lwr=Mean-SE*1.96) %>%
  ggplot(aes(x=elytraLength))+
  geom_ribbon(aes(ymax=upr,ymin=lwr,fill=size),alpha=0.3)+
  geom_line(aes(y=Mean,col=size))+
  geom_rug(data=aeDat, aes(x=elytraLength), alpha=0.3)+
  scale_colour_brewer(aesthetics = c('fill','colour'),palette='Dark2')
