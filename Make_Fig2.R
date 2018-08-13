library(ggplot2)
require(ggpubr)

mod1<-read.table("mod1_all.csv",header=T,sep=",")
mod1_v2<-read.table("mod1_v2_all.csv",header=T,sep=",")

mod1<-rbind(mod1,mod1_v2)

mod1$hyst_diffeqm<-ifelse(((mod1$critC_diffeqm<900)&(mod1$critM_diffeqm<900)),1,0)
mod1$hyst_diffeqm<-ifelse((mod1$critC_diffeqm<mod1$critM_diffeqm)&(mod1$hyst_diffeqm==1),1,0)
mod1$hyst_unstable<-ifelse(((mod1$critC_unstable<900)&(mod1$critM_unstable<900)),1,0)
mod1$hyst_Cmax<-ifelse((mod1$critC_Cmax < mod1$critM_Cmax),1,0)

mod1$delta_dV_unstable<-ifelse(mod1$hyst_unstable==1,mod1$critM_unstable-mod1$critC_unstable,999)
mod1$delta_dV_diffeqm<-ifelse(mod1$hyst_diffeqm==1,mod1$critM_diffeqm-mod1$critC_diffeqm,999)
mod1$delta_dV_Cmax<-mod1$critM_Cmax - mod1$critC_Cmax

mod1$new_hyst<-mod1$hyst_diffeqm
mod1$new_delta_dV<-ifelse(mod1$new_hyst==1,mod1$delta_dV_diffeqm,mod1$delta_dV_Cmax)

#to calculate fraction of cases with hystersis:
sum(mod1$new_hyst)/nrow(mod1)
#to calculate maximum delta_dV
max(mod1$new_delta_dV)

#to calculate fraction of cases with abrupt phase shifts:
abrupt<-mod1[mod1$new_delta_dV<0.5,]
abrupt<-abrupt[abrupt$new_delta_dV> -0.5,]
nrow(abrupt)/nrow(mod1)


################
mod2<-read.table("mod2_all.csv",header=T,sep=",")
mod2_v2<-read.table("mod2_v2_all.csv",header=T,sep=",")

mod2<-rbind(mod2,mod2_v2)

mod2$hyst_diffeqm<-ifelse(((mod2$critC_diffeqm<900)&(mod2$critM_diffeqm<900)),1,0)
mod2$hyst_diffeqm<-ifelse((mod2$critC_diffeqm<mod2$critM_diffeqm)&(mod2$hyst_diffeqm==1),1,0)
mod2$hyst_unstable<-ifelse(((mod2$critC_unstable<900)&(mod2$critM_unstable<900)),1,0)
mod2$hyst_Cmax<-ifelse((mod2$critC_Cmax < mod2$critM_Cmax),1,0)

mod2$delta_dV_unstable<-ifelse(mod2$hyst_unstable==1,mod2$critM_unstable-mod2$critC_unstable,999)
mod2$delta_dV_diffeqm<-ifelse(mod2$hyst_diffeqm==1,mod2$critM_diffeqm-mod2$critC_diffeqm,999)
mod2$delta_dV_Cmax<-mod2$critM_Cmax - mod2$critC_Cmax

mod2$new_hyst<-mod2$hyst_diffeqm
mod2$new_delta_dV<-ifelse(mod2$new_hyst==1,mod2$delta_dV_diffeqm,mod2$delta_dV_Cmax)


#to calculate fraction of cases with hystersis:
sum(mod2$new_hyst)/nrow(mod2)
#to calculate maximum delta_dV
max(mod1$new_delta_dV)

#to calculate fraction of cases with abrupt phase shifts:
abrupt<-mod1[mod1$new_delta_dV<0.5,]
abrupt<-abrupt[abrupt$new_delta_dV> -0.5,]
nrow(abrupt)/nrow(mod1)





####################

#install.packages("ggpubr")


tiff(file = "Fig2.tiff", width = 1800, height = 1800, units = "px", res = 300)
par(mfrow=c(2,1))
par(mar=c(5.1,4.5,2.5,2.1))
fig2a<-ggplot(data=mod1, aes(x=new_delta_dV, fill=as.factor(new_hyst))) +
  geom_histogram(aes(y=..count../sum(..count..)),
  				 breaks=seq(-12, 12, by = 0.5), 
                 col="black", 
                 alpha = .8) + 
  labs(x=expression(Delta*d["V"]), y="fraction of cases") + 
  xlim(c(-12,12)) +
  coord_cartesian(ylim=c(0, 0.65)) + 
    scale_y_continuous(breaks=seq(0, 0.65, 0.25))+
  scale_fill_manual(values=c("lightgray", "red"))+
  geom_vline(aes(xintercept=0),
             color="blue", linetype="solid", size=1)+
  theme_bw(base_size = 12, base_family = "Helvetica") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) + theme(legend.position="none")+
  annotate("text", x = 7, y = 0.6, label = "hysteresis",color="red") +
  annotate("text", x = -7, y = 0.6, label = "no hysteresis",color="black") +
  annotate("text", x = -12, y = 0.6, label = "(a)",size = 5, color="black")




fig2b<-ggplot(data=mod2, aes(x=new_delta_dV, fill=as.factor(new_hyst))) +
  geom_histogram(aes(y=..count../sum(..count..)),
  				 breaks=seq(-12, 12, by = 0.5), 
col="black",
alpha = .8) +
labs(x=expression(Delta*d["V"]), y="fraction of cases") +
xlim(c(-12,12)) +
scale_fill_manual(values=c("lightgray", "red"))+
geom_vline(aes(xintercept=0),
color="blue", linetype="solid", size=1)+
theme_bw(base_size = 12, base_family = "Helvetica") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black")) + theme(legend.position="none")+
annotate("text", x = 7, y = 0.12, label = "hysteresis",color="red") +
annotate("text", x = -7, y = 0.12, label = "no hysteresis",color="black") +
annotate("text", x = 11.75, y = 0.08, label = "*",size = 10, color="black") +
annotate("text", x = -12, y = 0.12, label = "(b)",size = 5, color="black")

ggarrange(fig2a,fig2b, 
          ncol = 1, nrow = 2)

dev.off()


