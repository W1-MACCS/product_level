DATA = output
#output = subset(output, pch>0)
options(scipen=999)


#DEGREE OF RESOURCE SHARING
panelglobal = data.frame(0)
DATA = output
panelglobal[1,1] = mean(DATA$EUCD)
panelglobal[2,1] = mean(DATA$acc)
panelglobal[3,1] = mean(DATA$mape_oc)
panelglobal[4,1] = mean(DATA$mape_uc)
panelglobal[5,1] = mean(DATA$error_disp)
panelglobal[6,1] = mean(DATA$UC_share)


panelA = data.frame(0)
DATA = subset(output, DENS == 0.25)
panelA[1,1] = mean(DATA$EUCD)
panelA[2,1] = mean(DATA$acc)
panelA[3,1] = mean(DATA$mape_oc)
panelA[4,1] = mean(DATA$mape_uc)
panelA[5,1] = mean(DATA$error_disp)
panelA[6,1] = mean(DATA$UC_share)

panelB = data.frame(0)
DATA = subset(output, DENS == 0.5)
panelB[1,1] = mean(DATA$EUCD)
panelB[2,1] = mean(DATA$acc)
panelB[3,1] = mean(DATA$mape_oc)
panelB[4,1] = mean(DATA$mape_uc)
panelB[5,1] = mean(DATA$error_disp)
panelB[6,1] = mean(DATA$UC_share)



panelC = data.frame(0)
DATA = subset(output, DENS == 0.75)
panelC[1,1] = mean(DATA$EUCD)
panelC[2,1] = mean(DATA$acc)
panelC[3,1] = mean(DATA$mape_oc)
panelC[4,1] = mean(DATA$mape_uc)
panelC[5,1] = mean(DATA$error_disp)
panelC[6,1] = mean(DATA$UC_share)


PANEL_A = cbind(panelglobal,panelA,panelB,panelC)





#COSTING SYSTEM REFINEMENT
panelglobal = data.frame(0)
DATA = output
panelglobal[1,1] = mean(DATA$EUCD)
panelglobal[2,1] = mean(DATA$acc)
panelglobal[3,1] = mean(DATA$mape_oc)
panelglobal[4,1] = mean(DATA$mape_uc)
panelglobal[5,1] = mean(DATA$error_disp)
panelglobal[6,1] = mean(DATA$UC_share)


panelA = data.frame(0)
DATA = subset(output, ACP == 1)
panelA[1,1] = mean(DATA$EUCD)
panelA[2,1] = mean(DATA$acc)
panelA[3,1] = mean(DATA$mape_oc)
panelA[4,1] = mean(DATA$mape_uc)
panelA[5,1] = mean(DATA$error_disp)
panelA[6,1] = mean(DATA$UC_share)

panelB = data.frame(0)
DATA = subset(output, ACP == 4)
panelB[1,1] = mean(DATA$EUCD)
panelB[2,1] = mean(DATA$acc)
panelB[3,1] = mean(DATA$mape_oc)
panelB[4,1] = mean(DATA$mape_uc)
panelB[5,1] = mean(DATA$error_disp)
panelB[6,1] = mean(DATA$UC_share)


panelC = data.frame(0)
DATA = subset(output, ACP == 10)
panelC[1,1] = mean(DATA$EUCD)
panelC[2,1] = mean(DATA$acc)
panelC[3,1] = mean(DATA$mape_oc)
panelC[4,1] = mean(DATA$mape_uc)
panelC[5,1] = mean(DATA$error_disp)
panelC[6,1] = mean(DATA$UC_share)

PANEL_B = cbind(panelglobal,panelA,panelB,panelC)






###SINGLE PRODUCT CHARACTERISTICS
DATA = subset(output, !is.na(reported_unit_cost_share))
DATA$unit_costs = (DATA$PCB - DATA$batch_costs)/DATA$PCB

mxq_data = summary(DATA$MXQ)
res_numb_data = summary(DATA$res_numb)
pcb_data = summary(DATA$pcb)
pch_data = summary(DATA$pch)
ulc_data = summary(DATA$reported_unit_cost_share)
driver_numb_data = summary(DATA$driver_numb)
consBig_data = summary(DATA$cons_bigDriver)

PANEL_C = rbind(mxq_data,res_numb_data,pcb_data,pch_data,ulc_data,driver_numb_data,consBig_data)



DATA = output
DATA = subset(DATA, pch>0)
DATA$unit_cost_share = (DATA$PCB - DATA$batch_costs)/DATA$PCB
DATA$unit_costs = (DATA$PCB - DATA$batch_costs)/DATA$MXQ
DATA$misreporting = DATA$unit_costs - DATA$reported_unit_costs
#DATA$reported_unit_cost_share = (DATA$pch-DATA$reported_unit_costs)/DATA$pch

comparison = data.frame(0)

undercosted1 = subset(DATA, pe <=-0.2)
comparison[1,1] = mean(undercosted1$pch)
comparison[1,2] = mean(undercosted1$unit_costs)
comparison[1,3] = mean(undercosted1$MXQ)
comparison[1,4] = mean(undercosted1$res_numb)
comparison[1,5] = mean(undercosted1$driver_numb)
comparison[1,6] = mean(undercosted1$cons_bigDriver)
comparison[1,7] = mean(undercosted1$unit_cost_share)
comparison[1,8] = mean(undercosted1$pcb)
comparison[1,9] = mean(undercosted1$reported_unit_cost_share)

undercosted2 = subset(DATA, pe <=-0.05)
comparison[2,1] = mean(undercosted2$pch)
comparison[2,2] = mean(undercosted2$unit_costs)
comparison[2,3] = mean(undercosted2$MXQ)
comparison[2,4] = mean(undercosted2$res_numb)
comparison[2,5] = mean(undercosted2$driver_numb)
comparison[2,6] = mean(undercosted2$cons_bigDriver)
comparison[2,7] = mean(undercosted2$unit_cost_share)
comparison[2,8] = mean(undercosted2$pcb)
comparison[2,9] = mean(undercosted2$reported_unit_cost_share)

no_error = subset(DATA, pe >=-0.05 & pe<=0.05)
comparison[3,1] = mean(no_error$pch)
comparison[3,2] = mean(no_error$unit_costs)
comparison[3,3] = mean(no_error$MXQ)
comparison[3,4] = mean(no_error$res_numb)
comparison[3,5] = mean(no_error$driver_numb)
comparison[3,6] = mean(no_error$cons_bigDriver)
comparison[3,7] = mean(no_error$unit_cost_share)
comparison[3,8] = mean(no_error$pcb)
comparison[3,9] = mean(no_error$reported_unit_cost_share)


overcosted = subset(DATA, pe >=0.05)
comparison[4,1] = mean(overcosted$pch)
comparison[4,2] = mean(overcosted$unit_costs)
comparison[4,3] = mean(overcosted$MXQ)
comparison[4,4] = mean(overcosted$res_numb)
comparison[4,5] = mean(overcosted$driver_numb)
comparison[4,6] = mean(overcosted$cons_bigDriver)
comparison[4,7] = mean(overcosted$unit_cost_share)
comparison[4,8] = mean(overcosted$pcb)
comparison[4,9] = mean(overcosted$reported_unit_cost_share)


overcosted = subset(DATA, pe >=0.2)
comparison[5,1] = mean(overcosted$pch)
comparison[5,2] = mean(overcosted$unit_costs)
comparison[5,3] = mean(overcosted$MXQ)
comparison[5,4] = mean(overcosted$res_numb)
comparison[5,5] = mean(overcosted$driver_numb)
comparison[5,6] = mean(overcosted$cons_bigDriver)
comparison[5,7] = mean(overcosted$unit_cost_share)
comparison[5,8] = mean(overcosted$pcb)
comparison[5,9] = mean(overcosted$reported_unit_cost_share)

overall = DATA
comparison[6,1] = mean(overall$pch)
comparison[6,2] = mean(overall$unit_costs)
comparison[6,3] = mean(overall$MXQ)
comparison[6,4] = mean(overall$res_numb)
comparison[6,5] = mean(overall$driver_numb)
comparison[6,6] = mean(overall$cons_bigDriver)
comparison[6,7] = mean(overall$unit_cost_share)
comparison[6,8] = mean(overall$pcb)
comparison[6,9] = mean(overall$reported_unit_cost_share)

colnames(comparison) =c("pch",'unit_costs',"MXQ","res_numb","driver_numb","cons_bigDriver","unit_cost_share","pcb","reported_unit_cost_share")


comparison = t(comparison)
#comparison = comparison[-2,]

comparison = comparison[c(3,4,8,1,7,5,6,2),]




##Correlation Table
DATA=output
DATA = subset(DATA, pch>0)
cor_data = data.frame(DATA$MXQ,DATA$res_numb,DATA$pch, DATA$reported_unit_cost_share,DATA$driver_numb,DATA$cons_bigDriver,DATA$pe)
Cor_table = round(cor(cor_data),2)



DATA = output
DATA$unit_costs = (DATA$PCB - DATA$batch_costs)/DATA$MXQ
DATA$misreporting = DATA$reported_unit_costs-DATA$unit_costs
summary(DATA$pch)

hist(DATA$misreporting)

scatter.smooth(((DATA$pch-DATA$reported_unit_costs)/DATA$pch),DATA$pe)


length(which(DATA$pch==0))



###Share of reported unit costs and EUCD
library(ggplot2)

output$unit_cost_share= (output$PCB-output$batch_costs)/output$PCB
DATA = output
DATA$class = 0
DATA$class[which(DATA$unit_cost_share>=mean(DATA$unit_cost_share))]<-"HIGH"
DATA$class[which(DATA$unit_cost_share<mean(DATA$unit_cost_share))]<-"LOW"
#DATA = subset(output, pch_rank>25)
#DATA = output
DATA$CSD <-1
#DATA = subset(DATA, pch>0)
DATA$unit_costs = (DATA$PCB - DATA$batch_costs)/DATA$MXQ
DATA$unit_cost_share = (DATA$PCB - DATA$batch_costs)/DATA$PCB
#DATA$reported_unit_cost_share = (DATA$pch-DATA$reported_unit_costs)/DATA$pch
DATA$misreporting = DATA$unit_costs - DATA$reported_unit_costs
library(dplyr)
DATA = DATA %>% mutate(new_bin = cut(reported_unit_cost_share, breaks=c(0,0.1, 0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))
DATA$new_bin = as.numeric(DATA$new_bin)/10
DATA$DENS = as.factor(DATA$DENS)
DATA$ACP = as.factor(DATA$ACP)
data_agg = aggregate(.~new_bin+CSD+class,data =  DATA,FUN = mean)
ggplot(data_agg, aes(y= pe,x = new_bin, linetype = class))+geom_line()+geom_point()+theme_classic()+
  geom_hline(yintercept =0)#+ylim(0.25,0.6)



###Number of unit-level cost drivers (share)
library(ggplot2)
output$unit_cost_share= (output$PCB-output$batch_costs)/output$PCB
DATA =output
DATA$unit_costs = (DATA$PCB - DATA$batch_costs)/DATA$MXQ
DATA$unit_cost_share = (DATA$PCB - DATA$batch_costs)/DATA$PCB
DATA$class = 0
DATA$class[which(DATA$MXQ>mean(DATA$MXQ))]<-"HIGH UNIT-LEVEL COST SHARE"
DATA$class[which(DATA$MXQ<mean(DATA$MXQ))]<-"LOW UNIT-LEVEL COST SHARE"
#DATA = output
DATA$CSD <-1

#DATA$reported_unit_cost_share = (DATA$pch-DATA$reported_unit_costs)/DATA$pch
DATA$DENS = as.factor(DATA$DENS)
DATA$ACP = as.factor(DATA$ACP)
#DATA$uldriver_share = as.factor(DATA$uldriver_share)
data_agg = aggregate(.~uldriver_share+CSD+class,data =  DATA,FUN = mean)
ggplot(data_agg, aes(y= pe,x = uldriver_share,linetype =class))+geom_line()+geom_point(aes(shape=class))+theme_classic()+
  geom_hline(yintercept = 0)+theme(legend.position="bottom")




write.csv(DATA, "DATA.csv")

###Misreported unit costs and EUCD
DATA = output
DATA$CSD <-1
DATA = subset(DATA, pch>0)
DATA$unit_cost_share = (DATA$PCB - DATA$batch_costs)/DATA$PCB
DATA$unit_costs = (DATA$PCB - DATA$batch_costs)/DATA$MXQ
DATA$misreporting = DATA$reported_unit_costs/DATA$unit_costs
DATA$misreporting[which(is.na(DATA$misreporting))]<-10
library(dplyr)
DATA = DATA %>% mutate(new_bin = cut(misreporting, breaks=c(-0.25,0,0.25,0.5,0.75,1,1.25,1.5,1.75,2,10)))
DATA$new_bin = as.numeric(DATA$new_bin)

data_agg = aggregate(.~new_bin+CSD,data =  DATA,FUN = mean)
ggplot(data_agg, aes(y= EUCD,x = new_bin))+geom_point()#+ylim(0,2)




#######################################################################################################

output$id = paste0(output$DENS,output$ACP)
ids = unique(output$id)

output1 = subset(output)


mxq_uc = c()
mxq_oc = c()

pch_uc = c()
pch_oc = c()

numb_res_oc = c()
numb_res_uc = c()

reported_unit_cost_share_uc = c()
reported_unit_cost_share_oc = c()

consBigDriver_uc = c()
consBigDriver_oc = c()

numb_driver_oc = c()
numb_driver_uc = c()

for(i in ids){
  
  DATA = output1[which(output1$id==i),]
  
  #mxq undercosting
  mxq_uc[i] = wilcox.test(DATA$MXQ[which(DATA$pe< -0.05)], DATA$MXQ[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  #mxq overcosting
  mxq_oc[i] = wilcox.test(DATA$MXQ[which(DATA$pe>0.05)], DATA$MXQ[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  
  #pch undercosting
  pch_uc[i] = wilcox.test(DATA$pch[which(DATA$pe< -0.05)], DATA$pch[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  #pch overcosting
  pch_oc[i] = wilcox.test(DATA$pch[which(DATA$pe>0.05)], DATA$pch[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  
  
  #numb_res undercosting
  numb_res_uc[i] = wilcox.test(DATA$NUMB_RES[which(DATA$pe< -0.05)], DATA$NUMB_RES[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  #numb_res overcosting
  numb_res_oc[i] = wilcox.test(DATA$NUMB_RES[which(DATA$pe>0.05)], DATA$NUMB_RES[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  
  
  #reported_unit_cost_share undercosting
  reported_unit_cost_share_uc[i] = wilcox.test(DATA$reported_unit_cost_share[which(DATA$pe< -0.05)], DATA$reported_unit_cost_share[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  #reported_unit_cost_share overcosting
  reported_unit_cost_share_oc[i] = wilcox.test(DATA$reported_unit_cost_share[which(DATA$pe>0.05)], DATA$reported_unit_cost_share[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  
  
  #driver_numb undercosting
  numb_driver_uc[i] = wilcox.test(DATA$driver_numb[which(DATA$pe< -0.05)], DATA$driver_numb[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  #driver_numb overcosting
  numb_driver_oc[i] = wilcox.test(DATA$driver_numb[which(DATA$pe>0.05)], DATA$driver_numb[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  
  #consBigDriver undercosting
  consBigDriver_uc[i] = wilcox.test(DATA$cons_bigDriver[which(DATA$pe< -0.05)], DATA$cons_bigDriver[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  #consBigDriver overcosting
  consBigDriver_oc[i] = wilcox.test(DATA$cons_bigDriver[which(DATA$pe>0.05)], DATA$cons_bigDriver[which(DATA$pe<0.05 & DATA$pe> -0.05)], exact = FALSE, correct = TRUE)$p.value
  
  print(i)
  
}

#output = subset(output, pch>0)
options(scipen=999)


median(mxq_oc)
median(mxq_uc)
median(pch_uc)
median(pch_oc)

mean(numb_res_oc)
mean(numb_res_uc)

median(reported_unit_cost_share_uc)
median(reported_unit_cost_share_oc)

median(numb_driver_oc[!is.na(numb_driver_oc)])
median(numb_driver_uc[!is.na(numb_driver_uc)])

median(consBigDriver_uc)
median(consBigDriver_oc)



####Boxplots######################################################################
#MXQ
library(ggplot2)
DATA = output
DATA$errortype = 0
DATA$errortype[which(DATA$pe< -0.05)]<-"UC"
DATA$errortype[which(DATA$pe> 0.05)]<-"OC"
DATA$errortype = factor(DATA$errortype,levels = c("UC",0,"OC"))
ggplot(data = DATA, aes(x = errortype, y =MXQ))+geom_boxplot(outlier.shape = NA)+theme_classic()+coord_cartesian(ylim = c(0, 100))


#pch
library(ggplot2)
DATA = output
DATA$errortype = 0
DATA$errortype[which(DATA$pe< -0.05)]<-"UC"
DATA$errortype[which(DATA$pe> 0.05)]<-"OC"
DATA$errortype = factor(DATA$errortype,levels = c("UC",0,"OC"))
ggplot(data = DATA, aes(x = errortype, y =pch))+geom_boxplot(outlier.shape = NA)+theme_classic()+coord_cartesian(ylim = c(0, 5000))


#ULCostShare
library(ggplot2)
DATA = output
DATA$errortype = 0
DATA$errortype[which(DATA$pe< -0.05)]<-"UC"
DATA$errortype[which(DATA$pe> 0.05)]<-"OC"
DATA$errortype = factor(DATA$errortype,levels = c("UC",0,"OC"))
ggplot(data = DATA, aes(x = errortype, y =reported_unit_cost_share))+geom_boxplot(outlier.shape = NA)+theme_classic()+coord_cartesian(ylim = c(0, 2))



#NumBRES
library(ggplot2)
DATA = output
DATA$errortype = 0
DATA$errortype[which(DATA$pe< -0.05)]<-"UC"
DATA$errortype[which(DATA$pe> 0.05)]<-"OC"
DATA$errortype = factor(DATA$errortype,levels = c("UC",0,"OC"))
ggplot(data = DATA, aes(x = errortype, y =res_numb))+geom_boxplot(outlier.shape = NA)+theme_classic()+coord_cartesian(ylim = c(0, 2))



#consBigDriver
library(ggplot2)
DATA = output
DATA$errortype = 0
DATA$errortype[which(DATA$pe< -0.05)]<-"UC"
DATA$errortype[which(DATA$pe> 0.05)]<-"OC"
DATA$errortype = factor(DATA$errortype,levels = c("UC",0,"OC"))
ggplot(data = DATA, aes(x = errortype, y =cons_bigDriver))+geom_boxplot(outlier.shape = NA)+theme_classic()+coord_cartesian(ylim = c(0, 0.1))




#NumBDriver
library(ggplot2)
DATA = output
DATA$errortype = 0
DATA$errortype[which(DATA$pe< -0.05)]<-"UC"
DATA$errortype[which(DATA$pe> 0.05)]<-"OC"
DATA$errortype = factor(DATA$errortype,levels = c("UC",0,"OC"))
ggplot(data = DATA, aes(x = errortype, y =driver_numb))+geom_boxplot(outlier.shape = NA)+theme_classic()+coord_cartesian(ylim = c(0, 2))



#####################################################################################################
DATA = output

nrow(subset(DATA, ape>0.05 & MXQ_rank>45))/nrow(subset(DATA,MXQ_rank>45))
nrow(subset(DATA, ape>0.05 & MXQ_rank<5))/nrow(subset(DATA,MXQ_rank<5))

nrow(subset(DATA, ape>0.05 & res_numb_rank>45))/nrow(subset(DATA,res_numb_rank>45))
nrow(subset(DATA, ape>0.05 & res_numb_rank<5))/nrow(subset(DATA,res_numb_rank<5))


nrow(subset(DATA, ape>0.05 & pch_rank>45))/nrow(subset(DATA,pch_rank>45))
nrow(subset(DATA, ape>0.05 & pch_rank<5))/nrow(subset(DATA,pch_rank<5))

nrow(subset(DATA, ape>0.05 & reported_unit_cost_share_rank>45))/nrow(subset(DATA,reported_unit_cost_share_rank>45))
nrow(subset(DATA, ape>0.05 & reported_unit_cost_share_rank<5))/nrow(subset(DATA,reported_unit_cost_share_rank<5))

nrow(subset(DATA, ape>0.05 & driver_numb_rank>45))/nrow(subset(DATA,driver_numb_rank>45))
nrow(subset(DATA, ape>0.05 & driver_numb_rank<5))/nrow(subset(DATA,driver_numb_rank<5))

nrow(subset(DATA, ape>0.05 & cons_bigDriver_rank>45))/nrow(subset(DATA,cons_bigDriver_rank>45))
nrow(subset(DATA, ape>0.05 & cons_bigDriver_rank<5))/nrow(subset(DATA,cons_bigDriver_rank<5))






nrow(subset(DATA, pe>0.05 & MXQ_rank>45))/nrow(subset(DATA,MXQ_rank>45))
nrow(subset(DATA, pe< -0.05 & MXQ_rank<5))/nrow(subset(DATA,MXQ_rank<5))

nrow(subset(DATA, pe>0.05 & res_numb_rank>45))/nrow(subset(DATA,res_numb_rank>45))
nrow(subset(DATA, pe< -0.05 & res_numb_rank<5))/nrow(subset(DATA,res_numb_rank<5))

nrow(subset(DATA, pe>0.05 & pch_rank>45))/nrow(subset(DATA,pch_rank>45))
nrow(subset(DATA, pe< -0.05 & pch_rank<5))/nrow(subset(DATA,pch_rank<5))

nrow(subset(DATA, pe>0.05 & reported_unit_cost_share_rank>45))/nrow(subset(DATA,reported_unit_cost_share_rank>45))
nrow(subset(DATA, pe< -0.05 & reported_unit_cost_share_rank<5))/nrow(subset(DATA,reported_unit_cost_share_rank<5))


nrow(subset(DATA, pe>0.05 & driver_numb_rank>45))/nrow(subset(DATA,driver_numb_rank>45))
nrow(subset(DATA, pe< -0.05 & driver_numb_rank<5))/nrow(subset(DATA,driver_numb_rank<5))

nrow(subset(DATA, pe>0.05 & cons_bigDriver_rank>45))/nrow(subset(DATA,cons_bigDriver_rank>45))
nrow(subset(DATA, pe< -0.05 & cons_bigDriver_rank<5))/nrow(subset(DATA,cons_bigDriver_rank<5))


nrow(subset(DATA,pe< -0.05))/nrow(DATA)

nrow(subset(DATA,pe> 0.05))/nrow(DATA)


