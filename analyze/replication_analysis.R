

ANAND = read.csv("data/ORIGINAL.csv", sep =";")
REPLICATION = read.csv("data/REPLICATION.csv", sep= ";")
#ANAND$DISP1[which(ANAND$DISP1 ==5)]<-6
REPLICATION = REPLICATION[which(!is.na(REPLICATION$MAPE)),]
REPLICATION$BE_AB = REPLICATION$UC5-REPLICATION$OC5


#=======================================Distributional equivalence=====================================####

REPLICATION$unique_setting = paste(REPLICATION$PACP,REPLICATION$CP,REPLICATION$DISP1,REPLICATION$DISP2_CLASS,REPLICATION$DENS_CLASS,REPLICATION$COR1_CLASS,REPLICATION$COR2_CLASS,REPLICATION$Q_VAR, sep = "-")
ANAND$unique_setting = paste(ANAND$PACP,ANAND$ACP,ANAND$DISP1,ANAND$DISP2_CLASS,ANAND$DENS_CLASS,ANAND$COR1_CLASS,ANAND$COR2_CLASS,ANAND$Q_VAR,sep="-")

rep = sort(unique(REPLICATION$unique_setting))
anand = sort(unique(ANAND$unique_setting))

setdiff(anand,rep)
settings = sort(unique(REPLICATION$unique_setting))

anand = data.frame(ANAND$unique_setting,ANAND$MPE)
rep = data.frame(REPLICATION$unique_setting,REPLICATION$MAPE)


comparison = data.frame()
mpe_diff_rel =c()
mpe_diff_abs = c()
KS = c()

for(i in 1:length(settings)){
  
  anand_obs = anand$ANAND.MPE[which(anand$ANAND.unique_setting== settings[i])]
  replication_obs = rep$REPLICATION.MAPE[which(rep$REPLICATION.unique_setting== settings[i])]
  
  mpe_diff_rel[i] = (mean(anand_obs)-mean(replication_obs))/mean(anand_obs)
  mpe_diff_abs[i] = (mean(anand_obs)-mean(replication_obs))
  KS[i] = ks.test(anand_obs,replication_obs)$p.value
  print(i)
  
}

mpe_diff_rel = abs(mpe_diff_rel)
mpe_diff_abs = abs(mpe_diff_abs)


comparison = data.frame(settings,KS,mpe_diff_abs,mpe_diff_rel)
write.csv(comparison, "comparison.csv")



length(comparison$KS[which(comparison$KS<0.01)])/length(comparison$KS)

comparison$mpe_diff_abs[which(is.na(comparison$mpe_diff_abs))]<-0
comparison$mpe_diff_rel[which(is.na(comparison$mpe_diff_rel))]<-0


#=======================================Relational equivalence===========================##

library(apaTables)

ANAND[ANAND =="LOW"]<-1
ANAND[ANAND =="MID"]<-2
ANAND[ANAND =="HIGH"]<-3

ANAND$COR1_CLASS = as.numeric(ANAND$COR1_CLASS)
ANAND$COR2_CLASS = as.numeric(ANAND$COR2_CLASS)
ANAND$Q_VAR = as.numeric(ANAND$Q_VAR)
ANAND$BE_AB = ANAND$OC5-ANAND$UC5

ANAND1 = subset(ANAND, PACP ==3)

reg = MPE ~ ACP + DISP1 +DISP2 + DENS + COR1_CLASS + COR2_CLASS + Q_VAR

reg_data = data.frame(lapply(ANAND[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("anand",2,".doc"), table.number = 1)



REPLICATION[REPLICATION =="LOW"]<-1
REPLICATION[REPLICATION =="MID"]<-2
REPLICATION[REPLICATION =="HIGH"]<-3
REPLICATION$COR1_CLASS = as.numeric(REPLICATION$COR1_CLASS)
REPLICATION$COR2_CLASS = as.numeric(REPLICATION$COR2_CLASS)
REPLICATION$Q_VAR = as.numeric(REPLICATION$Q_VAR)
REPLICATION$BE_AB = REPLICATION$UC5-REPLICATION$OC5

REPLICATION1 = subset(REPLICATION, PACP == 3)
reg = MAPE ~ ACP + DISP1 +DISP2 + DENS + COR1_CLASS + COR2_CLASS + Q_VAR

reg_data = data.frame(lapply(REPLICATION[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)





#=======================================Test for normality and KS Test===========================##


normality_test = c()

for(i in 1:100){
  
  excerpt = sample(c(1:nrow(ANAND)),1000,replace=FALSE)
  
  result = shapiro.test(sqrt(ANAND$MPE[excerpt]))

  normality_test[i] = result$p.value
  
}




  sample_sizes = c(10,20,30,40,50,100,200,300,400,500,1000,2000,3000,4000,5000,7500,10000,12500,15000)
  test_statistic = c(0)
  p.value = c(0)
  coefficient_diff = c(0)
  
  ks_results = data.frame(sample_sizes,test_statistic,p.value,coefficient_diff)
  
  
  reg_rep = MAPE ~ ACP + DISP1 +DISP2 + DENS + COR1_CLASS + COR2_CLASS + Q_VAR
  reg_abl = MPE ~ ACP + DISP1 +DISP2 + DENS + COR1_CLASS + COR2_CLASS + Q_VAR
  
  
  
  for(i in 1:length(sample_sizes)){
    
    statistic = c()
    p = c()
    for(j in 1:100){
      
    excerpt = sample(c(1:nrow(ANAND)),sample_sizes[i],replace=FALSE)
    
    test = ks.test(ANAND$MPE[excerpt],REPLICATION$MAPE[excerpt])
    
    
    statistic[j] = test$statistic
    p[j] = test$p.value
    
    
    
    
    reg_data = data.frame(lapply(ANAND[excerpt,all.vars(reg_abl)], scale))
    linear_reg_std_abl = lm(reg_abl, data = reg_data)
    

    reg_data_rep = data.frame(lapply(REPLICATION[excerpt,all.vars(reg_rep)], scale))
    linear_reg_std_rep = lm(reg_rep, data = reg_data_rep)
    
    coeff_diff = mean(abs(linear_reg_std_abl$coefficients-linear_reg_std_rep$coefficients))
    

  }
    ks_results$test_statistic[i] = mean(statistic)
    ks_results$p.value[i] = mean(p)
    ks_results$coefficient_diff[i] = coeff_diff
  

  
  }
  
  
  

ggplot(ks_results, aes(x=sample_sizes,y=p.value))+geom_line()+geom_line(aes(y=coefficient_diff))+theme_classic()

write.csv(ks_results,"ks_ressults.csv")

#=======================================Cost Pool Rule===========================##


library(ggplot2)
library(moments)

plot_data = data.frame(rep("ORIGINAL",nrow(ANAND)),ANAND$ACP,ANAND$MPE)
colnames(plot_data) = c('Model','ACP',"MAPE")

plot_data_rep = data.frame(rep("REPLICATION",nrow(REPLICATION)),REPLICATION$CP,REPLICATION$MAPE)
colnames(plot_data_rep) = c('Model','ACP',"MAPE")


plot_data =rbind(plot_data,plot_data_rep)

plot_data$CP = as.factor(plot_data$ACP)


ggplot(plot_data, aes(x = CP, y= MAPE)) + geom_boxplot(outlier.shape= NA,aes(fill=Model)) + theme_classic()+theme(legend.position="bottom")+ylim(0,1.0)


plot_data = subset(plot_data, ACP!= 50)

ggplot(data = plot_data, aes(x=MAPE, fill = Model))+geom_density(alpha = 0.75)+theme_minimal()+facet_wrap(vars(ACP))+xlim(0,1)+ylim(0,10)

cp = 50

mean(subset(REPLICATION, ACP ==cp)$MAPE)
sd(subset(REPLICATION, ACP ==cp)$MAPE)


mean(subset(ANAND, ACP ==cp)$MPE)
sd(subset(ANAND, ACP ==cp)$MPE)


ks.test(subset(REPLICATION, ACP ==cp)$MAPE,subset(ANAND, ACP ==cp)$MPE)

subset_plot_data = subset(plot_data, ACP == cp)

wilcox.test(plot_data$MAPE ~plot_data$Model)


skewness(subset(REPLICATION,ACP ==cp)$MAPE)
skewness(subset(ANAND,ACP ==cp)$MPE)

kurtosis(subset(REPLICATION,ACP ==cp)$MAPE)
kurtosis(subset(ANAND,ACP ==cp)$MPE)

#=======================================Degree of resource sharing===========================##


library(ggplot2)

plot_data = data.frame(rep("ORIGINAL",nrow(ANAND)),ANAND$DENS_CLASS,ANAND$ACP,ANAND$MPE)
colnames(plot_data) = c('Model','DENS','ACP',"MAPE")

plot_data_rep = data.frame(rep("REPLICATION",nrow(REPLICATION)),REPLICATION$DENS_CLASS,REPLICATION$CP,REPLICATION$MAPE)
colnames(plot_data_rep) = c('Model','DENS','ACP',"MAPE")


plot_data =rbind(plot_data,plot_data_rep)

plot_data=subset(plot_data, ACP == 1 | ACP ==20 | ACP==40)

plot_data$DENS = factor(plot_data$DENS, levels = c('LOW','MID','HIGH'))


ggplot(plot_data, aes(x = DENS, y= MAPE)) + geom_boxplot(outlier.shape= NA,aes(fill=Model)) + theme_classic()+facet_grid(cols = vars(ACP))+theme(legend.position="bottom")+ylim(0,1)


plot_data = subset(plot_data, ACP!= 50)
ggplot(data = plot_data, aes(x=MAPE, fill = Model))+geom_density(alpha = 0.75)+theme_minimal()+facet_wrap(vars(ACP,DENS))+xlim(0,1)+ylim(0,10)



class = "HIGH"
cp = 40

subset_plot_data = subset(plot_data, DENS = class & ACP ==cp)

subset_rep = subset(REPLICATION, DENS_CLASS == class & ACP ==cp)$MAPE
subset_abl = subset(ANAND, DENS_CLASS == class & ACP ==cp)$MPE

mean(subset_rep)
sd(subset_rep)
skewness(subset_rep)
kurtosis(subset_rep)

mean(subset_abl)
sd(subset_abl)
skewness(subset_abl)
kurtosis(subset_abl)


ks.test(subset(REPLICATION, DENS_CLASS == class & ACP ==cp)$MAPE,subset(ANAND, DENS_CLASS == class & ACP ==cp)$MPE)

wilcox.test(subset_plot_data$MAPE~subset_plot_data$Model)


#=======================================Cross-Subsidization pattern===========================##


plot_data = data.frame(rep("ORIGINAL",nrow(ANAND)),ANAND$DENS_CLASS,ANAND$ACP,ANAND$BE_AB)
colnames(plot_data) = c('Model','DENS','ACP',"BE_AB")

plot_data_rep = data.frame(rep("REPLICATION",nrow(REPLICATION)),REPLICATION$DENS_CLASS,REPLICATION$CP,REPLICATION$BE_AB)
colnames(plot_data_rep) = c('Model','DENS','ACP',"BE_AB")


plot_data =rbind(plot_data,plot_data_rep)

plot_data=subset(plot_data, ACP == 1 | ACP ==20 | ACP==40)

plot_data$DENS = factor(plot_data$DENS, levels = c('LOW','MID','HIGH'))


ggplot(plot_data, aes(x = DENS, y= BE_AB)) + geom_boxplot(outlier.shape= NA,aes(fill=Model)) + theme_classic()+facet_grid(cols = vars(ACP))+theme(legend.position="bottom")+geom_hline(yintercept = 0)




plot_data = subset(plot_data, ACP!= 50)
ggplot(data = plot_data, aes(x=TVC_BIAS, fill = Model))+geom_density(alpha = 0.75)+theme_minimal()#+facet_wrap(vars(ACP,DENS))+ylim(0,10)



class = "HIGH"
cp = 40 

subset_plot_data = subset(plot_data, DENS = class & ACP ==cp)

subset_rep = subset(REPLICATION, DENS_CLASS == class & ACP ==cp)$BE_AB
subset_abl = subset(ANAND, DENS_CLASS == class & ACP ==cp)$BE_AB

mean(subset_rep)
sd(subset_rep)
skewness(subset_rep)
kurtosis(subset_rep)

mean(subset_abl)
sd(subset_abl)
skewness(subset_abl)
kurtosis(subset_abl)


ks.test(subset(REPLICATION, DENS_CLASS == class & ACP ==cp)$MAPE,subset(ANAND, DENS_CLASS == class & ACP ==cp)$MPE)

y = wilcox.test(subset_plot_data$BE_AB~subset_plot_data$Model)


