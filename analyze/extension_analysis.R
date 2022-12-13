
DATA = output

DATA = DATA[which(!is.na(DATA$MAPE)),]


#DATA$CS = as.factor(DATA$CS)
#DATA$PDR = as.factor(DATA$PDR)
#DATA= subset(DATA,CS ==0 & PDR ==0)

dec_1 = c(rep("dec_1",nrow(DATA)))
d1 = data.frame(dec_1,DATA$pe_dec_1,DATA$PDR, DATA$CS_levels)
colnames(d1) = c("group","pe", "PDR","CS")

dec_2 = c(rep("dec_2",nrow(DATA)))
d2 = data.frame(dec_2,DATA$pe_dec_2,DATA$PDR, DATA$CS_levels)
colnames(d2) = c("group","pe", "PDR","CS")

dec_3 = c(rep("dec_3",nrow(DATA)))
d3 = data.frame(dec_3,DATA$pe_dec_3,DATA$PDR, DATA$CS_levels)
colnames(d3) = c("group","pe", "PDR","CS")

dec_4 = c(rep("dec_4",nrow(DATA)))
d4 = data.frame(dec_4,DATA$pe_dec_4,DATA$PDR, DATA$CS_levels)
colnames(d4) = c("group","pe", "PDR","CS")

dec_5 = c(rep("dec_5",nrow(DATA)))
d5 = data.frame(dec_5,DATA$pe_dec_5,DATA$PDR, DATA$CS_levels)
colnames(d5) = c("group","pe", "PDR","CS")

dec_6 = c(rep("dec_6",nrow(DATA)))
d6 = data.frame(dec_6,DATA$pe_dec_6,DATA$PDR, DATA$CS_levels)
colnames(d6) = c("group","pe", "PDR","CS")

dec_7 = c(rep("dec_7",nrow(DATA)))
d7 = data.frame(dec_7,DATA$pe_dec_7,DATA$PDR, DATA$CS_levels)
colnames(d7) = c("group","pe", "PDR","CS")

dec_8 = c(rep("dec_8",nrow(DATA)))
d8 = data.frame(dec_8,DATA$pe_dec_8,DATA$PDR, DATA$CS_levels)
colnames(d8) = c("group","pe", "PDR","CS")

dec_9 = c(rep("dec_9",nrow(DATA)))
d9 = data.frame(dec_9,DATA$pe_dec_9,DATA$PDR, DATA$CS_levels)
colnames(d9) = c("group","pe", "PDR","CS")

dec_10 = c(rep("dec_10",nrow(DATA)))
d10 = data.frame(dec_10,DATA$pe_dec_10,DATA$PDR, DATA$CS_levels)
colnames(d10) = c("group","pe", "PDR","CS")



dataset = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)


dataset$PDR[dataset$PDR == 1] <- "VOLUME"
dataset$PDR[dataset$PDR == 0] <- "ACTIVITY"
dataset$CS[dataset$CS == 0] <- "ALL UNIT-LEVEL COSTS"
#dataset$CS[dataset$CS == 1] <- "WITH NON-UNIT-LEVEL COSTS"

colnames(dataset) = c("group","PE", "DRIVER","CS")

dataset$group = factor(dataset$group, levels = c("dec_1","dec_2","dec_3","dec_4","dec_5","dec_6","dec_7","dec_8","dec_9","dec_10"))
dataset$CS = factor(dataset$CS, levels = c("ALL UNIT-LEVEL COSTS", "LOW","MID","HIGH"))
library(ggplot2)



#1. Testing in unchanged model
ggplot(dataset, aes(y=PE, x=group, fill = DRIVER))+geom_boxplot(outlier.shape= NA)+ylim(-1,1)+
  theme_classic()+geom_hline(yintercept = 0)+xlab("RANK-ORDERED PRODUCTS - Low to High Volume")+
  theme(legend.position = "bottom", axis.text.x = element_blank())


#2. Extension with four levels of cost structure and two types of drivers 
ggplot(dataset, aes(y=PE, x=group,fill =DRIVER))+geom_boxplot(outlier.shape= NA)+ylim(-1,1)+
  theme_classic()+geom_hline(yintercept = 0)+facet_grid(cols = vars(CS))+xlab("RANK-ORDERED PRODUCTS - Low to High Volume")+
  theme(legend.position = "bottom", axis.text.x = element_blank())
        


#3. Mean for line plot 

data = aggregate(.~group+DRIVER+CS, dataset, median)
dataset$group = factor(dataset$group, levels = c("dec_1","dec_2","dec_3","dec_4","dec_5","dec_6","dec_7","dec_8","dec_9","dec_10"))

data$DRIVER = as.factor(data$DRIVER)
data$CS = as.factor(data$CS)

ggplot(data, aes(y=PE, x=group, group = DRIVER))+geom_line(aes(col = DRIVER))+geom_point(aes(col = DRIVER))+ylim(-0.5,0.5)+
  theme_classic()+geom_hline(yintercept = 0)+facet_grid(cols = vars(CS))+xlab("RANK-ORDERED PRODUCTS - Low to High Volume")+
  theme(legend.position = "bottom", axis.text.x = element_blank())


########################################################################################

DATA = output

DATA = DATA[which(!is.na(DATA$MAPE)),]


#DATA$CS = as.factor(DATA$CS)
#DATA$PDR = as.factor(DATA$PDR)
#DATA= subset(DATA,CS ==1 & PDR ==1 & Q_VAR == "EXTREME")

dec_1 = c(rep("dec_1",nrow(DATA)))
d1 = data.frame(dec_1,DATA$ce_dec_1,DATA$PDR, DATA$CS)
colnames(d1) = c("group","pe", "PDR","CS")

dec_2 = c(rep("dec_2",nrow(DATA)))
d2 = data.frame(dec_2,DATA$ce_dec_2,DATA$PDR, DATA$CS)
colnames(d2) = c("group","pe", "PDR","CS")

dec_3 = c(rep("dec_3",nrow(DATA)))
d3 = data.frame(dec_3,DATA$ce_dec_3,DATA$PDR, DATA$CS)
colnames(d3) = c("group","pe", "PDR","CS")

dec_4 = c(rep("dec_4",nrow(DATA)))
d4 = data.frame(dec_4,DATA$ce_dec_4,DATA$PDR, DATA$CS)
colnames(d4) = c("group","pe", "PDR","CS")

dec_5 = c(rep("dec_5",nrow(DATA)))
d5 = data.frame(dec_5,DATA$ce_dec_5,DATA$PDR, DATA$CS)
colnames(d5) = c("group","pe", "PDR","CS")

dec_6 = c(rep("dec_6",nrow(DATA)))
d6 = data.frame(dec_6,DATA$ce_dec_6,DATA$PDR, DATA$CS)
colnames(d6) = c("group","pe", "PDR","CS")

dec_7 = c(rep("dec_7",nrow(DATA)))
d7 = data.frame(dec_7,DATA$ce_dec_7,DATA$PDR, DATA$CS)
colnames(d7) = c("group","pe", "PDR","CS")

dec_8 = c(rep("dec_8",nrow(DATA)))
d8 = data.frame(dec_8,DATA$ce_dec_8,DATA$PDR, DATA$CS)
colnames(d8) = c("group","pe", "PDR","CS")

dec_9 = c(rep("dec_9",nrow(DATA)))
d9 = data.frame(dec_9,DATA$ce_dec_9,DATA$PDR, DATA$CS)
colnames(d9) = c("group","pe", "PDR","CS")

dec_10 = c(rep("dec_10",nrow(DATA)))
d10 = data.frame(dec_10,DATA$ce_dec_10,DATA$PDR, DATA$CS)
colnames(d10) = c("group","pe", "PDR","CS")



dataset = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)


dataset$PDR[dataset$PDR == 1] <- "VOLUME"
dataset$PDR[dataset$PDR == 0] <- "ACTIVITY"
dataset$CS[dataset$CS == 0] <- "ALL UNIT-LEVEL COSTS"
dataset$CS[dataset$CS == 1] <- "WITH NON-UNIT-LEVEL COSTS"

colnames(dataset) = c("group","PE", "DRIVER","CS")


dataset$group = factor(dataset$group, levels = c("dec_1","dec_2","dec_3","dec_4","dec_5","dec_6","dec_7","dec_8","dec_9","dec_10"))

ggplot(dataset, aes(y=PE, x=group,fill =DRIVER))+geom_boxplot(outlier.shape= NA)+ylim(-1,1)+
  theme_classic()+geom_hline(yintercept = 0)+facet_grid(cols = vars(CS))+xlab("RANK-ORDERED PRODUCTS - High to Low Complexity")+
  theme(legend.position = "bottom", axis.text.x = element_blank())






###################################################################################


DATA = output

library(ggplot2)
DATA$PDR = as.factor(DATA$PDR)
DATA$CS_levels[DATA$CS_levels == 0] <- "ALL UNIT-LEVEL COSTS"
DATA$CS_levels = factor(DATA$CS_levels,levels = c("ALL UNIT-LEVEL COSTS", "LOW","MID","HIGH") )
ggplot(DATA,aes(fill=CS_levels,y = BE_AB))+geom_boxplot()+theme_classic()+
  ylab("Pearson correlation between MXQ and RES_CONS_PAT")+facet_grid(vars(PDR))


##########################################################################################################
##EFFECT MATRIX


DATA = output

DATA = DATA[which(!is.na(DATA$VB_PATTERN)),]

DATA$Q_VAR = as.character(DATA$Q_VAR)

DATA$Q_VAR[DATA$Q_VAR == "LOW"] <- 1
DATA$Q_VAR[DATA$Q_VAR == "MID"] <- 2
DATA$Q_VAR[DATA$Q_VAR == "HIGH"] <- 3

DATA$VB_PATTERN2 = (DATA$pe_dec_10+DATA$pe_dec_9)/2- (DATA$pe_dec_1+DATA$pe_dec_2)/2
DATA$CB_PATTERN2 = (DATA$ce_dec_10+DATA$ce_dec_9)/2 - (DATA$ce_dec_1+DATA$ce_dec_2)/2


library(apaTables)
DATA = subset(DATA, PDR == 1)
DATA$CS = as.numeric(DATA$CS)
DATA$PDR = as.numeric(DATA$PDR)
DATA$ACP = as.numeric(DATA$ACP)
DATA$Q_VAR = as.numeric(DATA$Q_VAR)

reg = CB_PATTERN2 ~ (ACP + DISP1 + DISP2 + DENS +COR1 +COR2 +Q_VAR+PDR+non_unit_size)^2
reg = VB_PATTERN ~  non_unit_size + I(non_unit_size*Q_VAR)+I(ACP*non_unit_size)#+I(PDR*non_unit_size)
reg = VB_PATTERN ~ non_unit_size + PDR + Q_VAR+ACP

reg_data = data.frame(lapply(DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


###############################################################################################

DATA = output

DATA = DATA[which(!is.na(DATA$VB_PATTERN)),]

cor_inter_pe = c(0)
cor_inter_mxq = c(0)
cor_pe_rankdiff = c(0)
cor_mxq_rankdiff = c(0)

for(i in 1:nrow(DATA)){
  #pe=116:165, inter=246:295, mxq = 170:219
  
  cor_inter_pe[i] = cor(t(DATA[i,116:165]),t(DATA[i,246:295]))
  cor_inter_mxq[i] = cor(t(DATA[i,170:219]),t(DATA[i,246:295]))
  cor_pe_rankdiff[i] = cor(t(DATA[i,116:165]),t(DATA[i,296:345]))
  cor_mxq_rankdiff[i] = cor(t(DATA[i,170:219]),t(DATA[i,296:345]))
}


DATA$cor_inter_pe = cor_inter_pe
DATA$cor_inter_mxq = cor_inter_mxq
DATA$cor_pe_rankdiff = cor_pe_rankdiff
DATA$cor_mxq_rankdiff = cor_mxq_rankdiff



DATA$mean_rankdiff = rowMeans(abs(DATA[,296:345]))

