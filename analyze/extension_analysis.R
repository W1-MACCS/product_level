

DATA = output

DATA = DATA[which(!is.na(DATA$MAPE)),]




dec_1 = c(rep("dec_1",nrow(DATA)))
d1 = data.frame(dec_1,DATA$pe_dec_1)
colnames(d1) = c("group","pe")

dec_2 = c(rep("dec_2",nrow(DATA)))
d2 = data.frame(dec_2,DATA$pe_dec_2)
colnames(d2) = c("group","pe")

dec_3 = c(rep("dec_3",nrow(DATA)))
d3 = data.frame(dec_3,DATA$pe_dec_3)
colnames(d3) = c("group","pe")

dec_4 = c(rep("dec_4",nrow(DATA)))
d4 = data.frame(dec_4,DATA$pe_dec_4)
colnames(d4) = c("group","pe")

dec_5 = c(rep("dec_5",nrow(DATA)))
d5 = data.frame(dec_5,DATA$pe_dec_5)
colnames(d5) = c("group","pe")

dec_6 = c(rep("dec_6",nrow(DATA)))
d6 = data.frame(dec_6,DATA$pe_dec_6)
colnames(d6) = c("group","pe")

dec_7 = c(rep("dec_7",nrow(DATA)))
d7 = data.frame(dec_7,DATA$pe_dec_7)
colnames(d7) = c("group","pe")

dec_8 = c(rep("dec_8",nrow(DATA)))
d8 = data.frame(dec_8,DATA$pe_dec_8)
colnames(d8) = c("group","pe")

dec_9 = c(rep("dec_9",nrow(DATA)))
d9 = data.frame(dec_9,DATA$pe_dec_9)
colnames(d9) = c("group","pe")

dec_10 = c(rep("dec_10",nrow(DATA)))
d10 = data.frame(dec_10,DATA$pe_dec_10)
colnames(d10) = c("group","pe")



dataset = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)



colnames(dataset) = c("group","PE")

dataset$group = factor(dataset$group, levels = c("dec_1","dec_2","dec_3","dec_4","dec_5","dec_6","dec_7","dec_8","dec_9","dec_10"))
library(ggplot2)



#1. Testing in unchanged model
ggplot(dataset, aes(y=PE, x=group))+geom_boxplot(outlier.shape= NA)+
  theme_classic()+geom_hline(yintercept = 0)+xlab("RANK-ORDERED PRODUCTS - Low to High Volume")+
  theme(legend.position = "bottom", axis.text.x = element_blank())




########################################################################################




DATA = output

DATA = DATA[which(!is.na(DATA$MAPE)),]




dec_1 = c(rep("dec_1",nrow(DATA)))
d1 = data.frame(dec_1,DATA$ce_dec_1)
colnames(d1) = c("group","pe")

dec_2 = c(rep("dec_2",nrow(DATA)))
d2 = data.frame(dec_2,DATA$ce_dec_2)
colnames(d2) = c("group","pe")

dec_3 = c(rep("dec_3",nrow(DATA)))
d3 = data.frame(dec_3,DATA$ce_dec_3)
colnames(d3) = c("group","pe")

dec_4 = c(rep("dec_4",nrow(DATA)))
d4 = data.frame(dec_4,DATA$ce_dec_4)
colnames(d4) = c("group","pe")

dec_5 = c(rep("dec_5",nrow(DATA)))
d5 = data.frame(dec_5,DATA$ce_dec_5)
colnames(d5) = c("group","pe")

dec_6 = c(rep("dec_6",nrow(DATA)))
d6 = data.frame(dec_6,DATA$ce_dec_6)
colnames(d6) = c("group","pe")

dec_7 = c(rep("dec_7",nrow(DATA)))
d7 = data.frame(dec_7,DATA$ce_dec_7)
colnames(d7) = c("group","pe")

dec_8 = c(rep("dec_8",nrow(DATA)))
d8 = data.frame(dec_8,DATA$ce_dec_8)
colnames(d8) = c("group","pe")

dec_9 = c(rep("dec_9",nrow(DATA)))
d9 = data.frame(dec_9,DATA$ce_dec_9)
colnames(d9) = c("group","pe")

dec_10 = c(rep("dec_10",nrow(DATA)))
d10 = data.frame(dec_10,DATA$ce_dec_10)
colnames(d10) = c("group","pe")



dataset = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)



colnames(dataset) = c("group","PE")

dataset$group = factor(dataset$group, levels = c("dec_1","dec_2","dec_3","dec_4","dec_5","dec_6","dec_7","dec_8","dec_9","dec_10"))
library(ggplot2)



#1. Testing in unchanged model
ggplot(dataset, aes(y=PE, x=group))+geom_boxplot(outlier.shape= NA)+
  theme_classic()+geom_hline(yintercept = 0)+xlab("RANK-ORDERED PRODUCTS - High to Low Complexity")+
  theme(legend.position = "bottom", axis.text.x = element_blank())



###################################################################################


DATA = output

DATA = DATA[which(!is.na(DATA$MAPE)),]




dec_1 = c(rep("dec_1",nrow(DATA)))
d1 = data.frame(dec_1,DATA$pcb_dec_1)
colnames(d1) = c("group","pe")

dec_2 = c(rep("dec_2",nrow(DATA)))
d2 = data.frame(dec_2,DATA$pcb_dec_2)
colnames(d2) = c("group","pe")

dec_3 = c(rep("dec_3",nrow(DATA)))
d3 = data.frame(dec_3,DATA$pcb_dec_3)
colnames(d3) = c("group","pe")

dec_4 = c(rep("dec_4",nrow(DATA)))
d4 = data.frame(dec_4,DATA$pcb_dec_4)
colnames(d4) = c("group","pe")

dec_5 = c(rep("dec_5",nrow(DATA)))
d5 = data.frame(dec_5,DATA$pcb_dec_5)
colnames(d5) = c("group","pe")

dec_6 = c(rep("dec_6",nrow(DATA)))
d6 = data.frame(dec_6,DATA$pcb_dec_6)
colnames(d6) = c("group","pe")

dec_7 = c(rep("dec_7",nrow(DATA)))
d7 = data.frame(dec_7,DATA$pcb_dec_7)
colnames(d7) = c("group","pe")

dec_8 = c(rep("dec_8",nrow(DATA)))
d8 = data.frame(dec_8,DATA$pcb_dec_8)
colnames(d8) = c("group","pe")

dec_9 = c(rep("dec_9",nrow(DATA)))
d9 = data.frame(dec_9,DATA$pcb_dec_9)
colnames(d9) = c("group","pe")

dec_10 = c(rep("dec_10",nrow(DATA)))
d10 = data.frame(dec_10,DATA$pcb_dec_10)
colnames(d10) = c("group","pe")



dataset = rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)



colnames(dataset) = c("group","PE")

dataset$group = factor(dataset$group, levels = c("dec_1","dec_2","dec_3","dec_4","dec_5","dec_6","dec_7","dec_8","dec_9","dec_10"))
library(ggplot2)



#1. Testing in unchanged model
ggplot(dataset, aes(y=PE, x=group))+geom_boxplot(outlier.shape= NA)+
  theme_classic()+geom_hline(yintercept = 0)+xlab("RANK-ORDERED PRODUCTS - Low to High Costs")+
  theme(legend.position = "bottom", axis.text.x = element_blank())




















## Appendix Correlation between demand and resource consumption

DATAplot = output

library(ggplot2)
DATAplot$PDR = as.factor(DATAplot$PDR)
DATAplot$CS[DATAplot$CS == 0] <- "ALL UNIT-LEVEL COSTS"
DATAplot$CS[DATAplot$CS == 1] <- "SIMPLE HIERARCHY"
DATAplot$CS[DATAplot$CS == 2] <- "ABC HIERARCHY-THEORY"
DATAplot$CS[DATAplot$CS == 3] <- "ABC HIERARCHY-EMPIRICISM"
DATAplot$CS = factor(DATAplot$CS,levels = c("ALL UNIT-LEVEL COSTS", "SIMPLE HIERARCHY","ABC HIERARCHY-THEORY","ABC HIERARCHY-EMPIRICISM") )
ggplot(DATAplot,aes(factor(CS),ul_fl))+geom_boxplot(aes(fill=CS))+theme_classic()+
  ylab("Pearson correlation between MXQ and RES_CONS_PAT")


##########################################################################################################
##EFFECT MATRIX

DATA = output

DATA = DATA[which(!is.na(DATA$VB_PATTERN)),]

DATA$Q_VAR = as.character(DATA$Q_VAR)

DATA$Q_VAR[DATA$Q_VAR == "LOW"] <- 1
#DATA$Q_VAR[DATA$Q_VAR == "MID"] <- 2
DATA$Q_VAR[DATA$Q_VAR == "HIGH"] <- 3

#DATA$VB_PATTERN2 = (DATA$pe_dec_10+DATA$pe_dec_9)/2- (DATA$pe_dec_1+DATA$pe_dec_2)/2
#DATA$CB_PATTERN2 = (DATA$ce_dec_10+DATA$ce_dec_9)/2 - (DATA$ce_dec_1+DATA$ce_dec_2)/2


library(apaTables)
DATA = subset(DATA, CS == 2)
DATA$CS = as.numeric(DATA$CS)
DATA$PDR = as.numeric(DATA$PDR)
DATA$ACP = as.numeric(DATA$ACP)
DATA$Q_VAR = as.numeric(DATA$Q_VAR)

if(DATA$CS[1] == 0){
  reg = VB_PATTERN ~ DISP1 + DISP2+COR1+COR2+ DENS +Q_VAR+bl_size+ACP+PDR
  
}else if(DATA$CS[1] == 1){
reg = VB_PATTERN ~ DISP1 + DISP2+COR1+COR2+ DENS +Q_VAR+fl_size+ACP+PDR
}else{
reg = VB_PATTERN ~ DISP1 + DISP2+DENS +Q_VAR+bl_size+pl_size+fl_size+ACP+PDR#+ul_bl+ul_pl+ul_fl+bl_pl+bl_fl+pl_fl#
}
  
#reg = VB_PATTERN ~  non_unit_size + I(non_unit_size*Q_VAR)+I(ACP*non_unit_size)#+I(PDR*non_unit_size)
#reg = VB_PATTERN ~ non_unit_size + PDR + Q_VAR+ACP

reg_data = data.frame(lapply(DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)






##########################################################################################################################


DATA = output

DATA = subset(DATA, CS == 3)


mean(DATA$ul_bl)
sd(DATA$ul_bl)

mean(DATA$ul_pl)
sd(DATA$ul_pl)

mean(DATA$ul_fl)
sd(DATA$ul_fl)



mean(DATA$bl_pl)
sd(DATA$bl_pl)

mean(DATA$bl_fl)
sd(DATA$bl_fl)



mean(DATA$pl_fl)
sd(DATA$pl_fl)

mean(DATA$VB_PATTERN)



#######################################################################################################################
DATA = output
DATA = subset(DATA, CS == 2)
mean(DATA$ul_size)/50
mean(DATA$bl_size)/50
mean(DATA$pl_size)/50
mean(DATA$fl_size)/50
