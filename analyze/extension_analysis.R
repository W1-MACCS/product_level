library(ggplot2)

DATA = output

DATA = DATA[which(!is.na(DATA$mape)),]



data_pch_rank = aggregate(.~ pch_rank, data = DATA, mean)

ggplot(data_pch_rank, aes(x = pch_rank, y= pch, col ="pch"))+geom_line(aes(y=pcb, col ="pcb"))+geom_line(aes(y=pch))+theme_classic()+facet_wrap(CS)+
  ggtitle("Comparison of pcb and pch over rank-ordered products by pch")



DATA = output

data_pch_rank = aggregate(.~ CS+pch_rank+DISP2, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
ggplot(data_pch_rank, aes(x = pch_rank, y= pe, col = CS))+geom_line()+theme_classic()+facet_grid(~DISP2)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)



data_mxq_rank = aggregate(.~ MXQ_rank, data = DATA, mean)

ggplot(data_mxq_rank, aes(x = MXQ_rank, y= PCH, col ="pch"))+geom_line(aes(y=PCB, col ="pcb"))+geom_line(aes(y=PCH))+theme_classic()+facet_wrap(CS)+
  ggtitle("Comparison of pcb and pch over rank-ordered products by mxq")








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
