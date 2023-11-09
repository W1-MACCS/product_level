library(ggplot2)

DATA = output

DATA = DATA[which(!is.na(DATA$mape)),]

###PCH RANKED PERCENTAGE ERROR###
DATA = output
DATA$CS[DATA$CS == 0] <- "ORIGINAL"
DATA$CS[DATA$CS == 1] <- "ORIGINAL + VOLUME MATCH"
DATA$CS[DATA$CS == 2] <- "HIERARCHICAL"
DATA$CS[DATA$CS == 3] <- "HIERARCHICAL + VOLUME MATCH"




data_pch_rank = aggregate(.~ CS+pch_rank, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("ORIGINAL","ORIGINAL + VOLUME MATCH","HIERARCHICAL","HIERARCHICAL + VOLUME MATCH"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
ggplot(data_pch_rank, aes(x = pch_rank, y= ape))+geom_line(size=1)+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)


# summary(subset(DATA, DENS == 0.25)$nonzero_cons)/50
# summary(subset(DATA, DENS == 0.5)$nonzero_cons)/50
# summary(subset(DATA, DENS == 0.75)$nonzero_cons)/50

summary(subset(DATA, CS == 0&DENS== 0.75)$BE_AB)
summary(subset(DATA, CS == 1&DENS== 0.75)$BE_AB)
summary(subset(DATA, CS == 2&DENS== 0.75)$BE_AB)
summary(subset(DATA, CS == 3&DENS== 0.75)$BE_AB)


###MXQ RANKED PERCENTAGE ERROR###
DATA = output
DATA$CS[DATA$CS == 0] <- "ORIGINAL"
DATA$CS[DATA$CS == 1] <- "ORIGINAL + VOLUME MATCH"
DATA$CS[DATA$CS == 2] <- "HIERARCHICAL"
DATA$CS[DATA$CS == 3] <- "HIERARCHICAL + VOLUME MATCH"


data_pch_rank = aggregate(.~ CS+MXQ_rank+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("ORIGINAL","ORIGINAL + VOLUME MATCH","HIERARCHICAL","HIERARCHICAL + VOLUME MATCH"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
ggplot(data_pch_rank, aes(x = MXQ_rank, y= pe, linetype = ACP))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)




###MXQ RANKED PERCENTAGE ERROR - Q_VAR###
DATA = output
DATA$CS[DATA$CS == 0] <- "ORIGINAL"
DATA$CS[DATA$CS == 1] <- "ORIGINAL + VOLUME MATCH"
DATA$CS[DATA$CS == 2] <- "HIERARCHICAL"
DATA$CS[DATA$CS == 3] <- "HIERARCHICAL + VOLUME MATCH"


data_pch_rank = aggregate(.~ CS+MXQ_rank+Q_VAR+DISP2, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("ORIGINAL","ORIGINAL + VOLUME MATCH","HIERARCHICAL","HIERARCHICAL + VOLUME MATCH"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
ggplot(data_pch_rank, aes(x = MXQ_rank, y= pe, linetype = Q_VAR))+geom_line()+theme_classic()+facet_grid(~CS+DISP2)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)


###INTER RANKED PERCENTAGE ERROR###
DATA = subset(output, inter_rank ==1 | inter_rank == -1)
DATA$CS[DATA$CS == 0] <- "ORIGINAL"
DATA$CS[DATA$CS == 1] <- "ORIGINAL + VOLUME MATCH"
DATA$CS[DATA$CS == 2] <- "HIERARCHICAL"
DATA$CS[DATA$CS == 3] <- "HIERARCHICAL + VOLUME MATCH"



data_pch_rank = aggregate(.~ CS+inter_rank+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("ORIGINAL","ORIGINAL + VOLUME MATCH","HIERARCHICAL","HIERARCHICAL + VOLUME MATCH"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
ggplot(data_pch_rank, aes(x = inter_rank, y= pe, linetype = DENS))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+ylim(-1,1)


###INTRA RANKED PERCENTAGE ERROR###
DATA = subset(output, intra_rank ==1 | intra_rank == -1)
DATA$CS[DATA$CS == 0] <- "ORIGINAL"
DATA$CS[DATA$CS == 1] <- "ORIGINAL + VOLUME MATCH"
DATA$CS[DATA$CS == 2] <- "HIERARCHICAL"
DATA$CS[DATA$CS == 3] <- "HIERARCHICAL + VOLUME MATCH"



data_pch_rank = aggregate(.~ CS+intra_rank+Q_VAR, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("ORIGINAL","ORIGINAL + VOLUME MATCH","HIERARCHICAL","HIERARCHICAL + VOLUME MATCH"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
ggplot(data_pch_rank, aes(x = intra_rank, y= pe, linetype = Q_VAR))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+ylim(-1,1)


###NONZERO RANKED PERCENTAGE ERROR###
DATA = subset(output, ACP>=3)
DATA$CS[DATA$CS == 0] <- "ORIGINAL"
DATA$CS[DATA$CS == 1] <- "ORIGINAL + VOLUME MATCH"
DATA$CS[DATA$CS == 2] <- "HIERARCHICAL"
DATA$CS[DATA$CS == 3] <- "HIERARCHICAL + VOLUME MATCH"

data_pch_rank = aggregate(.~ CS+nonzero_cons_rank+DISP2, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("ORIGINAL","ORIGINAL + VOLUME MATCH","HIERARCHICAL","HIERARCHICAL + VOLUME MATCH"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
ggplot(data_pch_rank, aes(x = nonzero_cons_rank, y= pe, linetype = DISP2))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)


###ACT CONS RANKED PERCENTAGE ERROR###
DATA = subset(output, ACP>=3)
DATA$CS[DATA$CS == 0] <- "ORIGINAL"
DATA$CS[DATA$CS == 1] <- "ORIGINAL + VOLUME MATCH"
DATA$CS[DATA$CS == 2] <- "HIERARCHICAL"
DATA$CS[DATA$CS == 3] <- "HIERARCHICAL + VOLUME MATCH"

data_pch_rank = aggregate(.~ CS+sd_cons_rank+Q_VAR, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("ORIGINAL","ORIGINAL + VOLUME MATCH","HIERARCHICAL","HIERARCHICAL + VOLUME MATCH"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
ggplot(data_pch_rank, aes(x = sd_cons_rank, y= pe, linetype = Q_VAR))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)




###ACT CONS RANKED PERCENTAGE ERROR###
DATA = output

data_pch_rank = aggregate(.~ CS+MXQ_rank+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
ggplot(data_pch_rank, aes(x = MXQ_rank, y= pe, col = DENS))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)








###MXQ RANKED PERCENTAGE ERROR  COST POOLS###
DATA = output

data_pch_rank = aggregate(.~ MXQ_rank+DENS+Q_VAR, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
#data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("ORIGINAL","ORIGINAL + VOLUME MATCH","HIERARCHICAL","HIERARCHICAL + VOLUME MATCH"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
ggplot(data_pch_rank, aes(x = MXQ_rank, y= pe, linetype = Q_VAR))+geom_line()+theme_classic()+facet_grid(~DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)






###pch PERCENTAGE ERROR###

DATA = output

data_pch_rank = aggregate(.~ CS+pch_rank+PDR, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
ggplot(data_pch_rank, aes(x = pch_rank, y= pcb))+geom_line()+geom_line(aes(y=PCB))+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)


############################################BE_AB#######################################################################
DATA = subset(output)
DATA$CS[DATA$CS == 0] <- "ORIGINAL"
DATA$CS[DATA$CS == 1] <- "ORIGINAL + VOLUME MATCH"
DATA$CS[DATA$CS == 2] <- "HIERARCHICAL"
DATA$CS[DATA$CS == 3] <- "HIERARCHICAL + VOLUME MATCH"
DATA$CS = factor(DATA$CS, levels = c("ORIGINAL","ORIGINAL + VOLUME MATCH","HIERARCHICAL","HIERARCHICAL + VOLUME MATCH"))
DATA$ACP = as.factor(DATA$ACP)
ggplot(DATA,aes(x= BE_AB))+geom_histogram(fill="black", alpha=0.2, position="identity")+geom_vline(xintercept = 0)+facet_grid(~DENS+DISP2)+theme_classic()



DATA$CS = as.factor(DATA$CS)
DATA$ACP = as.factor(DATA$ACP)
ggplot(DATA,aes(x= acc))+geom_histogram(fill="black", alpha=0.2, position="identity")+facet_grid(~ACP)





##########################################################################################################
##Correlation MATRIX

DATA = output

DATA$Q_VAR = as.character(DATA$Q_VAR)

DATA$Q_VAR[DATA$Q_VAR == "LOW"] <- 1
DATA$Q_VAR[DATA$Q_VAR == "MID"] <- 2
DATA$Q_VAR[DATA$Q_VAR == "HIGH"] <- 3



library(apaTables)
DATA$CS = as.numeric(DATA$CS)
DATA$PDR = as.numeric(DATA$PDR)
DATA$ACP = as.numeric(DATA$ACP)
DATA$Q_VAR = as.numeric(DATA$Q_VAR)
DATA$DENS = as.numeric(DATA$DENS)


cor_data= data.frame(DATA$CS,DATA$ACP,DATA$DENS,DATA$Q_VAR,DATA$MXQ,DATA$intra_rank,DATA$inter_rank,DATA$sd_cons,DATA$nonzero_cons,DATA$BE_AB,DATA$acc,DATA$pcb,DATA$pch,DATA$pe)
colnames(cor_data) = c("CS","ACP","DENS","Q_VAR","MXQ","intra","inter","sd_cons","nonzero_cons","BE_AB","acc","pcb","pch","pe")
apa.cor.table(cor_data,filename = paste0("cor",".doc"), table.number = 1)



##System Level
REG_DATA = DATA


reg = mape ~ ACP+PDR+DENS+Q_VAR+CS+PMH

reg_data = data.frame(lapply(REG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



reg = PMH ~ DENS+Q_VAR+CS+DISP2

reg_data = data.frame(lapply(REG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



reg = BE_AB ~ ACP+DENS+Q_VAR+PMH+CS

reg_data = data.frame(lapply(REG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)




##Regression Analysis
DATA = output
#DATA = subset(DATA, ACP>1)



VBC_DATA = subset(DATA, PDR ==1 & CS==0)

reg = pe ~ MXQ+act_cons+pch+inter_rank+intra_rank#+nonzero_cons

reg_data = data.frame(lapply(VBC_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


VBC_DATA = subset(DATA, PDR ==1 & CS==1)

reg = pe ~ MXQ+act_cons+pch+inter_rank+intra_rank#+nonzero_cons

reg_data = data.frame(lapply(VBC_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


ABC_DATA = subset(DATA, PDR ==0 &CS==0)

reg = pe ~ MXQ+act_cons+pch+inter_rank+intra_rank+nonzero_cons

reg_data = data.frame(lapply(ABC_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



ABC_DATA = subset(DATA, PDR ==0 &CS==1)

reg = pe ~ MXQ+act_cons+pch+inter_rank+intra_rank+nonzero_cons

reg_data = data.frame(lapply(ABC_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)






##############################################################################################################################

##Regression Analysis
DATA = output
#DATA = subset(DATA, ACP>1)



ORIG_DATA = subset(DATA, PDR ==0 & CS==0)

reg = pe ~ MXQ+pch+inter_rank+intra_rank+sd_cons+nonzero_cons

reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


MATCH_DATA = subset(DATA, PDR ==0 & CS==1)

reg = pe ~ MXQ+pch+inter_rank+intra_rank+sd_cons+nonzero_cons

reg_data = data.frame(lapply(MATCH_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


DIAG_DATA = subset(DATA, PDR ==0 &CS==2)

reg = pe ~ MXQ+pch+inter_rank+intra_rank+sd_cons+nonzero_cons

reg_data = data.frame(lapply(DIAG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



DIAG_MATCH_DATA = subset(DATA, PDR ==0 &CS==3)

reg = pe ~ MXQ+pch+inter_rank+intra_rank+sd_cons+nonzero_cons

reg_data = data.frame(lapply(DIAG_MATCH_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)












