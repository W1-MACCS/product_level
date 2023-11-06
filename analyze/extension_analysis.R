library(ggplot2)

DATA = output

DATA = DATA[which(!is.na(DATA$mape)),]

###PCH RANKED PERCENTAGE ERROR###
DATA = output

data_pch_rank = aggregate(.~ CS+pch_rank+PDR+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$Agg_Degr = as.factor(data_pch_rank$Agg_Degr)
ggplot(data_pch_rank, aes(x = pch_rank, y= pe, col = PDR))+geom_line()+theme_classic()+facet_grid(~CS+DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)


summary(subset(DATA, DENS == 0.25)$nonzero_cons)/50
summary(subset(DATA, DENS == 0.5)$nonzero_cons)/50
summary(subset(DATA, DENS == 0.75)$nonzero_cons)/50

summary(subset(DATA, CS == 0&PDR== 0)$BE_AB)
summary(subset(DATA, CS == 1&PDR== 0)$BE_AB)
summary(subset(DATA, CS == 0&PDR== 1)$BE_AB)
summary(subset(DATA, CS == 1&PDR== 1)$BE_AB)

###MXQ RANKED PERCENTAGE ERROR###
DATA = output

data_pch_rank = aggregate(.~ CS+MXQ_rank+PDR+Q_VAR, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
ggplot(data_pch_rank, aes(x = MXQ_rank, y= pe, col = PDR))+geom_line()+theme_classic()+facet_grid(~CS+Q_VAR)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)


###INTER RANKED PERCENTAGE ERROR###
DATA = output

data_pch_rank = aggregate(.~ CS+inter_rank+PDR+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
ggplot(data_pch_rank, aes(x = inter_rank, y= pe, col = PDR))+geom_line()+theme_classic()+facet_grid(~CS+DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)


###INTRA RANKED PERCENTAGE ERROR###
DATA = output

data_pch_rank = aggregate(.~ CS+intra_rank+PDR+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
ggplot(data_pch_rank, aes(x = intra_rank, y= pe, col = PDR))+geom_line()+theme_classic()+facet_grid(~CS+DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)



###NONZERO RANKED PERCENTAGE ERROR###
DATA = output

data_pch_rank = aggregate(.~ CS+nonzero_cons_rank+PDR+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
ggplot(data_pch_rank, aes(x = nonzero_cons_rank, y= pe, col = PDR))+geom_line()+theme_classic()+facet_grid(~CS+DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)


###SD NONZERO RANKED PERCENTAGE ERROR###
DATA = output

data_pch_rank = aggregate(.~ CS+sd_cons_rank+PDR+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
ggplot(data_pch_rank, aes(x = sd_cons_rank, y= pe, col = PDR))+geom_line()+theme_classic()+facet_grid(~CS+DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)






###PCH RANKED PERCENTAGE ERROR### PACP Heuristics
DATA = output

data_pch_rank = aggregate(.~ CS+MXQ_rank+PACP+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PACP = as.factor(data_pch_rank$PACP)
ggplot(data_pch_rank, aes(x = MXQ_rank, y= pe, col = PACP))+geom_line()+theme_classic()+facet_grid(~CS+ACP)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)




###MXQ RANKED PERCENTAGE ERROR  COST POOLS###
DATA = output

data_pch_rank = aggregate(.~ CS+MXQ_rank+PDR+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
ggplot(data_pch_rank, aes(x = MXQ_rank, y= pe, col = PDR))+geom_line()+theme_classic()+facet_grid(~CS+ACP)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)






###pch PERCENTAGE ERROR###

DATA = output

data_pch_rank = aggregate(.~ CS+pch_rank+PDR, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
ggplot(data_pch_rank, aes(x = pch_rank, y= pcb))+geom_line()+geom_line(aes(y=PCB))+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)






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


cor_data= data.frame(DATA$CS,DATA$ACP,DATA$PDR,DATA$DENS,DATA$Q_VAR,DATA$MXQ,DATA$intra,DATA$inter,DATA$entropy,DATA$nonzero_cons,DATA$sd_cons,DATA$pcb,DATA$PCB,DATA$pch,DATA$PCH,DATA$pe)
colnames(cor_data) = c("CS","ACP","Driver","DENS","Q_VAR","MXQ","intra","inter","entropy","nonzero_cons","sd_cons","pcb","PCB","pch","PCH","pe")
apa.cor.table(cor_data)




##Regression Analysis
DATA = output
DATA = subset(DATA, ACP>1)


VBC_DATA = subset(DATA, PDR ==1 & CS==0)

reg = pe ~ MXQ+sd_cons+pch#+intra+inter+sd_cons

reg_data = data.frame(lapply(VBC_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


VBC_DATA = subset(DATA, PDR ==1 & CS==1)

reg = pe ~ MXQ+sd_cons+pch

reg_data = data.frame(lapply(VBC_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


ABC_DATA = subset(DATA, PDR ==0 &CS==0)

reg = pe ~ MXQ+sd_cons+pch

reg_data = data.frame(lapply(ABC_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)

ABC_DATA = subset(DATA, PDR ==0 &CS==1)

reg = pe ~ MXQ+sd_cons+pch

reg_data = data.frame(lapply(ABC_DATA[,all.vars(reg)], scale))
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
