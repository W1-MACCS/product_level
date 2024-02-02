library(ggplot2)


output$PERROR = abs(output$ERROR)/output$EUCD
output$AERROR = abs(output$ERROR)

DATA =output
#DATA$PERROR = abs(DATA$ERROR)/1000000

###PCH RANKED PERCENTAGE ERROR###
DATA = output
#DATA$CS[DATA$CS == 0] <- "LowVar"
#DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"


data_pch_rank = aggregate(.~ DENS+pch_rank+CSD, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
#data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$Q_VAR)
#data_pch_rank$SCD = as.factor(data_pch_rank$SCD)
ggplot(data_pch_rank, aes(x = pch_rank, y= ape,linetype = CSD))+geom_line(linewidth=1)+theme_classic()+facet_grid(~DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)


# summary(subset(DATA, DENS == 0.25)$nonzero_cons)/50
# summary(subset(DATA, DENS == 0.5)$nonzero_cons)/50
# summary(subset(DATA, DENS == 0.75)$nonzero_cons)/50





###MXQ RANKED PERCENTAGE ERROR###
DATA = output
#DATA$CS[DATA$CS == 0] <- "LowVar"
#DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"


data_pch_rank = aggregate(.~ DENS+MXQ_rank+CSD, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
#data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)

ggplot(data_pch_rank, aes(x = MXQ_rank, y= pe, linetype = CSD))+geom_line()+theme_classic()+facet_grid(~DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)


###MXQ RANKED pch###
DATA = output
#DATA$CS[DATA$CS == 0] <- "LowVar"
#DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"


data_pch_rank = aggregate(.~ DENS+MXQ_rank+SCD, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
#data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$SCD = as.factor(data_pch_rank$SCD)
ggplot(data_pch_rank, aes(x = MXQ_rank, y= pch, linetype = SCD))+geom_line()+theme_classic()+facet_grid(~DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)


###Cost Share Standard Ressources RANKED PERCENTAGE ERROR###
DATA = output
#DATA$CS[DATA$CS == 0] <- "LowVar"
#DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"


data_pch_rank = aggregate(.~ DENS+cost_ratio_std_res_rank+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
#data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
ggplot(data_pch_rank, aes(x = cost_ratio_std_res_rank, y= pe, linetype = ACP))+geom_line()+theme_classic()+facet_grid(~DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)








###INTER RANKED PERCENTAGE ERROR###
DATA = output


data_pch_rank = aggregate(.~ DENS+individuality_rank, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = individuality_rank, y= pe))+geom_line()+theme_classic()+facet_grid(~DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)






###INTRA RANKED PERCENTAGE ERROR###
DATA = subset(output, intra_rank ==1 | intra_rank == -1)
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"



data_pch_rank = aggregate(.~ CS+intra_rank+DISP2, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
ggplot(data_pch_rank, aes(x = intra_rank, y= pe, linetype = DISP2))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+ylim(-1,1)


###Number of resources RANKED PERCENTAGE ERROR###
DATA = output
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"

data_pch_rank = aggregate(.~ CS+res_numb_rank, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = res_numb_rank, y= pe))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)



###MEAN CONS RANKED PERCENTAGE ERROR###
DATA = output
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"


data_pch_rank = aggregate(.~ CS+mean_cons_rank+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = mean_cons_rank, y= pe, linetype = ACP))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)





###DRIVER VAR RANKED PERCENTAGE ERROR###
DATA = subset(output, ACP>1)
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"


data_pch_rank = aggregate(.~ DENS+driverVar_rank+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = driverVar_rank, y= pe, linetype = ACP))+geom_line()+theme_classic()+facet_grid(~DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)


###cons BigDriver RANKED PERCENTAGE ERROR###
DATA = subset(output, ACP>=3)
DATA$PERROR = abs(DATA$ERROR)/1000000
#DATA$CS[DATA$CS == 0] <- "LowVar"
#DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"

data_pch_rank = aggregate(.~ CS+cons_bigDriver_rank+ACP+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
#data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = cons_bigDriver_rank, y= pe, linetype = ACP))+geom_line()+theme_classic()+facet_grid(~DENS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)#+ylim(-1,1)






###cons smallDriver RANKED PERCENTAGE ERROR###
DATA = subset(output, ACP>=3)
DATA$PERROR = abs(DATA$ERROR)/1000000
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"

data_pch_rank = aggregate(.~ CS+cons_smallDriver_rank+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = cons_smallDriver_rank, y= pe, linetype = ACP))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)#+ylim(-1,1)





###ACT CONS RANKED PERCENTAGE ERROR###
DATA = output

data_pch_rank = aggregate(.~ CS+pch_rank+ME, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = as.factor(data_pch_rank$CS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = pch_rank, y= pe, col = ME))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)








###MXQ RANKED PERCENTAGE ERROR  COST POOLS###
DATA = output
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"
data_pch_rank = aggregate(.~ACP+ME, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
#data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
#data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = ACP, y= UC, col = "UC"))+geom_line()+geom_line(aes(y=OC, col = "OC"))+theme_classic()+facet_grid(~ME)+geom_hline(yintercept = 25)#+geom_vline(xintercept = 25)+ylim(-1,1)






###MAPE

DATA = output
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"
data_pch_rank = aggregate(.~ACP+CS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
#data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
#data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = ACP, y= acc,col = CS))+geom_line()+theme_classic()+facet_grid()+ylim(0,0.75)#+geom_hline(yintercept = 25)#+geom_vline(xintercept = 25)ylim(-1,1)


############################################BE_AB#######################################################################
DATA = subset(output)
data_pch_rank = aggregate(.~driver_share+CSD+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
DATA$driver_share = as.factor(DATA$driver_share)
DATA$numb_std_res = as.factor(DATA$numb_std_res)

#data_pch_rank$numb_std_res = as.factor(data_pch_rank$numb_std_res)

ggplot(DATA,aes(y = EUCD, x= numb_std_res, col =DENS))+geom_boxplot()+theme_classic()



DATA$CS = as.factor(DATA$CS)
DATA$ACP = as.factor(DATA$ACP)
DATA$DENS = as.factor(DATA$DENS)
DATA$Q_VAR = as.factor(DATA$Q_VAR)
ggplot(DATA,aes(x= acc, color =Q_VAR))+geom_histogram(fill="white", alpha=0.5, position="identity")+facet_grid(~CS)+theme_classic()+geom_vline(xintercept = median(DATA$acc))


###############################################################################################################



summary_statistics = data.frame(rbind(summary(output$pcb),summary(output$pch),summary(output$pe),summary(output$ape),summary(output$PERROR),
                 summary(output$MXQ),summary(output$sd_cons),summary(output$nonzero_cons),
                 summary(output$cons_bigDriver),summary(output$inter),summary(output$intra)))

write.csv(summary_statistics, "summary_statistics.csv")

summary(output$pcb)
summary(output$pch)
summary(output$pe)

summary(output$EUCD)


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


cor_data= data.frame(DATA$CS,DATA$ACP,DATA$DENS,DATA$Q_VAR,DATA$VarSize,DATA$MXQ,DATA$intra_rank,DATA$inter_rank,DATA$sd_cons,DATA$nonzero_cons,DATA$cons_bigDriver,DATA$pcb,DATA$pch,DATA$pe,DATA$ape,DATA$PERROR)
colnames(cor_data) = c("ENV","ACP","DENS","Q_VAR","VarSize","MXQ","intra","inter","DriverVar","#Driver","consBigDriver","pcb","pch","pe","ape","PERROR")
apa.cor.table(cor_data,filename = paste0("cor",".doc"), table.number = 1)



DATA = output
##System Level
REG_DATA = DATA


reg = EUCD ~ ACP+DENS+Q_VAR+DISP2+CS+VarSize+NoBigDriver+acc

reg_data = data.frame(lapply(REG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



reg = VarSize ~ DENS+Q_VAR+CS+DISP2

reg_data = data.frame(lapply(REG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



reg = acc ~ ACP+DENS+Q_VAR+DISP2+CS+VarSize+NoBigDriver

reg_data = data.frame(lapply(REG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



output$acc_weighted = output$acc*output$EUCD



#############################################################################################################################


DATA = output
##System Level
REG_DATA = DATA

reg = ape ~ ACP+DENS+Q_VAR+DISP2+CS+VarSize+ME

reg_data = data.frame(lapply(REG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)






##############################################################################################################################

##Regression Analysis - Percentage Error



##Regression Analysis - Percentage Error - Single Driver Costing System 
DATA = output
#DATA = subset(DATA, ACP>1)
library(apaTables)


ORIG_DATA = subset(DATA, CSD == "SCD" & DENS ==0.25) 

reg = pe ~ pch+MXQ+res_numb+cost_ratio_std_res
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)

ORIG_DATA = subset(DATA, CSD == "SCD" & DENS ==0.5) 

reg = pe ~ pch+MXQ+res_numb+cost_ratio_std_res
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)

ORIG_DATA = subset(DATA, CSD == "SCD" & DENS ==0.75) 

reg = pe ~ pch+MXQ+res_numb+cost_ratio_std_res
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



##Regression Analysis - Percentage Error - Multiple  Driver Costing System 
DATA = output
#DATA = subset(DATA, ACP>1)
library(apaTables)


ORIG_DATA = subset(DATA, CSD == "MCD" & DENS==0.25)

reg = pe ~ pch+MXQ+res_numb+cost_ratio_std_res+cons_bigDriver+driverVar+driver_numb
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)

ORIG_DATA = subset(DATA, CSD == "MCD" & DENS==0.5)

reg = pe ~ pch+MXQ+res_numb+cost_ratio_std_res+cons_bigDriver+driverVar+driver_numb
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)

ORIG_DATA = subset(DATA, CSD == "MCD" & DENS==0.75)

reg = pe ~ pch+MXQ+res_numb+cost_ratio_std_res+cons_bigDriver+driverVar+driver_numb
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



################################################################################################################################

##Regression Analysis - Absolute Percentage Error



##Regression Analysis - Percentage Error - Single Driver Costing System 
DATA = output
#DATA = subset(DATA, ACP>1)
library(apaTables)


ORIG_DATA = subset(DATA, CSD == "SCD" & DENS ==0.25) 

reg = ape ~ pch+MXQ+res_numb+cost_ratio_std_res
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)

ORIG_DATA = subset(DATA, CSD == "SCD" & DENS ==0.5) 

reg = ape ~ pch+MXQ+res_numb+cost_ratio_std_res
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)

ORIG_DATA = subset(DATA, CSD == "SCD" & DENS ==0.75) 

reg = ape ~ pch+MXQ+res_numb+cost_ratio_std_res
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



##Regression Analysis - Percentage Error - Multiple  Driver Costing System 
DATA = output
#DATA = subset(DATA, ACP>1)
library(apaTables)


ORIG_DATA = subset(DATA, CSD == "MCD" & DENS==0.25)

reg = ape ~ pch+MXQ+res_numb+cost_ratio_std_res+cons_bigDriver+driverVar+driver_numb
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)

ORIG_DATA = subset(DATA, CSD == "MCD" & DENS==0.5)

reg = ape ~ pch+MXQ+res_numb+cost_ratio_std_res+cons_bigDriver+driverVar+driver_numb
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)

ORIG_DATA = subset(DATA, CSD == "MCD" & DENS==0.75)

reg = ape ~ pch+MXQ+res_numb+cost_ratio_std_res+cons_bigDriver+driverVar+driver_numb
reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)






