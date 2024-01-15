library(ggplot2)


output$PERROR = abs(output$ERROR)/output$EUCDdata
output$AERROR = abs(output$ERROR)
DATA =output
#DATA$PERROR = abs(DATA$ERROR)/1000000

###PCH RANKED PERCENTAGE ERROR###
DATA = output
#DATA$CS[DATA$CS == 0] <- "CaseStudy"

data_pch_rank = aggregate(.~ pch_rank+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = pch_rank, y= pe,linetype = ACP))+geom_line(linewidth=1)+theme_classic()+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)



###MXQ RANKED PERCENTAGE ERROR###
DATA = output

data_pch_rank = aggregate(.~Q_VAR+MXQ_rank+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
ggplot(data_pch_rank, aes(x = MXQ_rank, y= pe, linetype = ACP))+geom_line()+theme_classic()+facet_wrap(~Q_VAR)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)




###MXQ RANKED PERCENTAGE ERROR - Q_VAR###
DATA = output


data_pch_rank = aggregate(.~ Q_VAR+MXQ_rank, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = MXQ_rank, y= pe, linetype = Q_VAR))+geom_line()+theme_classic()+geom_hline(yintercept = 0)+geom_vline(xintercept = 75)#+ylim(-1,1)


###INTER RANKED PERCENTAGE ERROR###
DATA = subset(output, inter_rank ==1 | inter_rank == -1)

data_pch_rank = aggregate(.~ inter_rank, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = inter_rank, y= pe))+geom_line()+theme_classic()+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+ylim(-1,1)


###INTRA RANKED PERCENTAGE ERROR###
DATA = subset(output, intra_rank ==1 | intra_rank == -1)

data_pch_rank = aggregate(.~ intra_rank, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
ggplot(data_pch_rank, aes(x = intra_rank, y= pe))+geom_line()+theme_classic()+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+ylim(-1,1)


###NONZERO RANKED PERCENTAGE ERROR###
DATA = subset(output, ACP>=3)
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"

data_pch_rank = aggregate(.~ CS+nonzero_cons_rank+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = nonzero_cons_rank, y= pe, linetype = DENS))+geom_line()+theme_classic()+facet_grid(~CS)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)


###SD CONS RANKED PERCENTAGE ERROR###
DATA = subset(output, ACP>=3)

data_pch_rank = aggregate(.~ sd_cons_rank+Q_VAR, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = sd_cons_rank, y= pe, linetype = Q_VAR))+geom_line()+theme_classic()+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)



###cons BigDriver RANKED PERCENTAGE ERROR###
DATA = subset(output, ACP>=3)
DATA$PERROR = abs(DATA$ERROR)/1000000

data_pch_rank = aggregate(.~cons_bigDriver_rank+Q_VAR+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = cons_bigDriver_rank, y= pe, linetype = Q_VAR))+geom_line()+theme_classic()+facet_grid(~ACP)+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)#+ylim(-1,1)






###cons smallDriver RANKED PERCENTAGE ERROR###
DATA = subset(output, ACP>=3)

data_pch_rank = aggregate(.~ cons_smallDriver_rank+ACP, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
ggplot(data_pch_rank, aes(x = cons_smallDriver_rank, y= pe, linetype = ACP))+geom_line()+theme_classic()+geom_hline(yintercept = 0)+geom_vline(xintercept = 25)#+ylim(-1,1)





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
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"
DATA$CS = factor(DATA$CS, levels = c("LowVar","LowVar+VolMatch"))
DATA$ACP = as.factor(DATA$ACP)
DATA$ME = as.factor(DATA$ME)
ggplot(DATA,aes(x= OC, color= ME))+geom_histogram(fill= "white", alpha=0.2, position="identity")+geom_vline(xintercept = 25)+facet_grid(~DENS)+theme_classic()



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

##Regression Analysis
DATA = output
#DATA = subset(DATA, ACP>1)
library(apaTables)


ORIG_DATA = DATA

reg = pe ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank

reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)





scatter.smooth(DATA$pe,DATA$MXQ)


##########################################################################################################################################################


#APE

##Regression Analysis
DATA = output
#DATA = subset(DATA, ME==0)



ORIG_DATA = subset(DATA, PDR ==0 & CS==0)

reg = ape ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


MATCH_DATA = subset(DATA, PDR ==0 & CS==1)

reg = ape ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(MATCH_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


DIAG_DATA = subset(DATA, PDR ==0 &CS==2)

reg = ape ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(DIAG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



DIAG_MATCH_DATA = subset(DATA, PDR ==0 &CS==3)

reg = ape ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(DIAG_MATCH_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)







##########################################################################################################################################################


#PERROR

##Regression Analysis
DATA = output
DATA$PERROR = abs(DATA$ERROR)/1000000
#DATA = subset(DATA, ACP>1)



ORIG_DATA = subset(DATA, PDR ==0 & CS==0)

reg = PERROR ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


MATCH_DATA = subset(DATA, PDR ==0 & CS==1)

reg = PERROR ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(MATCH_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


DIAG_DATA = subset(DATA, PDR ==0 &CS==2)

reg = PERROR ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(DIAG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



DIAG_MATCH_DATA = subset(DATA, PDR ==0 &CS==3)

reg = PERROR ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(DIAG_MATCH_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



###############################################################################################################################################################
firm_envs = unique(output$FIRM_ENV)
j=1
for(j in firm_envs){
  
  sub_data = subset(output, FIRM_ENV == j)
  
  sub_data$pch
  
  
  
  
  
  
  
}

output$cons_predict = 0
output$err_predict =0

for(t in 1:nrow(output)){
  
  if(output$cons_bigDriver[t]>output$mean_cons_bigDriver[t]){output$cons_predict[t] = 1 }else{output$cons_predict[t] = -1 }
  if(output$pe[t]>0){output$err_predict[t] = 1}else{output$err_predict[t] = -1}
  print(t)
}


apply(output, MARGIN =1, FUN = function(x){})

sum(output$err_predict == output$cons_predict)


i=1
output$cons_bigDriver[which(is.na(output$cons_bigDriver))]<-0
for(i in 1:nrow(output)){
  output$error_direction=0
  output$pch_predict = 0
  output$driver_predict =0
  if(output$pe[i]>0){output$error_direction[i] = 1}else{output$error_direction[i] = -1}
  
  if(output$pch[i]>median(output$pch)){output$pch_predict[i] = 1}else{output$pch_predict[i] = -1}
  
  if(output$cons_bigDriver[i]>median(output$cons_bigDriver)){output$driver_predict[i] = 1}else{output$driver_predict[i] = -1}
  
  print(i)
  
}



