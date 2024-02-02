library(ggplot2)


output$PERROR = abs(output$ERROR)/output$EUCD
output$AERROR = abs(output$ERROR)

output$acc_weighted = output$acc * output$EUCD
output$error_disp2 = output$error_disp/output$EUCD
output$VarSize_class = output$VarSize
output$VarSize_class[output$VarSize>median(output$VarSize)]<- 1
output$VarSize_class[output$VarSize<median(output$VarSize)]<- -1
output$UC_share = output$BE_AB
###MAPE###########

DATA = output
#DATA$CS[DATA$CS == 0] <- "LowVar"
#DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
#DATA$CS[DATA$CS == 2] <- "HighVar"
#DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"
#DATA$Q_VAR[DATA$Q_VAR == 1] <- "Low"
#DATA$Q_VAR[DATA$Q_VAR == 2] <- "Medium"
#DATA$Q_VAR[DATA$Q_VAR == 3] <- "High"

data_pch_rank = aggregate(.~ ACP+PDR+CSD+DENS, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
#data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
# data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
#data_pch_rank$Q_VAR = factor(data_pch_rank$Q_VAR, levels = c("Low","Medium","High"))
data_pch_rank$ME = as.factor(data_pch_rank$ME)
#data_pch_rank$VarSize_class = as.factor(data_pch_rank$VarSize_class)
ggplot(data_pch_rank, aes(x = ACP, y= EUCD))+geom_line(aes(y = EUCD, linetype = DENS))+geom_point(aes(shape = DENS))+
  theme_classic()+theme(text = element_text(size=16),legend.position = "bottom")+guides(linetype=guide_legend(title="Degree of Resource Sharing"))+guides(shape=guide_legend(title="Degree of Resource Sharing"))+
  facet_grid()+scale_y_continuous(labels = scales::comma)#+ylim(0.5,1)#+geom_hline(yintercept = 25)#+geom_vline(xintercept = 25)


#kp was ich hier mache
DATA = output

data_a=aggregate(.~ DENS, data = DATA, mean)
#data_a$cost_share_standard_res = as.factor(data_a$cost_share_standard_res)

cost_data = subset(output, cost_share_standard_res < data_a$cost_share_standard_res[1] & DENS ==0.25)
mean_PCH = mean(cost_data$PCH)
share = length(cost_data$PCH)/15000
print(mean_PCH)
print(share)

cost_data = subset(output, cost_share_standard_res < data_a$cost_share_standard_res[2] & DENS ==0.5)
mean_PCH = mean(cost_data$PCH)
share = length(cost_data$PCH)/15000
print(mean_PCH)
print(share)

cost_data = subset(output, cost_share_standard_res < data_a$cost_share_standard_res[3] & DENS ==0.75)
mean_PCH = mean(cost_data$PCH)
share = length(cost_data$PCH)/15000
print(mean_PCH)
print(share)

###############################################################################################################

##Correlation MATRIX

DATA = output


library(apaTables)
DATA$CS = as.numeric(DATA$CS)
DATA$PDR = as.numeric(DATA$PDR)
DATA$ACP = as.numeric(DATA$ACP)
DATA$Q_VAR = as.numeric(DATA$Q_VAR)
DATA$DENS = as.numeric(DATA$DENS)


cor_data= data.frame(DATA$DENS,DATA$ACP,DATA$individuality,DATA$cost_share_standard_res,DATA$driver_numb,DATA$ape,DATA$UC_share,DATA$EUCD)
colnames(cor_data) = c("DENS","ACP","individuality","cost_share_standard_res","#driver","ape","UC_share","EUCD")
apa.cor.table(cor_data,filename = paste0("cor",".doc"), table.number = 1)



#PCH RANKED PERCENTAGE ERROR###
DATA = output
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"


data_pch_rank = aggregate(.~ CS+pch_rank, data = DATA, mean)
data_pch_rank_sd = aggregate(.~ CS+pch_rank, data = DATA, sd)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
data_pch_rank$pe_lb = data_pch_rank$pe - data_pch_rank_sd$pe*0.5
data_pch_rank$pe_ub = data_pch_rank$pe + data_pch_rank_sd$pe*0.5
ggplot(data_pch_rank, aes(x = pch_rank))+geom_line(aes(y=pe), size =1)+geom_ribbon(aes(ymin=pe_lb, ymax=pe_ub), alpha=0.2)+theme_classic()+facet_grid(~CS)+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)+xlab("Rank Ordered Products - Low to High reported product costs (pch)")




###MXQ rank###
DATA = subset(output, ACP ==1 |ACP ==5 | ACP == 10)
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"

#DATA$Q_VAR[DATA$Q_VAR == 1] <- "Low"
#DATA$Q_VAR[DATA$Q_VAR == 2] <- "Medium"
#DATA$Q_VAR[DATA$Q_VAR == 3] <- "High"

data_pch_rank = aggregate(.~ CS+MXQ_rank+ACP, data = DATA, mean)
data_pch_rank_sd = aggregate(.~ CS+MXQ_rank+ACP, data = DATA, sd)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = factor(data_pch_rank$Q_VAR, levels = c("Low","Medium","High"))
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
data_pch_rank$pe_lb = data_pch_rank$pe - data_pch_rank_sd$pe*0.5
data_pch_rank$pe_ub = data_pch_rank$pe + data_pch_rank_sd$pe*0.5
ggplot(data_pch_rank, aes(x = MXQ_rank))+geom_line(aes(y=pe,linetype=ACP), size =1)+geom_ribbon(aes(ymin=pe_lb, ymax=pe_ub, fill = ACP), alpha=0.2)+theme_classic()+facet_grid(~CS)+
  geom_hline(yintercept = 0)+ylim(-1,1)+xlab("Rank Ordered Products - Low to High Production Volumes (MXQ)")+
  ylab("percentage error (pe)")#+geom_hline(yintercept = c(-0.05,0.05),colour = "blue")+geom_vline(xintercept = 25)


####Driver rank###
DATA = subset(output, ACP >1)
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"

data_pch_rank = aggregate(.~ CS+nonzero_cons_rank+DENS, data = DATA, mean)
data_pch_rank_sd = aggregate(.~ CS+nonzero_cons_rank+DENS, data = DATA, sd)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = factor(data_pch_rank$Q_VAR, levels = c("Low","Medium","High"))
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
data_pch_rank$pe_lb = data_pch_rank$pe - data_pch_rank_sd$pe*0.5
data_pch_rank$pe_ub = data_pch_rank$pe + data_pch_rank_sd$pe*0.5
ggplot(data_pch_rank, aes(x = nonzero_cons_rank))+geom_line(aes(y=pe,linetype=DENS), size =1)+geom_ribbon(aes(ymin=pe_lb, ymax=pe_ub, fill =DENS), alpha=0.2)+theme_classic()+facet_grid(~CS)+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)+xlab("Rank Ordered Products - Few to Many Drivers (#Driver)")+ylab("percentage error (pe)")





####DriverVar rank###
DATA = subset(output, ACP >1)
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"

DATA$Q_VAR[DATA$Q_VAR == 1] <- "Low"
DATA$Q_VAR[DATA$Q_VAR == 2] <- "Medium"
DATA$Q_VAR[DATA$Q_VAR == 3] <- "High"

data_pch_rank = aggregate(.~ CS+sd_cons_rank+Q_VAR, data = DATA, mean)
data_pch_rank_sd = aggregate(.~ CS+sd_cons_rank+Q_VAR, data = DATA, sd)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = factor(data_pch_rank$Q_VAR, levels = c("Low","Medium","High"))
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
data_pch_rank$pe_lb = data_pch_rank$pe - data_pch_rank_sd$pe*0.5
data_pch_rank$pe_ub = data_pch_rank$pe + data_pch_rank_sd$pe*0.5
ggplot(data_pch_rank, aes(x = sd_cons_rank))+geom_line(aes(y=pe,linetype=Q_VAR), size =1)+geom_ribbon(aes(ymin=pe_lb, ymax=pe_ub, fill =Q_VAR), alpha=0.2)+theme_classic()+facet_grid(~CS)+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)+xlab("Rank Ordered Products - Low to High variation in Driver usage (DriverVar)")+ylab("percentage error (pe)")





####cost driver information rank###
DATA = subset(output, ACP >1)
#DATA$CS[DATA$CS == 0] <- "LowVar"
#DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
#DATA$CS[DATA$CS == 2] <- "HighVar"
#DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"
DATA$Q_VAR[DATA$Q_VAR == 1] <- "Low"
DATA$Q_VAR[DATA$Q_VAR == 2] <- "Medium"
DATA$Q_VAR[DATA$Q_VAR == 3] <- "High"



data_pch_rank = aggregate(.~ DENS+Q_VAR+pe_rank, data = DATA, mean)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
#data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = factor(data_pch_rank$Q_VAR, levels = c("Low","Medium","High"))
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
#data_pch_rank$pe_lb = data_pch_rank$pe - data_pch_rank_sd$pe*0.5
#data_pch_rank$pe_ub = data_pch_rank$pe + data_pch_rank_sd$pe*0.5

ggplot(data_pch_rank, aes(x = pe_rank))+geom_line(aes(y=sd_cons_rank), size =1,linetype = "dashed")+
  geom_line(aes(y=cons_bigDriver_rank), size =1)+geom_line(aes(y=nonzero_cons_rank), size =1,linetype = "dotted")+
  theme_classic()+facet_grid(~DENS+Q_VAR)+
  geom_vline(xintercept = 25)+xlab("Rank Ordered Products - Negative to Positive Percentage Error (PE)")+
  ylab("Costing Driver Information")+
  guides(linetype=guide_legend(title="Production Environment"))










####BigDriver rank###
DATA = output
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"

data_pch_rank = aggregate(.~ CS+cons_bigDriver_rank+DISP2, data = DATA, mean)
data_pch_rank_sd = aggregate(.~ CS+cons_bigDriver_rank+DISP2, data = DATA, sd)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = factor(data_pch_rank$Q_VAR, levels = c("Low","Medium","High"))
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
data_pch_rank$pe_lb = data_pch_rank$pe - data_pch_rank_sd$pe*0.5
data_pch_rank$pe_ub = data_pch_rank$pe + data_pch_rank_sd$pe*0.5
ggplot(data_pch_rank, aes(x = cons_bigDriver_rank))+geom_line(aes(y=pe,linetype=DISP2), size =1)+geom_ribbon(aes(ymin=pe_lb, ymax=pe_ub, fill =DISP2), alpha=0.2)+theme_classic()+facet_grid(~CS)+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)+xlab("Rank Ordered Products - Low to High consumption of 'BigDrivers' (BigDriver)")+ylab("percentage error (pe)")




###INTRA RANKED PE ##
DATA = subset(output, intra_rank ==1 | intra_rank == -1)
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"

DATA$intra_rank[DATA$intra_rank==-1] <-"Low"
DATA$intra_rank[DATA$intra_rank== 1] <-"High"

DATA$intra_rank = factor(DATA$intra_rank, levels = c("Low","High"))
DATA$CS = factor(DATA$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
DATA$DENS = as.factor(DATA$DENS)
DATA$DISP2 = as.factor(DATA$DISP2)
DATA$intra = as.factor(DATA$intra)
ggplot(DATA, aes(x = intra_rank, y= pe))+geom_boxplot(outlier.shape = NA)+theme_classic()+facet_grid(~CS)+
  geom_hline(yintercept = 0)+xlab("Rank Ordered Products - Low to High Resource Usage")+
  ylab("Percentage error (pe)")+ylim(-1,1)




###INTRA RANKED PERROR ##
DATA = subset(output, intra_rank ==1 | intra_rank == -1)
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"

DATA$intra_rank[DATA$intra_rank==-1] <-"Low"
DATA$intra_rank[DATA$intra_rank== 1] <-"High"

DATA$intra_rank = factor(DATA$intra_rank, levels = c("Low","High"))
DATA$CS = factor(DATA$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
DATA$DENS = as.factor(DATA$DENS)
DATA$DISP2 = as.factor(DATA$DISP2)
DATA$intra = as.factor(DATA$intra)
ggplot(DATA, aes(x = intra_rank, y= ape))+geom_boxplot(outlier.shape = NA)+theme_classic()+facet_grid(~CS)+
  geom_hline(yintercept = 0)+xlab("Rank Ordered Products - Low to High Resource Usage")+
  ylab("Absolute percentage error (ape)")+ylim(0,1)





###INTER RANKED pe ##
DATA = subset(output, inter_rank ==1 | inter_rank == -1)
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"

DATA$inter_rank[DATA$inter_rank==-1] <-"Low"
DATA$inter_rank[DATA$inter_rank== 1] <-"High"

DATA$inter_rank = factor(DATA$inter_rank, levels = c("Low","High"))
DATA$CS = factor(DATA$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
DATA$DENS = as.factor(DATA$DENS)
DATA$DISP2 = as.factor(DATA$DISP2)
ggplot(DATA, aes(x = inter_rank, y= pe))+geom_boxplot(outlier.shape = NA)+theme_classic()+facet_grid(~CS)+
  geom_hline(yintercept = 0)+xlab("Rank Ordered Products - Low to High Variety in resource consumption")+
  ylab("Percentage error (pe)")+ylim(-1,1)



###INTER RANKED PERROR ##
DATA = subset(output, inter_rank ==1 | inter_rank == -1)
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"

DATA$inter_rank[DATA$inter_rank==-1] <-"Low"
DATA$inter_rank[DATA$inter_rank== 1] <-"High"

DATA$inter_rank = factor(DATA$inter_rank, levels = c("Low","High"))
DATA$CS = factor(DATA$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
DATA$DENS = as.factor(DATA$DENS)
DATA$DISP2 = as.factor(DATA$DISP2)
ggplot(DATA, aes(x = inter_rank, y= ape))+geom_boxplot(outlier.shape = NA)+theme_classic()+facet_grid(~CS)+
  geom_hline(yintercept = 0)+xlab("Rank Ordered Products - Low to High Variety in resource consumption")+
  ylab("Absolute percentage error (ape)")+ylim(0,1)






###rank over ACP###
DATA = output
#DATA$CS[DATA$CS == 0] <- "LowVar"
#DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
#DATA$CS[DATA$CS == 2] <- "HighVar"
#DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"


data_pch_rank = aggregate(.~ DENS+MXQ_rank+ACP, data = DATA, mean)
data_pch_rank_sd = aggregate(.~ DENS+pch_rank+ACP, data = DATA, sd)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
data_pch_rank$pe_lb = data_pch_rank$pe - data_pch_rank_sd$pe*0.5
data_pch_rank$pe_ub = data_pch_rank$pe + data_pch_rank_sd$pe*0.5
ggplot(data_pch_rank, aes(x = MXQ_rank))+geom_line(aes(y=pe,linetype = DENS), size =1)+geom_ribbon(aes(ymin=pe_lb, ymax=pe_ub, fill = DENS), alpha=0.2)+theme_classic()+facet_grid(~ACP)+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 25)+ylim(-1,1)+xlab("Rank Ordered Products - Low to High Production Volumes (MXQ)")






###PCH RANKED PERROR###
DATA = output
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"


data_pch_rank = aggregate(.~ CS+MXQ_rank, data = DATA, mean)
data_pch_rank_sd = aggregate(.~ CS+MXQ_rank, data = DATA, sd)
data_pch_rank$DENS = as.factor(data_pch_rank$DENS)
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
data_pch_rank$DISP2 = as.factor(data_pch_rank$DISP2)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
data_pch_rank$PDR = as.factor(data_pch_rank$PDR)
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$ME = as.factor(data_pch_rank$ME)
data_pch_rank$pe_lb = data_pch_rank$AERROR - data_pch_rank_sd$AERROR*0.5
data_pch_rank$pe_ub = data_pch_rank$AERROR + data_pch_rank_sd$AERROR*0.5
ggplot(data_pch_rank, aes(x = MXQ_rank))+geom_line(aes(y=AERROR), size =1)+geom_ribbon(aes(ymin=pe_lb, ymax=pe_ub), alpha=0.2)+theme_classic()+facet_grid(~CS)+
  geom_hline(yintercept = 0)+xlab("Rank Ordered Products - Low to High reported product costs (pch)")



DATA = output
DATA$ACP = as.factor(DATA$ACP)
DATA$CS = as.factor(DATA$CS)
DATA$DISP2 = as.factor(DATA$DISP2)
ggplot(DATA,aes(x=UC_share, fill = ACP))+geom_histogram(alpha=0.3, position="identity")+theme_classic()#+xlim(-1,2)



DATA = output
DATA$error_class = DATA$EUCD
DATA$CS[DATA$CS == 0] <- "LowVar"
DATA$CS[DATA$CS == 1] <- "LowVar+VolMatch"
DATA$CS[DATA$CS == 2] <- "HighVar"
DATA$CS[DATA$CS == 3] <- "HighVar+VolMatch"
DATA$error_class[which(DATA$EUCD> (1.5*mean(output$EUCD)))]<-"HIGH"
DATA$error_class[which(DATA$EUCD< (1.5*mean(output$EUCD)))]<-"MED"
DATA$error_class[which(DATA$EUCD<=(0.5*mean(output$EUCD)))]<-"LOW"

data_pch_rank = aggregate(.~PERROR_rank+error_class+Q_VAR,data =DATA, mean)
data_pch_rank$error_class = factor(data_pch_rank$error_class,levels = c("LOW","MED","HIGH"))
data_pch_rank$CS = factor(data_pch_rank$CS, levels = c("LowVar","LowVar+VolMatch","HighVar","HighVar+VolMatch"))
data_pch_rank$ACP = as.factor(data_pch_rank$ACP)
data_pch_rank$Q_VAR = as.factor(data_pch_rank$Q_VAR)
ggplot(data_pch_rank,aes(x = PERROR_rank, y= PERROR))+geom_line(aes(linetype=Q_VAR))+geom_point(aes(shape=Q_VAR))+facet_grid(~error_class)+
  theme_classic()+geom_hline(yintercept =0)



DATA = output
DATA$Q_VAR[DATA$Q_VAR == 1] <- "Low"
DATA$Q_VAR[DATA$Q_VAR == 2] <- "Medium"
DATA$Q_VAR[DATA$Q_VAR == 3] <- "High"
DATA$Q_VAR = factor(DATA$Q_VAR, levels = c("Low","Medium","High"))
ggplot(DATA,aes(x = EUCD,y=error_disp))+geom_point(aes(shape = Q_VAR, color=Q_VAR))+theme_classic()







###############################################################################################################

##Correlation MATRIX

DATA = output


library(apaTables)
DATA$CS = as.numeric(DATA$CS)
DATA$PDR = as.numeric(DATA$PDR)
DATA$ACP = as.numeric(DATA$ACP)
DATA$Q_VAR = as.numeric(DATA$Q_VAR)
DATA$DENS = as.numeric(DATA$DENS)


cor_data= data.frame(DATA$DENS,DATA$ACP,DATA$ape,DATA$UC_share,DATA$EUCD)
colnames(cor_data) = c("DENS","ACP","ape","UC_share","EUCD")
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


ORIG_DATA = subset(DATA, PDR ==0 & CS==0)

reg = pe ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(ORIG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


MATCH_DATA = subset(DATA, PDR ==0 & CS==1)

reg = pe ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(MATCH_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)


DIAG_DATA = subset(DATA, PDR ==0 &CS==2)

reg = pe ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(DIAG_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)



DIAG_MATCH_DATA = subset(DATA, PDR ==0 &CS==3)

reg = pe ~ pch+MXQ+sd_cons+nonzero_cons+cons_bigDriver+cons_smallDriver+inter_rank+intra_rank

reg_data = data.frame(lapply(DIAG_MATCH_DATA[,all.vars(reg)], scale))
linear_reg_std = lm(reg, data = reg_data)
apa.reg.table(linear_reg_std,filename = paste0("replication",1,".doc"), table.number = 1)







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



