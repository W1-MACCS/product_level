
######Nested Design following ABL 2019######

#moin 

## ====================================== INPUT FOR FIRM GENERATION ==================================================

NUMB_FIRMS = 200
NUMB_PRO = 50
NUMB_RES = 50
DISP1 = c(10)
Q_VAR = c("MID")
DENS = c(-1)
DISP2 = c(-1)
COR1 = c(-1)
COR2 = c(-1)

CS = c(3)



runs = c(1:NUMB_FIRMS)
FIRM = expand.grid(NUMB_PRO,NUMB_RES, DISP1, DISP2, DENS, COR1,COR2,Q_VAR, CS)

FirmID = c(1:nrow(FIRM))

FIRM = cbind(FirmID, FIRM)


FIRM = do.call("rbind", replicate(NUMB_FIRMS,FIRM, simplify = FALSE))
randID = c(1:nrow(FIRM))
FIRM = cbind(randID, FIRM)
colnames(FIRM) = c('randID',"FirmID",'NUMB_PRO','NUMB_RES',"DISP1", "DISP2", "DENS", "COR1","COR2","Q_VAR", "CS")



## ====================================== INPUT FOR COSTING SYSTEM GENERATION ==================================================
COSTING_SYSTEM = list()

CP = c(1,3,6,10,15,20)#as in Anand et al. (2017)
CP_HEURISTIC = c(2)#as in Anand et al. (2017)
CD_HEURISTIC = c(0)#as in Anand et al. (2017)

COSTING_SYSTEM = expand.grid(CP,CP_HEURISTIC,CD_HEURISTIC)
CostSysID = c(1:nrow(COSTING_SYSTEM))
COSTING_SYSTEM = cbind(CostSysID,COSTING_SYSTEM)
colnames(COSTING_SYSTEM) = c('CostSysID','CP','PACP','PDR')

## ====================================== oUTPUT DATA SET ==================================================
rand = expand.grid(c(1:nrow(COSTING_SYSTEM)),c(1:nrow(FIRM)))
colnames(rand) = c('CostSysID','randID')

DATA_2 = dplyr::full_join(COSTING_SYSTEM, rand, by = c('CostSysID' = 'CostSysID'))

DATA = dplyr::full_join(DATA_2,FIRM, by = c('randID' = "randID"))

DATA = data.frame(DATA$randID,DATA$FirmID,DATA$CostSysID,DATA$PACP, DATA$CP, DATA$PDR,DATA[,9:15])

colnames(DATA) = c("randID","FirmID",'CostSysID','PACP','ACP','PDR',"DISP1", "DISP2", "DENS", "COR1","COR2","Q_VAR","CS")


## ====================================== MULTI CORE SETTING ==================================================


# 

DATA_list = list()

for(i in 1:nrow(FIRM)){

  
  MXQ = .gen_Demand_Anand(FIRM$Q_VAR[i],FIRM$NUMB_PRO[i])
  cost_hierarchy = .gen_cost_hierarchy()
  

  if(FIRM$CS[i] == 0){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Anand(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 1){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Anand_CS(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 2){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Anand_CH(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 3){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Anand_CH2(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }

  
  
  
  RCC_list = .gen_RCC_Anand(FIRM$DISP1[i], FIRM$DISP2[i],FIRM$NUMB_RES[i])
  
  
  PCB = RES_CONS_PAT_list$RES_CONS_PATp %*% RCC_list$RCC
  
  FIRM$DENS[i] = RES_CONS_PAT_list$DENS
  FIRM$COR1[i] = RES_CONS_PAT_list$COR1
  FIRM$COR2[i] = RES_CONS_PAT_list$COR2
  FIRM$non_unit_size[i] = RES_CONS_PAT_list$non_unit_size
  FIRM$DISP2[i] = RCC_list$DISP2_draw
  FIRM[i,13:62] = PCB
  FIRM[i,63:112] = MXQ

  
  DATA_list$RES_CONS_PATp[[i]] = RES_CONS_PAT_list$RES_CONS_PATp
  DATA_list$RES_CONS_PAT[[i]] = RES_CONS_PAT_list$RES_CONS_PAT
  DATA_list$RES_CONS_PAT_TOTAL[[i]] = RES_CONS_PAT_list$RES_CONS_PAT_TOTAL
  DATA_list$cost_hierarchy[[i]] = RES_CONS_PAT_list$cost_hierarchy
  DATA_list$RCC[[i]] = RCC_list$RCC
  print(i)
  
}

cores=detectCores()
cl <- makeCluster(cores[1]-1)
registerDoSNOW(cl)
iterations <- nrow(DATA)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

output = data.frame()
preDATA = data.frame()



output <- foreach(i = 1:nrow(DATA), .combine = rbind, .options.snow = opts) %dopar% { #dopar
#for(i in 1:nrow(DATA)){
    
      rand_id = DATA$randID[i]
      pdr = DATA$PDR[i]
      firmid = DATA$FirmID[i]
      costsysid = DATA$CostSysID[i]
      pacp = DATA$PACP[i]
      acp = DATA$ACP[i]
      
  
      RES_CONS_PATp = DATA_list$RES_CONS_PATp[[DATA$randID[i]]]
      RES_CONS_PAT = DATA_list$RES_CONS_PAT[[DATA$randID[i]]]
      RES_CONS_PAT_TOTAL = DATA_list$RES_CONS_PAT_TOTAL[[DATA$randID[i]]]
      cost_hierarchy = DATA_list$cost_hierarchy[[DATA$randID[i]]]
      RCC = DATA_list$RCC[[DATA$randID[DATA$randID[i]]]]
      MXQ = t(FIRM[which(DATA$randID[i] == FIRM$randID),63:112])
      
      NUMB_PRO =nrow(RES_CONS_PATp)

      if(DATA$PACP[i] == 0){
        CostingSystem_list = MAP_RES_CP_SIZE_MISC(DATA$ACP[i],RCC,RES_CONS_PATp)
      }else if(DATA$PACP[i]== 1){
        CostingSystem_list = MAP_RES_CP_SIZE_CORREL_MISC_ANAND(DATA$ACP[i],RCC,RES_CONS_PATp)
      }else if(DATA$PACP[i] == 2){
        CostingSystem_list = MAP_RES_CP_SIZE_RANDOM_MISC(DATA$ACP[i],RCC,RES_CONS_PATp)
      }else if(DATA$PACP[i] == 3){
        CostingSystem_list = MAP_RES_CP_SIZE_CORREL_CUTOFF_MISC_ANAND(DATA$ACP[i],RCC,RES_CONS_PATp)
      }
      
      if(DATA$PDR[i] == 0){
        ACT_CONS_PAT = MAP_CP_P_BIGPOOL(CostingSystem_list$RC_to_ACP,RES_CONS_PATp,RCC, NUMB_PRO)
      }else if(DATA$PDR[i] == 1){
        ACT_CONS_PAT = MAP_CP_P_VOLUME(CostingSystem_list$RC_to_ACP,MXQ,NUMB_PRO)
      }
      

      
      PCH =  ACT_CONS_PAT %*% CostingSystem_list$ACP
      #PCB = RES_CONS_PATp %*% RCC
      PCB = t(FIRM[which(FIRM$randID==DATA$randID[i]),13:62])
      MXQ = t(FIRM[which(FIRM$randID==DATA$randID[i]),63:112])
      DENS = FIRM[which(FIRM$randID == DATA$randID[i]),]$DENS
      COR1 = FIRM[which(FIRM$randID == DATA$randID[i]),]$COR1
      COR2 = FIRM[which(FIRM$randID == DATA$randID[i]),]$COR2
      DISP1 = FIRM[which(FIRM$randID == DATA$randID[i]),]$DISP1
      DISP2 = FIRM[which(FIRM$randID == DATA$randID[i]),]$DISP2
      Q_VAR = FIRM[which(FIRM$randID == DATA$randID[i]),]$Q_VAR
      CS = FIRM[which(FIRM$randID == DATA$randID[i]),]$CS
      non_unit_size = FIRM[which(FIRM$randID == DATA$randID[i]),]$non_unit_size
      if(CS ==0){CS_levels = 0}else{CS_levels=1}
      #else if(non_unit_size<0.33){CS_levels = "LOW"}else if(non_unit_size>0.46){CS_levels = "HIGH"}else{CS_levels = "MID"}
      
      
      
      MAPE = mean(abs(PCB-PCH)/PCB)
      PE = PCH-PCB
      UC = sum((PCB-PCH)/PCB>0)/NUMB_PRO
      OC = sum((PCB-PCH)/PCB<0)/NUMB_PRO
      
      UC5 = sum((PCB-PCH)/PCB>0.05)/NUMB_PRO
      OC5 = sum((PCB-PCH)/PCB< -0.05)/NUMB_PRO
      
      BE_AB =  UC5-OC5
      
      pcb = PCB/MXQ
      pch = PCH/MXQ
      pe = (pch-pcb)/pcb
      
      #######
      pcb_dec_1 =  mean(pcb[sort(MXQ,index.return = TRUE)$ix[1:5]])#percentage error for lowest volume products
      pcb_dec_2 =  mean(pcb[sort(MXQ,index.return = TRUE)$ix[6:10]])
      pcb_dec_3 =  mean(pcb[sort(MXQ,index.return = TRUE)$ix[11:15]])
      pcb_dec_4 =  mean(pcb[sort(MXQ,index.return = TRUE)$ix[16:20]])
      pcb_dec_5 =  mean(pcb[sort(MXQ,index.return = TRUE)$ix[21:25]])
      pcb_dec_6 =  mean(pcb[sort(MXQ,index.return = TRUE)$ix[26:30]])
      pcb_dec_7 =  mean(pcb[sort(MXQ,index.return = TRUE)$ix[31:35]])
      pcb_dec_8 =  mean(pcb[sort(MXQ,index.return = TRUE)$ix[36:40]])
      pcb_dec_9 =  mean(pcb[sort(MXQ,index.return = TRUE)$ix[41:45]])
      pcb_dec_10 =  mean(pcb[sort(MXQ,index.return = TRUE)$ix[46:50]])#percentage error for highest volume products
      
      
      
      
      
      
      
      ###VB_PATTERN -pattern for rank-ordered products by volume
      
      high_volume_p = sort(MXQ,index.return = TRUE)$ix[37:50]
      low_volume_p = sort(MXQ,index.return = TRUE)$ix[1:13]
      
      pe_high_volume_p = mean(pe[high_volume_p])
      pe_low_volume_p = mean(pe[low_volume_p])
      
      count_high_volume_p = sum(pe[high_volume_p]>0.05)/length(pe[high_volume_p])
      count_low_volume_p = sum(pe[low_volume_p]< -0.05)/length(pe[low_volume_p])
      
      
      pe_dec_1 =  mean(pe[sort(MXQ,index.return = TRUE)$ix[1:5]])#percentage error for lowest volume products
      pe_dec_2 =  mean(pe[sort(MXQ,index.return = TRUE)$ix[6:10]])
      pe_dec_3 =  mean(pe[sort(MXQ,index.return = TRUE)$ix[11:15]])
      pe_dec_4 =  mean(pe[sort(MXQ,index.return = TRUE)$ix[16:20]])
      pe_dec_5 =  mean(pe[sort(MXQ,index.return = TRUE)$ix[21:25]])
      pe_dec_6 =  mean(pe[sort(MXQ,index.return = TRUE)$ix[26:30]])
      pe_dec_7 =  mean(pe[sort(MXQ,index.return = TRUE)$ix[31:35]])
      pe_dec_8 =  mean(pe[sort(MXQ,index.return = TRUE)$ix[36:40]])
      pe_dec_9 =  mean(pe[sort(MXQ,index.return = TRUE)$ix[41:45]])
      pe_dec_10 =  mean(pe[sort(MXQ,index.return = TRUE)$ix[46:50]])#percentage error for highest volume products
      
      VB_PATTERN = (pe_dec_10+pe_dec_9)/2- (pe_dec_1+pe_dec_2)/2
      
      ##CB_PATTERN - pattern for rank-ordered products by complexity
      
     
      #entropy = calc_complexity(RES_CONS_PAT) #entropy complexity (ElMaraghy et al., 2013)
      #intra = calc_intra(RES_CONS_PAT_TOTAL) #intra-product heterogeneity (Gupta, 1993; Mertens, 2020)
      inter = calc_inter(RES_CONS_PATp) ##inter-product heterogeneity (Gupta, 1993; Mertens, 2020)
      #inter = intra+inter
      #inter = intra
      
      

      ce_dec_1 =  mean(pe[sort(inter,decreasing = TRUE,index.return = TRUE)$ix[1:5]])#percentage error for highest-complexity products 
      ce_dec_2 =  mean(pe[sort(inter,decreasing = TRUE,index.return = TRUE)$ix[6:10]])
      ce_dec_3 =  mean(pe[sort(inter,decreasing = TRUE,index.return = TRUE)$ix[11:15]])
      ce_dec_4 =  mean(pe[sort(inter,decreasing = TRUE,index.return = TRUE)$ix[16:20]])
      ce_dec_5 =  mean(pe[sort(inter,decreasing = TRUE,index.return = TRUE)$ix[21:25]])
      ce_dec_6 =  mean(pe[sort(inter,decreasing = TRUE,index.return = TRUE)$ix[26:30]])
      ce_dec_7 =  mean(pe[sort(inter,decreasing = TRUE,index.return = TRUE)$ix[31:35]])
      ce_dec_8 =  mean(pe[sort(inter,decreasing = TRUE,index.return = TRUE)$ix[36:40]])
      ce_dec_9 =  mean(pe[sort(inter,decreasing = TRUE,index.return = TRUE)$ix[41:45]])
      ce_dec_10 =  mean(pe[sort(inter,decreasing = TRUE,index.return = TRUE)$ix[46:50]])#percentage error for lowest-complexity products
      
 
      CB_PATTERN = (ce_dec_10+ce_dec_9)/2- (ce_dec_1+ce_dec_2)/2
  
      #other measures
      ul_cost = sum(RCC[cost_hierarchy$ul])/1000000
      bl_cost = sum(RCC[cost_hierarchy$bl])/1000000
      pl_cost = sum(RCC[cost_hierarchy$pl])/1000000
      fl_cost = sum(RCC[cost_hierarchy$fl])/1000000
      ul_size = length(cost_hierarchy$ul)
      bl_size = length(cost_hierarchy$bl)
      pl_size = length(cost_hierarchy$pl)
      fl_size = length(cost_hierarchy$fl)
      
      demand_cor = mean(cor(MXQ,RES_CONS_PATp))
      cost_cor = cor((PCB/sum(PCB)), (MXQ/sum(MXQ)))
      inter_cor = cor(inter, pe)
      
#Data writing

      preDATA = data.frame(rand_id,
                           firmid,
                           costsysid,
                           pacp,
                           acp,
                           pdr,
                           DISP1,
                           DISP2,
                           DENS,
                           COR1,
                           COR2,
                           Q_VAR,
                           CS,
                           non_unit_size,
                           CS_levels,
                           t(PCB),
                           t(PCH),
                           MAPE,
                           t(pe),
                           UC,
                           OC,
                           UC5,
                           OC5,
                           t(MXQ),
                           VB_PATTERN,
                           CB_PATTERN,
                           demand_cor,
                           cost_cor,
                           BE_AB,
                           pe_dec_1,pe_dec_2,pe_dec_3,pe_dec_4,pe_dec_5,pe_dec_6,pe_dec_7,pe_dec_8,pe_dec_9,pe_dec_10,
                           ce_dec_1,ce_dec_2,ce_dec_3,ce_dec_4,ce_dec_5,ce_dec_6,ce_dec_7,ce_dec_8,ce_dec_9,ce_dec_10,
                           inter,
                           inter_cor,
                           ul_cost,bl_cost,pl_cost,fl_cost,
                           ul_size,bl_size,pl_size,fl_size,
                           cost_hierarchy$ul_bl,cost_hierarchy$ul_pl,cost_hierarchy$ul_fl,cost_hierarchy$bl_pl,cost_hierarchy$bl_fl,cost_hierarchy$pl_fl)
       
      colnames(preDATA) = c('randID','FirmID','CostSysID','PACP','ACP','PDR',"DISP1","DISP2","DENS","COR1","COR2","Q_VAR","CS","non_unit_size","CS_levels",
                            c(paste0("PCB_", 0:49)),c(paste0("PCH_", 0:49)),
                            "MAPE",c(paste0("PE_", 0:49)),"UC","OC","UC5","OC5",c(paste0("MXQ_", 0:49)),"VB_PATTERN","CB_PATTERN",'demand_pattern','cost_pattern',
                            "BE_AB",
                            c(paste0("pe_dec_", 1:10)),c(paste0("ce_dec_", 1:10)),"inter","inter_cor","ul_cost","bl_cost","pl_cost","fl_cost",
                            "ul_size","bl_size","pl_size","fl_size",
                            "ul_bl","ul_pl","ul_fl","bl_pl","bl_fl","pl_fl")
      




preDATA    
  }

output_link = paste("output/CSD_",format(Sys.time(),"%Y-%m-%d-%H%M"),".csv", sep = "")
write.csv(output, file = output_link)
print("Cost System Design FILE has been written")