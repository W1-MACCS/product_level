
######Nested Design following ABL 2019######



## ====================================== INPUT FOR FIRM GENERATION ==================================================

NUMB_FIRMS = 100
NUMB_PRO = c(50)
NUMB_RES = c(50)
DISP1 = c(10)
Q_VAR = c("MID")
DENS = c(0.25,0.5,0.75)
DISP2 = c(0.25,0.5,0.75)
COR1 = c(-1)
COR2 = c(-1)

CS = c(0,1)



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

CP = c(1,3,6,10,15,20)#1,3,6,10,15,20#as in Anand et al. (2017)
CP_HEURISTIC = c(2)#2 -size-random-misc #as in Anand et al. (2017)
CD_HEURISTIC = c(0)#0 - Big Pool #as in Anand et al. (2017)

COSTING_SYSTEM = expand.grid(CP,CP_HEURISTIC,CD_HEURISTIC)
CostSysID = c(1:nrow(COSTING_SYSTEM))
COSTING_SYSTEM = cbind(CostSysID,COSTING_SYSTEM)
colnames(COSTING_SYSTEM) = c('CostSysID','CP','PACP','PDR')

## ====================================== OUTPUT DATA SET ==================================================
rand = expand.grid(c(1:nrow(COSTING_SYSTEM)),c(1:nrow(FIRM)))
colnames(rand) = c('CostSysID','randID')

DATA_2 = dplyr::full_join(COSTING_SYSTEM, rand, by = c('CostSysID' = 'CostSysID'))

DATA = dplyr::full_join(DATA_2,FIRM, by = c('randID' = "randID"))

DATA = data.frame(DATA$randID,DATA$FirmID,DATA$CostSysID,DATA$PACP, DATA$CP, DATA$PDR,DATA[,8:15])

colnames(DATA) = c("randID","FirmID",'CostSysID','PACP','ACP','PDR',"NUMB_RES","DISP1", "DISP2", "DENS", "COR1","COR2","Q_VAR","CS")


## ====================================== MULTI CORE SETTING ==================================================


# 

DATA_list = list()

for(i in 1:nrow(FIRM)){

  
  MXQ = .gen_Demand(FIRM$Q_VAR[i],FIRM$NUMB_PRO[i])
  #cost_hierarchy = .gen_cost_hierarchy()
  cost_hierarchy = list()
  

  if(FIRM$CS[i] == 0){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Anand(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 1){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_diag(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
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
  FIRM[i,13:(13+FIRM$NUMB_PRO[i]-1)] = PCB
  FIRM[i,(13+FIRM$NUMB_PRO[i]):(13+FIRM$NUMB_PRO[i]+FIRM$NUMB_PRO[i]-1)] = MXQ

  
  DATA_list$RES_CONS_PATp[[i]] = RES_CONS_PAT_list$RES_CONS_PATp
  DATA_list$RES_CONS_PAT[[i]] = RES_CONS_PAT_list$RES_CONS_PAT
  DATA_list$RES_CONS_PAT_TOTAL[[i]] = RES_CONS_PAT_list$RES_CONS_PAT_TOTAL
  #DATA_list$cost_hierarchy[[i]] = RES_CONS_PAT_list$cost_hierarchy
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
      NUMB_PRO =nrow(RES_CONS_PATp)
      NUMB_RES = ncol(RES_CONS_PATp)
      RES_CONS_PAT = DATA_list$RES_CONS_PAT[[DATA$randID[i]]]
      RES_CONS_PAT_TOTAL = DATA_list$RES_CONS_PAT_TOTAL[[DATA$randID[i]]]
      #cost_hierarchy = DATA_list$cost_hierarchy[[DATA$randID[i]]]
      RCC = DATA_list$RCC[[DATA$randID[DATA$randID[i]]]]
      MXQ = t(FIRM[which(DATA$randID[i] == FIRM$randID),23:(13+NUMB_PRO+NUMB_PRO-1)])
      

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
      
      browser()
      
      PCH =  ACT_CONS_PAT %*% CostingSystem_list$ACP
      #PCB = RES_CONS_PATp %*% RCC
      PCB = t(FIRM[which(FIRM$randID==DATA$randID[i]),(13:(13+NUMB_PRO-1))])
      MXQ = t(FIRM[which(FIRM$randID==DATA$randID[i]),(13+NUMB_PRO):(13+NUMB_PRO+NUMB_PRO-1)])
      DENS = FIRM[which(FIRM$randID == DATA$randID[i]),]$DENS
      COR1 = FIRM[which(FIRM$randID == DATA$randID[i]),]$COR1
      COR2 = FIRM[which(FIRM$randID == DATA$randID[i]),]$COR2
      DISP1 = FIRM[which(FIRM$randID == DATA$randID[i]),]$DISP1
      DISP2 = FIRM[which(FIRM$randID == DATA$randID[i]),]$DISP2
      Q_VAR = FIRM[which(FIRM$randID == DATA$randID[i]),]$Q_VAR
      CS = FIRM[which(FIRM$randID == DATA$randID[i]),]$CS
      non_unit_size = FIRM[which(FIRM$randID == DATA$randID[i]),]$non_unit_size
      #if(CS ==0){CS_levels = 0}else{CS_levels=1}
      #else if(non_unit_size<0.33){CS_levels = "LOW"}else if(non_unit_size>0.46){CS_levels = "HIGH"}else{CS_levels = "MID"}
      
      
      
      MAPE = mean(abs(PCB-PCH)/PCB)
      
      PE = (PCH-PCB)/PCB
      ERROR = PCH-PCB
      UC = sum((PCB-PCH)/PCB>0)/NUMB_PRO
      OC = sum((PCB-PCH)/PCB<0)/NUMB_PRO
      
      UC5 = sum((PCB-PCH)/PCB>0.05)/NUMB_PRO
      OC5 = sum((PCB-PCH)/PCB< -0.05)/NUMB_PRO
      
      
      
      pcb = PCB/MXQ
      pch = PCH/MXQ
      pe = (pch-pcb)/pcb
      error = pch-pcb
      mape = mean(abs(pcb-pch)/pcb)#Anand et al. 2019
      
    
      UC5 = sum(pe>0.05)
      OC5 = sum(pe< -0.05)
      acc = 1-((UC5+OC5)/NUMB_PRO)
      BE_AB =  UC5-OC5
      
      pe_factor = abs(pe)/mape
      
      
      entropy = calc_entropy(RES_CONS_PAT) #entropy complexity (ElMaraghy et al., 2013)
      intra = calc_intra(RES_CONS_PAT) #intra-product heterogeneity (Gupta, 1993; Mertens, 2020)
      inter = calc_inter(RES_CONS_PAT) ##inter-product heterogeneity (Gupta, 1993; Mertens, 2020)
      nonzero_cons = calc_nonzero_cons(RES_CONS_PAT)
      complexity = calc_complexity(RES_CONS_PAT)
      
      PCB_rank = rank(PCB,ties.method = "random")
      pcb_rank = rank(pcb,ties.method = "random")
      PCH_rank = rank(PCH,ties.method = "random")
      pch_rank = rank(pch,ties.method = "random")
      pe_rank = rank(pe, ties.method = "random")
      ERROR_rank = rank(ERROR, ties.method = "random")
      MXQ_rank = rank(MXQ,ties.method = "random")
      intra_rank = rank(intra,ties.method = "random")
      inter_rank = rank(inter,ties.method = "random")
      entropy_rank = rank(entropy,ties.method = "random")
      nonzero_cons_rank = rank(nonzero_cons,ties.method = "random")
      complexity_rank = rank(complexity,ties.method = "random")
      
      #data_logging
      PRODUCT = c(1:NUMB_PRO)
      FIRM_ENV = c()
      COST_SYS = c()
      NUMB_RES_out = c()
      PACP_out = c()
      ACP_out = c()
      PDR_out = c()
      DISP1_out = c()
      DISP2_out = c()
      DENS_out = c()
      COR1_out = c()
      COR2_out = c()
      Q_VAR_out = c()
      BE_AB_out = c()
      acc_out = c()
      MAPE_out = c()
      CS_out = c()
      
      
      FIRM_ENV[PRODUCT] = rand_id
      COST_SYS[PRODUCT] = costsysid
      NUMB_RES_out[PRODUCT] = NUMB_RES
      PACP_out[PRODUCT] = pacp
      ACP_out[PRODUCT] = acp
      PDR_out[PRODUCT] = pdr
      DISP1_out[PRODUCT] = DISP1
      DISP2_out[PRODUCT] = DISP2
      DENS_out[PRODUCT] = DENS
      COR1_out[PRODUCT] = COR1
      COR2_out[PRODUCT] = COR2
      Q_VAR_out[PRODUCT] = Q_VAR
      BE_AB_out[PRODUCT] = BE_AB
      acc_out[PRODUCT] = acc
      MAPE_out[PRODUCT] = mape
      CS_out[PRODUCT] = CS
      
      preDATA = data.frame(FIRM_ENV,PRODUCT,COST_SYS,CS,NUMB_RES_out,PACP_out,ACP_out,PDR_out,DISP1_out,DISP2_out,DENS_out,COR1_out,COR2_out,Q_VAR_out,acc_out,MAPE_out,
                           MXQ,MXQ_rank,PCB,PCB_rank,PCH,PCH_rank,PE,ERROR,ERROR_rank,pcb,pcb_rank,pch,pch_rank,pe,pe_rank,error,inter,inter_rank,intra,intra_rank,entropy,entropy_rank,
                           nonzero_cons,nonzero_cons_rank,complexity,complexity_rank,BE_AB_out,pe_factor) 
      
      colnames(preDATA) = c('FIRM_ENV','PRODUCT','COST_SYS','CS','NUMB_RES','PACP','ACP','PDR','DISP1','DISP2','DENS','COR1','COR2','Q_VAR',"acc","mape",
                            'MXQ','MXQ_rank','PCB','PCB_rank','PCH','PCH_rank','PE','ERROR','ERROR_rank','pcb','pcb_rank','pch','pch_rank','pe','pe_rank','error','inter','inter_rank','intra','intra_rank',
                            'entropy','entropy_rank','nonzero_cons','nonzero_cons_rank','complexity','complexity_rank',"BE_AB",'pe_factor')
      
      browser()
      
preDATA    
  }

output_link = paste("output/CSD_",format(Sys.time(),"%Y-%m-%d-%H%M"),".csv", sep = "")
write.csv(output, file = output_link)
print("Cost System Design FILE has been written")