
######Nested Design following ABL 2019######

CaseStudy = 1

## ====================================== INPUT FOR FIRM GENERATION ==================================================

NUMB_FIRMS = 50
NUMB_PRO = c(50)
NUMB_RES = c(50)
DISP1 = c(4)
Q_VAR = c("LOW","MID","HIGH")
DENS = c(-1)
DISP2 = c(-1)
COR1 = c(-1)
COR2 = c(-1)

CS = c(0)

if(CaseStudy ==1){
  
  RES_CONS_PAT = read.csv("Casestudy.csv", sep = ";", header = FALSE)[1:23]
  
  NUMB_PRO = nrow(RES_CONS_PAT)
  NUMB_RES = ncol(RES_CONS_PAT)
  
}



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

CP = c(1,5,10,15,20)#1,3,6,10,15,20#as in Anand et al. (2017)
CP_HEURISTIC = c(1)#2 -size-random-misc #as in Anand et al. (2017)
CD_HEURISTIC = c(0)#0 - Big Pool #as in Anand et al. (2017)
ME = c(0)

COSTING_SYSTEM = expand.grid(CP,CP_HEURISTIC,CD_HEURISTIC,ME)
CostSysID = c(1:nrow(COSTING_SYSTEM))
COSTING_SYSTEM = cbind(CostSysID,COSTING_SYSTEM)
colnames(COSTING_SYSTEM) = c('CostSysID','CP','PACP','PDR','ME')

## ====================================== OUTPUT DATA SET ==================================================
rand = expand.grid(c(1:nrow(COSTING_SYSTEM)),c(1:nrow(FIRM)))
colnames(rand) = c('CostSysID','randID')

DATA_2 = dplyr::full_join(COSTING_SYSTEM, rand, by = c('CostSysID' = 'CostSysID'))

DATA = dplyr::full_join(DATA_2,FIRM, by = c('randID' = "randID"))

DATA = data.frame(DATA$randID,DATA$FirmID,DATA$CostSysID,DATA$PACP, DATA$CP, DATA$PDR,DATA$ME,DATA[,9:16])

colnames(DATA) = c("randID","FirmID",'CostSysID','PACP','ACP','PDR','ME',"NUMB_RES","DISP1", "DISP2", "DENS", "COR1","COR2","Q_VAR","CS")


## ====================================== MULTI CORE SETTING ==================================================


# 

DATA_list = list()

for(i in 1:nrow(FIRM)){

  
  MXQ = .gen_Demand(FIRM$Q_VAR[i],FIRM$NUMB_PRO[i])
  #cost_hierarchy = .gen_cost_hierarchy()
  cost_hierarchy = list()

  
if(CaseStudy==0){
  if(FIRM$CS[i] == 0){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Anand(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 2){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_diag(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 1){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Anand_match(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 3){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_diag_match(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 4){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_EAD(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }
}else if(CaseStudy==1){
  
  RES_CONS_PAT_list = .gen_RES_CONS_PAT_Case_match(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy,RES_CONS_PAT)
  

  
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

output <- foreach(i = 1:nrow(DATA), .combine = rbind, .options.snow = opts, .packages = c('DescTools')) %dopar% { #dopar
#for(i in 1:nrow(DATA)){
    
      rand_id = DATA$randID[i]
      pdr = DATA$PDR[i]
      firmid = DATA$FirmID[i]
      costsysid = DATA$CostSysID[i]
      pacp = DATA$PACP[i]
      acp = DATA$ACP[i]
      me = DATA$ME[i]

      
  
      RES_CONS_PATp = DATA_list$RES_CONS_PATp[[DATA$randID[i]]]
      NUMB_PRO =nrow(RES_CONS_PATp)
      NUMB_RES = ncol(RES_CONS_PATp)
      RES_CONS_PAT = DATA_list$RES_CONS_PAT[[DATA$randID[i]]]
      RES_CONS_PAT_TOTAL = DATA_list$RES_CONS_PAT_TOTAL[[DATA$randID[i]]]
      #cost_hierarchy = DATA_list$cost_hierarchy[[DATA$randID[i]]]
      RCC = DATA_list$RCC[[DATA$randID[DATA$randID[i]]]]
      MXQ = t(FIRM[which(DATA$randID[i] == FIRM$randID),(13+NUMB_PRO):(13+NUMB_PRO+NUMB_PRO-1)])
      

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
      
      
      if(me>0){
        ACT_CONS_PAT = apply_ME(ACT_CONS_PAT,me,acp,NUMB_PRO)
      }
      
      

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
      if(acp<=6){Agg_degree = 0}else{Agg_degree=1}
      
      
      MAPE = mean(abs(PCB-PCH)/PCB)
      
      PE = (PCH-PCB)/PCB
      ERROR = PCH-PCB
      PERROR = abs(ERROR)/sum(abs(ERROR))
      UC = sum((PCB-PCH)/PCB>0)/NUMB_PRO
      OC = sum((PCB-PCH)/PCB<0)/NUMB_PRO
      
      UC5 = sum((PCB-PCH)/PCB>0.05)/NUMB_PRO
      OC5 = sum((PCB-PCH)/PCB< -0.05)/NUMB_PRO
      
      
      
      pcb = PCB/MXQ
      pch = PCH/MXQ
      pe = (pch-pcb)/pcb
      error = pch-pcb
      mape = mean(abs(pcb-pch)/pcb)#Anand et al. 2019
      
    
      UC5 = sum(pe< -0.05)
      OC5 = sum(pe> 0.05)
      UC = sum(pe<0)
      OC = sum(pe>0)
      acc = 1-((UC5+OC5)/NUMB_PRO)
      UC_share =  UC5/((UC5+OC5))
      BE_AB = UC5-OC5
      ape = abs(pe)
      pe_factor = abs(pe)/mape
      
      
      #entropy = calc_entropy(ACT_CONS_PAT) #entropy complexity (ElMaraghy et al., 2013)
      intra = calc_nonzero_cons(RES_CONS_PATp) #intra-product heterogeneity (Gupta, 1993; Mertens, 2020)
      inter = calc_cons_var(RES_CONS_PATp) ##inter-product heterogeneity (Gupta, 1993; Mertens, 2020)
      mean_cons = calc_mean_cons(RES_CONS_PATp)
      matching_measure = calc_mean_cons(RES_CONS_PATp)*calc_nonzero_cons(RES_CONS_PATp)
      #complexity = calc_complexity(ACT_CONS_PAT)
      sd_cons = calc_cons_var(ACT_CONS_PAT)
      act_cons = rowMeans(ACT_CONS_PAT)
      nonzero_cons = 1-(calc_zero_cons(ACT_CONS_PAT)/acp)
      cons_bigDriver = calc_cons_bigDriver(ACT_CONS_PAT)
      cons_smallDriver = calc_cons_smallDriver(ACT_CONS_PAT)
      
      PCB_rank = rank(PCB,ties.method = "random")
      pcb_rank = rank(pcb,ties.method = "random")
      PCH_rank = rank(PCH,ties.method = "random")
      pch_rank = rank(pch,ties.method = "random")
      MXQ_rank = rank(MXQ,ties.method = "random")
      pe_rank = rank(pe, ties.method = "random")
      ERROR_rank = rank(ERROR, ties.method = "random")
      PERROR_rank = rank(PERROR, ties.method = "random")
      intra_rank = calc_ranking(intra)
      inter_rank = calc_ranking(inter)
      #entropy_rank = rank(entropy,ties.method = "random")
      sd_cons_rank = rank(sd_cons,ties.method = "random")
      act_cons_rank = rank(act_cons,ties.method = "random")
      nonzero_cons_rank = rank(nonzero_cons,ties.method = "random")
      cons_bigDriver_rank =rank(cons_bigDriver,ties.method = "random")
      cons_smallDriver_rank = rank(cons_smallDriver,ties.method = "random")
      
      #PMH = max(abs(pcb - mean(pcb)))/sum(abs(pcb - mean(pcb)))
      #PMH = max(abs(pcb))/sum(abs(abs(pcb)-mean(abs(pcb))))
      PMH = Gini(pcb)
      #acc =  Gini(abs(error)/sum(abs(error)))
      error_disp = sum(sort(abs(ERROR),decreasing = TRUE)[c(1:5)])/sum(abs(ERROR))
      #error_disp = max(abs(pe))/sum(abs(abs(pe)-mean(abs(pe))))
      #error_disp = Gini(ape)
      EUCD = sum(abs(ERROR))
      No_bigDriver = sum(CostingSystem_list$ACP>200000)
      mean_cons_bigDriver = mean(cons_bigDriver)
      
      #data_logging
      PRODUCT = c(1:NUMB_PRO)
      UNIQUE_ID = c()
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
      Agg_Degr =c()
      PMH_out = c()
      No_bigDriver_out = c()
      UC_out = c()
      OC_out = c()
      UC_share_out = c()
      ME_out = c()
      EUCD_out = c()
      mean_cons_bigDriver_out = c()
      error_disp_out = c()
      
      UNIQUE_ID[PRODUCT] = i
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
      Agg_Degr[PRODUCT] = Agg_degree
      PMH_out[PRODUCT] = PMH
      No_bigDriver_out[PRODUCT] = No_bigDriver
      UC_out[PRODUCT] = UC
      OC_out[PRODUCT] = OC
      UC_share_out[PRODUCT] = UC_share
      ME_out[PRODUCT] = me
      EUCD_out[PRODUCT] = EUCD
      mean_cons_bigDriver_out[PRODUCT] = mean_cons_bigDriver
      error_disp_out[PRODUCT] = error_disp
      
      preDATA = data.frame(FIRM_ENV,PRODUCT,COST_SYS,CS,NUMB_RES_out,PACP_out,ACP_out,PDR_out,ME_out,DISP1_out,DISP2_out,DENS_out,COR1_out,COR2_out,Q_VAR_out,PMH_out,No_bigDriver_out,acc_out,MAPE_out,
                           MXQ,MXQ_rank,PCB,PCB_rank,PCH,PCH_rank,PE,ERROR,PERROR_rank,pcb,pcb_rank,pch,pch_rank,pe,pe_rank,error,inter,inter_rank,intra,intra_rank,
                           sd_cons,sd_cons_rank,nonzero_cons,nonzero_cons_rank,act_cons,act_cons_rank,
                           cons_bigDriver,cons_bigDriver_rank,cons_smallDriver,cons_smallDriver_rank,BE_AB_out,ape,UC_out,OC_out,EUCD_out,mean_cons_bigDriver_out,error_disp_out,mean_cons,UC_share_out) 
      
      colnames(preDATA) = c('FIRM_ENV','PRODUCT','COST_SYS','CS','NUMB_RES','PACP','ACP','PDR',"ME",'DISP1','DISP2','DENS','COR1','COR2','Q_VAR','VarSize',"NoBigDriver","acc","mape",
                            'MXQ','MXQ_rank','PCB','PCB_rank','PCH','PCH_rank','PE','ERROR','PERROR_rank','pcb','pcb_rank','pch','pch_rank','pe','pe_rank','error','inter','inter_rank','intra','intra_rank',
                            'sd_cons','sd_cons_rank','nonzero_cons','nonzero_cons_rank','act_cons','act_cons_rank',
                            'cons_bigDriver','cons_bigDriver_rank','cons_smallDriver','cons_smallDriver_rank',"BE_AB",'ape','UC','OC','EUCD','mean_cons_bigDriver','error_disp','mean_cons','UC_share')
      
      # if(round(sum(PCH),0)>1000000){stop(paste(c("PCH zu groß",CS,sum(PCH))))}
      # print(EUCD)

preDATA    
  }

output_link = paste("output/CSD_",format(Sys.time(),"%Y-%m-%d-%H%M"),".csv", sep = "")
write.csv(output, file = output_link)
print("Cost System Design FILE has been written")