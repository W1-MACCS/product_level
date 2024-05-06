
######Nested Design following ABL 2019######

CaseStudy = 0

## ====================================== INPUT FOR FIRM GENERATION ==================================================

NUMB_FIRMS = 50
NUMB_PRO = c(50)
NUMB_RES = c(50)
DISP1 = c(10)
Q_VAR = c(-1)
DENS = c(0.25, 0.5, 0.75)
DISP2 = c(-1)
COR1 = c(-1)
COR2 = c(-1)

CS = c(1)

if(CaseStudy ==1){
  
  RES_CONS_PAT = read.csv("Casestudy.csv", sep = ";", header = FALSE)[1:23]
  
  NUMB_PRO = nrow(RES_CONS_PAT)
  NUMB_RES = ncol(RES_CONS_PAT)
  CS = c(1)
  DISP1 = c(4)
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

CP = c(1,2,3,6,10,15,20)#1,3,6,10,15,20#as in Anand et al. (2017)
CP_HEURISTIC = c(2)#2 -size-random-misc #as in Anand et al. (2017)
CD_HEURISTIC = c(0)#0 - Big Pool #as in Anand et al. (2017)
ME = c(0.3)
if(CaseStudy ==1){CP = c(1,2,4,6,8,10)}
  


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
  standard_res_size = runif(1,0.5,1)  #Parameter for VolMatch
  
  
  RCC_list = .gen_RCC_Anand(FIRM$DISP1[i], FIRM$DISP2[i],FIRM$NUMB_RES[i])
  RCC_list = .gen_RCC(FIRM$DISP2[i],FIRM$NUMB_RES[i])
  RCC = RCC_list$RCC
  non_unit_level_resources = RCC_list$non_unit_level_resources

  
if(CaseStudy==0){
  if(FIRM$CS[i] == 0){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Anand(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 2){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_diag(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 1){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Anand_match(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy,standard_res_size,RCC,non_unit_level_resources)
    
  }else if(FIRM$CS[i] == 3){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_diag_match(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }else if(FIRM$CS[i] == 4){
    
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_EAD(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy)
    
  }
}else if(CaseStudy==1){
  if(FIRM$CS[i]==0){
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Case(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy,RES_CONS_PAT)
  }
  else if(FIRM$CS[i]==1){
    RES_CONS_PAT_list = .gen_RES_CONS_PAT_Case_match(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ,cost_hierarchy,RES_CONS_PAT,standard_res_size,RCC)
  }

  
}
  
  print(i)

  

 
  
  PCB = RES_CONS_PAT_list$RES_CONS_PATp %*% RCC
  
  FIRM$DENS[i] = RES_CONS_PAT_list$DENS
  FIRM$COR1[i] = RES_CONS_PAT_list$COR1
  FIRM$COR2[i] = RES_CONS_PAT_list$COR2
  FIRM$non_unit_size[i] = RES_CONS_PAT_list$non_unit_size
  FIRM$DISP2[i] = RCC_list$DISP2_draw
  FIRM$standard_res_size[i] = standard_res_size
  FIRM$non_unit_level_costs[i] = RCC_list$non_unit_level_costs
  FIRM[i,14:(14+FIRM$NUMB_PRO[i]-1)] = PCB
  FIRM[i,(14+FIRM$NUMB_PRO[i]):(14+FIRM$NUMB_PRO[i]+FIRM$NUMB_PRO[i]-1)] = MXQ
  

  
  DATA_list$RES_CONS_PATp[[i]] = RES_CONS_PAT_list$RES_CONS_PATp
  DATA_list$RES_CONS_PAT[[i]] = RES_CONS_PAT_list$RES_CONS_PAT
  DATA_list$RES_CONS_PAT_TOTAL[[i]] = RES_CONS_PAT_list$RES_CONS_PAT_TOTAL
  DATA_list$batch_costs[[i]] = RES_CONS_PAT_list$batch_costs
  DATA_list$non_unit_resources[[i]] = RES_CONS_PAT_list$non_unit_resources
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
      
    print(i)
      rand_id = DATA$randID[i]
      pdr = DATA$PDR[i]
      firmid = DATA$FirmID[i]
      costsysid = DATA$CostSysID[i]
      pacp = DATA$PACP[i]
      acp = DATA$ACP[i]
      me = DATA$ME[i]
      

      
  
      RES_CONS_PATp = DATA_list$RES_CONS_PATp[[DATA$randID[i]]]
      non_unit_resources = DATA_list$non_unit_resources[[DATA$randID[i]]]
      batch_costs = DATA_list$batch_costs[[DATA$randID[i]]]
      NUMB_PRO =nrow(RES_CONS_PATp)
      NUMB_RES = ncol(RES_CONS_PATp)
      RES_CONS_PAT = DATA_list$RES_CONS_PAT[[DATA$randID[i]]]
      RES_CONS_PAT_TOTAL = DATA_list$RES_CONS_PAT_TOTAL[[DATA$randID[i]]]
      #cost_hierarchy = DATA_list$cost_hierarchy[[DATA$randID[i]]]
      RCC = DATA_list$RCC[[DATA$randID[DATA$randID[i]]]]
      MXQ = t(FIRM[which(DATA$randID[i] == FIRM$randID),(13+NUMB_PRO):(13+NUMB_PRO+NUMB_PRO-1)])
      standard_res_size = FIRM[which(FIRM$randID == DATA$randID[i]),]$standard_res_size
      non_unit_level_costs = FIRM[which(FIRM$randID == DATA$randID[i]),]$non_unit_level_costs
      

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
      }else if(DATA$PDR[i] == 2){
        cost_core_list = calc_cost_ratio_std_res(RES_CONS_PATp,RCC,standard_res_size) #Relative Costs of Core Resources
        std_res = unlist(cost_core_list$std_res)
        ACT_CONS_PAT = MAP_CP_P_STD(CostingSystem_list$RC_to_ACP,RES_CONS_PATp,RCC, NUMB_PRO,std_res)
      }
      
      
      if(me>0){
        ACT_CONS_PAT = apply_ME(ACT_CONS_PAT,me,acp,NUMB_PRO)
      }
      
      cost_drivers = unlist(lapply(CostingSystem_list$RC_to_ACP,function(x){x[which.max(RCC[x])]}))
      
      PCH =  ACT_CONS_PAT %*% CostingSystem_list$ACP
      #PCB = RES_CONS_PATp %*% RCC
      PCB = t(FIRM[which(FIRM$randID==DATA$randID[i]),(14:(14+NUMB_PRO-1))])
      MXQ = t(FIRM[which(FIRM$randID==DATA$randID[i]),(14+NUMB_PRO):(14+NUMB_PRO+NUMB_PRO-1)])
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
      
      
      mape_oc = mean(abs(pe[which(pe>0)]))
      mape_uc = mean(abs(pe[which(pe<0)]))
      
      #entropy = calc_entropy(ACT_CONS_PAT) #entropy complexity (ElMaraghy et al., 2013)
      intra = calc_intra(RES_CONS_PATp) #intra-product heterogeneity (Gupta, 1993; Mertens, 2020)
      directed_inter = calc_directed_inter(RES_CONS_PATp) ##inter-product heterogeneity (Gupta, 1993; Mertens, 2020)
      individuality = calc_individuality(RES_CONS_PAT,standard_res_size)#*rowSums(RES_CONS_PAT)
      cost_core_list = calc_cost_ratio_std_res(RES_CONS_PATp,RCC,standard_res_size) #Relative Costs of Core Resources
      cost_ratio_std_res = cost_core_list$cost_ratio_std_res
      std_res = unlist(cost_core_list$std_res)
      driver_share = (acp - length(setdiff(cost_drivers,non_unit_resources)))/acp
      
      
      
      
      unit_level_cost_drivers = setdiff(cost_drivers,non_unit_resources)
      index_ul_cost_drivers = match(unit_level_cost_drivers,cost_drivers)
      reported_unit_costs = (as.matrix(ACT_CONS_PAT[,index_ul_cost_drivers]) %*% CostingSystem_list$ACP[index_ul_cost_drivers])/MXQ
      reported_unit_cost_share = reported_unit_costs/pch
      
      uldriver_share = length(unit_level_cost_drivers)/acp

      numb_std_res = cost_core_list$numb_std_res
      unit_costs = PCB-batch_costs
      #complexity = calc_complexity(ACT_CONS_PAT)
      driverVar = calc_cons_var(ACT_CONS_PAT)
      resVar = calc_cons_var(RES_CONS_PAT)
      mean_cons = rowMeans(RES_CONS_PAT)
      res_numb = calc_nonzero_cons(RES_CONS_PATp)
      act_cons = rowMeans(ACT_CONS_PAT)
      driver_numb = calc_nonzero_cons(ACT_CONS_PAT)/acp
      cons_bigDriver = calc_cons_bigDriver(ACT_CONS_PAT)
      cons_smallDriver = calc_cons_smallDriver(ACT_CONS_PAT)
      

      PCB_rank = rank(PCB,ties.method = "random")
      pcb_rank = rank(pcb,ties.method = "random")
      PCH_rank = rank(PCH,ties.method = "random")
      pch_rank = rank(pch,ties.method = "random")
      MXQ_rank = rank(MXQ,ties.method = "random")
      pe_rank = rank(pe, ties.method = "random")
      ape_rank = rank(ape, ties.method = "random")
      ERROR_rank = rank(abs(ERROR), ties.method = "random")
      PERROR_rank = rank(PERROR, ties.method = "random")
      intra_rank = calc_ranking(intra)
      directed_inter_rank = calc_ranking(directed_inter)
      #entropy_rank = rank(entropy,ties.method = "random")
      driverVar_rank = rank(driverVar,ties.method = "random")
      reported_unit_cost_share_rank = rank(reported_unit_cost_share,ties.method = "random")
      mean_cons_rank = rank(mean_cons,ties.method = "random")
      res_numb_rank = rank(res_numb,ties.method = "random")
      batch_costs_rank = rank(batch_costs,ties.method = "random")
      individuality_rank = rank(individuality,ties.method = "random")
      cost_ratio_std_res_rank = rank(cost_ratio_std_res,ties.method = "random")
      resVar_rank = rank(resVar,ties.method = "random")
      act_cons_rank = rank(act_cons,ties.method = "random")
      driver_numb_rank = rank(driver_numb,ties.method = "random")
      cons_bigDriver_rank =rank(cons_bigDriver,ties.method = "random")
      cons_smallDriver_rank = rank(cons_smallDriver,ties.method = "random")
      
      #PMH = max(abs(pcb - mean(pcb)))/sum(abs(pcb - mean(pcb)))
      #PMH = max(abs(pcb))/sum(abs(abs(pcb)-mean(abs(pcb))))
      error_gini = Gini(abs(ERROR))
      #acc =  Gini(abs(error)/sum(abs(error)))
      error_disp = sum(sort(abs(ERROR),decreasing = TRUE)[c(1:5)])/sum(abs(ERROR))
      #error_disp = max(abs(pe))/sum(abs(abs(pe)-mean(abs(pe))))
      #error_disp = Gini(ape)
      EUCD = sum(abs(ERROR))
      No_bigDriver = sum(CostingSystem_list$ACP>200000)
      mean_cons_bigDriver = mean(cons_bigDriver)
      if(acp ==1){CSD = "SCD"}else{CSD = "MCD"}
      
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
      No_bigDriver_out = c()
      UC_out = c()
      OC_out = c()
      UC_share_out = c()
      ME_out = c()
      EUCD_out = c()
      mean_cons_bigDriver_out = c()
      error_disp_out = c()
      numb_std_res_out = c()
      standard_res_size_out = c()
      CSD_out = c()
      uldriver_share_out = c()
      non_unit_level_costs_out = c()
      error_gini_out = c()
      mape_oc_out = c()
      mape_uc_out = c()
      
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
      No_bigDriver_out[PRODUCT] = No_bigDriver
      UC_out[PRODUCT] = UC
      OC_out[PRODUCT] = OC
      UC_share_out[PRODUCT] = UC_share
      ME_out[PRODUCT] = me
      EUCD_out[PRODUCT] = EUCD
      mean_cons_bigDriver_out[PRODUCT] = mean_cons_bigDriver
      error_disp_out[PRODUCT] = error_disp
      numb_std_res_out[PRODUCT] = numb_std_res
      standard_res_size_out[PRODUCT] = standard_res_size
      CSD_out[PRODUCT] = CSD
      uldriver_share_out[PRODUCT] = uldriver_share
      non_unit_level_costs_out[PRODUCT] = non_unit_level_costs
      error_gini_out[PRODUCT] = error_gini
      mape_oc_out[PRODUCT] = mape_oc
      mape_uc_out[PRODUCT] = mape_uc
      
      
      

      
      preDATA = data.frame(UNIQUE_ID,FIRM_ENV,PRODUCT,COST_SYS,CS,NUMB_RES_out,PACP_out,ACP_out,CSD_out,PDR_out,ME_out,DISP1_out,DISP2_out,DENS_out,COR1_out,COR2_out,Q_VAR_out,No_bigDriver_out,acc_out,MAPE_out,
                           MXQ,MXQ_rank,PCB,PCB_rank,PCH,PCH_rank,PE,ERROR,ERROR_rank,pcb,pcb_rank,pch,pch_rank,pe,pe_rank,ape,ape_rank,error,
                           res_numb, res_numb_rank, driver_numb,driver_numb_rank,
                           cons_bigDriver,cons_bigDriver_rank,cons_smallDriver,cons_smallDriver_rank,BE_AB_out,UC_out,OC_out,EUCD_out,error_disp_out,UC_share_out,mape_oc_out,mape_uc_out,
                           non_unit_level_costs_out,batch_costs,batch_costs_rank,error_gini_out,reported_unit_costs,reported_unit_cost_share,reported_unit_cost_share_rank,uldriver_share_out) 
      
      colnames(preDATA) = c('UNIQUE_ID','FIRM_ENV','PRODUCT','COST_SYS','CS','NUMB_RES','PACP','ACP',"CSD",'PDR',"ME",'DISP1','DISP2','DENS','COR1','COR2','Q_VAR',"NoBigDriver","acc","mape",
                            'MXQ','MXQ_rank','PCB','PCB_rank','PCH','PCH_rank','PE','ERROR','ERROR_rank','pcb','pcb_rank','pch','pch_rank','pe','pe_rank','ape','ape_rank','error',
                            'res_numb','res_numb_rank','driver_numb','driver_numb_rank',
                            'cons_bigDriver','cons_bigDriver_rank','cons_smallDriver','cons_smallDriver_rank',"BE_AB",'UC','OC','EUCD','error_disp','UC_share','mape_oc','mape_uc',
                            'non_unit_level_costs','batch_costs','batch_costs_rank','error_gini','reported_unit_costs','reported_unit_cost_share','reported_unit_cost_share_rank','uldriver_share')
      
      # if(round(sum(PCH),0)>1000000){stop(paste(c("PCH zu groß",CS,sum(PCH))))}
      # print(EUCD)

preDATA    
  }

output_link = paste("output/CSD_",format(Sys.time(),"%Y-%m-%d-%H%M"),".csv", sep = "")
write.csv(output, file = output_link)
print("Cost System Design FILE has been written")