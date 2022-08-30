
######Nested Design following ABL 2019######



## ====================================== INPUT FOR FIRM GENERATION ==================================================

NUMB_FIRMS = 1800
NUMB_PRO = 50
NUMB_RES = 50
DISP1 = c(2,5,10)
Q_VAR = c('LOW','MID','HIGH')
DENS = c(-1)
DISP2 = c(-1)
COR1 = c(-1)
COR2 = c(-1)



runs = c(1:NUMB_FIRMS)
FIRM = expand.grid(NUMB_PRO,NUMB_RES, DISP1, DISP2, DENS, COR1,COR2,Q_VAR)

FirmID = c(1:nrow(FIRM))

FIRM = cbind(FirmID, FIRM)


FIRM = do.call("rbind", replicate(NUMB_FIRMS,FIRM, simplify = FALSE))
randID = c(1:nrow(FIRM))
FIRM = cbind(randID, FIRM)
colnames(FIRM) = c('randID',"FirmID",'NUMB_PRO','NUMB_RES',"DISP1", "DISP2", "DENS", "COR1","COR2","Q_VAR")



## ====================================== INPUT FOR COSTING SYSTEM GENERATION ==================================================
COSTING_SYSTEM = list()

CP = c(1,5,10,15,20,25,30,35,40,45,50)
CP_HEURISTIC = c(0,1,2,3)
CD_HEURISTIC = c(0)

COSTING_SYSTEM = expand.grid(CP,CP_HEURISTIC,CD_HEURISTIC)
CostSysID = c(1:nrow(COSTING_SYSTEM))
COSTING_SYSTEM = cbind(CostSysID,COSTING_SYSTEM)
colnames(COSTING_SYSTEM) = c('CostSysID','CP','PACP','PDR')

## ====================================== oUTPUT DATA SET ==================================================
rand = expand.grid(c(1:nrow(COSTING_SYSTEM)),c(1:nrow(FIRM)))
colnames(rand) = c('CostSysID','randID')

DATA_2 = dplyr::full_join(COSTING_SYSTEM, rand, by = c('CostSysID' = 'CostSysID'))

DATA = dplyr::full_join(DATA_2,FIRM, by = c('randID' = "randID"))

DATA = data.frame(DATA$randID,DATA$FirmID,DATA$CostSysID,DATA$PACP, DATA$CP, DATA$PDR,DATA[,9:14])

colnames(DATA) = c("randID","FirmID",'CostSysID','PACP','ACP','PDR',"DISP1", "DISP2", "DENS", "COR1","COR2","Q_VAR")

output = data.frame()

## ====================================== MULTI CORE SETTING ==================================================


# 
# output <- foreach(i = 1:100, .combine = rbind, .options.snow = opts) %dopar% { #dopar
DATA_list = list()

for(i in 1:nrow(FIRM)){

  
  MXQ = .gen_Demand_Anand(FIRM$Q_VAR[i],FIRM$NUMB_PRO[i])
  
  
  RES_CONS_PAT_list = .gen_RES_CONS_PAT_Anand(FIRM$NUMB_PRO[i],FIRM$NUMB_RES[i], FIRM$DENS[i], FIRM$DISP1[i],FIRM$COR1[i],FIRM$COR2[i],MXQ)
  
  
  RCC_list = .gen_RCC_Anand(FIRM$DISP1[i], FIRM$DISP2[i],FIRM$NUMB_RES[i])
  
  
  PCB = RES_CONS_PAT_list$RES_CONS_PATp %*% RCC_list$RCC
  
  FIRM$DENS[i] = RES_CONS_PAT_list$DENS
  FIRM$COR1[i] = RES_CONS_PAT_list$COR1
  FIRM$COR2[i] = RES_CONS_PAT_list$COR2
  FIRM$DISP2[i] = RCC_list$DISP2_draw
  FIRM[i,11:60] = PCB
  FIRM[i,61:110] = MXQ

  
  DATA_list$RES_CONS_PAT[[i]] = RES_CONS_PAT_list$RES_CONS_PATp
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
  
      RES_CONS_PATp = DATA_list$RES_CONS_PAT[[DATA$randID[i]]]
      RCC = DATA_list$RCC[[DATA$randID[DATA$randID[i]]]]
      
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

      ACT_CONS_PAT = MAP_CP_P_BIGPOOL(CostingSystem_list$RC_to_ACP,RES_CONS_PATp,RCC, NUMB_PRO)
      
    
      
      PCH =  ACT_CONS_PAT %*% CostingSystem_list$ACP
      #PCB = RES_CONS_PATp %*% RCC
      PCB = t(FIRM[which(FIRM$randID==DATA$randID[i]),11:60])
      MXQ = t(FIRM[which(DATA$randID[i] == FIRM$randID),61:110])
      DENS = FIRM[which(DATA$randID[i] == FIRM$randID),]$DENS
      COR1 = FIRM[which(DATA$randID[i] == FIRM$randID),]$COR1
      COR2 = FIRM[which(DATA$randID[i] == FIRM$randID),]$COR2
      DISP2 = FIRM[which(DATA$randID[i] == FIRM$randID),]$DISP2
      Q_VAR = FIRM[which(DATA$randID[i] == FIRM$randID),]$Q_VAR
      
      MAPE = mean(abs(PCB-PCH)/PCB)
      PE = (PCB-PCH)/PCB
      UC = sum((PCB-PCH)/PCB>0)/NUMB_PRO
      OC = sum((PCB-PCH)/PCB<0)/NUMB_PRO
      
      UC5 = sum((PCB-PCH)/PCB>0.05)/NUMB_PRO
      OC5 = sum((PCB-PCH)/PCB<-0.05)/NUMB_PRO
      
      
      TVC_BIAS = cor(MXQ,PE)
      
      

      preDATA = data.frame(DATA[i,1:7],
                           DISP2,
                           DENS,
                           COR1,
                           COR2,
                           Q_VAR,
                           t(PCB),
                           t(PCH),
                           MAPE,
                           t(PE),
                           UC,
                           OC,
                           UC5,
                           OC5,
                           t(MXQ),
                           TVC_BIAS)
       
      colnames(preDATA) = c('randID','FirmID','CostSysID','PACP','ACP','PDR',"DISP1","DISP2","DENS","COR1","COR2","Q_VAR",c(paste0("PCB_", 0:49)),c(paste0("PCH_", 0:49)),
                            "MAPE",c(paste0("PE_", 0:49)),"UC","OC","UC5","OC5",c(paste0("MXQ_", 0:49)),"TVC_BIAS")
      

      
      # preDATA[1:12] = DATA[i,1:12]
      # preDATA[13:62] = PCB
      # preDATA[63:112] = PCH
      # preDATA[113] = mean(abs(PCB-PCH)/PCB)
      # preDATA[114:163] = (PCB-PCH)/PCB
      # preDATA[164:213] = MXQ
      # preDATA[9] = DENS
      # preDATA[10] = COR1
      # preDATA[11] = COR2
      # preDATA[8] = DISP2

      
#print(i)


preDATA    
  }
  

output_link = paste("output/CSD_",format(Sys.time(),"%Y-%m-%d-%H%M"),".csv", sep = "")
write.csv(output, file = output_link)
print("Cost System Design FILE has been written")