##### Initizalization of the CostSystemDesignSim (CSDS)

## ======================================INPUT MASK============================================================
  FIRM = list()
  FIRM$PRODUCTION_ENVIRONMENT = list()
  FIRM$COSTING_SYSTEM = list()
  DATA = data.frame()
  DATAp = data.frame()
    
  NUMB_PRO =         50                     #INPUT independent Variable - Number of products 
  NUMB_RES  =        50                  #INPUT independent variable - Number of factors
  SIM_NUMB =         200              #Control Variable - Number of Simulations for every single environment (standard: 30)     
  TC =               1000000             #Total costs

  ProductCostOutput= 0                      #Control Variable -  Zero = no tracking of the product level

  set_DEMAND_fix=   0                  #Control Variable -  Decide if always the same demand is used
  set_RES_CONS_PAT_fix = 0            #Control variable - Decide if the resource consumption is constant
  set_RCU_fix = 0                      #Control variable - Decide if the resource costs per unit are constant
  set_RCC_fix = 0                      # Control Variable - Decide if the reosurce costs vector is the same
  set_CSD_fix=  0                      #Control Variable -  Decide if CD_Heuristic always uses the same resources.
  
  cost_structure = c(1,0)                   #1 = costs can be unit or batch/product/facility -level; 0 = all costs are unit-level
  unit_level_cost_share = c(0.25,0.5,0.75)            #defines the share of total costs, that are consumed by unit-level resources -1 --> 50 - 80 % of all costs are unit level costs (Ittner et al. 1997)
                                                # Cooper, Kaplan (1999)  12% Facility sustaining, 15 % Product sustaining, 16 % Batch-level, 57% Unit-level
  unit_level_number_share = c(0.25,0.5,0.75)          #defines the share of resources that are considered unit-level 40 - 70% of all resources are considered as unit level resources 

  CP = c(1,2,4,6,8,10,12,14,16,18,20)       #No. of Cost Pools
  COR1 = c(-1)                              #Correlation between resources
  COR2 = c(-1)
  RC_VAR =  c(-1)                           #Resource cost variation --> base for DISP2
  Q_VAR = c(0.1,0.75,1.5)                   #Demand variation
  Error = c(0)                              #Measurement error
  NUMB_Error = c(1)                         #Number of errornoues links
  DENS = c(-1)                    #Number of links between products and resources (sharing)
  CC = c(0.4)                               #Correlation Cutoff for correlative assignement in CP HEURISTICS
  MISCPOOLSIZE = c(0.25)                    #share of total costs that are supposed to go into the miscpool if there is a miscpool in the Costing System
  DISP1 = c(10)                             #No. of the biggest resources that have a DISP2 share of the total costs
  NUM = c(4)                                #No. of Resources used for indexed driver
  CP_HEURISTIC = c(1,7)                       #Which Heuristic for pooling resources? # 0-6
  CD_HEURISTIC = c(0,3)                       #which Heuristic for selecting a driver? #0-1



## ====================================== END OF INPUT MASK=====================================================                           

            set.seed(13) #Reproducibility
            
            runs = c(1:SIM_NUMB)
            factorial_design = expand.grid(cost_structure,unit_level_cost_share,unit_level_number_share,
                                 CP,COR1,COR2,RC_VAR,Q_VAR,Error,NUMB_Error,DENS,CC,MISCPOOLSIZE,DISP1,NUM,CP_HEURISTIC,CD_HEURISTIC,runs)
            colnames(factorial_design) = c('cost_structure','unit_level_cost_share','unit_level_number_share',
                                 'CP','COR1','COR2','RC_VAR','Q_VAR','Error','NUMB_Error','DENS','CC','MISCPOOLSIZE','DISP1','NUM','CP_HEURISTIC','CD_HEURISTIC','runs')

           
            
## ====================================== MULTI CORE SETTING ==================================================
            cores=detectCores()
            cl <- makeCluster(cores[1]-1)
            registerDoSNOW(cl)
            iterations <- nrow(factorial_design)
            pb <- txtProgressBar(max = iterations, style = 3)
            progress <- function(n) setTxtProgressBar(pb, n)
            opts <- list(progress = progress)
            
            ##running with more cores
            DATA = foreach(i = 1:nrow(factorial_design), .combine = rbind, .options.snow = opts) %dopar% {
            
             
  ## ====================== PREDETERMINING AND PREALLOCATION  =========================
                #INPUT VARIABLES
                FIRM$PRODUCTION_ENVIRONMENT$DENS = factorial_design$DENS[i]
                FIRM$PRODUCTION_ENVIRONMENT$COR1  = factorial_design$COR1[i]
                FIRM$PRODUCTION_ENVIRONMENT$COR2 = factorial_design$COR2[i]
                FIRM$PRODUCTION_ENVIRONMENT$Q_VAR = factorial_design$Q_VAR[i]  
                FIRM$PRODUCTION_ENVIRONMENT$DISP1 = factorial_design$DISP1[i]
                FIRM$PRODUCTION_ENVIRONMENT$unit_level_cost_share = factorial_design$unit_level_cost_share[i]
                FIRM$PRODUCTION_ENVIRONMENT$unit_level_number_share = factorial_design$unit_level_number_share[i]
                FIRM$PRODUCTION_ENVIRONMENT$cost_structure = factorial_design$cost_structure[i]
                FIRM$COSTING_SYSTEM$CP = factorial_design$CP[i]
                FIRM$COSTING_SYSTEM$RC_VAR = factorial_design$RC_VAR[i]
                FIRM$COSTING_SYSTEM$Error = factorial_design$Error[i]
                FIRM$COSTING_SYSTEM$NUMB_Error = factorial_design$NUMB_Error[i]
                FIRM$COSTING_SYSTEM$CC = factorial_design$CC[i]
                FIRM$COSTING_SYSTEM$MISCPOOLSIZE = factorial_design$MISCPOOLSIZE[i]
                FIRM$COSTING_SYSTEM$NUM = factorial_design$NUM[i]
                FIRM$COSTING_SYSTEM$CP_HEURISTIC = factorial_design$CP_HEURISTIC[i]
                FIRM$COSTING_SYSTEM$CD_HEURISTIC = factorial_design$CD_HEURISTIC[i]
                        
                        
                #CONTROL VARIABLES       
                FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO = NUMB_PRO
                FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES = NUMB_RES
                FIRM$PRODUCTION_ENVIRONMENT$set_DEMAND_fix = set_DEMAND_fix
                FIRM$PRODUCTION_ENVIRONMENT$set_RES_CONS_PAT_fix = set_RES_CONS_PAT_fix
                FIRM$COSTING_SYSTEM$TC = TC
                FIRM$COSTING_SYSTEM$set_CSD_fix = set_CSD_fix
                FIRM$COSTING_SYSTEM$set_RCC_fix = set_RCC_fix
                FIRM$COSTING_SYSTEM$set_RCU_fix = set_RCU_fix
   



   #### ============================== SIMULATION ======================================
   
    FIRM = gen_ProductionEnvironment(FIRM,set_PE_constant) #Generate Production Environment with RES_CONS_PAT
 
    ##Building the cost pools
    if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 'base'){FIRM = MAP_RES_CP_RANDOM(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 0){FIRM = MAP_RES_CP_SIZE_MISC(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 1){FIRM = MAP_RES_CP_SIZE_CORREL_MISC_ANAND(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 2){FIRM = MAP_RES_CP_SIZE_RANDOM_MISC(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 3){FIRM = MAP_RES_CP_SIZE_CORREL_CUTOFF_MISC_ANAND(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 4){FIRM = MAP_CP_CORREL_MISC(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 5){FIRM = MAP_RES_CP_SIZE_CORREL(FIRM)}

    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 6){FIRM = MAP_RES_CP_SIZE_RANDOM(FIRM)}
    
    else if(FIRM$COSTING_SYSTEM$CP_HEURISTIC == 7){FIRM = MAP_RES_CP_RANDOM_CORREL(FIRM)}

    else{print("error: no chosen CP_Heuristic")}

    # Selecting the drivers of a cost pool
    if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 'base'){FIRM = MAP_CP_P_RANDOM(FIRM,Error,NUMB_Error)}

    else if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 0){FIRM = MAP_CP_P_BIGPOOL(FIRM,Error,NUMB_Error)}

    else if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 1){FIRM = MAP_CP_P_AVERAGE(FIRM,Error,NUMB_Error)}

    else if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 2){FIRM = MAP_CP_P_INDEXED(FIRM,Error,NUMB_Error)}
    
    else if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 2 & FIRM$COSTING_SYSTEM$NUM == 0){FIRM = MAP_CP_P_RANDOM(FIRM,Error,NUMB_Error)}
    
    else if(FIRM$COSTING_SYSTEM$CD_HEURISTIC == 3){FIRM = MAP_CP_P_DIVISION(FIRM,Error,NUMB_Error)}

    else{print("error: no chosen CD_Heuristic")}
    ## Calculating the estimated product costs

    FIRM$COSTING_SYSTEM$PCH =  FIRM$COSTING_SYSTEM$ACT_CONS_PAT %*% FIRM$COSTING_SYSTEM$ACP # CHECKED 2019/09/12

    ## ERROR MEASURES AFTER LABRO & VANHOUCKE 2007
    EUCD = round(sqrt(sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2)
    MAPE = round(mean(abs(FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB),digits=4)
    MSE = round(mean(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)^2)),digits=2);

    
    ## OVER AND UNDERCOSTING ##
    
    
    TVC_BIAS = cor(FIRM$PRODUCTION_ENVIRONMENT$DEMAND,(FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH))
    
    UC = sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)>0)/NUMB_PRO
    OC = sum((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)<=0)/NUMB_PRO

    UC5 = sum(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB)>0.05)/NUMB_PRO
    OC5 = sum(((FIRM$COSTING_SYSTEM$PCB-FIRM$COSTING_SYSTEM$PCH)/FIRM$COSTING_SYSTEM$PCB)<=-0.05)/NUMB_PRO
    

  #### ======== COLLECTING THE DATA FOR OUTPUT ==== ####

      preData = data.frame(
                         FIRM$COSTING_SYSTEM$CP_HEURISTIC,
                         FIRM$COSTING_SYSTEM$CD_HEURISTIC,
                         FIRM$COSTING_SYSTEM$CP,
                         FIRM$COSTING_SYSTEM$RC_VAR_draw,
                         FIRM$COSTING_SYSTEM$NUMB_Error,
                         FIRM$COSTING_SYSTEM$Error,
                         FIRM$COSTING_SYSTEM$CC,
                         FIRM$PRODUCTION_ENVIRONMENT$DENS_draw,
                         FIRM$PRODUCTION_ENVIRONMENT$COR1_draw,
                         FIRM$PRODUCTION_ENVIRONMENT$COR2_draw,
                         FIRM$PRODUCTION_ENVIRONMENT$Q_VAR_draw,
                         FIRM$PRODUCTION_ENVIRONMENT$NUMB_PRO,
                         FIRM$PRODUCTION_ENVIRONMENT$NUMB_RES,
                         FIRM$PRODUCTION_ENVIRONMENT$cost_structure,
                         FIRM$PRODUCTION_ENVIRONMENT$unit_level_number_share_draw,
                         FIRM$PRODUCTION_ENVIRONMENT$unit_level_cost_share_draw,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC20,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC10,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$RCC02,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q20,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q10,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$Q02,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$NonZeroConsumption,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$countNonZero,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR1,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$COR2,
                         FIRM$PRODUCTION_ENVIRONMENT$CHECK$MISCPOOL,
                         FIRM$COSTING_SYSTEM$NUM,
                         TVC_BIAS,
                         OC,
                         UC,
                         OC5,
                         UC5,
                         EUCD,
                         MAPE,
                         MSE)
    colnames(preData) = c('CPH','CDH','CP','RC_VAR', 'NUMB_ME', 'ME_AD','CC','DENS', 'COR1','COR2', 'Q_VAR',
                          'NUMB_PRO', 'NUMB_RES','CS','UL_RES','UL_COST','CHECK_RCC20','CHECK_RCC10','CHECK_RCC02','CHECK_Q20',
                          'CHECK_Q10','CHECK_Q02','CHECK_NonZeroCons','CHECK_countNonZero','COR','CHECK_COR1','CHECK_COR2',
                          'MISCPOOL','NUM','TVC_BIAS','OC','UC','OC5','UC5','EUCD','MAPE','MSE')
    preData
            }
            
            close(pb)
            stopCluster(cl)
#### ====================================OUTPUT WRITING ===================================

#output data
output = paste("output/CSD_",format(Sys.time(),"%Y-%m-%d-%H%M"),".csv", sep = "")
write.csv(DATA, file = output)
print("Cost System Design FILE has been written")


if (ProductCostOutput==1)
{
  output = paste("output/ProductCost_",format(Sys.time(),"%Y-%m-%d-%H%M"), ".csv", sep = "")
  write.csv(DATAp, file = output)
  print("Product costs FILE has been written")
}


