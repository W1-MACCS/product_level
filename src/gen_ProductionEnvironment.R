.gen_cost_hierarchy<-function(){
  
  #productiolevel = between 40% and 80% of all costs are on unit- and batch-level costs
  productionlevel= runif(1,0.4,0.8)
  
  unit_level = runif(1,0.5*productionlevel,0.75*productionlevel)#in the paper 0.6 and 0.85
  batch_level = productionlevel-unit_level
  product_level = runif(1,0.25*(1-productionlevel),0.75*(1-productionlevel))
  facility_level = (1-productionlevel)-product_level
  sum(unit_level,batch_level,product_level,facility_level)

  
  unit_level_resources = round(unit_level*NUMB_RES)
  batch_level_resources = round(batch_level*NUMB_RES)
  product_level_resources = round(product_level*NUMB_RES)
  facility_level_resources = round(facility_level*NUMB_RES)
  
  if(sum(unit_level_resources,batch_level_resources,product_level_resources,facility_level_resources)>50){
    unit_level_resources = unit_level_resources-(sum(unit_level_resources,batch_level_resources,product_level_resources,facility_level_resources)-50)
  }
  if(sum(unit_level_resources,batch_level_resources,product_level_resources,facility_level_resources)<50){
    unit_level_resources = unit_level_resources+(50-sum(unit_level_resources,batch_level_resources,product_level_resources,facility_level_resources))
  }
  
  ul = c(1:unit_level_resources)
  bl = c((unit_level_resources+1):(unit_level_resources+batch_level_resources))
  pl = c((max(bl)+1):(max(bl)+product_level_resources))
  fl = c((max(pl)+1):(max(pl)+facility_level_resources))
  #if(max(fl)>50){browser()}
  
  cost_hierarchy = list()
  cost_hierarchy$ul = ul
  cost_hierarchy$bl = bl
  cost_hierarchy$pl = pl
  cost_hierarchy$fl = fl
  
  return(cost_hierarchy)
  
}


.gen_Demand_Anand <- function(Q_VAR,NUMB_PRO){
  
  LB = 10
  
  if(Q_VAR == "LOW"){UB = 20}else if(Q_VAR == "MID"){UB= 40}else if(Q_VAR=='HIGH'){UB = 60}
 
  DEMAND = as.integer(runif(NUMB_PRO,LB,UB))
  
  
  return(DEMAND)
}

.gen_Demand <- function(Q_VAR,NUMB_PRO){
  
  
  if(Q_VAR == "LOW"){sd = 0.5}else if(Q_VAR == "MID"){sd = 1}else if(Q_VAR=='HIGH'){sd = 1.5}else if(Q_VAR == -1){sd = runif(1,min = 0.5,max =1.5)}
  units = 10^3
  preDemand = rlnorm(NUMB_PRO, meanlog = 1, sdlog = sd) #preDemand is buildup as a -> LogNormal Distribution 
  DEMAND = ceiling((preDemand/sum(preDemand))*units)
  
  return(DEMAND)
  
}


##original model
.gen_RES_CONS_PAT_Anand <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ,cost_hierarchy) {
  
  RES_CONS_PAT_list = list()
  
  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  
  DENS_draw = DENS
  
  ## ====================== STEP 1 BASELINE NORM ========================= 
  
  repeat    {
    
    BASE = rnorm(NUMB_PRO) #creates for every CO (product) a random number
    
    RES_CONS_PATpre = matrix(rnorm(NUMB_PRO*NUMB_RES,mean=0,sd=1), 
                             NUMB_PRO, NUMB_RES)                            #random pre matrix, as Baseline
    
    RES_CONS_PAT = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE) #empy matrix, that is going to be filled 
    
    


    ## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    
    

    # Correlation of the top [DISP1] resources
    if(COR1 == -1){
      COR1 <- runif(1, -0.2, 0.8)
    }
    COR1_draw = COR1
    sqrt_const_1 <- sqrt(1 - (COR1 * COR1))
    
    # Correlation of the remaining resources
    if(COR2 == -1){
      COR2 <- runif(1, -0.8, 0.2)
    }
    COR2_draw = COR2
    sqrt_const_2 <- sqrt(1 - (COR2 * COR2))

    
    
    for (i in 1:(DISP1-1)) #unitsize+1
    {
      RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_1 * RES_CONS_PATpre[,(i)];
    }
    
    for (i in ((DISP1-1)) : NUMB_RES) #nonunitsize+1 (34+1)
    {
      RES_CONS_PAT[,i] <- (COR2 * BASE)+ sqrt_const_2 * RES_CONS_PATpre[,(i)];
    }
    
    ## ====================== STEP 1.b DENSITY =========================
    
    res_cons_pat_b_pre = runif(NUMB_PRO*NUMB_RES)
    ## 1/0 DENSITY
    res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
                              NUMB_PRO,NUMB_RES)
    
    RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
    

    ## ====================== STEP 1.c Ceiling and Scaling ============= 
    
    # take absolute value of X and Z and scale by 10 and round them
    # Anand et al. 2019
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
    RES_CONS_PAT[,1] <- (BASE)
    RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
    
    #EAD structure
    
    #A_PFR, N_FR<=N_P<=2^N_FR-1 (linearly independent rows)
    NUMB_FR_min = ceiling(log(NUMB_PRO+1)/log(2))
    NUMB_FR = round(runif(1,NUMB_FR_min,NUMB_PRO))
    A_PFR <- diag(NUMB_PRO)
    for(i in c(1:(NUMB_PRO-1))){
      v <- runif(n=(ncol(A_PFR)-i),0,1)
      v[which(v>=DENS)] <- 1
      v[which(v<DENS)] <- 0
      A_PFR[i,c((i+1):ncol(A_PFR))] <- v
    }
    A_PFR <- A_PFR[,c((NUMB_PRO-NUMB_FR):NUMB_PRO)]
    
    #A_FRCM, N_CM=N_P
    A_FRCM <- diag(NUMB_PRO)
    for(i in c(1:(NUMB_PRO-1))){
      v <- runif(n=(ncol(A_FRCM)-i),0,1)
      v[which(v>=DENS)] <- 1
      v[which(v<DENS)] <- 0
      A_FRCM[i,c((i+1):ncol(A_FRCM))] <- v
    }
    A_FRCM <- A_FRCM[c(1:ncol(A_PFR)),]
    
    A_PCM <- A_PFR%*%A_FRCM #N_P x N_P dimension
    # RES_CONS_PAT <- A_PCM%*%RES_CONS_PAT
    
    #product portfolio --> matching demand with complexity 
    complexity = rowSums(RES_CONS_PAT)
    complexity_sorted = sort(complexity,decreasing=TRUE,index.return=TRUE)
    mxq_sorted = sort(MXQ,index.return=TRUE)$ix

    RES_CONS_PAT_new = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE)

    for(i in 1:length(complexity)){
      
      complexity_index = complexity_sorted$ix[i]
      RES_CONS_PAT_new[mxq_sorted[i],] <- RES_CONS_PAT[complexity_index,]

    }
    #RES_CONS_PAT = RES_CONS_PAT_new
   
    # 
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    #RES_CONS_PAT_TOTAL = RES_CONS_PAT*MXQ
    RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this need to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
    
    
    # browser()
    # rescons = melt(RES_CONS_PAT)
    # colnames(rescons) = c("NUMB_PRO","NUMB_RES","value")
    # 
    # 
    # ggplot(rescons, aes(x= NUMB_RES,y=NUMB_PRO,fill=value))+geom_tile()+theme_classic()+
    #   scale_fill_gradientn(colours = c("white","blue"))

    ## ===================== EXCPETION HANDLER ====================
    
    
    PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource (exclude column one??)
    RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
    BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?
    
    if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
    {
      break
    }
    
  }
  
  ul = c(1:DISP1)
  bl = c((DISP1+1):NUMB_RES)
  pl = 0
  fl = 0
  
  
  
  # ul_bl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,bl]))
  # bl_pl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,pl]))
  # ul_pl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,pl]))
  # bl_fl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,fl]))
  # pl_fl = mean(cor(RES_CONS_PATp[,pl],RES_CONS_PATp[,fl]))
  # ul_fl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,fl]))
  # 
  
  ul_bl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,bl]))
  bl_pl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,pl]))
  ul_pl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,pl]))
  bl_fl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,fl]))
  pl_fl = cor(rowMeans(RES_CONS_PATp[,pl]),rowMeans(RES_CONS_PATp[,fl]))
  ul_fl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,fl]))
  mxq_ul = cor(MXQ,rowMeans(RES_CONS_PATp[,ul]))
  mxq_bl = cor(MXQ,rowMeans(RES_CONS_PATp[,bl]))
  mxq_pl = cor(MXQ,rowMeans(RES_CONS_PATp[,pl]))
  mxq_fl = cor(MXQ,rowMeans(RES_CONS_PATp[,fl]))
  
  
  cost_hierarchy$ul = ul
  cost_hierarchy$bl = bl
  cost_hierarchy$pl = pl
  cost_hierarchy$fl = fl
  
  cost_hierarchy$ul_bl = ul_bl
  cost_hierarchy$bl_pl = bl_pl
  cost_hierarchy$ul_pl = ul_pl
  cost_hierarchy$bl_fl = bl_fl
  cost_hierarchy$pl_fl = pl_fl
  cost_hierarchy$ul_fl = ul_fl
  
RES_CONS_PAT_list$DENS = DENS_draw
RES_CONS_PAT_list$COR1 = COR1_draw
RES_CONS_PAT_list$COR2 = COR2_draw
RES_CONS_PAT_list$non_unit_size = 0 #all costs are unit-level
RES_CONS_PAT_list$RES_CONS_PAT = RES_CONS_PAT
RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
RES_CONS_PAT_list$RES_CONS_PATp = RES_CONS_PATp
RES_CONS_PAT_list$cost_hierarchy = cost_hierarchy
  #
  #
  return(RES_CONS_PAT_list)
  
  
}

.gen_RES_CONS_PAT_Anand_match <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ,cost_hierarchy) {
  
  RES_CONS_PAT_list = list()
  
  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  
  DENS_draw = DENS
  
  ## ====================== STEP 1 BASELINE NORM ========================= 
  
  repeat    {
    
    BASE = rnorm(NUMB_PRO) #creates for every CO (product) a random number
    
    RES_CONS_PATpre = matrix(rnorm(NUMB_PRO*NUMB_RES,mean=0,sd=1), 
                             NUMB_PRO, NUMB_RES)                            #random pre matrix, as Baseline
    
    RES_CONS_PAT = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE) #empy matrix, that is going to be filled 
    
    
    
    
    ## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    
    
    
    # Correlation of the top [DISP1] resources
    if(COR1 == -1){
      COR1 <- runif(1, -0.2, 0.8)
    }
    COR1_draw = COR1
    sqrt_const_1 <- sqrt(1 - (COR1 * COR1))
    
    # Correlation of the remaining resources
    if(COR2 == -1){
      COR2 <- runif(1, -0.8, 0.2)
    }
    COR2_draw = COR2
    sqrt_const_2 <- sqrt(1 - (COR2 * COR2))
    
    
    
    for (i in 1:(DISP1-1)) #unitsize+1
    {
      RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_1 * RES_CONS_PATpre[,(i)];
    }
    
    for (i in ((DISP1-1)) : NUMB_RES) #nonunitsize+1 (34+1)
    {
      RES_CONS_PAT[,i] <- (COR2 * BASE)+ sqrt_const_2 * RES_CONS_PATpre[,(i)];
    }
    
    ## ====================== STEP 1.b DENSITY =========================
    
    res_cons_pat_b_pre = runif(NUMB_PRO*NUMB_RES)
    ## 1/0 DENSITY
    res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
                              NUMB_PRO,NUMB_RES)
    
    RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
    
    
    ## ====================== STEP 1.c Ceiling and Scaling ============= 
    
    # take absolute value of X and Z and scale by 10 and round them
    # Anand et al. 2019
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
    RES_CONS_PAT[,1] <- (BASE)
    RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
    
    #product portfolio --> matching demand with complexity 
    complexity = calc_individuality(RES_CONS_PAT)
    #complexity = calc_directed_inter(RES_CONS_PAT)
    complexity_sorted = sort(complexity,decreasing=FALSE,index.return=TRUE)
    mxq_sorted = sort(MXQ,index.return=TRUE)$ix
    
    RES_CONS_PAT_new = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE)
    
    for(i in 1:length(complexity)){
      
      complexity_index = complexity_sorted$ix[i]
      RES_CONS_PAT_new[mxq_sorted[i],] <- RES_CONS_PAT[complexity_index,]
      
    }
    RES_CONS_PAT = RES_CONS_PAT_new
    
    # 
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    #RES_CONS_PAT_TOTAL = RES_CONS_PAT*MXQ
    RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this needs to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
    
    
    ## ===================== EXCPETION HANDLER ====================
    
    
    PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource (exclude column one??)
    RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
    BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?
    
    if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
    {
      break
    }
    
  }
  
  ul = c(1:DISP1)
  bl = c((DISP1+1):NUMB_RES)
  pl = 0
  fl = 0
  
  
  
  # ul_bl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,bl]))
  # bl_pl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,pl]))
  # ul_pl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,pl]))
  # bl_fl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,fl]))
  # pl_fl = mean(cor(RES_CONS_PATp[,pl],RES_CONS_PATp[,fl]))
  # ul_fl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,fl]))
  # 
  
  ul_bl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,bl]))
  bl_pl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,pl]))
  ul_pl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,pl]))
  bl_fl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,fl]))
  pl_fl = cor(rowMeans(RES_CONS_PATp[,pl]),rowMeans(RES_CONS_PATp[,fl]))
  ul_fl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,fl]))
  mxq_ul = cor(MXQ,rowMeans(RES_CONS_PATp[,ul]))
  mxq_bl = cor(MXQ,rowMeans(RES_CONS_PATp[,bl]))
  mxq_pl = cor(MXQ,rowMeans(RES_CONS_PATp[,pl]))
  mxq_fl = cor(MXQ,rowMeans(RES_CONS_PATp[,fl]))
  
  
  cost_hierarchy$ul = ul
  cost_hierarchy$bl = bl
  cost_hierarchy$pl = pl
  cost_hierarchy$fl = fl
  
  cost_hierarchy$ul_bl = ul_bl
  cost_hierarchy$bl_pl = bl_pl
  cost_hierarchy$ul_pl = ul_pl
  cost_hierarchy$bl_fl = bl_fl
  cost_hierarchy$pl_fl = pl_fl
  cost_hierarchy$ul_fl = ul_fl
  
  RES_CONS_PAT_list$DENS = DENS_draw
  RES_CONS_PAT_list$COR1 = COR1_draw
  RES_CONS_PAT_list$COR2 = COR2_draw
  RES_CONS_PAT_list$non_unit_size = 0 #all costs are unit-level
  RES_CONS_PAT_list$RES_CONS_PAT = RES_CONS_PAT
  RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
  RES_CONS_PAT_list$RES_CONS_PATp = RES_CONS_PATp
  RES_CONS_PAT_list$cost_hierarchy = cost_hierarchy
  #
  #
  return(RES_CONS_PAT_list)
  
  
}

.gen_RES_CONS_PAT_Case_match <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ,cost_hierarchy, RES_CONS_PAT) {
  
  RES_CONS_PAT_list = list()
  
  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  
  DENS_draw = DENS
  
  ## ====================== STEP 1 BASELINE NORM ========================= 
  
 
    ## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    
    
    
    # Correlation of the top [DISP1] resources
    if(COR1 == -1){
      COR1 <- runif(1, -0.2, 0.8)
    }
    COR1_draw = COR1
    sqrt_const_1 <- sqrt(1 - (COR1 * COR1))
    
    # Correlation of the remaining resources
    if(COR2 == -1){
      COR2 <- runif(1, -0.8, 0.2)
    }
    COR2_draw = COR2
    sqrt_const_2 <- sqrt(1 - (COR2 * COR2))
    
    ####CASE STUDY LOADING#######
    
    
    
    
    
    
    
    #product portfolio --> matching demand with complexity 
    complexity = calc_nonzero_cons(RES_CONS_PAT)*rowMeans(RES_CONS_PAT)
    #complexity = calc_directed_inter(RES_CONS_PAT)
    complexity_sorted = sort(complexity,decreasing=TRUE,index.return=TRUE)
    mxq_sorted = sort(MXQ,index.return=TRUE)$ix
    
    RES_CONS_PAT_new = RES_CONS_PAT
    

    for(i in 1:length(complexity)){
      
      complexity_index = complexity_sorted$ix[i]
      RES_CONS_PAT_new[mxq_sorted[i],] <- RES_CONS_PAT[complexity_index,]
   
    }
    RES_CONS_PAT = RES_CONS_PAT_new
    
    # 
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    #RES_CONS_PAT_TOTAL = RES_CONS_PAT*MXQ
    RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this needs to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
    
    

  ul = c(1:DISP1)
  bl = c((DISP1+1):NUMB_RES)
  pl = 0
  fl = 0
  
  
  
  # ul_bl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,bl]))
  # bl_pl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,pl]))
  # ul_pl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,pl]))
  # bl_fl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,fl]))
  # pl_fl = mean(cor(RES_CONS_PATp[,pl],RES_CONS_PATp[,fl]))
  # ul_fl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,fl]))
  # 
  
  ul_bl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,bl]))
  bl_pl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,pl]))
  ul_pl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,pl]))
  bl_fl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,fl]))
  pl_fl = cor(rowMeans(RES_CONS_PATp[,pl]),rowMeans(RES_CONS_PATp[,fl]))
  ul_fl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,fl]))
  mxq_ul = cor(MXQ,rowMeans(RES_CONS_PATp[,ul]))
  mxq_bl = cor(MXQ,rowMeans(RES_CONS_PATp[,bl]))
  mxq_pl = cor(MXQ,rowMeans(RES_CONS_PATp[,pl]))
  mxq_fl = cor(MXQ,rowMeans(RES_CONS_PATp[,fl]))
  
  
  cost_hierarchy$ul = ul
  cost_hierarchy$bl = bl
  cost_hierarchy$pl = pl
  cost_hierarchy$fl = fl
  
  cost_hierarchy$ul_bl = ul_bl
  cost_hierarchy$bl_pl = bl_pl
  cost_hierarchy$ul_pl = ul_pl
  cost_hierarchy$bl_fl = bl_fl
  cost_hierarchy$pl_fl = pl_fl
  cost_hierarchy$ul_fl = ul_fl
  
  RES_CONS_PAT_list$DENS = DENS_draw
  RES_CONS_PAT_list$COR1 = COR1_draw
  RES_CONS_PAT_list$COR2 = COR2_draw
  RES_CONS_PAT_list$non_unit_size = 0 #all costs are unit-level
  RES_CONS_PAT_list$RES_CONS_PAT = as.matrix(RES_CONS_PAT)
  RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = as.matrix(RES_CONS_PAT_TOTAL)
  RES_CONS_PAT_list$RES_CONS_PATp = as.matrix(RES_CONS_PATp)
  RES_CONS_PAT_list$cost_hierarchy = cost_hierarchy
  #
  #
  return(RES_CONS_PAT_list)
  
  
}

.gen_RES_CONS_PAT_Case <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ,cost_hierarchy, RES_CONS_PAT) {
  
  RES_CONS_PAT_list = list()
  
  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  
  DENS_draw = DENS
  
  ## ====================== STEP 1 BASELINE NORM ========================= 
  
  
  ## ====================== STEP 1.a CORRELATION ========================= 
  # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
  # Rows Products Colums Resources
  
  
  
  # Correlation of the top [DISP1] resources
  if(COR1 == -1){
    COR1 <- runif(1, -0.2, 0.8)
  }
  COR1_draw = COR1
  sqrt_const_1 <- sqrt(1 - (COR1 * COR1))
  
  # Correlation of the remaining resources
  if(COR2 == -1){
    COR2 <- runif(1, -0.8, 0.2)
  }
  COR2_draw = COR2
  sqrt_const_2 <- sqrt(1 - (COR2 * COR2))
  
  ####CASE STUDY LOADING#######
  
  
  
  
  
  
  
  #product portfolio --> matching demand with complexity 
  complexity = calc_nonzero_cons(RES_CONS_PAT)*rowMeans(RES_CONS_PAT)
  #complexity = calc_directed_inter(RES_CONS_PAT)
  complexity_sorted = sort(complexity,decreasing=TRUE,index.return=TRUE)
  mxq_sorted = sort(MXQ,index.return=TRUE)$ix
  
  RES_CONS_PAT_new = RES_CONS_PAT
  
  
  for(i in 1:length(complexity)){
    
    complexity_index = complexity_sorted$ix[i]
    RES_CONS_PAT_new[mxq_sorted[i],] <- RES_CONS_PAT[complexity_index,]
    
  }
  #RES_CONS_PAT = RES_CONS_PAT_new
  
  # 
  ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
  #RES_CONS_PAT_TOTAL = RES_CONS_PAT*MXQ
  RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this needs to be a matrix multiplication?
  ##CALCULATING TCU
  TCU <- colSums(RES_CONS_PAT_TOTAL)
  ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
  RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
  
  
  
  ul = c(1:DISP1)
  bl = c((DISP1+1):NUMB_RES)
  pl = 0
  fl = 0
  
  
  
  # ul_bl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,bl]))
  # bl_pl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,pl]))
  # ul_pl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,pl]))
  # bl_fl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,fl]))
  # pl_fl = mean(cor(RES_CONS_PATp[,pl],RES_CONS_PATp[,fl]))
  # ul_fl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,fl]))
  # 
  
  ul_bl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,bl]))
  bl_pl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,pl]))
  ul_pl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,pl]))
  bl_fl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,fl]))
  pl_fl = cor(rowMeans(RES_CONS_PATp[,pl]),rowMeans(RES_CONS_PATp[,fl]))
  ul_fl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,fl]))
  mxq_ul = cor(MXQ,rowMeans(RES_CONS_PATp[,ul]))
  mxq_bl = cor(MXQ,rowMeans(RES_CONS_PATp[,bl]))
  mxq_pl = cor(MXQ,rowMeans(RES_CONS_PATp[,pl]))
  mxq_fl = cor(MXQ,rowMeans(RES_CONS_PATp[,fl]))
  
  
  cost_hierarchy$ul = ul
  cost_hierarchy$bl = bl
  cost_hierarchy$pl = pl
  cost_hierarchy$fl = fl
  
  cost_hierarchy$ul_bl = ul_bl
  cost_hierarchy$bl_pl = bl_pl
  cost_hierarchy$ul_pl = ul_pl
  cost_hierarchy$bl_fl = bl_fl
  cost_hierarchy$pl_fl = pl_fl
  cost_hierarchy$ul_fl = ul_fl
  
  RES_CONS_PAT_list$DENS = DENS_draw
  RES_CONS_PAT_list$COR1 = COR1_draw
  RES_CONS_PAT_list$COR2 = COR2_draw
  RES_CONS_PAT_list$non_unit_size = 0 #all costs are unit-level
  RES_CONS_PAT_list$RES_CONS_PAT = as.matrix(RES_CONS_PAT)
  RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = as.matrix(RES_CONS_PAT_TOTAL)
  RES_CONS_PAT_list$RES_CONS_PATp = as.matrix(RES_CONS_PATp)
  RES_CONS_PAT_list$cost_hierarchy = cost_hierarchy
  #
  #
  return(RES_CONS_PAT_list)
  
  
}



.gen_RES_CONS_PAT_diag <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ,cost_hierarchy) {
  
  RES_CONS_PAT_list = list()
  
  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  
  DENS_draw = DENS
  COR1_draw = 0
  COR2_draw = 0
  
  ## ====================== STEP 1 BASELINE NORM ========================= 
  
  repeat    {
    
    BASE = rnorm(NUMB_PRO) #creates for every CO (product) a random number
    
    RES_CONS_PATpre = matrix(rnorm(NUMB_PRO*NUMB_RES,mean=0,sd=1), 
                             NUMB_PRO, NUMB_RES)                            #random pre matrix, as Baseline
    
    RES_CONS_PAT = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE) #empy matrix, that is going to be filled 
    
    
    
    
    ## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    

    for(i in 1:NUMB_RES){
      random_vector =  rnorm(NUMB_PRO, mean = i, sd = i*DENS)
      random_vector = unique(round(abs(random_vector),0))
      random_vector = random_vector[random_vector>0 & random_vector<= NUMB_PRO]
      
      RES_CONS_PAT[random_vector,i] <- 1
      
    }
    
    RES_CONS_PATpre = matrix(rnorm(NUMB_PRO*NUMB_RES,mean=0,sd=1),
                             NUMB_PRO, NUMB_RES)
    
    RES_CONS_PAT = RES_CONS_PAT * RES_CONS_PATpre
    
    #browser()
    #RES_CONS_PAT = RES_CONS_PAT[,sample(c(1:NUMB_RES),NUMB_RES)]
    RES_CONS_PAT[,c(2:NUMB_RES)] = RES_CONS_PAT[,sort(c(2:NUMB_RES),decreasing = TRUE)]
    ## ====================== STEP 1.b DENSITY =========================
    # 
    # res_cons_pat_b_pre = runif(NUMB_PRO*NUMB_RES)
    # ## 1/0 DENSITY
    # res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
    #                           NUMB_PRO,NUMB_RES)
    # 
    # RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
    
    
    ## ====================== STEP 1.c Ceiling and Scaling ============= 
    
    # take absolute value of X and Z and scale by 10 and round them
    # Anand et al. 2019
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
    RES_CONS_PAT[,1] <- (BASE)
    RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
    
    #product portfolio --> matching demand with complexity 
    complexity = rowSums(RES_CONS_PAT)
    complexity_sorted = sort(complexity,decreasing=TRUE,index.return=TRUE)
    mxq_sorted = sort(MXQ,index.return=TRUE)$ix
    
    RES_CONS_PAT_new = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE)
    
    for(i in 1:length(complexity)){
      
      complexity_index = complexity_sorted$ix[i]
      RES_CONS_PAT_new[mxq_sorted[i],] <- RES_CONS_PAT[complexity_index,]
      
    }
    #RES_CONS_PAT = RES_CONS_PAT_new
    
    # 
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    #RES_CONS_PAT_TOTAL = RES_CONS_PAT*MXQ
    RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this needs to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
# 
    # browser()
    # rescons = melt(RES_CONS_PAT)
    # colnames(rescons) = c("NUMB_PRO","NUMB_RES","value")
    # 
    # 
    # ggplot(rescons, aes(x= NUMB_RES,y=NUMB_PRO,fill=value))+geom_tile()+theme_classic()+
    #   scale_fill_gradientn(colours = c("white","blue"))
    ## ===================== EXCPETION HANDLER ====================
    
    
    PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource (exclude column one??)
    RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
    BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?
    
    if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
    {
      break
    }
    
  }
  
  ul = c(1:DISP1)
  bl = c((DISP1+1):NUMB_RES)
  pl = 0
  fl = 0
  
  
  
  # ul_bl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,bl]))
  # bl_pl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,pl]))
  # ul_pl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,pl]))
  # bl_fl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,fl]))
  # pl_fl = mean(cor(RES_CONS_PATp[,pl],RES_CONS_PATp[,fl]))
  # ul_fl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,fl]))
  # 
  
  ul_bl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,bl]))
  bl_pl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,pl]))
  ul_pl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,pl]))
  bl_fl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,fl]))
  pl_fl = cor(rowMeans(RES_CONS_PATp[,pl]),rowMeans(RES_CONS_PATp[,fl]))
  ul_fl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,fl]))
  mxq_ul = cor(MXQ,rowMeans(RES_CONS_PATp[,ul]))
  mxq_bl = cor(MXQ,rowMeans(RES_CONS_PATp[,bl]))
  mxq_pl = cor(MXQ,rowMeans(RES_CONS_PATp[,pl]))
  mxq_fl = cor(MXQ,rowMeans(RES_CONS_PATp[,fl]))
  
  
  cost_hierarchy$ul = ul
  cost_hierarchy$bl = bl
  cost_hierarchy$pl = pl
  cost_hierarchy$fl = fl
  
  cost_hierarchy$ul_bl = ul_bl
  cost_hierarchy$bl_pl = bl_pl
  cost_hierarchy$ul_pl = ul_pl
  cost_hierarchy$bl_fl = bl_fl
  cost_hierarchy$pl_fl = pl_fl
  cost_hierarchy$ul_fl = ul_fl
  
  RES_CONS_PAT_list$DENS = DENS_draw
  RES_CONS_PAT_list$COR1 = COR1_draw
  RES_CONS_PAT_list$COR2 = COR2_draw
  RES_CONS_PAT_list$non_unit_size = 0 #all costs are unit-level
  RES_CONS_PAT_list$RES_CONS_PAT = RES_CONS_PAT
  RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
  RES_CONS_PAT_list$RES_CONS_PATp = RES_CONS_PATp
  RES_CONS_PAT_list$cost_hierarchy = cost_hierarchy
  #
  #
  return(RES_CONS_PAT_list)
  
  
}

.gen_RES_CONS_PAT_diag_match <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ,cost_hierarchy) {
  
  RES_CONS_PAT_list = list()
  
  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  
  DENS_draw = DENS
  COR1_draw = 0
  COR2_draw = 0
  
  ## ====================== STEP 1 BASELINE NORM ========================= 
  
  repeat    {
    
    BASE = rnorm(NUMB_PRO) #creates for every CO (product) a random number
    
    RES_CONS_PATpre = matrix(rnorm(NUMB_PRO*NUMB_RES,mean=0,sd=1), 
                             NUMB_PRO, NUMB_RES)                            #random pre matrix, as Baseline
    
    RES_CONS_PAT = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE) #empy matrix, that is going to be filled 
    
    
    
    
    ## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    
    
    for(i in 1:NUMB_RES){
      random_vector =  rnorm(NUMB_PRO, mean = i, sd = i*DENS)
      random_vector = unique(round(abs(random_vector),0))
      random_vector = random_vector[random_vector>0 & random_vector<= NUMB_PRO]
      
      RES_CONS_PAT[random_vector,i] <- 1
      
    }
    
    RES_CONS_PATpre = matrix(rnorm(NUMB_PRO*NUMB_RES,mean=0,sd=1),
                             NUMB_PRO, NUMB_RES)
    
    RES_CONS_PAT = RES_CONS_PAT * RES_CONS_PATpre
    
    #browser()
    #RES_CONS_PAT = RES_CONS_PAT[,sample(c(1:NUMB_RES),NUMB_RES)]
    RES_CONS_PAT[,c(2:NUMB_RES)] = RES_CONS_PAT[,sort(c(2:NUMB_RES),decreasing = TRUE)]
    ## ====================== STEP 1.b DENSITY =========================
    # 
    # res_cons_pat_b_pre = runif(NUMB_PRO*NUMB_RES)
    # ## 1/0 DENSITY
    # res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
    #                           NUMB_PRO,NUMB_RES)
    # 
    # RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
    
    
    ## ====================== STEP 1.c Ceiling and Scaling ============= 
    
    # take absolute value of X and Z and scale by 10 and round them
    # Anand et al. 2019
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
    RES_CONS_PAT[,1] <- (BASE)
    RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
    
    #product portfolio --> matching demand with complexity 
    complexity = calc_nonzero_cons(RES_CONS_PAT)
    complexity_sorted = sort(complexity,decreasing=TRUE,index.return=TRUE)
    mxq_sorted = sort(MXQ,index.return=TRUE)$ix
    
    RES_CONS_PAT_new = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE)
    
    for(i in 1:length(complexity)){
      
      complexity_index = complexity_sorted$ix[i]
      RES_CONS_PAT_new[mxq_sorted[i],] <- RES_CONS_PAT[complexity_index,]
      
    }
    RES_CONS_PAT = RES_CONS_PAT_new
    
    # 
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    #RES_CONS_PAT_TOTAL = RES_CONS_PAT*MXQ
    RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this needs to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
    
    # browser()
    # rescons = melt(RES_CONS_PAT)
    # colnames(rescons) = c("NUMB_PRO","NUMB_RES","value")
    # 
    # 
    # ggplot(rescons, aes(x= NUMB_RES,y=NUMB_PRO,fill=value))+geom_tile()+theme_classic()+
    #   scale_fill_gradientn(colours = c("white","blue"))
    ## ===================== EXCPETION HANDLER ====================
    
    
    PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource (exclude column one??)
    RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
    BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?
    
    if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
    {
      break
    }
    
  }
  

  ul = c(1:DISP1)
  bl = c((DISP1+1):NUMB_RES)
  pl = 0
  fl = 0
  
  
  
  # ul_bl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,bl]))
  # bl_pl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,pl]))
  # ul_pl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,pl]))
  # bl_fl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,fl]))
  # pl_fl = mean(cor(RES_CONS_PATp[,pl],RES_CONS_PATp[,fl]))
  # ul_fl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,fl]))
  # 
  
  ul_bl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,bl]))
  bl_pl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,pl]))
  ul_pl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,pl]))
  bl_fl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,fl]))
  pl_fl = cor(rowMeans(RES_CONS_PATp[,pl]),rowMeans(RES_CONS_PATp[,fl]))
  ul_fl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,fl]))
  mxq_ul = cor(MXQ,rowMeans(RES_CONS_PATp[,ul]))
  mxq_bl = cor(MXQ,rowMeans(RES_CONS_PATp[,bl]))
  mxq_pl = cor(MXQ,rowMeans(RES_CONS_PATp[,pl]))
  mxq_fl = cor(MXQ,rowMeans(RES_CONS_PATp[,fl]))
  
  
  cost_hierarchy$ul = ul
  cost_hierarchy$bl = bl
  cost_hierarchy$pl = pl
  cost_hierarchy$fl = fl
  
  cost_hierarchy$ul_bl = ul_bl
  cost_hierarchy$bl_pl = bl_pl
  cost_hierarchy$ul_pl = ul_pl
  cost_hierarchy$bl_fl = bl_fl
  cost_hierarchy$pl_fl = pl_fl
  cost_hierarchy$ul_fl = ul_fl
  
  RES_CONS_PAT_list$DENS = DENS_draw
  RES_CONS_PAT_list$COR1 = COR1_draw
  RES_CONS_PAT_list$COR2 = COR2_draw
  RES_CONS_PAT_list$non_unit_size = 0 #all costs are unit-level
  RES_CONS_PAT_list$RES_CONS_PAT = RES_CONS_PAT
  RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
  RES_CONS_PAT_list$RES_CONS_PATp = RES_CONS_PATp
  RES_CONS_PAT_list$cost_hierarchy = cost_hierarchy
  #
  #
  return(RES_CONS_PAT_list)
  
  
}

##simple cost hierarchy
.gen_RES_CONS_PAT_Anand_CS <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ,cost_hierarchy) {
  
  RES_CONS_PAT_list = list()
  
  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.2;
    DENS_MAX = 0.9;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  
  DENS_draw = DENS
  
  ## ====================== STEP 1 BASELINE NORM ========================= 
  
  repeat    {
    
    BASE = rnorm(NUMB_PRO) #creates for every CO (product) a random number
    
    RES_CONS_PATpre = matrix(rnorm(NUMB_PRO*NUMB_RES,mean=0,sd=1), 
                             NUMB_PRO, NUMB_RES)                            #random pre matrix, as Baseline
    
    RES_CONS_PAT = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE) #empy matrix, that is going to be filled 
    
    
    
    
    ## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    
    
    
    # Correlation of the top [DISP1] resources
    if(COR1 == -1){
      COR1 <- runif(1, -0.8, 0.8)
    }
    COR1_draw = COR1
    sqrt_const_1 <- sqrt(1 - (COR1 * COR1))
    
    # Correlation of the remaining resources
    if(COR2 == -1){
      COR2 <- runif(1, -0.8, 0.8)
    }
    COR2_draw = COR2
    sqrt_const_2 <- sqrt(1 - (COR2 * COR2))
    
    
    
    for (i in 1:(DISP1-1)) #unitsize+1
    {
      RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_1 * RES_CONS_PATpre[,(i)];
    }
    
    for (i in ((DISP1-1)) : NUMB_RES) #nonunitsize+1 (34+1)
    {
      RES_CONS_PAT[,i] <- (COR2 * BASE)+ sqrt_const_2 * RES_CONS_PATpre[,(i)];
    }
    
    ## ====================== STEP 1.b DENSITY =========================
    
    res_cons_pat_b_pre = runif(NUMB_PRO*NUMB_RES)
    ## 1/0 DENSITY
    res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
                              NUMB_PRO,NUMB_RES)
    
    RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
    
    
    ## ====================== STEP 1.c Ceiling and Scaling ============= 
    
    # take absolute value of X and Z and scale by 10 and round them
    # Anand et al. 2019
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
    RES_CONS_PAT[,1] <- (BASE)
    RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
    #product portfolio --> matching demand with complexity 
    complexity = rowMeans(RES_CONS_PAT)
    complexity_sorted = sort(complexity,decreasing=TRUE,index.return=TRUE)
    mxq_sorted = sort(MXQ,index.return=TRUE)$ix
    
    RES_CONS_PAT_new = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE)
    
    for(i in 1:length(complexity)){
      
      complexity_index = complexity_sorted$ix[i]
      RES_CONS_PAT_new[mxq_sorted[i],] <- RES_CONS_PAT[complexity_index,]
      
    }
    #RES_CONS_PAT = RES_CONS_PAT_new
    
    
    
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    #RES_CONS_PAT_TOTAL <- RES_CONS_PAT * MXQ
    
    RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this needs to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
    
    
    ## ===================== EXCPETION HANDLER ====================
    
    PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource (exclude column one??)
    RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
    BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?
    
    if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
    {
      break
    }
    
  }
  
  RES_CONS_PATp_single = sweep((RES_CONS_PAT),2,colSums(RES_CONS_PAT),"/")
  
  non_unit_size = runif(1,0.2,0.6)
  non_unit_size = 0.5
  
  non_unit = round(non_unit_size*50)
  
  non_unit_resources = sample(c(1:50),non_unit)


  RES_CONS_PATp[,non_unit_resources] = RES_CONS_PATp_single[,non_unit_resources]
  
  ul = setdiff(c(1:50),non_unit_resources)
  bl = 0
  pl = 0
  fl = non_unit_resources
  

  # ul_bl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,bl]))
  # bl_pl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,pl]))
  # ul_pl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,pl]))
  # bl_fl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,fl]))
  # pl_fl = mean(cor(RES_CONS_PATp[,pl],RES_CONS_PATp[,fl]))
  # ul_fl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,fl]))
  
  ul_bl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,bl]))
  bl_pl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,pl]))
  ul_pl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,pl]))
  bl_fl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,fl]))
  pl_fl = cor(rowMeans(RES_CONS_PATp[,pl]),rowMeans(RES_CONS_PATp[,fl]))
  ul_fl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,fl]))
  mxq_ul = cor(MXQ,rowMeans(RES_CONS_PATp[,ul]))
  mxq_bl = cor(MXQ,rowMeans(RES_CONS_PATp[,bl]))
  mxq_pl = cor(MXQ,rowMeans(RES_CONS_PATp[,pl]))
  mxq_fl = cor(MXQ,rowMeans(RES_CONS_PATp[,fl]))
  
  
  cost_hierarchy$ul = ul
  cost_hierarchy$bl = bl
  cost_hierarchy$pl = pl
  cost_hierarchy$fl = fl
  
  
  cost_hierarchy$ul_bl = ul_bl
  cost_hierarchy$bl_pl = bl_pl
  cost_hierarchy$ul_pl = ul_pl
  cost_hierarchy$bl_fl = bl_fl
  cost_hierarchy$pl_fl = pl_fl
  cost_hierarchy$ul_fl = ul_fl
  
  RES_CONS_PAT_list$DENS = DENS_draw
  RES_CONS_PAT_list$COR1 = COR1_draw
  RES_CONS_PAT_list$COR2 = COR2_draw
  RES_CONS_PAT_list$non_unit_size = non_unit_size
  RES_CONS_PAT_list$RES_CONS_PAT = RES_CONS_PAT
  RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
  RES_CONS_PAT_list$RES_CONS_PATp = RES_CONS_PATp
  RES_CONS_PAT_list$cost_hierarchy = cost_hierarchy
  #
  #
  return(RES_CONS_PAT_list)
  
  
}

##theoretical cost hierarchy
.gen_RES_CONS_PAT_Anand_CH <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ,cost_hierarchy) {
  
  RES_CONS_PAT_list = list()
  

  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.2;
    DENS_MAX = 0.9;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  
  DENS_draw = DENS
  
  ul = cost_hierarchy$ul
  bl = cost_hierarchy$bl
  pl = cost_hierarchy$pl
  fl = cost_hierarchy$fl
  
  
  ## ====================== STEP 1 BASELINE NORM ========================= 
  
  repeat    {
    
    BASE = rnorm(NUMB_PRO,mean=1,sd=0.25) #creates for every CO (product) a random number
    
    RES_CONS_PAT = matrix(rnorm(NUMB_PRO*NUMB_RES,mean=1,sd=0.25), 
                             NUMB_PRO, NUMB_RES)                            #random pre matrix, as Baseline
    
   
    
    
    
    
    ## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    
   
   
 
    ## ====================== STEP 1.b DENSITY =========================
    
    res_cons_pat_b_pre = runif(NUMB_PRO*NUMB_RES)
    ## 1/0 DENSITY
    res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
                              NUMB_PRO,NUMB_RES)
    
    RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
    
    
    ## ====================== STEP 1.c Ceiling and Scaling ============= 
    
    # take absolute value of X and Z and scale by 10 and round them
    # Anand et al. 2019
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
    RES_CONS_PAT[,1] <- (BASE)
    RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
    
    
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    RES_CONS_PAT_TOTAL <- RES_CONS_PAT * MXQ
    
    #RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this needs to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
    
    
    ## ===================== EXCPETION HANDLER ====================
    
    PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource (exclude column one??)
    RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
    BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?
    
    if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
    {
      break
    }
    
  }
  
 

  #batch level consumption that is negatively correlated with volumes
  batches = 1/MXQ

  RES_CONS_PAT_batches = RES_CONS_PAT
  RES_CONS_PAT_batches[,bl] = RES_CONS_PAT[,bl]*batches
  RES_CONS_PATp_batches = sweep((RES_CONS_PAT_batches),2,colSums(RES_CONS_PAT_batches),"/")
  RES_CONS_PATp[,bl] = RES_CONS_PATp_batches[,bl]
  
  #product sustaining level that is slightly positively correlated with volumes
  product = MXQ*rnorm(50,mean=1,sd=0.25)
  RES_CONS_PAT_product = RES_CONS_PAT
  RES_CONS_PAT_product[,pl] = RES_CONS_PAT[,pl]*product
  RES_CONS_PATp_product = sweep((RES_CONS_PAT_product),2,colSums(RES_CONS_PAT_product),"/")
  RES_CONS_PATp[,pl] = RES_CONS_PATp_product[,pl]
  
  ##facility level consumption that is not correlated with volumes
  RES_CONS_PATp_single = sweep((RES_CONS_PAT),2,colSums(RES_CONS_PAT),"/")
  RES_CONS_PATp[,fl] = RES_CONS_PATp_single[,fl]
  
  
  #product portfolio --> matching demand with complexity 
  complexity = calc_complexity(RES_CONS_PATp)#entropy
  complexity_sorted = sort(complexity,index.return=TRUE)
  mxq_sorted = sort(MXQ,decreasing = TRUE,index.return=TRUE)
  
  RES_CONS_PAT_new = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE)
  i=1
  for(i in 1:length(complexity)){
    volume_index = mxq_sorted$ix[i]
    complexity_index = complexity_sorted$ix[i]
    RES_CONS_PAT_new[volume_index,] <- RES_CONS_PAT[complexity_index,]
    
  }
  RES_CONS_PATp = RES_CONS_PAT_new

  # rescons = melt(RES_CONS_PATp)
  # colnames(rescons) = c("NUMB_PRO","NUMB_RES","value")
  # 
  # ggplot(rescons, aes(x= NUMB_RES,y=NUMB_PRO,fill=value))+geom_tile()+theme_classic()+
  #   scale_fill_gradientn(colours = c("white","blue"),limits = c(0,0.05))


  # ul_bl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,bl]))
  # bl_pl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,pl]))
  # ul_pl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,pl]))
  # bl_fl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,fl]))
  # pl_fl = mean(cor(RES_CONS_PATp[,pl],RES_CONS_PATp[,fl]))
  # ul_fl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,fl]))
  
  
  
  ul_bl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,bl]))
  bl_pl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,pl]))
  ul_pl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,pl]))
  bl_fl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,fl]))
  pl_fl = cor(rowMeans(RES_CONS_PATp[,pl]),rowMeans(RES_CONS_PATp[,fl]))
  ul_fl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,fl]))
  mxq_ul = cor(MXQ,rowMeans(RES_CONS_PATp[,ul]))
  mxq_bl = cor(MXQ,rowMeans(RES_CONS_PATp[,bl]))
  mxq_pl = cor(MXQ,rowMeans(RES_CONS_PATp[,pl]))
  mxq_fl = cor(MXQ,rowMeans(RES_CONS_PATp[,fl]))


 
  column_shuffle = sample(c(1:50))
  
RES_CONS_PATp = RES_CONS_PATp[,column_shuffle]
RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL[,column_shuffle]
RES_CONS_PAT = RES_CONS_PAT[,column_shuffle]


cost_hierarchy$ul = ul
cost_hierarchy$bl = bl
cost_hierarchy$pl = pl
cost_hierarchy$fl = fl
cost_hierarchy$ul_bl = ul_bl
cost_hierarchy$bl_pl = bl_pl
cost_hierarchy$ul_pl = ul_pl
cost_hierarchy$bl_fl = bl_fl
cost_hierarchy$pl_fl = pl_fl
cost_hierarchy$ul_fl = ul_fl
  
  RES_CONS_PAT_list$DENS = DENS_draw
  RES_CONS_PAT_list$COR1 = 0
  RES_CONS_PAT_list$COR2 = 0
  RES_CONS_PAT_list$non_unit_size = (length(bl)+length(pl)+length(fl))/50
  RES_CONS_PAT_list$RES_CONS_PAT = RES_CONS_PAT
  RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
  RES_CONS_PAT_list$RES_CONS_PATp = RES_CONS_PATp
  RES_CONS_PAT_list$cost_hierarchy = cost_hierarchy
  #
  #
  return(RES_CONS_PAT_list)
  
  
}

##empirical cost hierarchy
.gen_RES_CONS_PAT_Anand_CH2 <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ,cost_hierarchy) {
  
  RES_CONS_PAT_list = list()
  
  
  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.2;
    DENS_MAX = 0.9;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  
  DENS_draw = DENS
  
  ul = cost_hierarchy$ul
  bl = cost_hierarchy$bl
  pl = cost_hierarchy$pl
  fl = cost_hierarchy$fl
  
  
  ## ====================== STEP 1 BASELINE NORM ========================= 
  
  repeat    {
    
    BASE = rnorm(NUMB_PRO,mean=1,sd=0.25) #creates for every CO (product) a random number
    
    RES_CONS_PAT = matrix(rnorm(NUMB_PRO*NUMB_RES,mean=1,sd=0.25), 
                          NUMB_PRO, NUMB_RES)                            #random pre matrix, as Baseline
    
    
    
    
    
    
    ## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    
    
    
    
    ## ====================== STEP 1.b DENSITY =========================
    
    res_cons_pat_b_pre = runif(NUMB_PRO*NUMB_RES)
    ## 1/0 DENSITY
    res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
                              NUMB_PRO,NUMB_RES)
    
    RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
    
    
    ## ====================== STEP 1.c Ceiling and Scaling ============= 
    
    # take absolute value of X and Z and scale by 10 and round them
    # Anand et al. 2019
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
    RES_CONS_PAT[,1] <- (BASE)
    RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    RES_CONS_PAT_TOTAL <- RES_CONS_PAT * MXQ
    
    #RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this needs to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
    
    
    ## ===================== EXCPETION HANDLER ====================
    
    PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource (exclude column one??)
    RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
    BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?
    
    if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
    {
      break
    }
    
  }
  
  
  #browser()
  
  #batch level consumption that is negatively correlated with volumes

  pre_batch = rnorm(50,mean=1,sd=0.25)
  batches = MXQ*pre_batch
  RES_CONS_PAT_batches = RES_CONS_PAT
  RES_CONS_PAT_batches[,bl] = RES_CONS_PAT[,bl]*batches
  RES_CONS_PATp_batches = sweep((RES_CONS_PAT_batches),2,colSums(RES_CONS_PAT_batches),"/")
  RES_CONS_PATp[,bl] = RES_CONS_PATp_batches[,bl]
  
  #product sustaining level that is slightly positively correlated with volumes
  pre_product = rnorm(50,mean=1,sd=0.25)
  product = MXQ*pre_product
  RES_CONS_PAT_product = RES_CONS_PAT
  RES_CONS_PAT_product[,pl] = RES_CONS_PAT[,pl]*product
  RES_CONS_PATp_product = sweep((RES_CONS_PAT_product),2,colSums(RES_CONS_PAT_product),"/")
  RES_CONS_PATp[,pl] = RES_CONS_PATp_product[,pl]
  
  ##facility level consumption that is not correlated with volumes
  RES_CONS_PATp_single = sweep((RES_CONS_PAT),2,colSums(RES_CONS_PAT),"/")
  dummy_cor = sample(c(0,1,2),1)
  
  if(dummy_cor ==1){weighting = pre_batch}else if(dummy_cor == 0){weighting = pre_product}else{weighting = c(rep(1,50))}
  RES_CONS_PATp[,fl] = RES_CONS_PATp_single[,fl]*weighting
  
  
  
  
  mxq_ul = cor(MXQ,rowMeans(RES_CONS_PATp[,ul]))
  mxq_bl = cor(MXQ,rowMeans(RES_CONS_PATp[,bl]))
  mxq_pl = cor(MXQ,rowMeans(RES_CONS_PATp[,pl]))
  mxq_fl = cor(MXQ,rowMeans(RES_CONS_PATp[,fl]))
  
  
  
  #rescons = melt(RES_CONS_PATp)
  
  #ggplot(rescons, aes(x= Var2,y=Var1,fill=value))+geom_tile()
  
  # 
  # ul_bl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,bl]))
  # bl_pl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,pl]))
  # ul_pl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,pl]))
  # bl_fl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,fl]))
  # pl_fl = mean(cor(RES_CONS_PATp[,pl],RES_CONS_PATp[,fl]))
  # ul_fl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,fl]))
  
  ul_bl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,bl]))
  bl_pl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,pl]))
  ul_pl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,pl]))
  bl_fl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,fl]))
  pl_fl = cor(rowMeans(RES_CONS_PATp[,pl]),rowMeans(RES_CONS_PATp[,fl]))
  ul_fl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,fl]))
  
  
  
  column_shuffle = sample(c(1:50))
  
  RES_CONS_PATp = RES_CONS_PATp[,column_shuffle]
  RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL[,column_shuffle]
  RES_CONS_PAT = RES_CONS_PAT[,column_shuffle]
  
  
  cost_hierarchy$ul = ul
  cost_hierarchy$bl = bl
  cost_hierarchy$pl = pl
  cost_hierarchy$fl = fl
  cost_hierarchy$ul_bl = ul_bl
  cost_hierarchy$bl_pl = bl_pl
  cost_hierarchy$ul_pl = ul_pl
  cost_hierarchy$bl_fl = bl_fl
  cost_hierarchy$pl_fl = pl_fl
  cost_hierarchy$ul_fl = ul_fl
  
  RES_CONS_PAT_list$DENS = DENS_draw
  RES_CONS_PAT_list$COR1 = 0
  RES_CONS_PAT_list$COR2 = 0
  RES_CONS_PAT_list$non_unit_size = (length(bl)+length(pl)+length(fl))/50
  RES_CONS_PAT_list$RES_CONS_PAT = RES_CONS_PAT
  RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
  RES_CONS_PAT_list$RES_CONS_PATp = RES_CONS_PATp
  RES_CONS_PAT_list$cost_hierarchy = cost_hierarchy
  #
  #
  return(RES_CONS_PAT_list)
  
  
}

##EAD cost hierarchy
.gen_RES_CONS_PAT_EAD <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ,cost_hierarchy){
  
  RES_CONS_PAT_list = list()
  
  ## ====================== STEP 0.b Determining the density (DENS)  =========================
  #Randomization and setting clear design points. 
  
  if(DENS == -1)
  {
    DENS_MIN = 0.4;
    DENS_MAX = 0.7;
    DENS = runif(1, DENS_MIN, DENS_MAX);
  }
  
  DENS_draw = DENS
  
  ## ====================== STEP 1 BASELINE NORM ========================= 
  
  repeat    {
    
    BASE = rnorm(NUMB_PRO) #creates for every CO (product) a random number
    
    RES_CONS_PATpre = matrix(rnorm(NUMB_PRO*NUMB_RES,mean=0,sd=1), 
                             NUMB_PRO, NUMB_RES)                            #random pre matrix, as Baseline
    
    RES_CONS_PAT = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE) #empy matrix, that is going to be filled 
    
    
    
    
    ## ====================== STEP 1.a CORRELATION ========================= 
    # Products and Resource are transposed in constrast to Anand 2019 but there is no issue in the model
    # Rows Products Colums Resources
    
    
    
    # Correlation of the top [DISP1] resources
    if(COR1 == -1){
      COR1 <- runif(1, -0.2, 0.8)
    }
    COR1_draw = COR1
    sqrt_const_1 <- sqrt(1 - (COR1 * COR1))
    
    # Correlation of the remaining resources
    if(COR2 == -1){
      COR2 <- runif(1, -0.8, 0.2)
    }
    COR2_draw = COR2
    sqrt_const_2 <- sqrt(1 - (COR2 * COR2))
    
    
    
    for (i in 1:(DISP1-1)) #unitsize+1
    {
      RES_CONS_PAT[,i] <- (COR1 * BASE)+ sqrt_const_1 * RES_CONS_PATpre[,(i)];
    }
    
    for (i in ((DISP1-1)) : NUMB_RES) #nonunitsize+1 (34+1)
    {
      RES_CONS_PAT[,i] <- (COR2 * BASE)+ sqrt_const_2 * RES_CONS_PATpre[,(i)];
    }
    
    ## ====================== STEP 1.b DENSITY =========================
    
    res_cons_pat_b_pre = runif(NUMB_PRO*NUMB_RES)
    ## 1/0 DENSITY
    res_cons_part_b <- matrix(ifelse(res_cons_pat_b_pre > DENS, 0,1),
                              NUMB_PRO,NUMB_RES)
    
    RES_CONS_PAT = res_cons_part_b * RES_CONS_PAT
    
    
    ## ====================== STEP 1.c Ceiling and Scaling ============= 
    
    # take absolute value of X and Z and scale by 10 and round them
    # Anand et al. 2019
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS
    RES_CONS_PAT[,1] <- (BASE)
    RES_CONS_PAT <- ceiling(abs(RES_CONS_PAT) * 10)
    
    #EAD structure
    
    #A_PFR, N_FR<=N_P<=2^N_FR-1 (linearly independent rows)
    NUMB_FR_min = ceiling(log(NUMB_PRO+1)/log(2))
    NUMB_FR = round(runif(1,NUMB_FR_min,NUMB_PRO))
    A_PFR <- diag(NUMB_PRO)
    for(i in c(1:(NUMB_PRO-1))){
      v <- runif(n=(ncol(A_PFR)-i),0,1)
      v[which(v>=DENS)] <- 1
      v[which(v<DENS)] <- 0
      A_PFR[i,c((i+1):ncol(A_PFR))] <- v
    }
    A_PFR <- A_PFR[,c((NUMB_PRO-NUMB_FR):NUMB_PRO)]

    #A_FRCM, N_CM=N_P
    A_FRCM <- diag(NUMB_PRO)
    for(i in c(1:(NUMB_PRO-1))){
      v <- runif(n=(ncol(A_FRCM)-i),0,1)
      v[which(v>=DENS)] <- 1
      v[which(v<DENS)] <- 0
      A_FRCM[i,c((i+1):ncol(A_FRCM))] <- v
    }
    A_FRCM <- A_FRCM[c(1:ncol(A_PFR)),]

    A_PCM <- A_PFR%*%A_FRCM #N_P x N_P dimension
    RES_CONS_PAT <- A_PCM%*%RES_CONS_PAT
    
    #product portfolio --> matching demand with complexity 
    complexity = rowSums(RES_CONS_PAT)
    complexity_sorted = sort(complexity,decreasing=TRUE,index.return=TRUE)
    mxq_sorted = sort(MXQ,index.return=TRUE)$ix
    
    RES_CONS_PAT_new = matrix(0, nrow = NUMB_PRO, ncol = NUMB_RES, byrow = TRUE)
    
    for(i in 1:length(complexity)){
      
      complexity_index = complexity_sorted$ix[i]
      RES_CONS_PAT_new[mxq_sorted[i],] <- RES_CONS_PAT[complexity_index,]
      
    }
    #RES_CONS_PAT = RES_CONS_PAT_new
    
    # 
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    #RES_CONS_PAT_TOTAL = RES_CONS_PAT*MXQ
    RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this need to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
    
    
    browser()
     rescons = melt(RES_CONS_PAT)
     colnames(rescons) = c("NUMB_PRO","NUMB_RES","value")
    # 
    # 
     ggplot(rescons, aes(x= NUMB_RES,y=NUMB_PRO,fill=value))+geom_tile()+theme_classic()+
     scale_fill_gradientn(colours = c("white","blue"))
    
    ## ===================== EXCEPTION HANDLER ====================
    
    
    PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource (exclude column one??)
    RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
    BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?
    
    if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
    {
      break
    }
    
  }
  
  ul = c(1:DISP1)
  bl = c((DISP1+1):NUMB_RES)
  pl = 0
  fl = 0
  
  
  
  # ul_bl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,bl]))
  # bl_pl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,pl]))
  # ul_pl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,pl]))
  # bl_fl = mean(cor(RES_CONS_PATp[,bl],RES_CONS_PATp[,fl]))
  # pl_fl = mean(cor(RES_CONS_PATp[,pl],RES_CONS_PATp[,fl]))
  # ul_fl = mean(cor(RES_CONS_PATp[,ul],RES_CONS_PATp[,fl]))
  # 
  
  ul_bl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,bl]))
  bl_pl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,pl]))
  ul_pl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,pl]))
  bl_fl = cor(rowMeans(RES_CONS_PATp[,bl]),rowMeans(RES_CONS_PATp[,fl]))
  pl_fl = cor(rowMeans(RES_CONS_PATp[,pl]),rowMeans(RES_CONS_PATp[,fl]))
  ul_fl = cor(rowMeans(RES_CONS_PATp[,ul]),rowMeans(RES_CONS_PATp[,fl]))
  mxq_ul = cor(MXQ,rowMeans(RES_CONS_PATp[,ul]))
  mxq_bl = cor(MXQ,rowMeans(RES_CONS_PATp[,bl]))
  mxq_pl = cor(MXQ,rowMeans(RES_CONS_PATp[,pl]))
  mxq_fl = cor(MXQ,rowMeans(RES_CONS_PATp[,fl]))
  
  
  cost_hierarchy$ul = ul
  cost_hierarchy$bl = bl
  cost_hierarchy$pl = pl
  cost_hierarchy$fl = fl
  
  cost_hierarchy$ul_bl = ul_bl
  cost_hierarchy$bl_pl = bl_pl
  cost_hierarchy$ul_pl = ul_pl
  cost_hierarchy$bl_fl = bl_fl
  cost_hierarchy$pl_fl = pl_fl
  cost_hierarchy$ul_fl = ul_fl
  
  RES_CONS_PAT_list$DENS = DENS_draw
  RES_CONS_PAT_list$COR1 = COR1_draw
  RES_CONS_PAT_list$COR2 = COR2_draw
  RES_CONS_PAT_list$non_unit_size = 0 #all costs are unit-level
  RES_CONS_PAT_list$RES_CONS_PAT = RES_CONS_PAT
  RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
  RES_CONS_PAT_list$RES_CONS_PATp = RES_CONS_PATp
  RES_CONS_PAT_list$cost_hierarchy = cost_hierarchy
  #
  #
  return(RES_CONS_PAT_list)
}
 



.gen_RCC_Anand <- function(DISP1, DISP2, NUMB_RES) {
  
  RCC_list = list()
    
  if (DISP2 == -1)
  {
    DISP2_MIN = 0.4              
    DISP2_MAX = 0.7
    DISP2 = runif(1, DISP2_MIN, DISP2_MAX)
  }
   
  
  
  
  TC = 1000000

  # Step 1
  r_MIN <- ((1 - DISP2) * TC) / (NUMB_RES - DISP1)
  
  #Step 2
  r1_MAX <- (DISP2 * TC) - ((DISP1 - 1) * r_MIN)
  
  # Step 3
  r_MIN <- r_MIN + (r1_MAX - r_MIN) * 0.025   #0.025?
  
  ## Step 4
  #Initalize Values
  RCC <- vector(mode = "numeric")
  r_MAX <- vector(mode = "numeric")
  temp1_ADD <- vector(mode = "numeric", length = DISP1 - 1)
  temp1_ADD[1] <- 0
  
  
  for (i in 1:(DISP1 - 1)) {
    r_MAX[i] <- (DISP2 * TC - sum(temp1_ADD)) - (DISP1 - i) * r_MIN
    
    RCC[i] <- runif(1, min = r_MIN, max = r_MAX[i])
    temp1_ADD[i] <- RCC[i]
    
    
  }
  
  ## The final element is computed to ensure that the total rescource cost is exactly DISP2*TC
  RCC <- c(RCC, DISP2 * TC - sum(temp1_ADD))
  
  ## Move the biggest resource to the front
  largest_RC <-
    sort(RCC, decreasing = TRUE, index.return = TRUE)$ix[1]
  RCC <- c(RCC[largest_RC], RCC[-largest_RC])
  
  
  #### Generate Small Rescources ####
  
  RC_small <-
    runif(length((length(RCC) + 1):NUMB_RES), min = 0.05, max =
            0.95)
  RC_small <- RC_small / sum(RC_small) #normalize
  RC_small <- RC_small * (1 - DISP2) * TC
  
  
  ## Some Checks ##
  # Sum of first DISP1 resources not correct.
  # if(min(RC)> ((1-DISP2)*TC)/(NUMB_RES-DISP1)){
  
  while (max(RC_small) - min(RCC) > 1.0) {
    RC_small <- sort(RC_small, decreasing = TRUE)
    min_bigRes <- min(RCC)
    for (i in 1:(length(RC_small))) {
      overage <- max(c(RC_small[i] - min_bigRes , 0))
      RC_small[i] <- RC_small[i] - overage
      RC_small[length(RC_small) - i + 1] <-
        RC_small[length(RC_small) - i + 1] + overage
    }
  }
  
  
  # Step 6 Schuffle small rescources
  RC_small <- RC_small[sample(length(RC_small))]
  RCC <- c(RCC, RC_small)
  
  # sum(RC)
  RCCs <- sort(RCC, decreasing = TRUE, index.return = TRUE)
  
  RCC <-
    list(
      RCC = RCC,
      CHECK = list(
        cost_largestRCP = RCCs$x[1] / RCCs$x[NUMB_RES],
        cost_topTEN = sum(RCCs$x[1:10]) / TC,
        DISP1 = DISP1,
        DISP2 = DISP2,
        RC_VAR = DISP2
      )
    )
  
  RCC = RCC$RCC
  
  #RCC = RCC[sample(c(1:NUMB_RES),NUMB_RES)]
  
  RCC_list$RCC = RCC
  
  RCC_list$DISP2_draw = DISP2

  
  return(RCC_list)
  
}

####################################GENERATION OF PRODUCT PORTFOLIO#############################################


.gen_product_portfolio<- function(RES_CONS_PAT_list,NUMB_PRO,MXQ){

}



###Additional functions
calc_entropy <-function(matrix){
  
  
  if(ncol(matrix)>1){
    #ElMaraghy(2012)
    connections = rowSums(matrix)
    
    complexity = lapply(connections,function(x){-(1/x)*log2(1/x)})
    
    complexity = unlist(complexity)
    
  }else{complexity = 0}

  
  return(complexity)
  
}




calc_intra <-function(matrix){
  
  
  if(ncol(matrix)>1){
    avg_cons =  rowMeans(matrix)
    
    intra = c()
    for(i in 1:nrow(matrix)){
      
      intra[i]= sum(((matrix[i,]-avg_cons)/avg_cons)^2)
      
    } 
    
    
  }else{intra =0}

  
  return(intra)
}



calc_inter<-function(matrix){
  
  
  if(ncol(matrix)>1){
    avg_cons =  colMeans(matrix)
    
    inter = c()
    for(i in 1:nrow(matrix)){
      
      inter[i]= sum(((matrix[i,]-avg_cons)/avg_cons)^2)
      
    }
    
  }else{inter =0}
 
  
  return(inter)
}



calc_directed_inter <-function(matrix){
  
  
  inter = calc_inter(matrix)
  
  nonzero_cons = calc_nonzero_cons(matrix)
  
  mean_nzc = mean(nonzero_cons)
  
  nonzero_cons[which(nonzero_cons<mean_nzc)]<- -1
  nonzero_cons[which(nonzero_cons>=mean_nzc)]<- 1
 
  directed_inter = nonzero_cons*inter
   
  return (directed_inter)
}

calc_individuality <- function(matrix){
  
  #relative matrix
  #matrix <- sweep((matrix),2,colSums(matrix),"/") 
  
  #find core-resources
  if(ncol(matrix)>1){
    core_res = c()
    for(i in 1:ncol(matrix)){
      core_res[i]=if(sum(matrix[,i]>0)==nrow(matrix)){1}else{0}
    }
  }else{core_res =1}
  #browser()
  matrix <- sweep((matrix),1,rowSums(matrix),"/")
  matrix_core <- matrix[,which(core_res==1)]
 
   if(is.null(ncol(matrix_core))){
     individuality=matrix_core
  }else{individuality <- rowSums(matrix_core)}
 
  return(individuality)
}

calc_cost_core_rel <- function(matrix, RCC){

  #relative matrix
  matrix <- sweep((matrix),2,colSums(matrix),"/") 
  
  #find core-resources
  if(ncol(matrix)>1){
    core_res = c()
    for(i in 1:ncol(matrix)){
      core_res[i]=if(sum(matrix[,i]>0)==nrow(matrix)){1}else{0}
      }
  }else{core_res =1}
  
  
  matrix_core <- matrix[,which(core_res==1)]
  RCC_core <- RCC[which(core_res==1)]
  
  cost_total = matrix %*% RCC
  cost_core_res = matrix_core %*% RCC_core
  
  cost_core_rel = cost_core_res/cost_total

  return(cost_core_rel)
}

calc_nonzero_cons <- function(matrix){
  
  
  if(ncol(matrix)>1){
    nonzero_cons = c()
    for(i in 1:nrow(matrix)){
      nonzero_cons[i]=sum(matrix[i,]>0)
      
      
    }
    
  }else{nonzero_cons =1}

  nonzero_cons= nonzero_cons/ncol(matrix)
  return(nonzero_cons)  
  
}

calc_mean_cons <- function(matrix){
  
  
  if(ncol(matrix)>1){
    mean_cons = rowMeans(replace(matrix, matrix == 0, NA), na.rm = TRUE)
    
  }else{mean_cons =1}
  
 
  return(mean_cons)  
  
}

calc_zero_cons <- function(matrix){
  
  
  if(ncol(matrix)>1){
    nonzero_cons = c()
    for(i in 1:nrow(matrix)){
      nonzero_cons[i]=sum(matrix[i,]==0)
      
      
    }
    
  }else{nonzero_cons =0}
  
  #nonzero_cons= nonzero_cons/ncol(matrix)
  return(nonzero_cons)  
  
}





calc_complexity<-function(matrix){
  
  if(ncol(matrix)>1){
    nonzero_cons = calc_intra(matrix)
    intra = calc_intra(matrix)
    x = nonzero_cons/sum(nonzero_cons)
    y = intra/sum(intra)
    
    complexity = x+y 
    
    
  }else{complexity = 0}

  return(complexity)
}

calc_sd_cons <- function(matrix){
  
  if(ncol(matrix)>1){
    sd_cons = c()
    for(i in 1:nrow(matrix)){
      sd_cons[i]=sd(matrix[i,])
      
      
    } 
    
  }else{sd_cons =0}


  return(sd_cons)  
  
}


calc_cons_bigDriver <- function(matrix){
  
  if(ncol(matrix)>1){
    cons_bigDriver = c()
    for(i in 1:nrow(matrix)){
      cons_bigDriver[i]= mean(matrix[i,which(CostingSystem_list$ACP>200000)])
      
      
    } 
    
  }else if(length(which(CostingSystem_list$ACP>200000))==0){cons_bigDriver==0}else{cons_bigDriver = matrix}
  
  
  return(cons_bigDriver)  
  
}


calc_cons_smallDriver <- function(matrix){
  
  if(ncol(matrix)>1){
    cons_smallDriver = c()
    for(i in 1:nrow(matrix)){
      cons_smallDriver[i]= mean(matrix[i,which(CostingSystem_list$ACP<200000)])
      
      
    } 
    
  }else if(length(which(CostingSystem_list$ACP<200000))==0){cons_smallDriver=0}else{cons_smallDriver = 0}
  
  
  return(cons_smallDriver)  
  
}


calc_ranking <-function(vector){
  
  ranking_vector = c()
  
  for(i in 1:length(vector)){
    if(vector[i]>(1.5*median(vector))){
      ranking_vector[i] = 1
    }else if(vector[i]<(0.5*median(vector))){
      ranking_vector[i] = 0
    }else{ranking_vector[i] = -1}
    
  }
  return(ranking_vector)
}


calc_index2 <- function(matrix){
  if(ncol(matrix)>1){
    entries = c(1:nrow(matrix))
    index_2 = sapply(entries, function(x){sum(rowSums(as.matrix(matrix[,which(matrix[x,]>0)]))>0)})
  }else{
    
    index_2 = 1
  }

  return(index_2)
} #interference with other function




calc_cons_var <- function(matrix){
  
  cons_var = c()
  if(ncol(matrix)>1){
    for(i in 1:nrow(matrix)){
      cons_var[i] = max(abs(matrix[i,]))/sum(abs(abs(matrix[i,])-mean(abs(matrix[i,]))))
      
    }
  }else{cons_var = matrix}

  
  return(cons_var)

  
}

