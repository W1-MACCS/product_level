.gen_Demand_Anand <- function(Q_VAR,NUMB_PRO){
  
  LB = 10
  
  if(Q_VAR == "LOW"){UB = 20}else if(Q_VAR == "MID"){UB= 40}else{UB = 60}
 
  DEMAND = as.integer(runif(NUMB_PRO,LB,UB))
  
  
  return(DEMAND)
}




.gen_RES_CONS_PAT_Anand <- function(NUMB_PRO,NUMB_RES, DENS, DISP1,COR1,COR2,MXQ) {
  
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
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAND
    RES_CONS_PAT_TOTAL <- sweep(RES_CONS_PAT,MARGIN = 1,MXQ,'*')     #does this needs to be a matrix multiplication?
    ##CALCULATING TCU
    TCU <- colSums(RES_CONS_PAT_TOTAL)
    ##INDIVIDUAL REQUIREMENTS OF THE PRODUCTS * DEMAMD / TRU (Currently like this in Anand et al. 2019)
    RES_CONS_PATp <- sweep((RES_CONS_PAT_TOTAL),2,TCU,"/") #Absolute matrix to relative matrix
    

    ## ===================== EXCPETION HANDLER ====================
    
    # EXPECTION HANDLER  & CHECKS AFTER ANAND ET AL. 2019 # It is important the the first RES_CONS_PAT column has no zeros
    # in accordance with Anand etl. 2019 and Balakrishnan et al. 2011; Substantiation of this hidden formalization remains unclear. 
    
    PRO_ZEROS<-any(rowSums(RES_CONS_PAT[,])==0)   #every product need at least one resource (exclude column one??)
    RES_ZEROS<-any(colSums(RES_CONS_PAT[,])==0)   #every resource needs to be used at least once
    BASE_ZEROS <-any(RES_CONS_PAT[,1]==0)         #first resource needs to be in every product ->why?
    
    if(PRO_ZEROS==FALSE & RES_ZEROS==FALSE & BASE_ZEROS==FALSE) #discard the matrix if one of these conditions is not met
    {
      break
    }
    
  }
  
  
RES_CONS_PAT_list$DENS = DENS_draw
RES_CONS_PAT_list$COR1 = COR1_draw
RES_CONS_PAT_list$COR2 = COR2_draw
RES_CONS_PAT_list$RES_CONS_PAT = RES_CONS_PAT
RES_CONS_PAT_list$RES_CONS_PAT_TOTAL = RES_CONS_PAT_TOTAL
RES_CONS_PAT_list$RES_CONS_PATp = RES_CONS_PATp
  #
  #
  return(RES_CONS_PAT_list)
  
  
}






.gen_RCC_Anand <- function(DISP1, DISP2, NUMB_RES) {
  
  RCC_list = list()
    
  if (DISP2 == -1)
  {
    DISP2_MIN = 0.2              #if bounds between 0.5 and 0.8 this could be an equivilent to per_BATCH costs in gen_RCC_unit
    DISP2_MAX = 0.9
    DISP2 = runif(1, DISP2_MIN, DISP2_MAX)
  }
   
  
  #FIRM$PRODUCTION_ENVIRONMENT$CHECK$DISP2 = RC_VAR
  
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
  
  RCC_list$RCC = RCC
  
  RCC_list$DISP2_draw = DISP2

  
  return(RCC_list)
  
}