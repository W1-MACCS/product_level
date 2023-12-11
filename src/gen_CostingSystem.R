## ANAND et al. 2019;
## ====================================== COST POOL ALLOCATION ==================================================

MAP_RES_CP_SIZE_MISC<-function(CP,RCC,RES_CONS_PATp){
  
  #### SIZE-BASED RANDOM ALLOCATION OF RESOURCES TO COST POOLS ####    
  
  RCCn = length(RCC)
  NUMB_RES = length(RCC)
  
  
  ####---- pre allocation of largest resorces ----####
  ACP_pre1<-vector(mode="numeric",length = (CP-1))
  RC_to_ACP = list()
  
  
  if (CP > 1){
    #### Find the largest RCC (resource costs) for one cost pool each####
    RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sort resource costs 
    
    
    for (i in 1:(CP-1)){               # assign the biggest RCC each to one cost pool
      
      ACP_pre1[i]<-RCCs$x[i]
      RC_to_ACP[[i]]<-RCCs$ix[i]
      
    }
    
    
    
    ####---- Assign remaining RC to Misc Pool ----####
    
    not_assigned = vector(mode = 'numeric')
    not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
    
    
    ACP_misc = sum(RCC[not_assigned])
    RC_to_ACP_misc = list(not_assigned)
    
    
    ACP = vector(mode='numeric')
    ACP<-append(ACP_pre1, ACP_misc)
    #RC_to_ACP = vector(mode="numeric")
    
    # Bringing the pre index vectors RC_to_ACP_pre together
    
    RC_to_ACP = append(RC_to_ACP, RC_to_ACP_misc)
    
  }else if (CP ==1){
    ACP = sum(RCC)
    RC_to_ACP = list(c(1:NUMB_RES))
  }
  
  
  CostingSystem_list = list()
  
  CostingSystem_list$ACP = ACP
  CostingSystem_list$RC_to_ACP = RC_to_ACP


  return(CostingSystem_list)
  
} # ANAND p==0

MAP_RES_CP_SIZE_CORREL_MISC_ANAND<-function(CP,RCC,RES_CONS_PATp){
  
    #RCC
  NUMB_RES = length(RCC)

  MISCPOOLSIZE = 250000
  CC = 0.4
  RCCn= length(RCC)

  ####SIZE RULE####
  ####pre allocation, one pool left open
  if (CP > 1){
    RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sorted Resource cost vector
    RC_to_ACP<-list()
    ACP_pre1<-vector(mode ='numeric', length = (CP-1))    #rep(0,(CP-1))
    for (i in 1:(CP-1)){ # assign the biggest Resource -1  each to one activity pool #last is msic
      
      ACP_pre1[i]<-RCCs$x[i] 
      RC_to_ACP[[i]]<-RCCs$ix[i]
      
    }
    already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
    not_assigned <- setdiff(c(1:RCCn),already_assigned)
    #correlative assignment only if there are more than one resource in not_assigned
    ####CORRELATION RULE####
    if (NUMB_RES > CP){
      
      
      #### BUILDIUNG OF CORRELATION MATRIX ####
      
      ##Create empty matrix that shows correlation between assigned and unassigned resources

      ##fill empty matrix with correlations
      
      RC_Correl = cor(RES_CONS_PATp,RES_CONS_PATp)
      
      
      if(CP==2){
        RC_Correl = t(RC_Correl[already_assigned,])
      }else{
        RC_Correl = RC_Correl[already_assigned,]
      }
      colnames(RC_Correl) = paste(c(1:ncol(RC_Correl)))#changing the column names to the resource number
      RC_Correl = matrix(RC_Correl[,-already_assigned], ncol = length(not_assigned)) #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
      colnames(RC_Correl) = paste(not_assigned) #change resources names back
      
      
      
      #### CREATING A LIST THAT SHOWS THE ALLOCATION OF RESOURCES TO COST POOLS---------------------
      
      #Assign resources to ACPs based on the correlation as long as there are more resources unassigned than the Miscpoolsize
      #Sorting the RC_Correl Matrix by high correlations
      
      RC_to_ACP_cor <- which(RC_Correl>=sort(RC_Correl, decreasing = T)[ncol(RC_Correl)*nrow(RC_Correl)], arr.ind = T)#list of length of size of RC_Correl
      RC_to_ACP_cor = data.frame(RC_to_ACP_cor) #transform it into a dataframe
      RC_Correl_V = as.vector(RC_Correl)  #transform correlations into vector
      RC_to_ACP_cor$cor = RC_Correl_V     #append vector to correl dataframe, so each correlation for every CP/Res combination gets a row
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$cor, decreasing = TRUE),]   #sort the df by dreasing correlations
      RC_to_ACP_cor = RC_to_ACP_cor[!duplicated(RC_to_ACP_cor$col, fromLast = FALSE),] #drop all duplicate resources, so you have the highest correlations left
      
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$col, decreasing = FALSE),]     # sort it by decreasing resource so resource numbers can get changed into the not assigned ones
      
      for (i in RC_to_ACP_cor$col){
        
        RC_to_ACP_cor$col[i] =  colnames(RC_Correl)[i]           #change the resource names, according to the ones that are not yet assigned
      }
      
      RC_to_ACP_cor = RC_to_ACP_cor[order(RC_to_ACP_cor$cor, decreasing = TRUE),] #sort it again by decreasing correlation so the biggest correlations are assigned first
      
      
      
      
      #### ALLOCATING RESOURCES TO COST POOLS AND TAKING INTO ACCOUNT THE MISCPOOLSIZE------------------
      ##It is possible and allowed that more than one resource is assigned to one cost pool
      ACP_pre2<-vector(mode='numeric', length = CP-1)
      i=1
      # while until the misc pool has a cost share of MISCPOOLSIZE
      while (sum(RCC)-sum(RCC[already_assigned])-sum(RCC[as.numeric(RC_to_ACP_cor$col[c(1:i)])])> (MISCPOOLSIZE-RCC[as.integer(RC_to_ACP_cor$col[i])])) {
        
        RC_to_ACP[[RC_to_ACP_cor$row[i]]] = c(RC_to_ACP[[RC_to_ACP_cor$row[i]]],as.integer(RC_to_ACP_cor$col[i]))
        ACP_pre2[[RC_to_ACP_cor$row[i]]] = sum(ACP_pre2[[RC_to_ACP_cor$row[i]]],RCC[as.integer(RC_to_ACP_cor$col[i])])
        not_assigned = as.integer(RC_to_ACP_cor$col[i+1:(length(RC_to_ACP_cor$col)-i)])
        i = i+1
      }
      
      #&& RC_to_ACP_cor$cor[i]> CC
      
      ###MISCPOOL RULE####
      
      #Appending 
      RC_to_ACP_misc =list(not_assigned)
      RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
      
      
      #Adding the misc pool value to ACP
      ACP_misc = sum(RCC[not_assigned])
      ACP = append((ACP_pre1 + ACP_pre2),ACP_misc)
      
    } else{
      
      RC_to_ACP_misc =list(not_assigned)
      RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
      
      
      #Adding the misc pool value to ACP
      ACP_misc = sum(RCC[not_assigned])
      ACP = append(ACP_pre1,ACP_misc)
      
    }
    
  }else if (CP == 1){
    
    ACP = sum(RCC)
    RC_to_ACP = list(c(1:NUMB_RES))
  }
  
  
  CostingSystem_list = list()
  
  CostingSystem_list$ACP = ACP
  CostingSystem_list$RC_to_ACP = RC_to_ACP
  
  return(CostingSystem_list)
} # ANAND p==1 / with misc pool and both conditions (MISCpool AND CC)

MAP_RES_CP_SIZE_RANDOM_MISC<-function(CP, RCC, RES_CONS_PATp){
  
  
  NUMB_RES = length(RCC)
  RCCn = length(RCC)                           #number of resources that need to be allocated to cost pools
  MISCPOOLSIZE = 250000
  
  
  
  if (CP > 1){
    ####---- pre allocation of largest resorces ----####
    ACP_pre1<-vector(mode="numeric")                      #empty vector for ACP-biggest resource assignment
    RCCs_random_index <- vector(mode = "list", CP-1)        #
    RC_to_ACP = list()
    
    
    #### Find the largest RCC (resource costs) for one cost pool each####
    RCCs<-sort(RCC,decreasing = TRUE,index.return=TRUE)   # sort resource costs
    
    for (i in 1:(CP-1)){                    #assign the biggest RCC each to one cost pool
      
      ACP_pre1[i]<-RCCs$x[i]           #Volume for each of the biggest Resources
      RC_to_ACP[[i]]<-RCCs$ix[i]  #Resource itself (index)
      
    }
    
    
    already_assigned<-unlist(RC_to_ACP)          #transforms the list into a vector with all resources that are already assigned
    not_assigned <- setdiff(c(1:RCCn),already_assigned)
    
    
    
    if(NUMB_RES > CP){
      
      ####Random Assignment####
      
      CP_ohne_misc = CP-1
      x = sum(RCC[unlist(RC_to_ACP)])
      y = length(not_assigned)
      
      while(x < 1000000-MISCPOOLSIZE & y > 1){
        not_assigned = setdiff(c(1:NUMB_RES),unlist(RC_to_ACP))
        random_cp = sample(length(c(1:CP_ohne_misc)),1)
        RC_to_ACP[[random_cp]] = c(RC_to_ACP[[random_cp]],sample(not_assigned,1))
        not_assigned = setdiff(c(1:NUMB_RES),unlist(RC_to_ACP))
        x = sum(RCC[unlist(RC_to_ACP)])
        y = length(not_assigned)
      }
      
      
      
      
      
      ##Miscpool building
      
      
      
      RC_to_ACP_misc =list(not_assigned)
      RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
      
      
      # #Adding the misc pool value to ACP
      # ACP_misc = sum(RCC[not_assigned])
      # ACP = append((ACP_pre1 + ACP_pre2),ACP_misc)
      ACP = vector(mode = 'numeric')
      
      for (i in 1:CP){
        ACP[i] = sum(RCC[unlist(RC_to_ACP[i])])
      }
      
    } else{
      
      RC_to_ACP_misc =list(not_assigned)
      RC_to_ACP = append(RC_to_ACP,RC_to_ACP_misc)
      
      
      #Adding the misc pool value to ACP
      # ACP_misc = sum(RCC[not_assigned])
      # ACP = append(ACP_pre1,ACP_misc)                                      #if there was no second assignment of remaining resources (No. of RC = No. of ACP) all RC are in ACP_pre1
      
      ACP = vector(mode = 'numeric')
      
      for (i in 1:CP){
        ACP[i] = sum(RCC[unlist(RC_to_ACP[i])])
      }
    }
    
    
    
  }else if(CP ==1){
    
    ACP = sum(RCC)
    RC_to_ACP = list(c(1:NUMB_RES))
    
    
  }
  

  CostingSystem_list = list()
  
  CostingSystem_list$ACP = ACP
  CostingSystem_list$RC_to_ACP = RC_to_ACP
  
  return(CostingSystem_list)
  
  
} #ANAND p==2

MAP_RES_CP_SIZE_CORREL_CUTOFF_MISC_ANAND<-function(CP, RCC, RES_CONS_PATp){
  
  ##INIT##
  
  NUMB_RES = length(RCC)

  MISCPOOLSIZE = 250000
  CC = 0.4
  
  if (CP > 1){
    
    RCCn =length(RCC)
    already_assigned<- vector(mode = 'numeric', length = RCCn)          #transforms the list into a vector with all resources that are already assigned
    not_assigned <- setdiff(c(1:RCCn),already_assigned)
    
    if(NUMB_RES>CP){
      
      
      RC_to_ACP<-list()
      
      
      
      for (i in 1:(CP-1)){               # assign the biggest Resource to an activity pool
        
        #if(length(not_assigned)> length(i:(CP-1)) && sum(RCC[not_assigned])>MISCPOOLSIZE){
        
        #ACP[i]<-max(RCC[not_assigned])
        RC_to_ACP[[i]]<- not_assigned[which.max(RCC[not_assigned])]  #may not work if some of the resources have exactly the same values
        
        #already_assigned[i] = RC_to_ACP[i]
        not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
        
        RC_Correl = cor(RES_CONS_PATp[,RC_to_ACP[[i]]],RES_CONS_PATp)
        colnames(RC_Correl) = paste(c(1:ncol(RES_CONS_PATp)))#changing the column names to the resource number
        RC_Correl = matrix(RC_Correl[,not_assigned],ncol = length(not_assigned))
        #delete resources that are already assigned from Correlation Matrix, so they dont get assigned twice
        colnames(RC_Correl) = paste(not_assigned)
        RC_Correl = data.frame(sort(RC_Correl,decreasing = TRUE, index.return = TRUE))
        
        
        x =1
        
        while(RC_Correl$x[x] >= CC && NUMB_RES - length(unlist(RC_to_ACP))> (CP-i) && sum(RCC[unlist(RC_to_ACP)])< (1000000- MISCPOOLSIZE)){
          
          
          RC_to_ACP[[i]] = c(RC_to_ACP[[i]],not_assigned[RC_Correl$ix[x]])
         
          x=x+1
          
        }
        
        #print(RC_to_ACP[i])
        not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))
        
        #}
        
        if(length(not_assigned) == length(i:(CP))){
          
          RC_to_ACP[[i]] =c(RC_to_ACP[[i]],not_assigned[i])
          
        }
        
        not_assigned = setdiff(c(1:RCCn),unlist(RC_to_ACP))   
        
        
      }
      
      RC_to_ACP_misc = list(not_assigned)
      RC_to_ACP = append(RC_to_ACP, RC_to_ACP_misc)
      
      ACP_misc = sum(RCC[not_assigned])
      
      ACP<-vector(mode ='numeric', length = (CP-1))
      
      for (i in 1:(CP-1)){
        
        ACP[i] = sum(RCC[unlist(RC_to_ACP[i])])
        
      }
      
      ACP = append(ACP,ACP_misc)
      
    }
    
    
    if(NUMB_RES == CP){
      
      
      
      RC_to_ACP = list()
      ACP = vector(mode='numeric', length = CP)
      
      for (i in 1:length(not_assigned)){
        
        RC_to_ACP[i] = not_assigned[i]
        ACP[i] = RCC[not_assigned[i]]
      }
      
      
    }
    
  }else if (CP == 1){
    
    ACP = sum(RCC)
    RC_to_ACP = list(c(1:NUMB_RES))
    
    ###SOURCING####  
    
  }  
  CostingSystem_list = list()
  
  CostingSystem_list$ACP = ACP
  CostingSystem_list$RC_to_ACP = RC_to_ACP
  
  

  return(CostingSystem_list)
  
}# ANAND P==3 like anands currel cutoff ( not one by one) w/ miscpool


## ====================================== COST DRIVER SELECTION ==================================================

MAP_CP_P_BIGPOOL <-function(RC_to_ACP,RES_CONS_PATp,RCC, NUMB_PRO){
 
  ACP_index_choosen<-vector(mode="numeric")
  # normalize RES_CONS_PAT
 
  RC_ACP_index<-RC_to_ACP
  
  # preallocation
  ACT_CONS_PAT<-matrix(0,nrow = NUMB_PRO,ncol = length(RC_ACP_index))
  
  
  for (i in 1:length(RC_ACP_index)){
    
    
    ## exception handler if there is only one resource in ACP[[i]] take this resource as the driver
    # in original version not needed, this is due to basic implementation of Rs function rowSums
    if(length(RC_ACP_index[[i]])==1){
      ACT_CONS_PAT[,i]<-RES_CONS_PATp[,RC_ACP_index[[i]]]
    }else{
      
      # 1. Order ACP_index in decreasing order of resource size
      RC_order<-sort(RCC, decreasing=TRUE)
      
      RC_ACP_index[[i]]<-RC_ACP_index[[i]][order(match(RC_ACP_index[[i]],RC_order))]
      RES_CONS_PAT_temp<-RES_CONS_PATp[,RC_ACP_index[[i]]] # subsetting for resources used in this ACP and ordering
      # ACs<-sort(colSums(RES_CONS_PAT_temp),decreasing = TRUE,index.return=TRUE)
      ACT_CONS_PAT[,i]<-RES_CONS_PAT_temp[,1] #use the largest Rescource as a driver
    }
  }
  

ACT_CONS_PAT = as.matrix(ACT_CONS_PAT)    
    
    
  return(ACT_CONS_PAT)
}


MAP_CP_P_VOLUME <-function(RC_to_ACP,MXQ, NUMB_PRO){
  

  RC_ACP_index<-RC_to_ACP
  
  # preallocation
  ACT_CONS_PAT<-matrix(0,nrow = NUMB_PRO,ncol = length(RC_ACP_index))
  
  normalized_MXQ = MXQ/sum(MXQ)
  
  for (i in 1:length(RC_ACP_index)){
    
    
    
    ACT_CONS_PAT[,i] = normalized_MXQ
    
  }
  
  
  ACT_CONS_PAT = as.matrix(ACT_CONS_PAT)    

  return(ACT_CONS_PAT)
}






apply_ME <-function(ACT_CONS_PAT,ME,CP,NUMB_PRO){

  err_mat = matrix(runif(CP*NUMB_PRO, min = 1-ME, max = 1+ME), nrow = NUMB_PRO, ncol = CP)
  
  ACT_CONS_PAT = ACT_CONS_PAT * err_mat
  
  ACT_CONS_PAT <- sweep((ACT_CONS_PAT),2,colSums(ACT_CONS_PAT),"/") #Absolute matrix to relative matrix
  
  return(ACT_CONS_PAT)
  
}


