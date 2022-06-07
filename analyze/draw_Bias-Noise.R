#install.packages("RColorBrewer")
#install.packages("ggplot2")
library(RColorBrewer)
library(ggplot2)
library(dplyr)


input <- read.csv("C:/Users/kaigm/OneDrive/00 Paperprojects/01 PRODUCT COST _ WORKING PAPER/16 SUBMISSION/01 EXPERIMENTS/01 DATA/ProductCost_2020-04-28-1801.csv")
input$PE <- input$PE * 100

###### COMPUTING BIAS AND IMPRECISION #####
pre = group_by(input, input$PCb, input$DENS, input$Q_VAR, input$DISP1, input$CP, input$NUMB_Error, input$Error)

input_grouped =  (summarize(pre,n=n(), md=median(PE), mn=mean(PE),sd=sd(PE)*2)) ####
rm(pre)


LookUP = data.frame(input_grouped$`input$PCb`, input_grouped$`input$CP`, input_grouped$`input$NUMB_Error`, input_grouped$`input$Error`, input_grouped$mn, input_grouped$sd) # Building data frame
colnames(LookUP) <- c("PCB", "cp", "NUMB_ERROR", "Error" ,"BIAS","IMPRECISION") # rename columns. 
Base = data.frame(cbind(input$PRODUCT, input$PCb, input$PCh, input$NUMB_Error, input$Error,
                        input$DISP1, input$Q_VAR, input$DENS, input$CP,
                        input$PE))
colnames(Base) <- c("P_NUMB","PC_B", "PC_H", "NUMB_ERROR", "Error",
                    "DISP1", "Q_VAR","DENS","CP", 
                    "PE") # rename columns
#Left JOIN : keep ll rows in X even if there is no match  // all.x true = join y into x ; 
input = merge(Base,LookUP , by.x = c("PC_B","CP","NUMB_ERROR","Error"), by.y =c("PCB","cp","NUMB_ERROR","Error"), all.x = TRUE)


rm(LookUP)
rm(Base)
rm(input_grouped)
#head(input)
#summary(input)

####
input$APE <- abs(input$PE)
input$EUCD <- input$PC_H - input$PC_B
input$ABSBIAS <- abs(input$BIAS)


input$Noise_F <- cut(input$IMPRECISION, quantile(input$IMPRECISION,(0:4)/4));
input$Bias_F <- cut(input$ABSBIAS, quantile(input$ABSBIAS,(0:4)/4));


#### PLOTTING PORTFOLIO  ####

zplot1 <-   ggplot(input, aes(x=input$IMPRECISION, y=input$ABSBIAS)) +
  geom_point(size=2, shape=23)
  
  
zplot1


