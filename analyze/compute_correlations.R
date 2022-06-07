### compute correlation table
#install.packages("apaTables")
library(apaTables)
library(dplyr)
library(reshape)



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





##### DELETE UNNECESSARY PARAMETERS #####
input$P_NUMB <- NULL
#input$CP<- NULL
#input$ERROR<- NULL
#input$Q_VAR<- NULL
#input$DENS<- NULL
#input$RCC_VAR<- NULL
input$OVERCOSTED = input$PE > 5
input$OVERCOSTED = as.numeric(input$OVERCOSTED)
input$UNDERCOSTED = input$PE < -5
input$UNDERCOSTED = as.numeric(input$UNDERCOSTED)
#input$WITHINRANGE = input$PE > -5 & input$PE <= 5
#input$WITHINRANGE = as.numeric(input$WITHINRANGE)
input$APE <- abs(input$PE)
input$EUCD <- input$PC_H - input$PC_B
input$ABSBIAS <- abs(input$BIAS)
input$EUCD_Noise <- (input$IMPRECISION/100) * input$PC_H
#Order for visualization
input = data.frame(cbind(input$CP, input$Error, input$NUMB_ERROR, input$DENS, input$Q_VAR, input$DISP1,
                         input$PC_B, input$PC_H,
                         input$PE, input$APE, input$BIAS, input$ABSBIAS, input$IMPRECISION,
                         input$OVERCOSTED, input$UNDERCOSTED, input$EUCD, input$EUCD_Noise))
colnames(input) <- c("CP","Error", "#Error", "DENS", "Q_VAR", "DISP1",
                    "PCB", "PCH", 
                    "PE","APE","BIAS","|BIAS|","IMPRECISION",
                    "OC","UC","EUCD","EUCD-N")
                    
                    

##### CORRELATION ######
#summary(input)
t=cor(input, method = c("pearson"))
t= round(t, digits=2) 
t


apa.cor.table(input, filename="output/Table1_APA.doc", table.number=1)

