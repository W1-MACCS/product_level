##### COMPUTE PROBABILITIES
library(dplyr)


input <- read.csv("output/Imprecision/ProductCost_2020-04-28-1801.csv")
input = subset(input, input$DENS==0.5 & input$DISP1==12 & input$Q_VAR==1 & input$CP == 25)
input$PRODUCT <- as.factor(input$PRODUCT)
input$PE <- input$PE * 100



###### Calculating +-5% after Anderson for botch datasets ####
input$OVERCOSTED = input$PE > 5
input$OVERCOSTED = as.numeric(input$OVERCOSTED)
input$UNDERCOSTED = input$PE < -5
input$UNDERCOSTED = as.numeric(input$UNDERCOSTED)
input$WITHINRANGE = input$PE > -5 & input$PE <= 5
input$WITHINRANGE = as.numeric(input$WITHINRANGE)

###### COUNTING OVER AND UNDERCOSTING #########
#ABC
#45,000   Used for the Table in this section

input = group_by(input, input$PRODUCT)
summarize(input,n=n())

tapply(input$OVERCOSTED,  list(input$PRODUCT), sum)
tapply(input$UNDERCOSTED,  list(input$PRODUCT), sum)
tapply(input$WITHINRANGE,  list(input$PRODUCT), sum)

output.overcost = tapply(input$OVERCOSTED,  list(input$PRODUCT), sum)*100/270
output.undercost = tapply(input$UNDERCOSTED, list(input$PRODUCT), sum)*100/270
output.withinrange = tapply(input$WITHINRANGE, list(input$PRODUCT), sum)*100/270

###### Computing Bias +- Imprecision

output.error_bias = (tapply(input$PE,  list(input$PRODUCT), mean))
output.error_imprecision = (tapply(input$PE,  list(input$PRODUCT), sd))*2

###### Computing PCh
output.PCh_bias = (tapply(input$PCh,  list(input$PRODUCT), mean))
output.PCh_imprecision = (tapply(input$PCh,  list(input$PRODUCT), sd))*2

#### Check for reasonability ; SD must be 0
output.PCb = (tapply(input$PCb,  list(input$PRODUCT), sd))
output.PCb = (tapply(input$PCb,  list(input$PRODUCT), sd)) 

### binding the variables. 

output <- cbind(output.PCh_bias,output.PCh_imprecision, output.overcost, output.undercost, output.withinrange, 
                output.error_bias, output.error_imprecision
                )
write.csv(output, "output/Imprecision/Portfolio.csv")
