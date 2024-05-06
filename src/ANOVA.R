library(report)
library(lattice)

DATA = output
DATA$class = "no error"
DATA$class[which(DATA$pe>=0.05)]<- "overcosted"
DATA$class[which(DATA$pe<=-0.05)]<- "undercosted"

MXQ_anova = aov(res_numb~class, data = DATA)
summary(MXQ_anova)

boxplot(MXQ ~ class,
        data = DATA
)



pe_anova = aov(pe~pch+MXQ+batch_costs+res_numb+driver_numb+cons_bigDriver,data = DATA)

summary(pe_anova)
report(pe_anova)
