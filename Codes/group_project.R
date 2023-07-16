library(readxl)
library(ggpubr)
#data_MDCK = read_excel("q3d1.xlsx", sheet = "MDCK")
#data_HPEC = read_excel("q3d1.xlsx", sheet = "HPEC")
#data_fib = read_excel("q3d1.xlsx", sheet = "Fib")
library(ggpubr)
data = read_excel("combined.xlsx")
ggboxplot(data, x = "Cell type", y = "Response time", 
          color = "Delivery method", 
          order = c("MDCK","HPEC","Fib"),
          ylab = "Response time", xlab = "Drug")



# homogeneity of variance

bartlett.test(data$`Response time` ~ data$`Cell type`)

aovOutput = aov(data$`Response time` ~ data$`Cell type`*data$`Delivery method`)
#summary(aovOutput)
tukOut = TukeyHSD(aovOutput)
print(tukOut)