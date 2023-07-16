library(readxl)
library(moments)
library("ggpubr")
#For metallic
Metallic=read_excel("q3d1.xlsx",sheet="Metallic",col_names = TRUE)
without_method_metallic=Metallic$"Without drug"
with_method_metallic=Metallic$"With Drug"

shapiro.test(without_method_metallic)
shapiro.test(with_method_metallic)

#plot(without_method_metallic-with_method_metallic, type = "line")
skewness(without_method_metallic-with_method_metallic)
kurtosis(without_method_metallic-with_method_metallic)
shapiro.test(without_method_metallic-with_method_metallic)
ggdensity(without_method_metallic-with_method_metallic, fill = "lightgray")
# QQ plot
ggqqplot(without_method_metallic-with_method_metallic)
#After checking if the data is normal or not
#         Shapiro-Wilk normality test

# data:  without_method_metallic
# W = 0.86611, p-value = 0.138

#         Shapiro-Wilk normality test

# data:  with_method_polymeric
# W = 0.96399, p-value = 0.8471

#Polymeric
Polymeric=read_excel("q3d1.xlsx",sheet="Polymeric",col_names = TRUE)
without_method_polymeric=Polymeric$"Without drug"
with_method_polymeric=Polymeric$"With Drug"

#plot(without_method_polymeric-with_method_polymeric, type = "line")
ggdensity(without_method_polymeric-with_method_polymeric, fill = "lightgray")
# QQ plot
ggqqplot(without_method_polymeric-with_method_polymeric)
skewness(without_method_polymeric-with_method_polymeric)
kurtosis(without_method_polymeric-with_method_polymeric)
wilcox.test(without_method_polymeric, with_method_polymeric, paired = TRUE, alternative = "greater")

shapiro.test(without_method_polymeric-with_method_polymeric)
shapiro.test(without_method_polymeric)
shapiro.test(with_method_polymeric)

#         Shapiro-Wilk normality test

# data:  without_method_polymeric
# W = 0.93051, p-value = 0.5207

#         Shapiro-Wilk normality test

# data:  with_method_polymeric
# W = 0.93727, p-value = 0.5844

#ECM
ECM=read_excel("q3d1.xlsx",sheet="ECM",col_names = TRUE)
without_method_ecm=ECM$"Without drug"
with_method_ecm=ECM$"With Drug"

plot(without_method_ecm - with_method_ecm, type = "line")
wilcox.test(without_method_ecm, with_method_ecm, paired = TRUE, alternative = "less")
ggdensity(without_method_ecm-with_method_ecm, fill = "lightgray")
# QQ plot
ggqqplot(without_method_ecm-with_method_ecm)
shapiro.test(without_method_ecm - with_method_ecm)
shapiro.test(without_method_ecm)
shapiro.test(with_method_ecm)

#         Shapiro-Wilk normality test

# data:  without_method_ecm
# W = 0.93314, p-value = 0.5451

#         Shapiro-Wilk normality test

# data:  with_method_ecm
# W = 0.93514, p-value = 0.564

#Liposome
Liposome=read_excel("q3d1.xlsx",sheet="Liposome",col_names = TRUE)
without_method_liposome=Liposome$"Without drug"
with_method_liposome=Liposome$"With Drug"

shapiro.test(without_method_liposome)
shapiro.test(with_method_liposome)
#plot(without_method_liposome - with_method_liposome, type = "line")
ggdensity(without_method_liposome-with_method_liposome, fill = "lightgray")
# QQ plot
ggqqplot(without_method_liposome-with_method_liposome)
shapiro.test(without_method_liposome - with_method_liposome)
wilcox.test(without_method_liposome, with_method_liposome, paired = TRUE, var.equal = TRUE, alternative = "greater")
skewness(without_method_liposome - with_method_liposome)
kurtosis(without_method_liposome - with_method_liposome)
 #         Shapiro-Wilk normality test

# data:  without_method_liposome
# W = 0.91348, p-value = 0.3792

#         Shapiro-Wilk normality test

# data:  with_method_liposome
# W = 0.9427, p-value = 0.6379

#Hypothesis Testing - Parametric since data is noramlly distributed
#Metallic
t.test(with_method_metallic,without_method_metallic,alternative="less",paired=TRUE,var.equal=FALSE)

#         Paired t-test

# data:  with_method_metallic and without_method_metallic
# t = -9.1117, df = 7, p-value = 1.968e-05
# alternative hypothesis: true mean difference is less than 0
# 95 percent confidence interval:
#       -Inf -13.21737
# sample estimates:
# mean difference
#       -16.68708

#Polymeric
t.test(with_method_polymeric,without_method_polymeric,alternative="less",paired=TRUE,var.equal=FALSE)

#         Paired t-test

# data:  with_method_polymeric and without_method_polymeric
# t = -35.699, df = 7, p-value = 1.757e-09
# alternative hypothesis: true mean difference is less than 0
# 95 percent confidence interval:
#      -Inf -48.3455
# sample estimates:
# mean difference
#         -51.055

#ECM
wilcox.test(with_method_ecm,without_method_ecm,alternative="less",paired=TRUE,var.equal=FALSE)

#         Paired t-test

# data:  with_method_ecm and without_method_ecm
# t = -18.683, df = 7, p-value = 1.562e-07
# alternative hypothesis: true mean difference is less than 0
# 95 percent confidence interval:
#       -Inf -15.35926
# sample estimates:
# mean difference
#       -17.09256

#Liposome
t.test(with_method_liposome,without_method_liposome,alternative="less",paired=TRUE,var.equal=FALSE)

#         Paired t-test

# data:  with_method_liposome and without_method_liposome
# t = -13.965, df = 7, p-value = 1.143e-06
# alternative hypothesis: true mean difference is less than 0
# 95 percent confidence interval:
#       -Inf -18.19672
# sample estimates:
# mean difference
#       -21.05297

#ANOVA - We only have one factor


#Pairwise Comparison of Delivery Methods

#Metallic vs Polymeric
t.test(with_method_metallic,with_method_polymeric,alternative="less")
#         Welch Two Sample t-test

# data:  with_method_metallic and with_method_polymeric
# t = 11.335, df = 13.065, p-value = 1
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#      -Inf 41.22427
# sample estimates:
# mean of x mean of y
#  283.3879  247.7323
#Metallic vs ECM
t.test(with_method_metallic,with_method_ecm,alternative="less")
#         Welch Two Sample t-test

# data:  with_method_metallic and with_method_ecm
# t = 0.68233, df = 12.944, p-value = 0.7465
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#      -Inf 10.28132
# sample estimates:
# mean of x mean of y
#  283.3879  280.5290
#Metallics vs Liposome
t.test(with_method_metallic,with_method_liposome,alternative="less")
#         Welch Two Sample t-test

# data:  with_method_metallic and with_method_liposome
# t = 0.52746, df = 12.438, p-value = 0.6964
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#      -Inf 10.15686
# sample estimates:
# mean of x mean of y
#  283.3879  281.0632
#Polymeric vs ECM
t.test(with_method_polymeric,with_method_ecm,alternative="less")
#         Welch Two Sample t-test

# data:  with_method_polymeric and with_method_ecm
# t = -8.494, df = 11.076, p-value = 1.755e-06
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#       -Inf -25.86689
# sample estimates:
# mean of x mean of y
#  247.7323  280.5290
#Polymeric vs Liposome
t.test(with_method_polymeric,with_method_liposome,alternative="less")
#         Welch Two Sample t-test

# data:  with_method_polymeric and with_method_liposome
# t = -8.1372, df = 10.585, p-value = 3.548e-06
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#       -Inf -25.94831
# sample estimates:
# mean of x mean of y
#  247.7323  281.0632
#ECM vs Liposome
t.test(with_method_ecm,with_method_liposome,alternative="less")
#         Welch Two Sample t-test

# data:  with_method_ecm and with_method_liposome
# t = -0.10806, df = 13.919, p-value = 0.4577
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#      -Inf 8.176426
# sample estimates:
# mean of x mean of y
#  280.5290  281.0632