setwd("~/Desktop/GitLab/early_LUAD_prognosis/Fig_4_KM_survival")

#==============//==================//
# Survival analysis in R: http://www.sthda.com/english/wiki/survival-analysis-basics
# Install and load required R package
# We’ll use two R packages:
  
  # survival for computing survival analyses
  # survminer for summarizing and visualizing the results of survival analysis
#==============//==================//
# Install the packages
install.packages(c("survival", "survminer"))

# Load the packages
library("survival")
library("survminer")
#==============//==================//
# STEP 1:
# Read the survival dataset
survstgAll = read.csv(file = "~/Desktop/GitLab/early_LUAD_prognosis/Fig_4_KM_survival/7.3_OUT_survivalTab_3clusterpyNBS_318samples(stg1234)_agesex_1Dec22.csv")
survtabC1C2 = read.csv(file = "~/Desktop/GitLab/early_LUAD_prognosis/Fig_4_KM_survival/7.3_OUT_survivalTab_3clusterpyNBS_318_onlyC1+C2(90+158)_agesex_1Dec22.csv")
survtabC1C3 = read.csv(file = "~/Desktop/GitLab/early_LUAD_prognosis/Fig_4_KM_survival/7.3_OUT_survivalTab_3clusterpyNBS_318_onlyC1+C3(90+70)_agesex_1Dec22.csv")
survtabC2C3 = read.csv(file = "~/Desktop/GitLab/early_LUAD_prognosis/Fig_4_KM_survival/7.3_OUT_survivalTab_3clusterpyNBS_318_onlyC2+C3(158+70)_agesex_1Dec22.csv")




# Take a quick look at what is in the data set:
dim(survstgAll) # 318  14
dim(survtabC1C2) # 248  14
dim(survtabC1C3) # 160  14
dim(survtabC2C3) # 228  14

#==============//==================//
# STEP 2: Compute survival curves: survfit()
# The function survfit() [in survival package] can be used to compute kaplan-Meier survival estimate. Its main arguments include:
   # a survival object created using the function Surv()
   # and the data set containing the variables.

# Example:
# Note: By default, the function print() shows a short summary of the survival curves. It prints the number of observations, 
# number of events, the median survival and the confidence limits for the median.

fitAll <- survfit(Surv(OS.time, OS) ~ cluster, data = survstgAll)   ##----> fitAll: OS.time, OS; missing = 5
print(fitAll)
  # Call: survfit(formula = Surv(OS.time, OS) ~ cluster, data = survstgAll)
# 5 observations deleted due to missingness 
# n events median 0.95LCL 0.95UCL
# cluster=c1  89     24   2617    1653      NA
# cluster=c2 155     56   1293    1115    1622
# cluster=c3  69     35   1043     711      NA

# Test on 25Mar2022: Compare remove either clus_2 or clus_3 when compared to clus_1 
fitC1C2 <- survfit(Surv(OS.time, OS) ~ cluster, data = survtabC1C2)   ##----> fitC1C2: OS.time, OS; missing = 4
print(fitC1C2)
  # Call: survfit(formula = Surv(OS.time, OS) ~ cluster, data = survtabC1C2)
  # 4 observations deleted due to missingness 
  # n events median 0.95LCL 0.95UCL
  # cluster=c1  89     24   2617    1653      NA
  # cluster=c2 155     56   1293    1115    1622
fitC1C3 <- survfit(Surv(OS.time, OS) ~ cluster, data = survtabC1C3)   ##----> fitC1C3: OS.time, OS; missing = 2
print(fitC1C3)
# Call: survfit(formula = Surv(OS.time, OS) ~ cluster, data = survtabC1C3)
# 2 observations deleted due to missingness 
# n events median 0.95LCL 0.95UCL
# cluster=c1 89     24   2617    1653      NA
# cluster=c3 69     35   1043     711      NA

fitC2C3 <- survfit(Surv(OS.time, OS) ~ cluster, data = survtabC2C3)   ##----> fitC2C3: OS.time, OS; missing = 4
print(fitC2C3)
# Call: survfit(formula = Surv(OS.time, OS) ~ cluster, data = survtabC2C3)
# 4 observations deleted due to missingness 
# n events median 0.95LCL 0.95UCL
# cluster=c2 155     56   1293    1115    1622
# cluster=c3  69     35   1043     711      NA

#---------------//
# If you want to display a more complete summary of the survival curves, type this:
summary(fitAll)                 ##----> fit1: OS.time, OS; missing = 5
 # Call: survfit(formula = Surv(OS.time, OS) ~ cluster, data = survstgAll)
# 5 observations deleted due to missingness 
# cluster=c1 
# time n.risk n.event survival std.err lower 95% CI upper 95% CI
# 4     88       1    0.989  0.0113        0.967        1.000
# 18     87       1    0.977  0.0159        0.947        1.000
# 62     83       1    0.965  0.0196        0.928        1.000
# .....
# 1632     10       1    0.354  0.0752       0.2332        0.537
# 2681      3       1    0.236  0.1085       0.0957        0.581


#==============//==================//
# STEP3: Visualize survival curves
# We’ll use the function ggsurvplot() [in Survminer R package] to produce the survival curves for the two groups of subjects.

#It’s also possible to show:
  
  ## the 95% confidence limits of the survivor function using the argument conf.int = TRUE.
    # the number and/or the percentage of individuals at risk by time using the option risk.table. 
  ## Allowed values for risk.table include:
  ## TRUE or FALSE specifying whether to show or not the risk table. Default is FALSE.
  ## “absolute” or “percentage”: to show the absolute number and the percentage of subjects at risk by time, respectively. 
  ## Use “abs_pct” to show both absolute number and percentage.
  ## the p-value of the Log-Rank test comparing the groups using pval = TRUE.
  ## horizontal/vertical line at median survival using the argument surv.median.line. 
  ## Allowed values include one of c(“none”, “hv”, “h”, “v”). v: vertical, h:horizontal.

##-----------##
## STEP3.2: KM plot with confidence intervals as "step"
#Ref:http://rpkgs.datanovia.com/survminer/reference/ggsurvplot_arguments.html
# The plot can be further customized using the following arguments:
# Edit plot to be simple on Oct1, 2022 <___*****
ggsurvplot(                ##----> fitAll: OS.time, OS; missing = 5 (all stages, 318 samples)
  fitAll,                   # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  pval.size = 2,            # font size
  conf.int = FALSE,        # show confidence intervals for 
  # point estimaes of survival curves.
  #conf.int.style = "step",  # customize style of confidence intervals  
                            #-------> Uncomment if want conf.int to be "step" not "tab"
  xlab = "Time in months",   # customize X axis label.
  xscale = "d_m",          # will transform labels from days to years #d_y
  xlim = c(0, 5500),
  break.time.by = 1000,     # break X axis in time intervals by 200.
  linetype= 'solid',size=0.9, #1 = straight line, #2 = dash line, 'solid', size is line size
  censor.shape= "*", # "|"
  #ggtheme = theme_light(), # customize plot and risk table with a theme.; theme_minimal(),
  ggtheme = theme_classic(base_size=15,base_rect_size = 10), #base_size is font size (15, 10)
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
                              # in legend of risk table.
  tables.height = 0.25,
  #ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.v: vertical, h:horizontal.
  legend.labs = 
    c("Cluster_1", "Cluster_2","Cluster_3"),    # change legend labels.
  palette = 
    c("lightgoldenrod2","mediumturquoise","lightcoral") # custom color palettes. c("#2E9FDF","#999999","#E7B800")
)

ggsurvplot(                ####----> fitC1C2
  fitC1C2,                   # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  pval.size = 2,           # font size
  conf.int = FALSE,        # show confidence intervals for 
  # point estimaes of survival curves.
  #conf.int.style = "step",  # customize style of confidence intervals  
  #-------> Uncomment if want conf.int to be "step" not "tab"
  xlab = "Time in months",   # customize X axis label.
  xscale = "d_m",          # will transform labels from days to years #d_y
  xlim = c(0, 5500),
  break.time.by = 1000,     # break X axis in time intervals by 200.
  linetype= 'solid',size=0.9, #1 = straight line, #2 = dash line, 'solid', size is line size
  censor.shape= "*", # "|"
  #ggtheme = theme_light(), # customize plot and risk table with a theme.; theme_minimal(),
  ggtheme = theme_classic(base_size=15,base_rect_size = 10), #base_size is font size
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  tables.height = 0.25,
  #ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.v: vertical, h:horizontal.
  legend.labs = 
    c("Cluster_1", "Cluster_2"),    # change legend labels.
  palette = 
    c("lightgoldenrod2","mediumturquoise") # custom color palettes. c("#2E9FDF","#999999","#E7B800")
)


ggsurvplot(                ####----> fitC1C3
  fitC1C3,                   # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  pval.size = 2,           # font size
  conf.int = FALSE,        # show confidence intervals for 
  # point estimaes of survival curves.
  #conf.int.style = "step",  # customize style of confidence intervals  
  #-------> Uncomment if want conf.int to be "step" not "tab"
  xlab = "Time in months",   # customize X axis label.
  xscale = "d_m",          # will transform labels from days to years #d_y
  xlim = c(0, 5500),
  break.time.by = 1000,     # break X axis in time intervals by 200.
  linetype= 'solid',size=0.9, #1 = straight line, #2 = dash line, 'solid', size is line size
  censor.shape= "*", # "|"
  #ggtheme = theme_light(), # customize plot and risk table with a theme.; theme_minimal(),
  ggtheme = theme_classic(base_size=15,base_rect_size = 10), #base_size is font size
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  tables.height = 0.25,
  #ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.v: vertical, h:horizontal.
  legend.labs = 
    c("Cluster_1", "Cluster_3"),    # change legend labels.
  palette = 
    c("lightgoldenrod2","lightcoral") # custom color palettes. c("#2E9FDF","#999999","#E7B800")
)


ggsurvplot(               ##----> ##----> fitC2C3
  fitC2C3,                   # survfit object with calculated statistics.
  pval = TRUE,             # show p-value of log-rank test.
  pval.size = 2,            # font size
  conf.int = FALSE,        # show confidence intervals for 
  # point estimaes of survival curves.
  #conf.int.style = "step",  # customize style of confidence intervals  
  #-------> Uncomment if want conf.int to be "step" not "tab"
  xlab = "Time in months",   # customize X axis label.
  xscale = "d_m",          # will transform labels from days to years #d_y
  xlim = c(0, 5500),
  break.time.by = 1000,     # break X axis in time intervals by 200.
  linetype= 'solid',size=0.9, #1 = straight line, #2 = dash line, 'solid', size is line size
  censor.shape= "*", # "|"
  #ggtheme = theme_light(), # customize plot and risk table with a theme.; theme_minimal(),
  ggtheme = theme_classic(base_size=15,base_rect_size = 10), #base_size is font size
  risk.table = "abs_pct",  # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  tables.height = 0.25,
  #ncensor.plot = TRUE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",  # add the median survival pointer.v: vertical, h:horizontal.
  legend.labs = 
    c("Cluster_2","Cluster_3"),    # change legend labels.
  palette = 
    c("mediumturquoise","lightcoral") # custom color palettes. c("#2E9FDF","#999999","#E7B800")
)


##-----------##

# The median survival times for each group can be obtained using the code below:
summary(fitAll)$table            ##----> fitAll: OS.time, OS; missing = 5 (all stages, 318 samples)
  #             records n.max n.start events    rmean se(rmean) median 0.95LCL 0.95UCL
  # cluster=c1      89    89      89     24 3850.414  603.1644   2617    1653      NA
  # cluster=c2     155   155     155     56 2102.741  406.7691   1293    1115    1622
  # cluster=c3      69    69      69     35 2436.159  551.6326   1043     711      NA

# Interpret: The median survival times for each group represent the time at which the survival probability, S(t), is 0.5.
# The median survival is approximately 2617 days for clus_1, suggesting a good survival for clus_1 compared to clus2,3.
# There appears to be a survival advantage for clus_1 with lung cancer compare to other cluster. However, to evaluate whether 
# this difference is statistically significant requires a formal statistical test, a subject that is discussed in the next sections.

# The Kaplan-Meier plot can be interpreted as follow:
# The horizontal axis (x-axis) represents time in days, and the vertical axis (y-axis) shows the probability of surviving or 
# the proportion of people surviving. The lines represent survival curves of the two groups. A vertical drop in the curves 
# indicates an event. The vertical tick mark on the curves means that a patient was censored at this time.
# At time zero, the survival probability is 1.0 (or 100% of the participants are alive).
# At time 250, the probability of survival is approximately 0.55 (or 55%) for sex=1 and 0.75 (or 75%) for sex=2.

#----<--Test on 25Mar2022: Compare remove either clus_2 or clus_3 when compared to clus_1 
summary(fitC1C2)$table            ##----> fitC1C2: OS.time, OS; missing = 4
#             records n.max n.start events    rmean se(rmean) median 0.95LCL 0.95UCL
# cluster=c1      89    89      89     24 3850.414  603.1644   2617    1653      NA
# cluster=c2     155   155     155     56 2102.741  406.7691   1293    1115    1622

summary(fitC1C3)$table            ##----> fitC1C3: OS.time, OS; missing = 2
#             records n.max n.start events    rmean se(rmean) median 0.95LCL 0.95UCL
# cluster=c1      89    89      89     24 3698.859  566.1175   2617    1653      NA
# cluster=c3      69    69      69     35 2358.335  516.8608   1043     711      NA

summary(fitC2C3)$table            ##----> fitC2C3: OS.time, OS; missing = 2
#             records n.max n.start events    rmean se(rmean) median 0.95LCL 0.95UCL
# cluster=c2     155   155     155     56 2102.741  406.7691   1293    1115    1622
# cluster=c3      69    69      69     35 2436.159  551.6326   1043     711      NA


#==============//==================//
# STEP 4: Log-Rank test comparing survival curves: survdiff()
# The log-rank test is the most widely used method of comparing two or more survival curves. The null hypothesis is that 
# there is no difference in survival between the two groups. The log rank test is a non-parametric test, which makes 
# no assumptions about the survival distributions. Essentially, the log rank test compares the observed number of events in 
# each group to what would be expected if the null hypothesis were true (i.e., if the survival curves were identical). 
# The log rank statistic is approximately distributed as a chi-square test statistic.

# The function survdiff() [in survival package] can be used to compute log-rank test comparing two or more survival curves.
# survdiff() can be used as follow:

surv_diff1 <- survdiff(Surv(OS.time, OS) ~ cluster, data = survstgAll)       ##----> fitAll: OS.time, OS; missing = 5 (all stages, 318 samples)
surv_diff1
#  Call:
    # survdiff(formula = Surv(OS.time, OS) ~ cluster, data = survstgAll)
  # n=313, 5 observations deleted due to missingness.
  #             N Observed Expected (O-E)^2/E (O-E)^2/V
  # cluster=c1  89       24     31.0   1.58209    2.1816
  # cluster=c2 155       56     56.7   0.00766    0.0153
  # cluster=c3  69       35     27.3   2.14764    2.8254
  # Chisq= 3.7  on 2 degrees of freedom, p= 0.2
 
# Interpret: The function returns a list of components, including:
  # n: the number of subjects in each group.
  # obs: the weighted observed number of events in each group.
  # exp: the weighted expected number of events in each group.
  # chisq: the chisquare statistic for a test of equality.
  # strata: optionally, the number of subjects contained in each stratum.
  # Note: The log rank test for difference in survival gives a p-value of p = 0.0013, indicating that 
  # the sex groups differ significantly in survival.

#------------
#----<--Test on 25Mar2022: Compare remove either clus_2 or clus_3 when compared to clus_1 
surv_diffC1C2 <- survdiff(Surv(OS.time, OS) ~ cluster, data = survtabC1C2)       ##----> fitC1C2: OS.time, OS; missing = 4
surv_diffC1C2
#  Call:
# survdiff(formula = Surv(OS.time, OS) ~ cluster, data = survtabC1C2)
# n=244, 4 observations deleted due to missingness.
#             N Observed Expected (O-E)^2/E (O-E)^2/V
# cluster=c1  89       24     28.7     0.768      1.22
# cluster=c2 155       56     51.3     0.430      1.22
# Chisq= 1.2  on 1 degrees of freedom, p= 0.3 

surv_diffC1C3 <- survdiff(Surv(OS.time, OS) ~ cluster, data = survtabC1C3)       ##----> fitC1C3: OS.time, OS; missing = 2
surv_diffC1C3
#  Call:
# survdiff(formula = Surv(OS.time, OS) ~ cluster, data = survtabC1C3)
# n=158, 2 observations deleted due to missingness.
#             N Observed Expected (O-E)^2/E (O-E)^2/V
# cluster=c1 89       24     31.6      1.82      3.94
# cluster=c3 69       35     27.4      2.10      3.94
# Chisq= 3.9  on 1 degrees of freedom, p= 0.05 

surv_diffC2C3 <- survdiff(Surv(OS.time, OS) ~ cluster, data = survtabC2C3)       ##----> fitC2C3: OS.time, OS; missing = 4
surv_diffC2C3
#  Call:
# survdiff(formula = Surv(OS.time, OS) ~ cluster, data = survtabC2C3)
# n=224, 4 observations deleted due to missingness.
#             N Observed Expected (O-E)^2/E (O-E)^2/V
# cluster=c2 155       56     61.1     0.422      1.29
# cluster=c3  69       35     29.9     0.861      1.29
# Chisq= 1.3  on 1 degrees of freedom, p= 0.3 



#==============//==================//
# 10Sep2022
# STEP 6: Compute  the Cox model: coxph()
# Compute "Univariate Cox regression" (consider only one variable)
# Ref: http://www.sthda.com/english/wiki/cox-proportional-hazards-model

# Example:
## res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
## res.cox

#==========Univariate Cox analysis==========

res.coxC1C2 <- coxph(Surv(OS.time, OS) ~ cluster, data = survtabC1C2)   ##----> fitC1C2: OS.time, OS; missing = 4
summary(res.coxC1C2)
# n= 244, number of events= 80 
# (4 observations deleted due to missingness)

#             coef    exp(coef) se(coef)  z   Pr(>|z|)
# clusterc2 0.2701    1.3100   0.2457 1.099    0.272

#             exp(coef) exp(-coef) lower .95  upper .95
# clusterc2      1.31     0.7633    0.8094      2.12

# Concordance= 0.482  (se = 0.033 )
# Likelihood ratio test= 1.25  on 1 df,   p=0.3
# Wald test            = 1.21  on 1 df,   p=0.3
# Score (logrank) test = 1.22  on 1 df,   p=0.3


res.coxC1C3 <- coxph(Surv(OS.time, OS) ~ cluster, data = survtabC1C3)   ##----> fitC1C3: OS.time, OS; missing = 2
summary(res.coxC1C3)
# n= 158, number of events= 59 
# (2 observations deleted due to missingness)

#             coef  exp(coef) se(coef)    z   Pr(>|z|)  
# clusterc3 0.5209    1.6836   0.2654 1.963   0.0497 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#             exp(coef) exp(-coef)   lower .95 upper .95
# clusterc3     1.684      0.594     1.001     2.832

# Concordance= 0.547  (se = 0.036 )
# Likelihood ratio test= 3.94  on 1 df,   p=0.05
# Wald test            = 3.85  on 1 df,   p=0.05
# Score (logrank) test = 3.94  on 1 df,   p=0.05

res.coxC2C3 <- coxph(Surv(OS.time, OS) ~ cluster, data = survtabC2C3)   ##----> fitC2C3: OS.time, OS; missing = 4
summary(res.coxC2C3)
# n= 224, number of events= 91 
# (4 observations deleted due to missingness)

#           coef      exp(coef) se(coef)  z   Pr(>|z|)
# clusterc3 0.2454    1.2781   0.2164 1.134    0.257

#           exp(coef) exp(-coef)    lower .95 upper .95
# clusterc3     1.278     0.7824    0.8363     1.953

# Concordance= 0.561  (se = 0.03 )
# Likelihood ratio test= 1.26  on 1 df,   p=0.3
# Wald test            = 1.29  on 1 df,   p=0.3
# Score (logrank) test = 1.29  on 1 df,   p=0.3 


#==========Multivariate Cox analysis==========

res.coxC1C2AgeSex <- coxph(Surv(OS.time, OS) ~ cluster + age + gender, data = survtabC1C2)   ##----> fitC1C2: OS.time, OS; missing = 7
summary(res.coxC1C2AgeSex)
#   n= 241, number of events= 79 
#  (7 observations deleted due to missingness)

#                 coef exp(coef) se(coef)      z Pr(>|z|)
#  clusterc2  0.22227   1.24890  0.24623  0.903    0.367
#  age        0.01686   1.01700  0.01189  1.418    0.156
#  gender    -0.11454   0.89177  0.22694 -0.505    0.614

#               exp(coef) exp(-coef) lower .95 upper .95
#  clusterc2    1.2489     0.8007    0.7708     2.024
#  age          1.0170     0.9833    0.9936     1.041
#  gender       0.8918     1.1214    0.5716     1.391

#  Concordance= 0.514  (se = 0.038 )
#  Likelihood ratio test= 3.4  on 3 df,   p=0.3
#  Wald test            = 3.26  on 3 df,   p=0.4
#  Score (logrank) test = 3.27  on 3 df,   p=0.4

res.coxC1C3AgeSex <- coxph(Surv(OS.time, OS) ~ cluster + age + gender, data = survtabC1C3)   ##----> fitC1C3: OS.time, OS; missing = 5
summary(res.coxC1C3AgeSex)
#   n= 155, number of events= 59 
#   (5 observations deleted due to missingness)

#               coef    exp(coef) se(coef)     z  Pr(>|z|)  
#   clusterc3  0.52707   1.69397  0.27017  1.951   0.0511 .
#   age        0.01317   1.01325  0.01309  1.006   0.3146  
#   gender    -0.04564   0.95539  0.27450 -0.166   0.8680  
#   ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#             exp(coef)  exp(-coef)   lower .95   upper .95
#   clusterc3    1.6940     0.5903    0.9976     2.877
#   age          1.0133     0.9869    0.9876     1.040
#   gender       0.9554     1.0467    0.5579     1.636

#   Concordance= 0.581  (se = 0.038 )
#   Likelihood ratio test= 5.58  on 3 df,   p=0.1
#   Wald test            = 5.46  on 3 df,   p=0.1
#   Score (logrank) test = 5.57  on 3 df,   p=0.1

res.coxC2C3AgeSex <- coxph(Surv(OS.time, OS) ~ cluster + age + gender, data = survtabC2C3)   ##----> fitC2C3: OS.time, OS; missing = 8
summary(res.coxC2C3AgeSex)
#   n= 220, number of events= 90 
#   (8 observations deleted due to missingness)

#                coef   exp(coef)  se(coef)      z  Pr(>|z|)
#   clusterc3  0.290291  1.336817  0.218025  1.331    0.183
#   age        0.003666  1.003673  0.010946  0.335    0.738
#   gender    -0.222695  0.800359  0.213979 -1.041    0.298

#               exp(coef) exp(-coef)  lower .95 upper .95
#   clusterc3    1.3368     0.7480    0.8719     2.050
#   age          1.0037     0.9963    0.9824     1.025
#   gender       0.8004     1.2494    0.5262     1.217

#   Concordance= 0.583  (se = 0.037 )
#   Likelihood ratio test= 3.3  on 3 df,   p=0.3
#   Wald test            = 3.37  on 3 df,   p=0.3
#   Score (logrank) test = 3.39  on 3 df,   p=0.3

