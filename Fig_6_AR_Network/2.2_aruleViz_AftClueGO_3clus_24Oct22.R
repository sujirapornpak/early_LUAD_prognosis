
setwd("~/Desktop/GitLab/early_LUAD_prognosis/Fig_6_AR_Network")
# https://www.datacamp.com/community/tutorials/market-basket-analysis-r
# https://www.analyticsvidhya.com/blog/2017/08/mining-frequent-items-using-apriori-algorithm/

# Install essential packages for Association rules
# install.packages("arules")
# install.packages("arulesViz")
# install.packages("plotly")
library(arules)
library(arulesViz)
library(tidyverse)
library(knitr)
library(lubridate)
library(plyr) ## must unload plyr and reload again (plyr must be loaded before dplyr)
library(dplyr)
library(plotly)
library(writexl)

## Step-by-step csv file loading and association rules  
## https://www.datacamp.com/community/tutorials/market-basket-analysis-r
#=============//=============// =============// =============// =============//  

# ***** Run on 24Oct2022: After getting 3 clusters done by pyNBS 

clus1 <- read.transactions('~/Desktop/GitLab/early_LUAD_prognosis/Fig_6_AR_Network/8.3_OUT_cSIF_Driv47_318LUAD_clus1-90_apriori_7Mar.csv', format = 'basket', sep=',')
clus2 <- read.transactions('~/Desktop/GitLab/early_LUAD_prognosis/Fig_6_AR_Network/8.3_OUT_cSIF_Driv47_318LUAD_clus2-158_apriori_7Mar.csv', format = 'basket', sep=',')
clus3 <- read.transactions('~/Desktop/GitLab/early_LUAD_prognosis/Fig_6_AR_Network/8.3_OUT_cSIF_Driv47_318LUAD_clus3-70_apriori_7Mar.csv', format = 'basket', sep=',')

clus1 
  # transactions in sparse format with
  # 90 transactions (rows) and
  # 45 items (columns)
clus2 
  # transactions in sparse format with
  # 158 transactions (rows) and
  # 47 items (columns)
clus3
  # transactions in sparse format with
  # 70 transactions (rows) and
  # 47 items (columns)
#=============//=============// =============// =============// =============//  

summary(clus1)      
  # transactions as itemMatrix in sparse format with
  # 90 rows (elements/itemsets/transactions) and
  # 45 columns (items) and a density of 0.111358
  # most frequent items:
    # KRAS    RYR2    TP53     ATM   STK11 (Other) 
      #88      41      39      22      18     243 
  #element (itemset/transaction) length distribution:
    #sizes
  #3  4  5  6  7  8  9 10 12 
  #23 24 13 14  6  5  1  1  3 
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #3.000   3.250   4.000   5.011   6.000  12.000 
  #includes extended item information - examples:
  #labels
  #1   AFF2
  #2  AMER1
  #3 ARID1A

summary(clus2)      
  # transactions as itemMatrix in sparse format with
  # 158 rows (elements/itemsets/transactions) and
  # 47 columns (items) and a density of 0.1206572 
  # most frequent items:
  #   TP53    RYR2   PTPRD   KMT2C   ASXL3 (Other) 
    # 134      94      47      44      37     540 
  # element (itemset/transaction) length distribution:
  #   sizes
  # 3  4  5  6  7  8  9 10 11 12 14 15 
  # 44 30 18 21 13  5  8  6  4  3  5  1 
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 3.000   3.000   5.000   5.671   7.000  15.000 
  # includes extended item information - examples:
  #   labels
  # 1   AFF2
  # 2  AMER1
  # 3 ARID1A

summary(clus3)      
  # transactions as itemMatrix in sparse format with
  # 70 rows (elements/itemsets/transactions) and
  # 47 columns (items) and a density of 0.1170213 
  # most frequent items:
  #   KEAP1    RYR2   STK11    TP53    KRAS (Other) 
      #60      34      28      24      21     218 
  #element (itemset/transaction) length distribution:
  #  sizes
  #3  4  5  6  7  8  9 10 11 13 
  #17 11 14  8  7  4  3  4  1  1 
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  #3.0     4.0     5.0     5.5     7.0    13.0 
  #includes extended item information - examples:
  #  labels
  #1   AFF2
  #2  AMER1
  #3 ARID1A

#=============//=============// =============// =============// =============//  

if (!require("RColorBrewer")) {
  # install color package of R
install.packages("RColorBrewer")
  #include library RColorBrewer
library(RColorBrewer)
}

# Create an item frequency plot for the top 20 items
# If type=absolute, it will plot numeric frequencies of each item independently. 
# If type=relative, it will plot how many times these items have appeared as compared to others.
itemFrequencyPlot(clus1,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

#=============//=============// =============// =============// =============//  

## STEP 2: Generating Rules!
#---26Oct22: For maxlen=2 (creating weighted undirected graph by Networkx)
association.rulesClus1_005_2 <- apriori(clus1, parameter = list(supp=0.05, conf=0.5,maxlen=2)) #42 rules 
association.rulesClus2_005_2 <- apriori(clus2, parameter = list(supp=0.05, conf=0.5,maxlen=2)) #57 rules
association.rulesClus3_005_2 <- apriori(clus3, parameter = list(supp=0.05, conf=0.5,maxlen=2)) #59 rules


summary(association.rulesClus1_005_2) #supp=0.05
# set of 42 rules
# rule length distribution (lhs + rhs):sizes
# 1  2 
# 1 41 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   2.000   1.976   2.000   2.000 
# summary of quality measures:
#   support          confidence        coverage            lift            count      
# Min.   :0.05556   Min.   :0.5000   Min.   :0.05556   Min.   :0.8523   Min.   : 5.00  
# 1st Qu.:0.06667   1st Qu.:0.7143   1st Qu.:0.07778   1st Qu.:1.0227   1st Qu.: 6.00  
# Median :0.08889   Median :0.8990   Median :0.10000   Median :1.0227   Median : 8.00  
# Mean   :0.13651   Mean   :0.8468   Mean   :0.15979   Mean   :1.3076   Mean   :12.29  
# 3rd Qu.:0.12222   3rd Qu.:1.0000   3rd Qu.:0.18889   3rd Qu.:1.5197   3rd Qu.:11.00  
# Max.   :0.97778   Max.   :1.0000   Max.   :1.00000   Max.   :3.3088   Max.   :88.00 

summary(association.rulesClus2_005_2) #supp=0.05
# set of 57 rules
# rule length distribution (lhs + rhs):sizes
# 1  2 
# 2 55 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   2.000   1.965   2.000   2.000 
# summary of quality measures:
#   support          confidence        coverage            lift            count       
# Min.   :0.05063   Min.   :0.5000   Min.   :0.05063   Min.   :0.8647   Min.   :  8.00  
# 1st Qu.:0.06329   1st Qu.:0.7500   1st Qu.:0.08228   1st Qu.:1.0057   1st Qu.: 10.00  
# Median :0.08861   Median :0.8261   Median :0.10759   Median :1.1054   Median : 14.00  
# Mean   :0.13891   Mean   :0.8220   Mean   :0.17744   Mean   :1.1467   Mean   : 21.95  
# 3rd Qu.:0.14557   3rd Qu.:0.9167   3rd Qu.:0.17722   3rd Qu.:1.2326   3rd Qu.: 23.00  
# Max.   :0.84810   Max.   :1.0000   Max.   :1.00000   Max.   :1.7955   Max.   :134.00  

summary(association.rulesClus3_005_2) #supp=0.05
# set of 59 rules
# rule length distribution (lhs + rhs):sizes
# 1  2 
# 1 58 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   2.000   1.983   2.000   2.000 
# summary of quality measures:
#   support          confidence        coverage            lift            count       
# Min.   :0.05714   Min.   :0.5000   Min.   :0.05714   Min.   :0.7778   Min.   : 4.000  
# 1st Qu.:0.05714   1st Qu.:0.6667   1st Qu.:0.08571   1st Qu.:1.0000   1st Qu.: 4.000  
# Median :0.08571   Median :0.8000   Median :0.11429   Median :1.1667   Median : 6.000  
# Mean   :0.11186   Mean   :0.7837   Mean   :0.14455   Mean   :1.3927   Mean   : 7.831  
# 3rd Qu.:0.10000   3rd Qu.:0.8750   3rd Qu.:0.12857   3rd Qu.:1.5956   3rd Qu.: 7.000  
# Max.   :0.85714   Max.   :1.0000   Max.   :1.00000   Max.   :3.5000   Max.   :60.000  

#=============//=============// =============// =============// =============//  
# Print only top 10 rules:
inspect(association.rulesClus1_005_2[1:10])   #For Cluster 1
       # lhs          rhs      support     confidence coverage    lift      count
  # [1]  {}        => {KRAS} 0.97777778 0.9777778  1.00000000 1.0000000 88   
  # [2]  {CREBBP}  => {KRAS} 0.05555556 1.0000000  0.05555556 1.0227273  5   
  # [3]  {SMARCA4} => {KRAS} 0.05555556 0.8333333  0.06666667 0.8522727  5   
  # [4]  {RBM10}   => {KRAS} 0.06666667 1.0000000  0.06666667 1.0227273  6   
  # [5]  {SETD2}   => {KRAS} 0.06666667 1.0000000  0.06666667 1.0227273  6   
  # [6]  {ARID1A}  => {TP53} 0.05555556 0.7142857  0.07777778 1.6483516  5   
  # [7]  {ARID1A}  => {KRAS} 0.07777778 1.0000000  0.07777778 1.0227273  7   
  # [8]  {KMT2A}   => {KRAS} 0.07777778 1.0000000  0.07777778 1.0227273  7   
  # [9]  {KMT2C}   => {TP53} 0.06666667 0.7500000  0.08888889 1.7307692  6   
  # [10] {KMT2C}   => {KRAS} 0.08888889 1.0000000  0.08888889 1.0227273  8   

inspect(association.rulesClus2_005_2[1:10])   #For Cluster 2
        # lhs          rhs      support     confidence coverage    lift      count
  # [1]  {}      => {RYR2} 0.59493671 0.5949367  1.00000000 1.0000000  94  
  # [2]  {}      => {TP53} 0.84810127 0.8481013  1.00000000 1.0000000 134  
  # [3]  {EPHA4} => {TP53} 0.05696203 1.0000000  0.05696203 1.1791045   9  
  # [4]  {CDK12} => {TP53} 0.05063291 0.8888889  0.05696203 1.0480929   8  
  # [5]  {AMER1} => {RYR2} 0.05063291 1.0000000  0.05063291 1.6808511   8  
  # [6]  {AMER1} => {TP53} 0.05063291 1.0000000  0.05063291 1.1791045   8  
  # [7]  {RBM10} => {TP53} 0.06329114 0.7692308  0.08227848 0.9070034  10  
  # [8]  {STK11} => {TP53} 0.08227848 1.0000000  0.08227848 1.1791045  13  
  # [9]  {PRKCB} => {RYR2} 0.05063291 0.8000000  0.06329114 1.3446809   8  
  # [10] {PRKCB} => {TP53} 0.05696203 0.9000000  0.06329114 1.0611940   9  

inspect(association.rulesClus3_005_2[1:10])   #For Cluster 3
      # lhs          rhs      support     confidence coverage    lift      count
  # [1]  {}       => {KEAP1} 0.85714286 0.8571429  1.00000000 1.000000 60   
  # [2]  {ATM}    => {STK11} 0.05714286 1.0000000  0.05714286 2.500000  4   
  # [3]  {MGA}    => {KEAP1} 0.05714286 1.0000000  0.05714286 1.166667  4   
  # [4]  {CDKN2A} => {RYR2}  0.05714286 1.0000000  0.05714286 2.058824  4   
  # [5]  {CDK12}  => {TP53}  0.07142857 1.0000000  0.07142857 2.916667  5   
  # [6]  {CDK12}  => {KEAP1} 0.07142857 1.0000000  0.07142857 1.166667  5   
  # [7]  {RB1}    => {RYR2}  0.05714286 1.0000000  0.05714286 2.058824  4   
  # [8]  {RB1}    => {KEAP1} 0.05714286 1.0000000  0.05714286 1.166667  4   
  # [9]  {CTNNB1} => {KEAP1} 0.08571429 0.7500000  0.11428571 0.875000  6   
  # [10] {NUP98}  => {RYR2}  0.05714286 0.8000000  0.07142857 1.647059  4 

#=============//=============// =============// =============// =============//  
## Step 6: Visualizing Association Rules
# 6.1 Scatterplot:
# A straight-forward visualization of association rules is to use a scatter plot using plot() of the arulesViz package. 
# It uses Support and Confidence on the axes. In addition, third measure Lift is used by default to color (grey levels) 
# of the points.

# Filter rules with confidence greater than 0.49 (or 50% up)
subRulesC1_005_2<-association.rulesClus1_005_2[quality(association.rulesClus1_005_2)$confidence>0.49]
subRulesC2_005_2<-association.rulesClus2_005_2[quality(association.rulesClus1_005_2)$confidence>0.49]
subRulesC3_005_2<-association.rulesClus3_005_2[quality(association.rulesClus1_005_2)$confidence>0.49]

plot(subRulesC1_005_2)  # Scatter plot for 42 rules
plot(subRulesC1_005_2,method="two-key plot") # 42 rules   

#=============//=============// =============// =============// =============//  

# 6.3 Graph-Based Visualizations
# Rank by "support" value
top42subRuleC1_005_Sup <- head(subRulesC1_005_2, n = 42, by = "support")  #42 rules, maxlength=2 
top57subRuleC2_005_Sup <- head(subRulesC2_005_2, n = 57, by = "support")  #57 rules, maxlength=2 
top59subRuleC3_005_Sup <- head(subRulesC3_005_2, n = 59, by = "support")  #59 rules, maxlength=2 

inspect(top42subRuleC1_005_Sup)
inspect(top57subRuleC2_005_Sup)
inspect(top59subRuleC3_005_Sup)

plot(top42subRuleC1_005_Sup, method="paracoord",control = list(reorder = TRUE)) 
plot(top57subRuleC2_005_Sup, method="paracoord",control = list(reorder = TRUE)) 
plot(top59subRuleC3_005_Sup, method="paracoord",control = list(reorder = TRUE)) 

plot(top42subRuleC1_005_Sup, method = "graph",  control = list(type="items")) 
plot(top57subRuleC2_005_Sup, method = "graph",  control = list(type="items")) 
plot(top59subRuleC3_005_Sup, method = "graph",  control = list(type="items")) 


#=============//=============// =============// =============// =============//  
# 7 Export to Excel file
# For maxlen=2 (creating weighted undirected graph by Networkx)
df_top42subRuleC1_005_2 <- inspect(top42subRuleC1_005_Sup)
df_top57subRuleC2_005_2 <- inspect(top57subRuleC2_005_Sup)
df_top59subRuleC3_005_2 <- inspect(top59subRuleC3_005_Sup)

write_xlsx(df_top42subRuleC1_005_2,"~/Desktop/GitLab/early_LUAD_prognosis/Fig_6_AR_Network/output/2.2_Out_Dri47_318clus1_90_sup005_maxlen2_42rules_24Oct.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(df_top57subRuleC2_005_2,"~/Desktop/GitLab/early_LUAD_prognosis/Fig_6_AR_Network/output/2.2_Out_Dri47_318clus2_158_sup005_maxlen2_57rules_24Oct.xlsx", col_names = TRUE,format_headers = TRUE)
write_xlsx(df_top59subRuleC3_005_2,"~/Desktop/GitLab/early_LUAD_prognosis/Fig_6_AR_Network/output/2.2_Out_Dri47_318clus3_70_sup005_maxlen2_59rules_24Oct.xlsx", col_names = TRUE,format_headers = TRUE)

  
