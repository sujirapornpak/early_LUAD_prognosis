
setwd("~/Desktop/GitLab/early_LUAD_prognosis/Fig_7_Oncoplot")

# maftools: Summarize, Analyze and Visualize MAF Files
# https://bioconductor.org/packages/release/bioc/vignettes/maftools/inst/doc/maftools.html
# Explanation of MAF file format: https://docs.gdc.cancer.gov/Data/File_Formats/MAF_Format/

# Rediscover is an R package to identify mutually exclusive genomic events coupled with maftools:
# https://cran.r-project.org/web/packages/Rediscover/vignettes/Rediscover.html

#=======//===========//=======//===========//=======//===========//

# STEP1: Installation

#if (!require("BiocManager"))
#  install.packages("BiocManager")
#BiocManager::install("maftools",force = TRUE)

library(maftools)
#=======//===========//=======//===========//=======//===========//
# STEP2: Loading file

# Troubleshooting loading file: https://github.com/PoisonAlien/maftools/issues/588
#path to TCGA LAML MAF file
  ##laml.maf = system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools') 
#clinical information containing survival information and histology. This is optional
  ##laml.clin = system.file('extdata', 'tcga_laml_annot.tsv', package = 'maftools') 

luadclusAll.maf = "~/Desktop/GitLab/early_LUAD_prognosis/Fig_7_Oncoplot/10.2_OUT_cSIF_Dri47_318LUAD_all3clus-maftools.maf"
luad.clin = "~/Desktop/Lab_DataMining/12.TCGA-LUAD/2_AFTpyNBS_3clus_21Feb22/10.2_OUT_318LUAD_ClinicalPhenoMaftools_3clus_OSsurv_30Mar.tsv"
luadclusAll = read.maf(maf = luadclusAll.maf, clinicalData = luad.clin)
luadclusAll
# An object of class  MAF 
# ID summary  Mean Median
# 1:        NCBI_Build      NA    NA     NA
# 2:            Center      NA    NA     NA
# 3:           Samples     318    NA     NA
# 4:            nGenes      47    NA     NA
# 5:   Frame_Shift_Del     107 0.336      0
# 6:   Frame_Shift_Ins      28 0.088      0
# 7:      In_Frame_Del      18 0.057      0
# 8:      In_Frame_Ins       1 0.003      0
# 9: Missense_Mutation    1531 4.814      4
# 10: Nonsense_Mutation     242 0.761      1
# 11:  Nonstop_Mutation       2 0.006      0
# 12:       Splice_Site     106 0.333      0
# 13:             total    2035 6.399      5

#=======//===========//=======//===========//=======//===========//
# STEP3: Oncoplot (Waterfall plot)
# Better representation of maf file can be shown as oncoplots, also known as waterfall plots.

#Select five genes belonging to an early epistatic gene set (EEGS)
EEGS = c("KRAS","RYR2","TP53","EGFR","KEAP1","STK11") 
colsAnn = list(
  'cluster' = c('c1' = 'lightgoldenrod1', 'c2' = 'mediumturquoise', 'c3' = 'lightcoral'),
  'tumor_stage' = c('Stage I' = 'lightgoldenrod1', 'Stage II' = 'mediumturquoise', 'Stage III' = 'lightcoral', 'Stage IV' = 'navy')
)

oncoplot(maf = luadclusAll, genes=EEGS, clinicalFeatures = c('cluster','tumor_stage'), 
         sortByAnnotation=FALSE, annotationColor = colsAnn,writeMatrix=TRUE,removeNonMutated = FALSE)



  

