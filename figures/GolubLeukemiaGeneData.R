############################################################
# Author: Sarah Fletcher Mercaldo, PhD
# smercaldo@mgh.harvard.edu
# Updated: April 2018, slight modificaiton by J. Blume
#
# Purpose: This script transforms the raw Golub (1999) gene expression data.
# Data are rank normalized after dropping the extreme value.
# This matches Efron (2010; Large Scale Inference), section 6.1, page 91.
###########################################################

#Intall the Golub leukemia results
#source("http://bioconductor.org/biocLite.R")
#biocLite("golubEsets")
library(golubEsets)

##############################################
# Start Analysis of the Original Leukemia Data
##############################################

# Read in Leukemia data
data(Golub_Merge)

# Raw expression data, rows are genes, columns are people
exprsDat <- exprs(Golub_Merge)

# 7129 genes
N.tmp <- nrow(exprsDat)

# Normalizing the gene expression data as described by Efron (2010; Large Scale Inference), section 6.1, page 91.
NormalizedGeneDat <-  apply(exprsDat, 2, function(z) qnorm((rank(z)-0.5)/N.tmp))

# Efron dropped the most extreme value so we will too
mostExtreme <- which(abs(NormalizedGeneDat) == max(abs(NormalizedGeneDat)))[1]
NormalizedGeneDat <- NormalizedGeneDat[-mostExtreme,]

# Transform the data so the rows are peopel and the columns are genes
NormalizedGeneDat_t <- t(NormalizedGeneDat)
N <- nrow(NormalizedGeneDat)
# N = 7128 Now

# Combining the normalized gene expression with the covariate information
# First 7128 columns are genes, last 6 are covariates (Col 7129 is ALL.AML status)
LeukDat <- cbind(NormalizedGeneDat_t,pData(Golub_Merge)[,c('ALL.AML','BM.PB','T.B.cell','Gender','PS','Source')])

# Sort by ALL.AML status
hold=LeukDat[with(LeukDat, order(LeukDat[7129])),]
leukdata=t(hold[,-(7129:7134)])

####
###
##
#


