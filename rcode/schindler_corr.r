# schindler_corr
# 
# Author: M.Weynants
# Date created: 2013/01/10
####################################################################

# general
# CAUTION X and Y reversed
tmp <- general$X_WGS84
general$X_WGS84 <- general$Y_WGS84
general$Y_WGS84 <- tmp
# RC_L1
general1$RC_L1 <- gsub("Brandenburg", "DE4", general1$RC_L1)
general1$RC_L1 <- gsub("Thuringia", "DEG", general1$RC_L1)
general1$RC_L1 <- gsub("Sachsen-Anhalt", "DEE", general1$RC_L1)
# RC_L2
general1$RC_L2 <- "ND"
# LC_L2 and LC_L3
general1 <- general.checks(general1)

# method: OK

# basic
# HOR1_NAME: second character: horizons G and D
# BD: a lot of very small values generally associated to percentages of OC > 80%  

basic1 <- basic.checks(basic1,general1[[1]],meth1[[1]])

# chemical: OK
# OC is given as OM
chemical1$OC <- round(chemical1$OC/1.724,digit=2)
# corrections from Uwe:
# 1173 	Method 3 (Woesthoff) not method 2 (ash content burned in oven at 650deg)
# 1357	not OC, it is ash content
# 1272	not OC, it is ash content
# 1273 	not OC, it is ash content
# 1274 	not OC, it is ash content
chemical1$OC_M[chemical1$ID == 1173] <- 131
chemical1$OC_M[chemical1$ID %in% c(1357,1272,1273,1274)] <- 130
#
chemical1 <- chemical.checks(chemical1,basic1[[2]],meth1[[1]])

# psize
# missing values in P_M

# ret
# HEAD: some values (204) negative
# Uwe: you can delete all negative data pairs except the first. The tension of the first data pair should be set to zero
neg <- ret1$HEAD < 0
ret1$HEAD[neg] <- 0
ret1 <- ret1[! (duplicated(ret1[,c("SAMPLE_ID","HEAD")]) & neg),]
# THETA: some values >1: all from same sample, but not just a factor 10.
ret1 <- ret.checks(ret1,basic1[[2]],meth1[[1]])

# cond
# COND: some negative data (missing values)