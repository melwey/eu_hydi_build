# javaux_corr.r
# corrections to javaux's contribution to HYDI
#
# Author: M.Weynants
# Date created: 2012/12/12
######################################################################
print('Apply corrections and re-check')
# general
general1[[4]] <- "ND"
# LC_L1 to LC_L3
# codes from levels 1, 2 and 3 are mixed
s <- general1$LC_L1
s2 <- substr(s,start=2,stop=2)
s3 <- substr(s,start=3,stop=3)
general1$LC_L3[!grepl('0',s3)] <- s[!grepl('0',s3)]
general1$LC_L2 <- paste(substr(s,start=1,stop=2),'0',sep='')
general1$LC_L2[grepl('ND',s) | grepl('0',s2) | grepl('X',s2)] <- 'ND'
general1$LC_L1 <- paste(substr(s,start=1,stop=1),'00',sep='')
general1$LC_L1[grepl('ND',s)] <- 'ND'
# unique(general1$LC_L1)
# unique(general1$LC_L2)
# unique(general1$LC_L3)
# LU_L1 to LU_L2
s <- general1$LU_L1
general1$LU_L2[s %in% c('U111','U112')] <- s[s %in% c('U111','U112')]
general1$LU_L1[s %in% c('U111','U112')] <- paste(substr(s[s %in% c('U111','U112')],start=1,stop=3),'0',sep='')
general1$LU_L1[grepl('U36x',s)] <- 'U360'
# unique(general1$LU_L1)
# unique(general1$LU_L2)

# SRF_SAL_COV
general1$SRF_SAL_THIC <- 'ND'

general1 <- general.checks(general1)

# meth
meth1[meth1$METH_PAR == 'PH_H20','METH_PAR'] <- 'PH_H2O_M'
meth1[meth1$METH_PAR == 'EC','METH_PAR'] <- 'EC_M'
meth1[meth1$METH_PAR == 'K_INV_M','METH_PAR'] <- 'K_INV_MOD'

meth1 <- meth.checks(meth1)

# # basic
basic1$HOR1_NAME[grepl('D',basic1$HOR1_NAME)] <- '1C     '
basic1$HOR1_NAME[substr(basic1$HOR1_NAME,5,5) %in% c('1','2')] <- paste(substr(basic1$HOR1_NAME[substr(basic1$HOR1_NAME,5,5) %in% c('1','2')],1,4),substr(basic1$HOR1_NAME[substr(basic1$HOR1_NAME,5,5) %in% c('1','2')],5,5),sep='  ')

basic1 <- basic.checks(basic1,general1$PROFILE_ID,meth1$CODE_M)

# cond
# sample_ID not in basic
cond1 <- cond1[cond1$SAMPLE_ID %in% basic1$SAMPLE_ID,]
names(cond)[15] <- 'K_INV_P9'