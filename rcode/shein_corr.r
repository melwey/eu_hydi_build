# shein_corr.r
# corrections to shein's contribution to HYDI
#
# Author: M.Weynants
# Date created: 2012/12/12
######################################################################
print('Apply corrections and re-check')
# general
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

# SRF_COAR_COV
general1$SRF_COAR_COV <- 'ND'

general1 <- general.checks(general1)

# meth: OK

# basic
# HOR1_NAME
hn <- unique(basic$HOR1_NAME)
basic1$HOR1_NAME[basic1$HOR1_NAME %in% hn[c(1,5)]] <- hn[2]
basic1$HOR1_NAME[basic1$HOR1_NAME %in% hn[c(6)]] <- ' E B   '
basic1$HOR1_NAME[basic1$HOR1_NAME %in% hn[c(7)]] <- ' A  h  '

basic1$BD_M[basic1$BD==-999]<-999
basic1$POR_M[basic1$POR==-999]<-999
basic1$COARSE_M[basic1$COARSE==-999]<-999

basic1 <- basic.checks(basic1,general1$PROFILE_ID,meth1$CODE_M)

# chemical

# psize

# ret

# cond
cond1 <- cond1[cond1$COND!=-999,]
#