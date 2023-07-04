# iovino_corr.r
# corrections to iovino's contribution to HYDI
#
# Author: M.Weynants
# Date created: 2012/12/03
######################################################################
print('Apply corrections and re-check')
# general
# WGS84: not in the right units!
general1$LOC_COOR_X <- general1$X_WGS84
general1$LOC_COOR_Y <- general1$Y_WGS84
general1[,c('X_WGS84','Y_WGS84')] <- -999
general1$LOC_COOR_SYST <- 'UTM 33S'
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
# SRF_ERO_COV
general1$SRF_ERO_COV <- -999
# SRF_SAL_COV
general1$SRF_SAL_COV <- -999
# WRB2006_RSG and qualifiers
general1[,37:43] <- 'ND'
# WRB1998
general1[,44:50] <- 'ND'

general1 <- general.checks(general1)

# meth: OK

# basic
# SAMPLE_POS
sp <- basic1$SAMPLE_POS
for (p in unique(basic1$PROFILE_ID)) {
ind <- basic1$PROFILE_ID == p
sp[ind] <- order(basic1$SAMPLE_ID[ind])
}
basic1$SAMPLE_POS <- sp
# HOR1_NAME
basic1$HOR1_NAME<- gsub(" A  p   "," A  p  ",basic1$HOR1_NAME)

basic1$STRUCTURE2 <- 'ND'

basic1 <- basic.checks(basic1,general1$PROFILE_ID,meth1$CODE_M)

# chemical
# SAMPLE_ID do not match PROFILE_ID

#n<-which(substr(as.character(chemical1$SAMPLE_ID),start=1,stop=8) != as.character(chemical1$PROFILE_ID))
n <- chemical1$PROFILE_ID != round(chemical1$SAMPLE_ID /100)
chemical1$SAMPLE_ID[n] <- basic1[match(chemical1$PROFILE_ID[n],basic1$PROFILE_ID),'SAMPLE_ID']

chemical1 <- chemical.checks(chemical1,basic1$SAMPLE_ID,meth1$CODE_M)

# psize
#n<-which(substr(as.character(psize1$SAMPLE_ID),start=1,stop=8) != as.character(psize1$PROFILE_ID))
n <- psize1$PROFILE_ID != round(psize1$SAMPLE_ID /100)
psize1$SAMPLE_ID[n] <- basic1[match(psize1$PROFILE_ID[n],basic1$PROFILE_ID),'SAMPLE_ID']

psize1 <- psize.checks(psize1,basic1$SAMPLE_ID,meth1$CODE_M)

# ret
#n<-which(substr(as.character(ret1$SAMPLE_ID),start=1,stop=8) != as.character(ret1$PROFILE_ID))
n <- ret1$PROFILE_ID != round(ret1$SAMPLE_ID/100)
ret1$SAMPLE_ID[n] <- basic1[match(ret1$PROFILE_ID[n],basic1$PROFILE_ID),'SAMPLE_ID']

ret1 <- ret.checks(ret1,basic1$SAMPLE_ID,meth1$CODE_M)

# cond
cond1$K_INV_MOD <- -999
cond1 <- cond.checks(cond1,basic1$SAMPLE_ID,meth1$CODE_M)