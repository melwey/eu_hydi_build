# cleanLilly
#
# Author: M. Weynants
# Date created
#################################################################
# GENERAL
general$REL_T_SER <- -999
general$COMMENTS3 <- 'ND'

# METHOD:
# ! CAUTION ! CODE_M do not follow guidelines
meth[c(6,12:14),'METH_PAR'] <- c('CEC_M','P_M','THETA_M','COND_M')

# BASIC
# SAMPLE_ID
#basic[basic$SAMPLE_ID %in% basic$SAMPLE_ID[duplicated(basic$SAMPLE_ID)],1:5]
basic[9,2:3] <- c(8260000403,3)
basic[is.na(basic$BD),'BD'] <- -999
names(basic)[22:23] <- c('COARSE','COARSE_M')

# CHEMICAL 
chemical[duplicated(chemical$SAMPLE_ID),2] <- 8260000403
tmp <- chemical$ACIDITY_NA4O
chemical$ACIDITY_NA4O <- chemical$ACIDITY_NA4O_M
chemical$ACIDITY_NA4O_M <- tmp

# PSIZE
psize[25:27,'SAMPLE_ID']<-8260000403

# RET
# ret[ret$PROFILE_ID == 82600004,1:5]
ret[seq(47,59,2),'SAMPLE_ID'] <- 8260000403
ret[,6:14] <- -999

# COND
# cond[cond$PROFILE_ID == 82600004,1:5]
cond[8,'SAMPLE_ID'] <- 8260000403
#
