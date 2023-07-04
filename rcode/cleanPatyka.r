# cleanPatyka
# Adapts HYDI tables to comply to guidelines v1.1
#
# Author: M.Weynants
# Date created: 2012/12/11
####################################################################
print('Adapting to guidelines')

# GENERAL
# YEAR
general$YEAR <- as.numeric(gsub(',','',general$YEAR))
general$REL_T_SER <- -999

# BASIC
# SAMPLE_ID not unique
# remove missing SAMPLE_ID
basic <- basic[basic$SAMPLE_ID != -999,]
# clean duplicates
dup <- duplicated(basic$SAMPLE_ID)
basic$SAMPLE_ID[dup] <- as.numeric(paste(basic$PROFILE_ID[dup],basic$SAMPLE_POS[dup],sep='0'))
names(basic)[22:23] <- c('COARSE','COARSE_M')

# CHEMICAL
# remove missing SAMPLE_ID. Problem profile 80400048
chemical <- chemical[chemical$SAMPLE_ID != -999,]
# profile 80400210 duplicated
chemical <- chemical[! (chemical$PROFILE_ID == 80400210 & chemical$OC_M==-999),]

# PSIZE
psize$P_SIZE <- as.numeric(gsub(',','',psize$P_SIZE))

# RET
# remove missing values
ret <- ret[ret$THETA!=-999 & ret$TH_INV_P1!=-999,]

# COND
# remove missing values
cond <- cond[cond$COND!=-999 & cond$K_INV_P1!=-999,]

