# cleanRomano

# Author: M. weynants
# Date created: 2012/12/14
####################################################################

# GENERAL
# profile_id not unique!
general0 <- general
general[is.na(general0)] <- -999

# METHOD
meth0 <- meth
#meth$CODE_M %in% c('POR_M','BD_M','COARSE_M','OC_M','CACO3_M','PH_H2O_M','PH_KCL_M','EC_M','SALT_M','CEC_M','EX_NA_M','EX_MG_M','EX_K_M','EX_CA_M','ACIDITY_NA4O_M','ACIDITY_KCL_M','P_M','THETA_M','TH_INV_MOD','COND_M','K_INV_M')
meth$METH_PAR[meth0$CODE_M == 701] <- 'TH_INV_MOD'
meth$METH_PAR[meth0$CODE_M %in% c(801,802)] <- 'COND_M'
meth$METH_PAR[meth0$CODE_M == 150] <- 'PH_H2O_M'
meth$METH_PAR[meth0$CODE_M == 140] <- 'CACO3_M'

# BASIC
basic0 <- basic
basic$COARSE_M <- -999

# chemical
chemical <- chemical[,!grepl("BASE_CATIONS_M",names(chemical))]

# PSIZE

# RET
ret[is.na(ret)] <- -999

# COND
cond0 <- cond
cond[is.na(cond0)]<- -999
names(cond)[6] <-"COND_M"