# chemical.checks(x,y)
# checks conformity of table CHEMICAL against EU-HYDI requirements
# y is a vector of SAMPLE_ID in table BASIC
# z is a vector of CODE_M in table METHOD
#
# Author: M.Weynants
# Date created: 2012/11/28
##################################################################
chemical.checks <- function(x,y,z){
print('checking CHEMICAL...')
# check size: 29 fields
if (length(x) < 29) stop("Table CHEMICAL has not enough columns")
if (length(x) > 29) warning("Table CHEMICAL has too many columns and will be truncated")
# check each field
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {all(abs(x-round(x)) < tol)}
# PROFILE_ID
if (! is.wholenumber(x[[1]])) warning("PROFILE_ID must be an integer")
# SAMPLE_ID
if (any(!x[[2]] %in% y)) warning("SAMPLE_ID must be a valid profile identifier from table BASIC")
if (any(!is.wholenumber(x[[2]]))) warning("SAMPLE_ID must be an integer")
if (any(duplicated(x[[2]]))) warning("SAMPLE_ID must be a unique sample identifier!!!!")
if (any(x[[1]] != floor(x[[2]]/100))) warning("The first 8 digits of SAMPLE_ID must be the same as PROFILE_ID")
attach(x)
# OC
if (!is.numeric(OC)) warning('OC must be numeric')
if (any(OC==-999)) warning('OC is a mandatory field')
if (any(OC[OC!=-999] < 0 | OC[OC!=-999] > 100)) warning('OC must be expressed in mass percentage')
# OC_M
if (!is.wholenumber(OC_M)) warning("OC_M must be an integer")
if (any(nchar(as.character(OC_M[OC!=-999]))!=3)) warning("OC_M must be a 3 digits integer")
if (any(!OC_M %in% c(z,-999))) warning('OC_M must be a valid method code from table METHOD')
# CACO3
if (!is.numeric(CACO3)) warning('CACO3 must be numeric')
if (any(CACO3[CACO3!=-999] < 0 | CACO3[CACO3!=-999] > 100)) warning('CACO3 must be expressed in mass percentage')
# CACO3_M
if (!is.wholenumber(CACO3_M)) warning("CACO3_M must be an integer")
if (any(nchar(as.character(CACO3_M[CACO3!=-999]))!=3)) warning("CACO3_M must be a 3 digits integer")
if (any(!CACO3_M %in% c(z,-999))) warning('CACO3_M must be a valid method code from table METHOD')
# PH_H2O
if (!is.numeric(PH_H2O)) warning('PH_H2O must be numeric')
if (any(PH_H2O[PH_H2O!=-999] < 1 | PH_H2O[PH_H2O!=-999] > 14)) warning('PH_H2O must be a valid pH value')
# PH_H2O_M
if (!is.wholenumber(PH_H2O_M)) warning("PH_H2O_M must be an integer")
if (any(nchar(as.character(PH_H2O_M[PH_H2O!=-999]))!=3)) warning("PH_H2O_M must be a 3 digits integer")
if (any(!PH_H2O_M %in% c(z,-999))) warning('PH_H2O_M must be a valid method code from table METHOD')
# PH_KCL
if (!is.numeric(PH_KCL)) warning('PH_KCL must be numeric')
if (any(PH_KCL[PH_KCL!=-999] < 1 | PH_KCL[PH_KCL!=-999] > 14)) warning('PH_KCL must be a valid pH value')
# PH_KCL_M
if (!is.wholenumber(PH_KCL_M)) warning("PH_KCL_M must be an integer")
if (any(nchar(as.character(PH_KCL_M[PH_KCL!=-999]))!=3)) warning("PH_KCL_M must be a 3 digits integer")
if (any(!PH_KCL_M %in% c(z,-999))) warning('PH_KCL_M must be a valid method code from table METHOD')
# EC # rule of thumb for a "good" soil: between 0.2 and 1.2 mS/cm
if (!is.numeric(EC)) warning('EC must be numeric')
if (any(EC[EC!=-999] < 0 | EC[EC!=-999] > 100)) warning('EC must be expressed in mS/cm. Some values are unrealistic.')
# EC_M
if (!is.wholenumber(EC_M)) warning("EC_M must be an integer")
if (any(nchar(as.character(EC_M[EC!=-999]))!=3)) warning("EC_M must be a 3 digits integer")
if (any(!EC_M %in% c(z,-999))) warning('EC_M must be a valid method code from table METHOD')
# SALT
if (!is.numeric(SALT)) warning('SALT must be numeric')
if (any(SALT[SALT!=-999] < 0 | SALT[SALT!=-999] > 100)) warning('SALT must be expressed in weight percentage. Some values are unrealistic.')
# SALT_M
if (!is.wholenumber(SALT_M)) warning("SALT_M must be an integer")
if (any(nchar(as.character(SALT_M[SALT!=-999]))!=3)) warning("SALT_M must be a 3 digits integer")
if (any(!SALT_M %in% c(z,-999))) warning('SALT_M must be a valid method code from table METHOD')
# CEC # standard values: kaolinite: 3-15, vermiculite: 100-150
if (!is.numeric(CEC)) warning('CEC must be numeric')
if (any(CEC[CEC!=-999] < 0 | CEC[CEC!=-999] > 200)) warning('CEC must be expressed in meq/(100g). Some values are unrealistic.')
# CEC_M
if (!is.wholenumber(CEC_M)) warning("CEC_M must be an integer")
if (any(nchar(as.character(CEC_M[CEC!=-999]))!=3)) warning("CEC_M must be a 3 digits integer")
if (any(!CEC_M %in% c(z,-999))) warning('CEC_M must be a valid method code from table METHOD')
# EX_NA
if (!is.numeric(EX_NA)) warning('EX_NA must be numeric')
if (any(EX_NA[EX_NA!=-999] < 0) | any(EX_NA[EX_NA!=-999 & CEC!=-999] > CEC[EX_NA!=-999 & CEC!=-999])) warning('EX_NA must be expressed in meq/(100g). Some values are unrealistic.')
# EX_NA_M
if (!is.wholenumber(EX_NA_M)) warning("EX_NA_M must be an integer")
if (any(nchar(as.character(EX_NA_M[EX_NA!=-999]))!=3)) warning("EX_NA_M must be a 3 digits integer")
if (any(!EX_NA_M %in% c(z,-999))) warning('EX_NA_M must be a valid method code from table METHOD')
# EX_MG
if (!is.numeric(EX_MG)) warning('EX_MG must be numeric')
if (any(EX_MG[EX_MG!=-999] < 0) | any(EX_MG[EX_MG!=-999 & CEC!=-999] > CEC[EX_MG!=-999 & CEC!=-999])) warning('EX_MG must be expressed in meq/(100g). Some values are unrealistic.')
# EX_MG_M
if (!is.wholenumber(EX_MG_M)) warning("EX_MG_M must be an integer")
if (any(nchar(as.character(EX_MG_M[EX_MG!=-999]))!=3)) warning("EX_MG_M must be a 3 digits integer")
if (any(!EX_MG_M %in% c(z,-999))) warning('EX_MG_M must be a valid method code from table METHOD')
# EX_K
if (!is.numeric(EX_K)) warning('EX_K must be numeric')
if (any(EX_K[EX_K!=-999] < 0) | any(EX_K[EX_K!=-999 & CEC!=-999] > CEC[EX_K!=-999 & CEC!=-999])) warning('EX_K must be expressed in meq/(100g). Some values are unrealistic.')
# EX_K_M
if (!is.wholenumber(EX_K_M)) warning("EX_K_M must be an integer")
if (any(nchar(as.character(EX_K_M[EX_K!=-999]))!=3)) warning("EX_K_M must be a 3 digits integer")
if (any(!EX_K_M %in% c(z,-999))) warning('EX_K_M must be a valid method code from table METHOD')
# EX_CA
if (!is.numeric(EX_CA)) warning('EX_CA must be numeric')
if (any(EX_CA[EX_CA!=-999] < 0) | any(EX_CA[EX_CA!=-999 & CEC!=-999] > CEC[EX_CA!=-999 & CEC!=-999])) warning('EX_CA must be expressed in meq/(100g). Some values are unrealistic.')
# EX_CA_M
if (!is.wholenumber(EX_CA_M)) warning("EX_CA_M must be an integer")
if (any(nchar(as.character(EX_CA_M[EX_CA!=-999]))!=3)) warning("EX_CA_M must be a 3 digits integer")
if (any(!EX_CA_M %in% c(z,-999))) warning('EX_CA_M must be a valid method code from table METHOD')
# BASE_CATIONS
if (!is.numeric(BASE_CATIONS)) warning('BASE_CATIONS must be numeric')
if (any(BASE_CATIONS[BASE_CATIONS!=-999] < 0) | any(BASE_CATIONS[BASE_CATIONS!=-999 & CEC!=-999] > CEC[BASE_CATIONS!=-999 & CEC!=-999])) warning('BASE_CATIONS must be expressed in meq/(100g). Some values are unrealistic.')
# ACIDITY_NA4O
if (!is.numeric(ACIDITY_NA4O)) warning('ACIDITY_NA4O must be numeric')
if (any(ACIDITY_NA4O[ACIDITY_NA4O!=-999] < 0) | any(ACIDITY_NA4O[ACIDITY_NA4O!=-999 & CEC!=-999] > CEC[ACIDITY_NA4O!=-999 & CEC!=-999])) warning('ACIDITY_NA4O must be expressed in meq/(100g). Some values are unrealistic.')
# ACIDITY_NA4O_M
if (!is.wholenumber(ACIDITY_NA4O_M)) warning("ACIDITY_NA4O_M must be an integer")
if (any(nchar(as.character(ACIDITY_NA4O_M[ACIDITY_NA4O!=-999]))!=3)) warning("ACIDITY_NA4O_M must be a 3 digits integer")
if (any(!ACIDITY_NA4O_M %in% c(z,-999))) warning('ACIDITY_NA4O_M must be a valid method code from table METHOD')
# ACIDITY_KCL
if (!is.numeric(ACIDITY_KCL)) warning('ACIDITY_KCL must be numeric')
if (any(ACIDITY_KCL[ACIDITY_KCL!=-999] < 0) | any(ACIDITY_KCL[ACIDITY_KCL!=-999 & CEC!=-999] > CEC[ACIDITY_KCL!=-999 & CEC!=-999])) warning('ACIDITY_KCL must be expressed in meq/(100g). Some values are unrealistic.')
# ACIDITY_KCL_M
if (!is.wholenumber(ACIDITY_KCL_M)) warning("ACIDITY_KCL_M must be an integer")
if (any(nchar(as.character(ACIDITY_KCL_M[ACIDITY_KCL!=-999]))!=3)) warning("ACIDITY_KCL_M must be a 3 digits integer")
if (any(!ACIDITY_KCL_M %in% c(z,-999))) warning('ACIDITY_KCL_M must be a valid method code from table METHOD')

detach(x)
print('... done')
# OUTPUT
x <- x[,1:29]
return(x)

}