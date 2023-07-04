# meth.checks(x)
# Checks table METHOD against EU-HYDI requirements
# 
# Author: M.Weynants
# Date created: 2012/11/29
##################################################################
meth.checks <- function(x){
print('checking METHOD...')
# check size: 4 fields
if (length(x) < 4) stop("Table METHOD has not enough columns")
if (length(x) > 4) warning("Table METHOD has too many columns and will be truncated")
x <- x[,1:4]
attach(x)
# check each field
# CODE_M
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {abs(x-round(x)) < tol}
if (!any(is.wholenumber(CODE_M))) stop('CODE_M must be an intger')
if (any(nchar(as.character(CODE_M))!=3)) warning("CODE_M must be a 3 digits integer")
if (any(duplicated(CODE_M))) warning('CODE_M must be unique')
# METHOD
if (!is.character(METHOD)) warning('METHOD must be entered as text')
# METH_REF
if (!is.character(METH_REF)) warning('METH_REF must be entered as text')
# METH_PAR
if (!is.character(METH_PAR)) warning('METH_PAR must be entered as text')
if (any(!METH_PAR %in% c('POR_M','BD_M','COARSE_M','OC_M','CACO3_M','PH_H2O_M','PH_KCL_M','EC_M','SALT_M','CEC_M','EX_NA_M','EX_MG_M','EX_K_M','EX_CA_M','ACIDITY_NA4O_M','ACIDITY_KCL_M','P_M','THETA_M','TH_INV_MOD','COND_M','K_INV_MOD'))) warning('METH_PAR must refer to an existing parameter')
detach(x)
print('... done')
# OUTPUT

return(x)
}
