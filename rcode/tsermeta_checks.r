# tsermeta.checks(x)
# checks table TSERMETA against HYDI requirements
# y is general1$PROFILE_ID
#
# Author: M.Weynants
# Date created: 2012/11/29
###############################################################
tsermeta.checks <- function(x,y){
print('checking TSERMETA...')
# check size: 13 fields
if (length(x) < 13) stop("Table TSERMETA has not enough columns")
if (length(x) > 13) warning("Table TSERMETA has too many columns and will be truncated")
x <- x[,1:13]
attach(x)
# T_SER_ID
if (any(duplicated(T_SER_ID))) stop("T_SER_ID must be a unique identification key")
# REL_PROFILE_ID
if (any(! T_SER_ID %in% y)) stop("REL_PROFILE must refer to an existing profile in table GENERAL")
# check each field
# OUTPUT
detach(x)
print('... done')
x
}