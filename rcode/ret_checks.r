# ret.checks(x,y)
# checks conformity of table RET against EU-HYDI requirements
# y is a vector of SAMPLE_ID in table BASIC
# z is a vector of CODE_M from table METHOD
#
# Author: M.Weynants
# Date created: 2012/11/28
##################################################################
ret.checks <- function(x,y,z){
print('checking RET...')
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {all(abs(x-round(x)) < tol)}

# check size: 14 fields
if (length(x) < 14) stop("Table RET has not enough columns")
if (length(x) > 14) warning("Table RET has too many columns and will be truncated")
# check each field
# PROFILE_ID
if (! is.wholenumber(x[[1]])) warning("PROFILE_ID must be an integer")
# SAMPLE_ID
if (any(!x[[2]] %in% y)) warning("SAMPLE_ID must be a valid profile identifier from table BASIC")
if (any(!is.wholenumber(x[[2]]))) warning("SAMPLE_ID must be an integer")
if (any(x[[1]] != floor(x[[2]]/100))) warning("The first 8 digits of SAMPLE_ID must be the same as PROFILE_ID")
attach(x)
# HEAD
if (! is.numeric(HEAD)) warning('HEAD must be numeric')
if (any(HEAD < 0)) warning('HEAD must be given in absolute value')
if (any(duplicated(paste(SAMPLE_ID,HEAD)))) warning("There are duplicated measurements in RET")
# THETA
if (! is.numeric(THETA)) warning('THETA must be numeric')
if (any((THETA<0 | THETA >1)& THETA !=-999)) warning('THETA must be expressed in volume of water by unit volume of soil')
# THETA_M
if (!is.wholenumber(THETA_M)) warning('THETA_M must be integer')
if (any(nchar(as.character(THETA_M))!=3)) warning("THETA_M must be a 3 digits integer")
if (any(!THETA_M %in% c(z,-999))) warning('THETA_M must be a valid method code from table METHOD')
# TH_INV_P1 to TH_INV_P8
if (!is.numeric(unlist(x[,6:13]))) warning('TH_INV parameters must be numeric')
# TH_INV_MOD
if (!is.wholenumber(TH_INV_MOD)) warning('TH_INV_MOD must be integer')
if (any(nchar(as.character(TH_INV_MOD[TH_INV_P1!=-999]))!=3)) warning("TH_INV_MOD must be a 3 digits integer")
if (any(!TH_INV_MOD %in% c(z,-999))) warning('TH_INV_MOD must be a valid method code from table METHOD')
detach(x)
print('... done')
# OUTPUT
ID <- x$ID
x <- x[,1:14]
if (!is.null(ID)) {x <- cbind(x,ID)} else {x <- cbind(x, ID=rep("ND",nrow(x)))}
return(x)
}