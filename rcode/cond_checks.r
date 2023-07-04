# cond.checks(x,y)
# checks conformity of table COND against EU-HYDI requirements
# y is a vector of SAMPLE_ID in table BASIC
# z is a vector of CODE_M from table METHOD
#
# Author: M.Weynants
# Date created: 2012/11/29
##################################################################
cond.checks <- function(x,y,z){
print('checking COND...')
# check size: 15 or 16 fields
if (length(x) < 15) stop("Table COND has not enough columns")
if (length(x) > 16) warning("Table COND has too many columns and will be truncated")
# check each field
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {all(abs(x-round(x)) < tol)}
# PROFILE_ID
if (! is.wholenumber(x[[1]])) warning("PROFILE_ID must be an integer")
# SAMPLE_ID
if (! all(x[[2]] %in% y)) warning("SAMPLE_ID must be a valid profile identifier from table BASIC")
if (any(!is.wholenumber(x[[2]]))) warning("SAMPLE_ID must be an integer")
if (any(x[[1]] != floor(x[[2]]/100))) warning("The first 8 digits of SAMPLE_ID must be the same as PROFILE_ID")
# remove empty lines
x <- x[!(x$VALUE==-999 & x$COND == -999 & x$K_INV_P1==-999 & x$K_INV_MOD==-999),]
attach(x)
# IND_VALUE
if (any(!IND_VALUE %in% c(0,1))) warning("IND_VALUE must be 0 or 1")
x$IND_VALUE <- as.logical(x$IND_VALUE)
# VALUE
if (!is.numeric(VALUE)) warning("VALUE must be numeric")
if (any(VALUE[IND_VALUE] < 0 | VALUE[IND_VALUE] > 20000) | any(VALUE[!IND_VALUE] < 0 | VALUE[!IND_VALUE]>1)) warning('Some VALUE are unrealistic')
if (any(duplicated(paste(SAMPLE_ID,VALUE)))) warning("There are duplicated measurements in COND")
# COND
if (!is.numeric(COND)) warning("COND must be numeric")
if (any(COND<0)) warning("COND must be positive")
# do I leave the negative values?
# COND_M
if (!is.wholenumber(COND_M)) warning('COND_M must be integer')
if (any(nchar(as.character(COND_M))!=3)) warning("COND_M must be a 3 digits integer")
if (any(!COND_M %in% c(z,-999))) warning('COND_M must be a valid method code from table METHOD')
# K_INV_P1 to K_INV_P8
if (!is.numeric(unlist(x[,7:14]))) warning('K_INV parameters must be numeric')
# K_INV_MOD
if (!is.wholenumber(K_INV_MOD)) warning('K_INV_MOD must be integer')
if (any(nchar(as.character(K_INV_MOD[K_INV_P1!=-999]))!=3)) warning("K_INV_MOD must be a 3 digits integer")
if (any(!K_INV_MOD %in% c(z,-999))) warning('K_INV_MOD must be a valid method code from table METHOD')
detach(x)
print('... done')
# OUTPUT
ID <- x$ID
if (names(x)[15] == 'K_INV_MOD') {
x <- cbind(x[,1:14],K_INV_P9=rep(-999,nrow(x)),K_INV_MOD=x[,15])
} else { x <- x[,1:16]}
if (!is.null(ID)) {x <- cbind(x,ID)} else {x <- cbind(x, ID=rep("ND",nrow(x)))}
return(x)
}
