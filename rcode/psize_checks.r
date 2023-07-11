# psize.checks(x,y,z)
# checks conformity of table PSIZE against EU-HYDI requirements
# y is a vector of SAMPLE_ID in table BASIC
# z is a vector of CODE_M from table METHOD
#
# Author: M.Weynants
# Date created: 2012/11/28
##################################################################
psize.checks <- function(x,y,z){
print('checking PSIZE...')
# check size: 5 fields
if (length(x) < 5) stop("Table PSIZE has not enough columns")
if (length(x) > 5) warning("Table PSIZE has too many columns and will be truncated")
# check each field
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {abs(x-round(x)) < tol}
# PROFILE_ID
if (any(! is.wholenumber(x[[1]]))) warning("PROFILE_ID must be an integer")
# SAMPLE_ID
if (! any(x[[2]] %in% y)) warning("SAMPLE_ID must be a valid profile identifier from table BASIC")
if (any(!is.wholenumber(x[[2]]))) warning("SAMPLE_ID must be an integer")
if (any(x[[1]] != floor(x[[2]]/100))) warning("The first 8 digits of SAMPLE_ID must be the same as PROFILE_ID")
attach(x)
# P_SIZE
if (!is.numeric(P_SIZE)) warning('P_SIZE must be numeric')
if (max(P_SIZE) > 2000) warning('Table PSIZE must hold particle size distributions of fine earth fraction (< 2 mm)')
if (min(P_SIZE) < 0.2) warning('P_SIZE must be expressed in micrometers')
if (any(duplicated(paste(SAMPLE_ID,P_SIZE)))) warning("There are duplicated samples in PSIZE")
# P_PERCENT
if (!is.numeric(P_PERCENT)) warning('P_PERCENT must be numeric')
if (any(P_PERCENT[P_PERCENT!=-999] < 0 | P_PERCENT[P_PERCENT!=-999] > 100)) warning('P_PERCENT must be expressed in mass percentage')
if (any(sapply(unique(SAMPLE_ID), function(a){sum(P_PERCENT[SAMPLE_ID==a]) <99 | sum(P_PERCENT[SAMPLE_ID==a])>101}))) warning("Fine earth fractions must sum up to 100 %")
# P_M
if (any(!is.wholenumber(P_M))) warning('P_M must be integer')
if (any(nchar(as.character(P_M[P_PERCENT!=-999]))!=3)) warning("P_M must be a 3 digits integer")
if (any(!P_M %in% c(z,-999))) warning('P_M must be a valid method code from table METHOD')
detach(x)
print('... done')
# OUTPUT
ID <- x$ID
x <- x[,1:5]
if (!is.null(ID)) {x<- cbind(x,ID)} else {x$ID <- "ND"}
return(x)
}
