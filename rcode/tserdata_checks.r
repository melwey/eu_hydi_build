# tserdata.checks(x,y)
# checks table TSERDATA against HYDI requirements
# y is field T_SER_ID from table TSERMETA
#
# Author: M.Weynants
# Date created: 2012/11/29
###############################################################
tserdata.checks <- function(x,y){
print('checking TSERDATA...')
# check size: 12 fields
if (length(x) < 12) stop("Table TSERDATA has not enough columns")
if (length(x) > 12) warning("Table TSERDATA has too many columns and will be truncated")
x <- x[,1:12]
attach(x)
# check each field
# T_SER_ID
if (any(! T_SER_ID %in% y)) warning("metadata can't be found")
# TIME
if (! is.character(TIME)) warning("TIME must be entered as text")
# OUTPUT
detach(x)
print('... done')
x
}
