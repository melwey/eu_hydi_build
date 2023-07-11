# basic.checks(x,y)
# checks table BASIC aginst EU-HYDI guidelines
# x: BASIC
# y: GENERAL$PROFILE_ID
# z: METHOD$CODE_M
# Author: M.Weynants
# Date created: 2012/11/27
######################################################################
basic.checks <- function(x,y,z) {
print('checking BASIC ...')
# check size: 23 fields
if (length(x) < 23) stop("Table BASIC has not enough columns")
if (length(x) > 23) warning("Table BASIC has too many columns and will be truncated")
# checks names
if (any(names(x[,1:23]) != c("PROFILE_ID","SAMPLE_ID","SAMPLE_POS","SAMPLE_DEP_TOP","SAMPLE_DEP_BOT","HOR1_NAME","HOR1_TOP","HOR1_BOT","HOR2_NAME","HOR2_TOP","HOR2_BOT","HOR3_NAME","HOR3_TOP","HOR3_BOT","STRUCTURE1","STR_COMB","STRUCTURE2","POR","POR_M","BD","BD_M","COARSE","COARSE_M"))){warning("Column names of BASIC are wrong")}

# check each field
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {all(abs(x-round(x)) < tol)}
# PROFILE_ID
if (! is.wholenumber(x[[1]])) warning("PROFILE_ID must be an integer")
if (! any(x[[1]] %in% y)) warning("PROFILE_ID must be a valid profile identifier from table GENERAL")
# SAMPLE_ID
if (any(!is.wholenumber(x[[2]]))) warning("SAMPLE_ID must be an integer")
if (any(duplicated(x[[2]]))) warning("SAMPLE_ID must be a unique sample identifier!!!!")
if (any(x[[1]] != floor(x[[2]]/100)))
# # 7 or 8 first digits of sample_id == profile_id
# prfl_char <- as.character(x[[1]])
# ind7 <-nchar(prfl_char)==7
# prfl_char[ind7] <- paste('0',prfl_char[ind7],sep='')
# smpl_char <- as.character(x[[2]])
# smpl_char[ind7] <- paste('0',smpl_char[ind7],sep='')
# if (any(substr(smpl_char,start=1,stop=8) != prfl_char)) 
{warning("The first 8 digits of SAMPLE_ID must be the same as PROFILE_ID")}
# SAMPLE_POS
if (!is.wholenumber(x[[3]])) warning("SAMPLE_POS must be an integer")
if (any(sapply(unique(x[[1]]),function(a){any(duplicated(x[x[[1]]==a & x[[3]] !=-999,3]))}))) warning("SAMPLE_POS must be unique in a profile")
# SAMPLE_DEP_TOP and SAMPLE_DEP_BOT
if (any(!(is.wholenumber(x[[4]]) & is.wholenumber(x[[5]])))) warning("SAMPLE_DEP_TOP and SAMPLE_DEP_BOT must be integers")
if (any(x[[4]]==-999) | any(x[[5]]==-999)) warning("SAMPLE_DEP_TOP and SAMPLE_DEP_BOT are mandatory fields!")
if (any(x[[4]] >= x[[5]] & x[[4]]!=-999 & x[[5]]!=-999)) warning("SAMPLE_DEP_TOP must be strictly smaller than SAMPLE_DEP_BOT")
# HOR1_NAME
if (!is.character(x[[6]])) warning("HOR1_NAME must be entered as text")
if (any(nchar(x[!grepl("ND",x[[6]]),6])!=7)) warning("HOR1_NAME must respect the coding convention presented in the guidelines for contributors")
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=1,stop=1) %in% c(' ',as.character(1:9),'b'))) warning ("HOR1_NAME's first character must be a numeral, a 'b' or a space")
# master horizon
mh <- c('H', 'O', 'A', 'E', 'B', 'C', 'R', 'I', 'L', 'W', ' ','')
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=2,stop=2) %in% mh)) warning ("HOR1_NAME's second character must be a valid master horizon")
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=3,stop=3) %in% c('/',' '))) warning ("HOR1_NAME's third character must be a space or a slash character")
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=4,stop=4) %in% mh)) warning ("HOR1_NAME's fourth character must be a valid master horizon")
# subhorizon designations
sd <- c(letters,'@',' ')
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=5,stop=5) %in% sd)) warning ("HOR1_NAME's fifth character must be a valid horizon designation")
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=6,stop=6) %in% sd)) warning ("HOR1_NAME's sixth character must be a valid horizon designation")
# vertical subdivision
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=7,stop=7) %in% c(1:9,' '))) warning ("HOR1_NAME's seventh character must be a valid horizon's vertical subdivision designation")
# HOR1_TOP and HOR1_BOT
if (any(!(is.wholenumber(x[[7]]) & is.wholenumber(x[[8]])))) warning("HOR1_TOP and HOR1_BOT must be integers")
if (any(x[[7]] > x[[8]] & x[[8]] !=-999)) warning("HOR1_TOP must be strictly smaller than HOR1_BOT")
# HOR2_NAME
if (!is.character(x[[9]])) warning("HOR2_NAME must be entered as text")
if (any(nchar(x[!grepl("ND",x[[9]]),9])!=7)) warning("HOR2_NAME must respect the coding convention presented in the guidelines for contributors")
# HOR2_TOP and HOR2_BOT
if (any(!(is.wholenumber(x[[10]]) & is.wholenumber(x[[11]])))) warning("HOR2_TOP and HOR2_BOT must be integers")
if (any(x[[10]] > x[[11]] & x[[11]] !=-999)) warning("HOR2_TOP must be strictly smaller than HOR2_BOT")
# HOR3_NAME
if (!is.character(x[[12]])) warning("HOR3_NAME must be entered as text")
if (any(nchar(x[!grepl("ND",x[[12]]),12])!=7)) warning("HOR3_NAME must respect the coding convention presented in the guidelines for contributors")
# HOR3_TOP and HOR3_BOT
if (any(!(is.wholenumber(x[[13]]) & is.wholenumber(x[[14]])))) warning("HOR3_TOP and HOR3_BOT must be integers")
if (any(x[[13]] > x[[14]] & x[[14]] !=-999)) warning("HOR3_TOP must be strictly smaller than HOR3_BOT")
# STRUCTURE1
if (!is.character(x[[15]])) warning("STRUCTURE1 must be entered as text")
if (any(nchar(x[!grepl("ND",x[[15]]),15])!=6)) warning("STRUCTURE1 must respect the coding convention presented in the guidelines for contributors")
gd <- c('WE','MO','ST','WM','MS','ND','  ')
ty <- c('RS','SS','SG','MA','PM','BL','AB','AP','AS','AW','SA','SB','SN','PR','PS','WE','CO','GR','WC','PL','CL','CR','LU','ND','  ')
sz <- c('VF','FI','ME','CO','VC','EC','FF','VM','FM','FC','MC','MV','CV','ND','  ')
if (any(!substr(x[[15]],start=1,stop=2) %in% gd) | any(!substr(x[!grepl("ND",x[[15]]),15],start=3,stop=4) %in% sz) | any(!substr(x[!grepl("ND",x[[15]]),15],start=5,stop=6) %in% ty)) warning("STRUCTURE1 is unvalid")
# STR_COMB
if (any(!x[[16]] %in% c("+", "-", "/", "0", "ND"))) warning("STR_COMB must be a valid code for structure combination")
# STRUCTURE2
if (!is.character(x[[17]])) warning("STRUCTURE2 must be entered as text")
if (any(nchar(x[!grepl("ND",x[[17]]),17])!=6)) warning("STRUCTURE2 must respect the coding convention presented in the guidelines for contributors")
if (any(!substr(x[[17]],start=1,stop=2) %in% gd) | any(!substr(x[!grepl("ND",x[[17]]),17],start=3,stop=4) %in% sz) | any(!substr(x[!grepl("ND",x[[17]]),17],start=5,stop=6) %in% ty)) warning("STRUCTURE2 is unvalid")
# POR
if (!is.numeric(x[[18]])) warning("POR must be numeric")
if (any(x[x[[18]]!=-999,18]< 0 | x[x[[18]]!=-999,18]>100)) warning("POR must be expressed as the volume percentage of soil occupied by pores (0<POR<100)")
# POR_M
if (!is.wholenumber(x[[19]]) | any(nchar(x[x[[18]]!=-999,19])!=3)) warning("POR_M must be a 3 digits integer")
if (any(!x[[19]] %in% c(z,-999))) warning('POR_M must be a valid method code from table METHOD')
# BD
if (!is.numeric(x[[20]])) warning("BD must be numeric")
if (any(x[[20]] == -999)) warning("BD is a mandatory field")
# density of pure organic matter, around 0.1
# density of pure mineral, around 2.5
if (any((x[,20]< 0.1 | x[,20]>2.5) & x[,20] != -999)) warning("BD must be expressed as grams per cubic centimeter")
# BD_M
if (!is.wholenumber(x[[21]])  | any(nchar(x[x[[20]]!=-999,21])!=3)) warning("BD_M must be a 3 digits integer")
if (any(!x[[21]] %in% c(z,-999))) warning('BD_M must be a valid method code from table METHOD')
# COARSE
if (!is.numeric(x[[22]])) warning("COARSE must be numeric")
if (any(x[[22]] == -999)) warning("COARSE is a mandatory field")
if (any(x[x[[22]]!=-999,22]< 0 | x[x[[22]]!=-999,22]>100)) warning("COARSE must be expressed as mass percentage")
# COARSE_M
if (!is.wholenumber(x[[23]])  | any(nchar(x[x[[22]]!=-999,21])!=3)) warning("COARSE_M must be a 3 digits integer")
if (any(!x[[23]] %in% c(z,-999))) warning('COARSE_M must be a valid method code from table METHOD')
print('... done')
# value
x <- x[,1:23]
return(x)
}
