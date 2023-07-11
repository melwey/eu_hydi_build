# Add data to HYDI
# 
# Author: M.Weynants
# Date: 2013/03/20
#####################################################################
source("general_checks.r")
source("basic_checks.r")
source("chemical_checks.r")
source("psize_checks.r")
source("ret_checks.r")
source("cond_checks.r")
source("meth_checks.r")

GENERAL <- as.data.frame(matrix(nrow=1,ncol=65,dimnames=list(NULL,c("PROFILE_ID","LOC_COOR_X","LOC_COOR_Y", "LOC_COOR_SYST","X_WGS84","Y_WGS84","ELEV","ISO_COUNTRY","RC_L1","RC_L2","LC_L1","LC_L2","LC_L3","LU_L1","LU_L2","SITE_LANDFORM","SITE_SLOP_POS","SITE_SLOP_FORM","SITE_SLOP_GRAD","SRF_ROCK_COV","SRF_ROCK_DIS","SRF_COAR_COV","SRF_COAR_SIZ","SRF_ERO_CAT","SRF_ERO_COV","SRF_ERO_DEG","SRF_ERO_ACT","SRF_SEAL_THIC","SRF_SEAL_CON","SRF_CRAC_WID","SRF_CRAC_DEP","SRF_CRAC_DIS","SRF_SAL_COV","SRF_SAL_THIC","PAR_MAT","AGE","WRB2006_RSG","WRB2006_PQ1","WRB2006_PQ2","WRB2006_PQ3","WRB2006_SQ1","WRB2006_SQ2","WRB2006_SQ3","WRB1998_RSG","WRB1998_ADJSPE1","WRB1998_ADJSPE2","WRB1998_ADJSPE3","WRB1998_ADJSPE4","WRB1998_ADJSPE5","WRB1998_ADJSPE6","NAT_CLAS","NAT_CLAS_REF","YEAR","MONTH","DAY","SURVEYOR_P","PUBL_REF","CONTACT_P","CONTACT_A","EMAIL","REL_ID","REL_T_SER","COMMENTS1","COMMENTS2","COMMENTS3"))))

BASIC <- as.data.frame(matrix(nrow=1,ncol=23,dimnames=list(NULL,c("PROFILE_ID","SAMPLE_ID","SAMPLE_POS","SAMPLE_DEP_TOP","SAMPLE_DEP_BOT","HOR1_NAME","HOR1_TOP","HOR1_BOT","HOR2_NAME","HOR2_TOP","HOR2_BOT","HOR3_NAME","HOR3_TOP","HOR3_BOT","STRUCTURE1","STR_COMB","STRUCTURE2","POR","POR_M","BD","BD_M","COARSE","COARSE_M"))))
# set field classes
BASIC[,c(1:5,7:8,10:11,13:14,19,21,23)] <- as.integer(BASIC[,c(1:5,7:8,10:11,13:14,19,21,23)])
BASIC[,c(18,20,22)] <- as.numeric(BASIC[,c(18,20,22)])
BASIC[,c(6,9,12,15:17)] <- as.character(BASIC[,c(6,9,12,15:17)])
# CHEMICAL
CHEMICAL <- as.data.frame(matrix(nrow=1,ncol=29,dimnames=list(NULL,c("PROFILE_ID","SAMPLE_ID","OC","OC_M","CACO3","CACO3_M","PH_H2O","PH_H2O_M","PH_KCL","PH_KCL_M","EC","EC_M","SALT","SALT_M","CEC","CEC_M","EX_NA","EX_NA_M","EX_MG","EX_MG_M","EX_K","EX_K_M","EX_CA","EX_CA_M","BASE_CATIONS","ACIDITY_NA4O","ACIDITY_NA4O_M","ACIDITY_KCL","ACIDITY_KCL_M"))))
# set field classes
CHEMICAL[,c(1,2,4,6,8,10,12,14,16,18,20,22,24,27,29)] <- as.integer(CHEMICAL[,c(1,2,4,6,8,10,12,14,16,18,20,22,24,27,29)])
CHEMICAL[,c(3,5,7,9,11,13,15,17,19,21,23,25,26,28)] <- as.numeric(CHEMICAL[,c(3,5,7,9,11,13,15,17,19,21,23,25,26,28)])


load("../output/HYDI_SOURCE_nd.Rdata")

dirs <- c("Cranfield","Morari")
# Add Cranfield data
for (i in 1:2){
# GENERAL
general <- read.csv(paste('../data/',dirs[i],'/GENERAL.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip = TRUE)
# remove empty lines
general<- general[!is.na(general[[1]]),]
# METHOD
meth <- read.csv(paste('../data/',dirs[i],'/METHOD.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
meth <- meth[!is.na(meth[[1]]),]
meth$METH_PAR <- toupper(meth$METH_PAR)
# BASIC
basic <- read.csv(paste('../data/',dirs[i],'/BASIC.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
basic <- basic[!is.na(basic[[1]]),]
# CHEMICAL
chemical <- read.csv(paste('../data/',dirs[i],'/CHEMICAL.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
chemical <- chemical[!is.na(chemical[[1]]),]
names(chemical) <- toupper(names(chemical))
# PSIZE
psize <- read.csv(paste('../data/',dirs[i],'/PSIZE.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
psize <- psize[!is.na(psize[[1]]),]
# RET
ret <- read.csv(paste('../data/',dirs[i],'/RET.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
ret <- ret[!is.na(ret[[1]]),]
# COND
cond <- read.csv(paste('../data/',dirs[i],'/COND.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
cond <- cond[!is.na(cond[[1]]),]

# remove all unused codes from meth

if (i == 1){source("cleanCranfield.r")}
if (i == 2){source("cleanMorari.r")}

print('checking tables')
general1 <- general.checks(general)
meth1 <- meth.checks(meth)
basic1 <- basic.checks(basic,general1[[1]],meth1[[1]])
chemical1 <- chemical.checks(chemical,basic1[[2]],meth1[[1]])
psize1 <- psize.checks(psize,basic1[[2]],meth1[[1]])
ret1 <- ret.checks(ret,basic1[[2]],meth1[[1]])
cond1 <- cond.checks(cond,basic1[[2]],meth1[[1]])

# add source
general1$SOURCE <- dirs[i]
meth1$SOURCE <- dirs[i]
basic1$SOURCE <- dirs[i]
chemical1$SOURCE <- dirs[i]
psize1$SOURCE <- dirs[i]
ret1$SOURCE <- dirs[i]


# bundle with existing hydi
hydi$GENERAL <- rbind(hydi$GENERAL,general1)
hydi$BASIC <- rbind(hydi$BASIC,basic1)
hydi$CHEMICAL <- rbind(hydi$CHEMICAL,chemical1)
hydi$PSIZE <- rbind(hydi$PSIZE,psize1)
hydi$RET <- rbind(hydi$RET,ret1)
hydi$METHOD <- rbind(hydi$METHOD,meth1)

# save
save("hydi",file="../output/HYDI_SOURCE_nd_add.Rdata")

}