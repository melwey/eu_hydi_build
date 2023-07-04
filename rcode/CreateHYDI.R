# Creation of EU-HYDI database structure according to guidelines EUHYDI_v1.1.pdf and example file EUHYDI_v1.1_example.xls
#
# Author: M. Weynants
# Date created: 2012/11/09
# Last update: 2012/11/22
######################################################################
# setwd('E:/weyname/Documents/Documents/EU_HYDI/ContributedData/Rcode')
# load packages
# require(RODBC)
# load functions
source('general_checks.r')
source('basic_checks.r')
source('chemical_checks.r')
source('psize_checks.r')
source('ret_checks.r')
source('cond_checks.r')
source('meth_checks.r')
source('tsermeta_checks.r')
source('tserdata_checks.r')

# Print warnings as they appear

# Create the tables with the right structure
# GENERAL
GENERAL <- as.data.frame(matrix(nrow=1,ncol=65,dimnames=list(NULL,c("PROFILE_ID","LOC_COOR_X","LOC_COOR_Y", "LOC_COOR_SYST","X_WGS84","Y_WGS84","ELEV","ISO_COUNTRY","RC_L1","RC_L2","LC_L1","LC_L2","LC_L3","LU_L1","LU_L2","SITE_LANDFORM","SITE_SLOP_POS","SITE_SLOP_FORM","SITE_SLOP_GRAD","SRF_ROCK_COV","SRF_ROCK_DIS","SRF_COAR_COV","SRF_COAR_SIZ","SRF_ERO_CAT","SRF_ERO_COV","SRF_ERO_DEG","SRF_ERO_ACT","SRF_SEAL_THIC","SRF_SEAL_CON","SRF_CRAC_WID","SRF_CRAC_DEP","SRF_CRAC_DIS","SRF_SAL_COV","SRF_SAL_THIC","PAR_MAT","AGE","WRB2006_RSG","WRB2006_PQ1","WRB2006_PQ2","WRB2006_PQ3","WRB2006_SQ1","WRB2006_SQ2","WRB2006_SQ3","WRB1998_RSG","WRB1998_ADJSPE1","WRB1998_ADJSPE2","WRB1998_ADJSPE3","WRB1998_ADJSPE4","WRB1998_ADJSPE5","WRB1998_ADJSPE6","NAT_CLAS","NAT_CLAS_REF","YEAR","MONTH","DAY","SURVEYOR_P","PUBL_REF","CONTACT_P","CONTACT_A","EMAIL","REL_ID","REL_T_SER","COMMENTS1","COMMENTS2","COMMENTS3"))))
# set field classes
GENERAL[,c(1,7)]<-as.integer(GENERAL[,c(1,7)])
GENERAL[,c(2,3,5,6)]<-as.numeric(GENERAL[,c(2,3,5,6)])
GENERAL[,c(4)]<-as.character(GENERAL[,c(4)])
# BASIC
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

# PSIZE
# RET
# COND
# METHOD
# TSERMETA
# TSERDATA


# loop on directories of contributors
contr <- c('Anaya','Goncalves','Iovino','Katterer','Mako','Patyka','Schindler','Shein','Javaux','Daroussin','Romano','Kvaerno','Lamorski','Lilly','Houskova','Strauss','Matula','Cornelis') # etc.
dirs <- contr
DB <- vector("list",length(dirs)); names(DB)<-dirs
for (i in 1:18){
print('-----------------')
print(dirs[i])
print('-----------------')
# warnings off
options(warn=1)

if (exists('general')) rm(general,basic,chemical,psize,ret,cond,meth,tsermeta,tserdata)
if (exists('general1')) rm(general1,basic1,chemical1,psize1,ret1,cond1,meth1,tsermeta1,tserdata1)
# read csv files
# GENERAL
# ---- 2023/04/05 fix: files were imported without specifying encoding and special characters were scrambled
if (file.exists(paste('../',dirs[i],'/general_utf8.csv',sep=''))) {
  general <- read.csv(paste('../',dirs[i],'/general_utf8.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip = TRUE)
} else {
  general <- read.csv(paste('../',dirs[i],'/GENERAL.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip = TRUE)
}
# ------
# remove empty lines
general<- general[!is.na(general[[1]]),]
# METHOD
# 2023/04/18 exported in utf8 because impossible to import Straus as is invalid multibyte string at '<d6>NOR<4d> L 1068'
if (grepl("Strauss",dirs[i])) {
  path <- paste('../',dirs[i],'/METHOD_utf8.csv',sep='')
} else {path <- paste('../',dirs[i],'/METHOD.csv',sep='')}
meth <- read.csv(path,header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
meth <- meth[!is.na(meth[[1]]),]
# BASIC
basic <- read.csv(paste('../',dirs[i],'/BASIC.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
basic <- basic[!is.na(basic[[1]]),]
# CHEMICAL
chemical <- read.csv(paste('../',dirs[i],'/CHEMICAL.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
chemical <- chemical[!is.na(chemical[[1]]),]
names(chemical) <- toupper(names(chemical))
# PSIZE
psize <- read.csv(paste('../',dirs[i],'/PSIZE.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
psize <- psize[!is.na(psize[[1]]),]
# RET
ret <- read.csv(paste('../',dirs[i],'/RET.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
ret <- ret[!is.na(ret[[1]]),]
# COND
cond <- read.csv(paste('../',dirs[i],'/COND.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
cond <- cond[!is.na(cond[[1]]),]

# clean specific contributions to match guidelines
if (grepl("Anaya",dirs[i])) source("cleanAnaya.r")
if (grepl("Iovino",dirs[i])) source("cleanIovino.r")
if (grepl("Katterer",dirs[i])) source('cleanKatterer.r')
if (grepl("Mako", dirs[i])) source("cleanMako.r")
if (grepl("Patyka", dirs[i])) source("cleanPatyka.r")
if (grepl("Schindler" ,dirs[i])) source('cleanSchindler.r')
if (grepl("Shein",dirs[i])) source('cleanShein.r')
if (grepl("Javaux",dirs[i])) source('cleanJavaux.r')
if (grepl("Daroussin",dirs[i])) source('cleanDaroussin.r')
if (grepl("Romano",dirs[i])) source('cleanRomano.r')
if (grepl("Kvaerno",dirs[i])) source('cleanKvaerno.r')
if (grepl("Lamorski",dirs[i])) source('cleanLamorski.r')
if (grepl("Lilly",dirs[i])) source('cleanLilly.r')
if (grepl("Houskova",dirs[i])) source('cleanHouskova.r')
if (grepl("Strauss",dirs[i])) source('cleanStrauss.r')
if (grepl("Matula",dirs[i])) source('cleanMatula.r')
if (grepl("Cornelis",dirs[i])) source('cleanCornelis.r')

# do the checks
print('checking tables')
general1 <- general.checks(general)
meth1 <- meth.checks(meth)
basic1 <- basic.checks(basic,general1[[1]],meth1[[1]])
chemical1 <- chemical.checks(chemical,basic1[[2]],meth1[[1]])
psize1 <- psize.checks(psize,basic1[[2]],meth1[[1]])
ret1 <- ret.checks(ret,basic1[[2]],meth1[[1]])
cond1 <- cond.checks(cond,basic1[[2]],meth1[[1]])

if (!(all(general1$REL_T_SER == -999) | all(general1$REL_T_SER == 'ND'))){
# TSERMETA
tsermeta <- read.csv(paste('../',dirs[i],'/TSERMETA.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
tsermeta <- tsermeta[!is.na(tsermeta[[1]]),]
tsermeta1 <- tsermeta.checks(tsermeta)
# TSERDATA
tserdata <- read.csv(paste('../',dirs[i],'/TSERDATA.csv',sep=''),header=TRUE,as.is=TRUE,blank.lines.skip=TRUE)
tserdata <- tserdata[!is.na(tserdata[[1]]),]
tserdata1 <- tserdata.checks(meth)
} else {tsermeta1<-NULL;tserdata1<-NULL}

# warnings on
options(warn=1)

if (grepl('Anaya',dirs[i])) source('anaya_corr.r')
if (grepl('Goncalves',dirs[i])) source('goncalves_corr.r')
if (grepl('Iovino',dirs[i])) source('iovino_corr.r')
if (grepl('Katterer',dirs[i])) source('katterer_corr.r')
if (grepl('Mako',dirs[i])) source('mako_corr.r')
if (grepl('Patyka',dirs[i])) source('patyka_corr.r')
if (grepl('Schindler',dirs[i])) source('schindler_corr.r')
if (grepl('Shein',dirs[i])) source('shein_corr.r')
if (grepl('Javaux',dirs[i])) source('javaux_corr.r')
if (grepl("Daroussin",dirs[i])) source('daroussin_corr.r')
if (grepl("Romano",dirs[i])) source('romano_corr.r')
if (grepl("Kvaerno",dirs[i])) source('kvaerno_corr.r')
if (grepl("Lamorski",dirs[i])) source('lamorski_corr.r')
if (grepl("Lilly",dirs[i])) source('lilly_corr.r')
if (grepl("Houskova",dirs[i])) source('houskova_corr.r')
if (grepl("Strauss",dirs[i])) source('strauss_corr.r')
if (grepl("Matula",dirs[i])) source('matula_corr.r')
if (grepl("Cornelis",dirs[i])) source('cornelis_corr.r')

# names(general): surveyor_p and not surveyer_p:
if (any(names(general1) != names(GENERAL))){names(general1) <- names(GENERAL); print(paste(dirs[i],": names(general) changed",sep=''))}

DB[[i]] <- list(general1,basic1,chemical1,psize1,ret1,cond1,meth1,tsermeta1,tserdata1)
names(DB[[i]]) <- c('general','basic','chemical','psize','ret','cond','meth','tsermeta','tserdata')

}

# for (i in 1:18){
# print("---------")
# print(dirs[i])
# print(table(nchar(DB[[i]]$basic$HOR1_NAME)))
# }

# add greek
# load("E:/weyname/Documents/Documents/MyWATER/Nestos-GR/Rcode/GRhydi.Rdata")
load("../Bilas/Nestos-GR/Rcode/GRhydi.Rdata")
DB[[19]] <- GRhydi
names(DB)[19] <- "Bilas"


save('DB', file='HYDI_single.Rdata')


# import hypres data
#source('hypres_hydi.R')
load('hypres_hydi.RData') #hypres_hydi
# checks
gen <- general.checks(hypres_hydi$general)
# Some EMAIL are missing
meth <- meth.checks(hypres_hydi$meth)
basic <- basic.checks(hypres_hydi$basic,hypres_hydi$general[[1]],hypres_hydi$meth[[1]])
# sample depths are missing; some BD are missing; coarse are missing
chem <- chemical.checks(hypres_hydi$chemical,hypres_hydi$basic[[2]],hypres_hydi$meth[[1]])
# some OC are missing
psize <- psize.checks(hypres_hydi$psize,hypres_hydi$basic[[2]],hypres_hydi$meth[[1]])
# CAUTION: replicates
ret <- ret.checks(hypres_hydi$ret,hypres_hydi$basic[[2]],hypres_hydi$meth[[1]])
# some negative heads
cond <- cond.checks(hypres_hydi$cond,hypres_hydi$basic[[2]],hypres_hydi$meth[[1]])

# data from Wosten, Hennings, Schindler and Slovakia
# select:
hypres.contr <- list(Wosten_HYPRES=hypres_hydi$general$PROFILE_ID[(grepl("Wosten",hypres_hydi$general$CONTACT_P) | grepl("Booltink",hypres_hydi$general$CONTACT_P) | grepl("Stolte",hypres_hydi$general$CONTACT_P) | grepl("Stricker",hypres_hydi$general$CONTACT_P)) & !grepl("UNSODA",hypres_hydi$general$COMMENTS2) & hypres_hydi$general$ISO_COUNTRY=="NL" ],
Hennings_HYPRES = hypres_hydi$general$PROFILE_ID[grepl("BGR",hypres_hydi$general$CONTACT_A)],
Schindler_HYPRES = hypres_hydi$general$PROFILE_ID[grepl("Schindler",hypres_hydi$general$CONTACT_P)],
Houskova_HYPRES = hypres_hydi$general$PROFILE_ID[hypres_hydi$general$ISO_COUNTRY=="SK"],
Romano_HYPRES = hypres_hydi$general$PROFILE_ID[grepl("Romano",hypres_hydi$general$CONTACT_P)])

for (i in 20:24) {
DB[[i]] <- list(hypres_hydi$general[hypres_hydi$general$PROFILE_ID %in% hypres.contr[[i-19]],1:65],
	hypres_hydi$basic[hypres_hydi$basic$PROFILE_ID %in% hypres.contr[[i-19]],],
	hypres_hydi$chemical[hypres_hydi$chemical$PROFILE_ID %in% hypres.contr[[i-19]],],
	hypres_hydi$psize[hypres_hydi$psize$PROFILE_ID %in% hypres.contr[[i-19]],],
	hypres_hydi$ret[hypres_hydi$ret$PROFILE_ID %in% hypres.contr[[i-19]],],
	hypres_hydi$cond[hypres_hydi$cond$PROFILE_ID %in% hypres.contr[[i-19]],],
	hypres_hydi$meth[is.na(hypres_hydi$meth[,1]),],
	hypres_hydi$tsermeta,
	hypres_hydi$tserdata)
names(DB)[i] <- names(hypres.contr)[i-19]
}
# add meth
DB$HYPRES[[7]] <- hypres_hydi$meth

save('DB', file='HYDI_single_hyp.Rdata')

# Next step: Harmonize methods codes!!! and check identifiers for countries where more than one dataset (Germany, Italy, Belgium)

# BUNDLE with SOURCE
load("HYDI_single_hyp.Rdata")
tnames <- c("GENERAL","BASIC","CHEMICAL","PSIZE","RET","COND","METHOD")
hydi <- list()
for (j in 1:length(DB)){
#print("---------")0
#print(names(DB)[j])
if (j != 25){
names(DB[[j]][[1]]) <- gsub("SLOPE","SLOP",names(DB[[j]][[1]]))
names(DB[[j]][[2]]) <- gsub("COURSE","COARSE",names(DB[[j]][[2]]))
names(DB[[j]][[3]]) <- toupper(names(DB[[j]][[3]]))
names(DB[[j]][[4]]) <- gsub("PSIZE","P_SIZE",names(DB[[j]][[4]]))}
for (k in 1:7){
if (j == 25 & k!=7){next}
#print("---------")
#print(tnames[k])
if (nrow(DB[[j]][[k]])>0){
# add ID to table COND 
if (k %in% 4:6){
if (!any(names(DB[[j]][[k]])=="ID")) 
{DB[[j]][[k]]$ID <- rep("ND",nrow(DB[[j]][[k]]))}
DB[[j]][[k]]$ID <- as.character(DB[[j]][[k]]$ID)}
#{DB[[j]][[k]] <- cbind(DB[[j]][[k]][,1:14],-999,DB[[j]][[k]][,15]);names(DB[[j]][[k]])[15:16] <- c("K_INV_P9","K_INV_MOD")}
tbl <- cbind(DB[[j]][[k]],SOURCE=names(DB)[j],stringsAsFactors=FALSE)
if (j==1){hydi[[k]] <- tbl} else {hydi[[k]] <- rbind(hydi[[k]],tbl)}
}}}
names(hydi) <- tnames
save('hydi',file="HYDI_SOURCE.Rdata")

load("HYDI_SOURCE.Rdata")
# harmonize ID's
unique(hydi$GENERAL$SOURCE[hydi$GENERAL$PROFILE_ID %in% hydi$GENERAL$PROFILE_ID[duplicated(hydi$GENERAL$PROFILE_ID)]])
# BE
M<-max(hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Cornelis"])
hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Javaux"] <- hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Javaux"] + M - 5600000
for (itbl in 2:7){
hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Javaux"] <- hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Javaux"] + M - 5600000
hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Javaux"] <- hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Javaux"] + (M-5600000)*100
}
# IT: Iovino
M<-max(hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Romano" & hydi$GENERAL$ISO_COUNTRY=="IT"]) - 38000000
hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Iovino"] <- hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Iovino"] + M 
for (itbl in 2:7){
hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Iovino"] <- hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Iovino"] + M 
hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Iovino"] <- hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Iovino"] + M*100
}
# IT: Romano_HYPRES
M<-max(hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Iovino" & hydi$GENERAL$ISO_COUNTRY=="IT"]) - 38000000
hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Romano_HYPRES"] <- hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Romano_HYPRES"] + M 
for (itbl in 2:7){
hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Romano_HYPRES"] <- hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Romano_HYPRES"] + M 
hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Romano_HYPRES"] <- hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Romano_HYPRES"] + M*100
}
# DE: 4 data sources (Schindler_HYPRES:52, Schindler:33, Hennings: 518, Romano: 14)
# give new IDs to Hennings
pid <- hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Hennings_HYPRES"]
npid <- 27600000 + 1:length(pid)
hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Hennings_HYPRES"] <- npid
for (itbl in 2:7){
hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Hennings_HYPRES"] <- npid[match(hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Hennings_HYPRES"],pid)]
sid <- as.numeric(substr(as.character(hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Hennings_HYPRES"]),9,10))
hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Hennings_HYPRES"] <- hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Hennings_HYPRES"]*100 + sid
}
# give new IDs to Schindler_HYPRES
M <- max(hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Hennings_HYPRES"])
pid <- hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Schindler_HYPRES"]
npid <- M + 1:length(pid) 
hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Schindler_HYPRES"] <- npid
for (itbl in 2:7){
hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Schindler_HYPRES"] <- npid[match(hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Schindler_HYPRES"],pid)]
sid <- as.numeric(substr(as.character(hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Schindler_HYPRES"]),9,10))
hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Schindler_HYPRES"] <- hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Schindler_HYPRES"]*100 + sid
}
# give new IDs to Schindler
M <- max(hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Schindler_HYPRES"])
pid <- hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Schindler"]
npid <- M + 1:length(pid) 
hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Schindler"] <- npid
for (itbl in 2:7){
hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Schindler"] <- npid[match(hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Schindler"],pid)]
sid <- as.numeric(substr(as.character(hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Schindler"]),9,10))
hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Schindler"] <- hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Schindler"]*100 + sid
}
# give new IDs to Romano
M <- max(hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Schindler"])
pid <- hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Romano" & hydi$GENERAL$ISO_COUNTRY=="DE"]
npid <- M + 1:length(pid) 
hydi$GENERAL$PROFILE_ID[hydi$GENERAL$SOURCE=="Romano" & hydi$GENERAL$ISO_COUNTRY=="DE"] <- npid
for (itbl in 2:7){
hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Romano" & hydi[[itbl]]$PROFILE_ID<27700000] <- npid[match(hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Romano" & hydi[[itbl]]$PROFILE_ID<27700000],pid)]
sid <- as.numeric(substr(as.character(hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Romano" & hydi[[itbl]]$PROFILE_ID<27700000]),9,10))
hydi[[itbl]]$SAMPLE_ID[hydi[[itbl]]$SOURCE=="Romano" & hydi[[itbl]]$PROFILE_ID<27700000] <- hydi[[itbl]]$PROFILE_ID[hydi[[itbl]]$SOURCE=="Romano" & hydi[[itbl]]$PROFILE_ID<27700000]*100 + sid
}
# control absence of duplicates:
any(duplicated(hydi$GENERAL$PROFILE_ID))
unique(hydi$GENERAL$SOURCE[hydi$GENERAL$PROFILE_ID %in% hydi$GENERAL$PROFILE_ID[duplicated(hydi$GENERAL$PROFILE_ID)]])
# character(0)

# remove missing data in RET and COND
hydi$RET <- hydi$RET[hydi$RET$HEAD != -999,]
hydi$RET <- hydi$RET[hydi$RET$THETA != -999,]
hydi$COND <- hydi$COND[hydi$COND$VALUE != -999,]
hydi$COND <- hydi$COND[hydi$COND$COND != -999,]

# METH_PAR in uppercase
hydi$METHOD$METH_PAR <- toupper(hydi$METHOD$METH_PAR)

save('hydi',file="HYDI_SOURCE_nd.Rdata")

# run checks on hydi
general_hydi <- general.checks(hydi$GENERAL) # remaining problems: LC and LU,, WRB2006_PQ1 and SQ1

meth_hydi <- meth.checks(hydi$METHOD) # need to be harmonized

basic_hydi <- basic.checks(hydi$BASIC,hydi$GENERAL$PROFILE_ID,hydi$METHOD$CODE_M) # profile and sample ID's don't match (Anaya)+ duplicates (anaya); HOR1_NAME; 0<POR<100; BD
unique(hydi$BASIC$SOURCE[!substr(hydi$BASIC$HOR1_NAME,2,2) %in% c('H', 'O', 'A', 'E', 'B', 'C', 'R', 'I', 'L', 'W', ' ','','N')])

chemical_hydi <-chemical.checks(hydi$CHEMICAL,hydi$BASIC$SAMPLE_ID,hydi$METHOD$CODE_M) # missing CODE_M; EX_CA, BASE_CATIONS, ACIDITY_NA4O out of range

psize_hydi <- psize.checks(hydi$PSIZE,hydi$BASIC$SAMPLE_ID,hydi$METHOD$CODE_M) # profile and sample ID's don't match (anaya); missing P_M 
unique(hydi$PSIZE$SOURCE[duplicated(hydi$PSIZE[,c("SAMPLE_ID","P_SIZE")])]) # Patyka, Romano, Cornelis

ret_hydi <- ret.checks(hydi$RET,hydi$BASIC$SAMPLE_ID,hydi$METHOD$CODE_M)# HEAD, THETA (>1 Schindler), missing THETA_M

cond_hydi <- cond.checks(hydi$COND,hydi$BASIC$SAMPLE_ID,hydi$METHOD$CODE_M)# COND<0; missing COND_M in method

save('hydi',file="HYDI_SOURCE_nd.Rdata")

# example *.mdb
# CAUTION: need to make de varchar(n) more efficient
# integer for id's
g=c("int","varchar(30)","varchar(30)","varchar(50)","float","float","int","varchar(2)","varchar(25)","varchar(25)","varchar(3)","varchar(3)","varchar(3)","varchar(4)","varchar(4)","varchar(2)","varchar(2)","varchar(2)","varchar(2)","varchar(2)","int","varchar(2)","varchar(2)","varchar(2)","int","varchar(2)","varchar(2)","varchar(2)","varchar(2)","varchar(2)","varchar(2)","varchar(2)","int","varchar(2)","int","varchar(3)","varchar(2)","varchar(2)","varchar(2)","varchar(2)","varchar(2)","varchar(2)","varchar(2)","varchar(2)","varchar(3)","varchar(3)","varchar(3)","varchar(3)","varchar(3)","varchar(3)","varchar(120)","varchar(255)","int","int","int","varchar(150)","varchar(255)","varchar(50)","varchar(255)","varchar(50)","varchar(100)","varchar(20)","varchar(255)","varchar(255)","varchar(255)","varchar(20)")
attr(g,"names") <- names(hydi[[1]])
b <- c("int","float","int","float","float","varchar(7)","float","float","varchar(7)","float","float","varchar(7)","float","float","varchar(6)","varchar(2)","varchar(6)","float","int","float","int","float","int","varchar(20)")
attr(b,"names") <- names(hydi[[2]])
c <- c("int","float","float","int","float","int","float","int","float","int","float","int","float","int","float","int","float","int","float","int","float","int","float","int","float","float","int","float","int","varchar(20)")
attr(c,"names") <- names(hydi[[3]])
p <- c("int","float","float","float","int","varchar(30)","varchar(20)")
attr(p,"names") <- names(hydi[[4]])
r <- c("int","float","float","float","int","float","float","float","float","float","float","float","float","int","varchar(30)","varchar(20)")
attr(r,"names") <- names(hydi[[5]])
k <- c("int","float","varchar(5)","float","float","int","float","float","float","float","float","float","float","float","float","int","varchar(30)","varchar(20)")
attr(k,"names") <- names(hydi[[6]])
m <- c("int","varchar(255)","varchar(255)","varchar(20)","varchar(20)")
attr(m,"names") <- names(hydi[[7]])
vT=list(general=g,basic=b,chemical=c,psize=p,ret=r,cond=k,meth=m)
# open connection with database
require("RODBC")
ch <- odbcConnectAccess2007('../HYDI-v1_BETA.accdb')
# for (j in 1:8){
# print("---------")
# print(names(DB)[j])
# #tnames <- toupper(paste(names(DB[[j]]),names(DB)[j],sep='_'))
for (k in 1:7){
print("---------")
print(tnames[k])
# if (nrow(DB[[j]][[k]])>0){
# tbl <- cbind(DB[[j]][[k]],SOURCE=names(DB)[j],stringsAsFactors=FALSE)
# if (j==1){
sqlDrop(ch, tnames[k], errors = FALSE)
sqlSave(ch, hydi[[k]], tablename = tnames[k], append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,varTypes=vT[[k]])
# } else {
# tryCatch(sqlSave(ch, tbl, tablename = tnames[k], append = TRUE,
        # rownames = FALSE, colnames = FALSE, verbose = FALSE,
        # safer = TRUE, addPK = FALSE,varTypes=vT[[k]]),
		# error=function(e){print(paste("Error when writing",tnames[k],"from",names(DB)[j],"to Access database"))})}
}
#}}
odbcClose(ch)
# #
