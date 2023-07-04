# Converting hypres database to new data structure:
# > creating the profile and layer IDs
# > averaging replicates
# > organising into tables
# 
# Author: M. Weynants
# Date: 26/04/2012
###############################################################################

# load packages
require(RODBC);require(ISOcodes)

# load original hypres
# connect to database
ch <- odbcDriverConnect(paste("DRIVER={Microsoft Access Driver (*.mdb, *.accdb)}",
				"DBQ=../../../MyWater/HYPRES_full/hypres access ver3.mdb",sep=";"))
T <- sqlTables(ch); print(T[T$TABLE_TYPE=="TABLE",3])
# querry data
df.basic <- sqlQuery(ch,"SELECT * FROM BASICDATA",stringsAsFactors=FALSE)
df.hprop <- sqlQuery(ch,"SELECT * FROM HYDRAULIC_PROPS",stringsAsFactors=FALSE)
df.k <- sqlQuery(ch,"SELECT * FROM RAWK",stringsAsFactors=FALSE)
df.psd <- sqlQuery(ch,"SELECT * FROM RAWPSD",stringsAsFactors=FALSE)
df.ret <- sqlQuery(ch,"SELECT * FROM RAWRET",stringsAsFactors=FALSE)
df.sp <- sqlQuery(ch,"SELECT * FROM SOIL_PROPS",stringsAsFactors=FALSE)
close(ch)
save(list=ls(pattern="^df"),file="hypres.RData")

# are profile ID unique?
length(unique(df.basic$LOCALNGR))== length(df.basic$LOCALNGR)
# yes, they are

## generate profile ID based on country code and profile number

# transforming hypres codes into ISO 3166_1 numeric codes
data("ISO_3166_1");data(ISO_3166_2)
hypresCountry <- data.frame("code"=c(30,31,32,33,34,39,42,45,46,49,351,440,441,442,7,41,48),
		"country"=c("Greece","Netherlands","Belgium","France","Spain","Italy",
				"Slovakia","Denmark","Sweden","Germany","Portugal","England",
				"Scotland","Northern Ireland","Russian Federation","Switzerland","Poland"))
all(unique(df.basic$COUNTRY) %in% hypresCountry[,1])
all(hypresCountry[,2] %in% ISO_3166_1$Name | hypresCountry[,2] %in% ISO_3166_2$Name)

Country <- sapply(sapply(df.basic$COUNTRY,function(x,cc=hypresCountry){cc[cc$code==x,2]}),
		function(x,iso1=ISO_3166_1,iso2=ISO_3166_2)
		{if (x %in% iso1$Name){iso1[iso1$Name==x,1]
			}else{iso2[iso2$Name==x,4]}})
CountryN <- sapply(Country,function(x,iso1=ISO_3166_1){iso1[iso1$Alpha_2==x,3]})
profID <- rep(NA,length(Country))
for (C in unique(Country)){
	indC <- Country==C
	o <- order(df.basic$LOCALNGR[indC])
	profID[indC] <- paste(CountryN[indC],gsub(" ","0",format(1:length(o),width=5))[o],sep="")
}
#
df.GEN <- as.data.frame(matrix(nrow=nrow(df.basic),ncol=65,dimnames=list(NULL,c("PROFILE_ID","LOC_COOR_X","LOC_COOR_Y", "LOC_COOR_SYST","X_WGS84","Y_WGS84","ELEV","ISO_COUNTRY","RC_L1","RC_L2","LC_L1","LC_L2","LC_L3","LU_L1","LU_L2","SITE_LANDFORM","SITE_SLOP_POS","SITE_SLOP_FORM","SITE_SLOP_GRAD","SRF_ROCK_COV","SRF_ROCK_DIS","SRF_COAR_COV","SRF_COAR_SIZ","SRF_ERO_CAT","SRF_ERO_COV","SRF_ERO_DEG","SRF_ERO_ACT","SRF_SEAL_THIC","SRF_SEAL_CON","SRF_CRAC_WID","SRF_CRAC_DEP","SRF_CRAC_DIS","SRF_SAL_COV","SRF_SAL_THIC","PAR_MAT","AGE","WRB2006_RSG","WRB2006_PQ1","WRB2006_PQ2","WRB2006_PQ3","WRB2006_SQ1","WRB2006_SQ2","WRB2006_SQ3","WRB1998_RSG","WRB1998_ADJSPE1","WRB1998_ADJSPE2","WRB1998_ADJSPE3","WRB1998_ADJSPE4","WRB1998_ADJSPE5","WRB1998_ADJSPE6","NAT_CLAS","NAT_CLAS_REF","YEAR","MONTH","DAY","SURVEYOR_P","PUBL_REF","CONTACT_P","CONTACT_A","EMAIL","REL_ID","REL_T_SER","COMMENTS1","COMMENTS2","COMMENTS3"))))

df.GEN[,1] <- as.numeric(profID)
tmp<-df.basic$LOCALNGR
df.GEN[,2:3]  <- t(sapply(1:length(tmp),function(x,a=strsplit(tmp,' ')){if (length(a[[x]])==1){y<-c(a[[x]],'ND')} else {if(length(a[[x]])>2){y <- c(a[[x]][1],paste0(a[[x]][-1],collapse=' '))}else{y<-a[[x]]}}}))
df.GEN[,4] <- df.basic$COORD_TYPE
# WGS
# elev
df.GEN[,8] <- Country
# RC; LC; LU
RSG <- data.frame(name=c('acrisol','albeluvisol','allisol','andosol','anthrosol','arenosol','calcisol','cambisol','chernozem','cryosol','durisol','ferralsol','fluvisol','gleysol','gypsisol','histosol','kastanozem','leptosol','lixisol','luvisol','nitisol','phaeozem','planosol','plinthosol','podzol','podsol','regosol','solonchak','solonetz','stagnosol','technosol','umbrisol','vertisol'),code=c('AC', 'AB', 'AL', 'AN', 'AT', 'AR', 'CL', 'CM', 'CH', 'CR', 'DU', 'FR', 'FL', 'GL', 'GY', 'HS', 'KS', 'LP' ,'LX', 'LV', 'NT', 'PH', 'PL', 'PT', 'PZ', 'PZ', 'RG', 'SC', 'SN', 'ST', 'TC', 'UM', 'VR'),stringsAsFactors=FALSE)
wrb <- (sapply(df.basic$FAO,function(x){b<-sapply(RSG$name,function(a){grepl(a,x,ignore.case=TRUE)}); if (any(b)) {y<-RSG[which(b)[1],'code']} else {y<-'ND'}}));attributes(wrb)$names<-NULL
# df.basic$FAO[!is.na(df.basic$FAO) & grepl('ND',wrb)]
# plaggensol -> anthrosol
# treposol -> terric anthrosol
# auftragsboden -> anthrosol
# rendzina -> cambisol?
wrb[grepl('plaggensol',df.basic$FAO,ignore.cas=TRUE)|grepl('treposol',df.basic$FAO,ignore.cas=TRUE)|grepl('auftragsboden',df.basic$FAO,ignore.cas=TRUE)]<-'AT'
wrb[grepl('rendzina',df.basic$FAO,ignore.cas=TRUE)]<-'CM'
ADJ <- data.frame(name=c('Abruptic','Endofluvic','Hemic','Mollic','Salic','Aceric','Endogleyic','Histic','Molliglossic','Sapric','Acric','Endoleptic','Hortic','Natric','Silandic','Acroxic','Endosalic','Humic','Nitic','Siltic','Albic','Entic','Hydragric','Novic','Skeletic','Alcalic','Epidystric','Hydric','Nudilithic','Sodic','Alic','Epieutric','Hdrophobic','Ombric','Solodic','Aluandic','Epileptic','Hyperalbic','Ornithic','Sombric','Alumic','Episalic','Hyperalic','Ortsteinic','Spodic','Andic','Escalic','Hypercalcic','Oxyaquic','Spolic','Anthraquic','Eutric','Hyperdystric','Pachic','Stagnic','Anthric','Eutrosilic','Hypereutric','Pellic','Sulphaquatic','Arenic','Ferralic','Hypergypsic','Petric','Sulphatic','Aric','Ferric','Hyperochric','Petrocalcic','Takyric','Aridic','Fibric','Hypersalic','Petroduric','Technic','Arzic','Floatic','Hyperskeletic','Petrogleyic','Tephric','Brunic','Fluvic','Hypocalcic','Petrogypsic','Terric','Calcaric','Folic','Hypogypsic','Petroplinthic','Thaptandic','Calcic','Fractipetric','Hypoluvic','Petrosalic','Thaptovitric','Cambic','Fractiplinthic','Hyposalic','Pisoplinthic','Thionic','Carbic','Fragic','Hyposodic','Placic','Thixotropic','Carbonatic','Fulvic','Irragric','Plaggic','Tidalic','Chloridic','Garbic','Lamellic','Plinthic','Toxic','Chromic','Gelic','Laxic','Posic','Transportic','Clayic','Gelistagnic','Leptic','Profondic','Turbic','Colluvic','Geric','Lignic','Protic','Umbric','Cryic','Gibbsic','Limnic','Puffic','Umbriglossic','Cutanic','Glacic','Linic','Reductaquic','Urbic','Densic','Gleyic','Lithic','Reductic','Vermic','Drainic','Glossalbic','Lixic','Regic','Vertic','Duric','Glossic','Luvic','Rendzic','Vetic','Dystric','Greyic','Magnesic','Rheic','Vitric','Ekranic','Grumic','Manganiferric','Rhodic','Voronic','Endoduric','Gypsic','Mazic','Rubic','Xanthic','Endodystric','Gypsiric','Melanic','Ruptic','Yermic','Endoeutric','Haplic','Mesotrophic','Rustic'), code=c('ap','nf','hm','mo','sz','ae','ng','hi','mi','sa','ac','nl','ht','na','sn','ao','ns','hu','ni','sl','ab','et','hg','nv','sk','ax','ed','hy','nt','so','al','ee','hf','om','sc','aa','el','ha','oc','sm','au','ea','hl','os','sd','an','ec','hc','oa','sp','aq','eu','hd','ph','st','am','es','he','pe','sq','ar','fl','hp','pt','su','ai','fr','ho','pc','ty','ad','fi','hs','pd','te','az','ft','hk','py','tf','br','fv','wc','pg','tr','ca','fo','wg','pp','ba','cc','fp','wl','ps','bv','cm','fa','ws','px','ti','cb','fg','wn','pi','tp','cn','fu','ir','pa','td','cl','ga','ll','pl','tx','cr','ge','la','po','tn','ce','gt','le','pf','tu','co','gr','lg','pr','um','cy','gi','lm','pu','ug','ct','gc','lc','ra','ub','dn','gl','li','rd','vm','dr','gb','Ix','rg','vr','du','gs','lv','rz','vt','dy','gz','mg','rh','vi','ek','gm','mf','ro','vo','nd','gy','mz','ru','xa','ny','gp','ml','rp','ye','ne','ha','ms','rs'),stringsAsFactors=FALSE)
ADJ$name_short <- tolower(gsub('ic','',ADJ$name))
tmp <- gsub('pelic','pellic',df.basic$FAO);tmp<-gsub('ic ',' ',tmp); tmp <- gsub('o ',' ',tmp); tmp <- gsub('o-',' ',tmp); tmp <- gsub('i-',' ',tmp); tmp <- gsub('-',' ',tmp)
fao <- strsplit(tolower(tmp),' ')
tmp2 <- tmp <- matrix('ND',nrow=length(fao),ncol=3)
for (x in 1:length(fao)){
if (length(fao[[x]])>=2){
for (iadj in 1:(length(fao[[x]])-1)){
tmp[x,iadj] <- fao[[x]][iadj]
tmp2[x,iadj] <- ADJ[match(tmp[x,iadj], ADJ$name_short),'code']
}
}}
tmp2[grepl('Treposol',df.basic$FAO),1]<-'tr'
df.GEN[,37] <- wrb
df.GEN[,38:40] <- tmp2
df.GEN[,51] <- df.basic$FAO_SOIL
df.GEN[,52] <- 'FAO 1974?'
tmp<-as.POSIXlt(df.basic$SAMPLEDATE)
df.GEN[,53:55] <- c(tmp$year+1900,tmp$mon+1,tmp$mday)
df.GEN[,58] <- df.basic$CONTACT_NAME
df.GEN[,59] <- df.basic$CONTACT_ADDRESS
df.GEN[,60] <- df.basic$EMAIL
df.GEN[,63] <- df.basic$SITEDESCRIP
df.GEN[,64] <- df.basic$COMMENTS1
df.GEN[,65] <- df.basic$COMMENTS2
df.GEN$methods <- df.basic$KEYWORDS
df.GEN[is.na(df.GEN)] <- -999
# some columns must be "ND" rather than -999
df.GEN[,c(9:20,22:24,26:32,34,36:52,56:65)][df.GEN[,c(9:20,22:24,26:32,34,36:52,56:65)]==-999] <- "ND"
# get gridref
gridref <- gsub('[[:space:]]+$', '',df.basic$GRIDREF)
gridref1 <- strsplit(gridref,split=" ")
gridref2 <- sapply(gridref1,function(x){if (length(x)>1){long<-x[[1]];lat<-x[[2]];return(c(long,lat))}else{return(c(NA,NA))}})
gridref3 <- matrix(as.numeric(gridref2),nrow=nrow(df.GEN),byrow=TRUE)
df.GEN[,c("GRIDREF_X","GRIDREF_Y")] <- gridref3
# transform gisco to wgs84
require(maptools);require(sp);require(rgdal)
source("gisco.r")
wgs84 <- CRS("+proj=latlon +ellps=WGS84")
nona<-!is.na(df.GEN$GRIDREF_X) & !is.na(df.GEN$GRIDREF_Y)
sp <- SpatialPoints(df.GEN[nona,c("GRIDREF_X","GRIDREF_Y")],proj4string=gisco)
sp2 <- spTransform(sp,wgs84)
require("maps")
#map("world",xlim=c(-12,45),ylim=c(30,60))
#points(sp2,col="blue")
writePointsShape(sp2,fn="./gis/hypres_wgs84")
df.GEN[nona,5:6]<-coordinates(sp2)

# BASIC
df.BAS <- as.data.frame(matrix(nrow=nrow(df.sp),ncol=23,dimnames=list(NULL,c("PROFILE_ID","SAMPLE_ID","SAMPLE_POS","SAMPLE_DEP_TOP","SAMPLE_DEP_BOT","HOR1_NAME","HOR1_TOP","HOR1_BOT","HOR2_NAME","HOR2_TOP","HOR2_BOT","HOR3_NAME","HOR3_TOP","HOR3_BOT","STRUCTURE1","STR_COMB","STRUCTURE2","POR","POR_M","BD","BD_M","COARSE","COARSE_M"))))
# create profile_id
df.sp[is.na(df.sp$LOCALNGR),'LOCALNGR']<-'manually added'
df.BAS[,1] <- as.numeric(profID[match(df.sp$LOCALNGR,df.basic$LOCALNGR)])
# df.basic[is.na(match(df.basic$LOCALNGR,df.sp$LOCALNGR)),'LOCALNGR']
# there are 113 profiles from df.basic that are not in df.sp!: remove them:
df.GEN <- df.GEN[!is.na(match(df.basic$LOCALNGR,df.sp$LOCALNGR)),]
# there are 2 profiles from df.sp that are not in df.basic: remove them too

horname <- paste(substr(df.sp[,'HORIZON'],1,2),substr(df.sp[,'HORIZON'],3,6),sep=' ')
horname[grepl("ND",df.sp$HORIZON)] <- "ND"
horname[horname=="1A ap 2"] <- "1A  ap2"
horname[horname==" A aB  "] <- " A B a  "
horname[horname=="nC rF  "] <- "nC  rf "
ind<-grepl("^[[:lower:]]$",substr(horname,4,4))
horname[ind] <- paste(substr(horname[ind],1,3)," ",substr(horname[ind],4,6),sep="") 
horname[horname==" H BCgr"] <- " B Cgr "
ind<-grepl("^[[:lower:]]$",substr(horname,7,7))
horname[ind] <- paste(substr(horname[ind],1,6)," ",sep="")
horname[horname==" H BCg "] <- " B Chg "
horname[horname==" A B a  "] <- " A Ba  "

horn <- rep(NA,nrow(df.BAS))
prof <-unique(df.BAS$PROFILE_ID);prof<-prof[!is.na(prof)]
for (iprof in 1:length(prof)) {ind <- df.BAS$PROFILE_ID==prof[iprof] & !is.na(df.BAS$PROFILE_ID)
# duplicated samples: same sample_id
# CAUTION! duplicates is a relative concept: should consider horname and depth, but also BD, PSD and SWR!
#horu <- unique(horname[ind])
#du <- sort(unique(df.sp$TOP_DEPTH[ind]))
# keep all HYPRES samples:
horn[ind] <- order(df.sp$TOP_DEPTH[ind])# match(df.sp$TOP_DEPTH[ind],du) # match(horname[ind],horu)#
}

df.BAS[,2] <- df.BAS$PROFILE_ID*100 + horn

df.BAS[,3] <- horn
df.BAS[,4:5] <- round(df.sp[,c('TOP_DEPTH','BOT_DEPTH')])
df.BAS[,6] <- horname
replicates <- substr(df.sp$HORIZON,7,7)
# structure
grade <- data.frame(name=c('weak','moderate','strong','weak moderate','moderate strong'),code=c('WE','MO','ST','WM','MS'),stringsAsFactors=FALSE)
type <- data.frame(name=c('rock structure','stratified structure','single grain','massive','prismatic','blocky','angular blocky','parallelipiped','angular and subangular blocky','angular wedge','subangular and angular blocky','subangular blocky','nuty','prismatic','subangular prismatic','wedge-shaped','columnar','granular','worm casts','platy','cloddy','crumbly','lumpy'),code=c('RS','SS','SG','MA','PM','BL','AB','AP','AS','AW','SA','SB','SN','PR','PS','WE','CO','GR','WC','PL','CL','CR','LU'),stringsAsFactors=FALSE)
size <- data.frame(name=c('very fine','fine','medium','coarse','very coarse','v.coarse','extremely coarse','very fine and fine','very fine to medium','fine and medium','fine to coarse','medium and coarse','medium to very coarse','coarse and very coarse'),code=c('VF','FI','ME','CO','VC','VC','EC','FF','VM','FM','FC','MC','MV','CV'),stringsAsFactors=FALSE)
#
gd <- sapply(grade$name,function(x){grepl(x,tolower(df.sp$STRUCTURE1))})
gd1 <- sapply(1:nrow(df.BAS),function(x){if (any(gd[x,])){y<-grade$code[max(which(gd[x,]))]} else y<-'ND'})
#
ty <- sapply(type$name,function(x){grepl(x,tolower(df.sp$STRUCTURE1))})
ty1 <- sapply(1:nrow(df.BAS),function(x){if (any(ty[x,])){y<-type$code[max(which(ty[x,]))]} else y<-'ND'})
# 
sz <- sapply(size$name,function(x){grepl(x,tolower(df.sp$STRUCTURE1))})
sz1 <- sapply(1:nrow(df.BAS),function(x){if (any(sz[x,])){y<-size$code[max(which(sz[x,]))]} else y<-'ND'})
sz1[sz[,1]] <- 'VF'
#
df.BAS[,15] <- paste(gd1,sz1,ty1,sep='')
df.BAS[,16] <-'ND'
# 
gd <- sapply(grade$name,function(x){grepl(x,tolower(df.sp$STRUCTURE2))})
gd2 <- sapply(1:nrow(df.BAS),function(x){if (any(gd[x,])){y<-grade$code[max(which(gd[x,]))]} else y<-'ND'})
#
ty <- sapply(type$name,function(x){grepl(x,tolower(df.sp$STRUCTURE2))})
ty2 <- sapply(1:nrow(df.BAS),function(x){if (any(ty[x,])){y<-type$code[max(which(ty[x,]))]} else y<-'ND'})
# 
sz <- sapply(size$name,function(x){grepl(x,tolower(df.sp$STRUCTURE2))})
sz2 <- sapply(1:nrow(df.BAS),function(x){if (any(sz[x,])){y<-size$code[max(which(sz[x,]))]} else y<-'ND'})
sz2[sz[,1]] <- 'VF'
df.BAS[,17] <- paste(gd2,sz2,ty2,sep='')
# POROSITY in HYPRES in cm3/cm3: change to %
df.BAS[,18] <-df.sp$POROSITY*100
df.BAS[df.BAS$POR<=0,18]<--999
df.BAS[,19] <- -999
df.BAS[,20] <- df.sp$BULK_DEN
df.BAS[df.BAS$BD<=0,20]<--999
df.BAS[,21:23]<- -999

# NA profiles
nona <- !is.na(df.BAS$PROFILE_ID)

# change NA to -999
df.BAS[is.na(df.BAS)] <- -999
# change -9 to -999
df.BAS[df.BAS==-9] <- -999
# change -999 to ND
df.BAS[,c(9,12)] <- "ND"

# remove duplicates an NA profile
df.BAS.nd <- df.BAS[!duplicated(df.BAS$SAMPLE_ID) & nona,]
# CAUTION average properties for duplicates
df.BAS.nd$POR <- sapply(df.BAS.nd$SAMPLE_ID,function(x){y<-mean(df.BAS$POR[df.BAS$SAMPLE_ID==x & df.BAS$POR != -999])})
df.BAS.nd$BD <- sapply(df.BAS.nd$SAMPLE_ID,function(x){y<-mean(df.BAS$BD[df.BAS$SAMPLE_ID==x & df.BAS$BD != -999])})
df.BAS.nd[is.na(df.BAS.nd)] <- -999


## CHEMICAL
df.CHEM <- as.data.frame(matrix(nrow=nrow(df.BAS[nona,]),ncol=29,dimnames=list(NULL,c("PROFILE_ID","SAMPLE_ID","OC","OC_M","CACO3","CACO3_M","PH_H2O","PH_H2O_M","PH_KCL","PH_KCL_M","EC","EC_M","SALT","SALT_M","CEC","CEC_M","EX_NA","EX_NA_M","EX_MG","EX_MG_M","EX_K","EX_K_M","EX_CA","EX_CA_M","BASE_CATIONS","ACIDITY_NA4O","ACIDITY_NA4O_M","ACIDITY_KCL","ACIDITY_KCL_M"))))
df.CHEM[,]<--999
df.CHEM[,1:2] <- df.BAS[nona,1:2]
df.CHEM[,3] <- df.sp[nona,'ORG_MAT']/1.724
df.CHEM$OC[df.sp$ORG_MAT[nona]==-9]<- -999

# remove duplicates: average properties
df.CHEM.nd <- df.CHEM[!duplicated(df.CHEM$SAMPLE_ID),]
df.CHEM.nd$OC <- sapply(df.CHEM.nd$SAMPLE_ID,function(x){y<-mean(df.CHEM$OC[df.CHEM$SAMPLE_ID==x & df.CHEM$OC != -999])})
df.CHEM.nd$OC[is.na(df.CHEM.nd$OC)] <- -999

## PSIZE
df.PS <- as.data.frame(matrix(nrow=nrow(df.psd),ncol=5,dimnames=list(NULL,c('PROFILE_ID','SAMPLE_ID','P_SIZE','P_PERCENT','P_M'))))
# id's
df.PS[,1] <- df.BAS$PROFILE_ID[match(df.psd$LOCALNGR,df.sp$LOCALNGR)]
df.PS[,2] <- df.BAS$SAMPLE_ID[match(paste(df.psd$LOCALNGR,df.psd$HORIZON),paste(df.sp$LOCALNGR,df.sp$HORIZON))]
df.PS[,3:4] <- df.psd[,3:4]
df.PS[,5] <- -999
# add column with original IDs
df.PS$ID <- paste(df.psd$LOCALNGR,df.psd$HORIZON,sep="__")
# remove is.na(SAMPLE_ID)
df.PS <- df.PS[!is.na(df.PS$SAMPLE_ID),]
# change NA to -999
df.PS[is.na(df.PS)] <- -999
df.PS[df.PS==-9] <- -999
# remove duplicates...

## RET
df.RET <- as.data.frame(matrix(nrow=nrow(df.ret),ncol=14,dimnames=list(NULL,c('PROFILE_ID','SAMPLE_ID','HEAD','THETA','THETA_M','TH_INV_P1','TH_INV_P2','TH_INV_P3','TH_INV_P4','TH_INV_P5','TH_INV_P6','TH_INV_P7','TH_INV_P8','TH_INV_MOD'))))
df.RET[,1] <- df.BAS$PROFILE_ID[match(df.ret$LOCALNGR,df.sp$LOCALNGR)]
df.RET[,2] <- df.BAS$SAMPLE_ID[match(paste(df.ret$LOCALNGR,df.ret$HORIZON),paste(df.sp$LOCALNGR,df.sp$HORIZON))]
df.RET[,3:4] <- df.ret[,c('HEAD','THETA')]
df.RET[,5:14]<- -999
# do not remove duplicates: keep all data
# add column with original IDs
df.RET$ID <- paste(df.ret$LOCALNGR,df.ret$HORIZON,sep="__")
# remove NA ID's
df.RET<-df.RET[!is.na(df.RET[,2]),]

# df.RET.nd <- df.RET[!duplicated(df.RET[,c('SAMPLE_ID','HEAD')]),]
# # problem: too slow if for example evaporation method
# df.RET.nd$THETA <- sapply(df.RET.nd$SAMPLE_ID,function(x){head<-unique(df.RET$HEAD[df.RET$SAMPLE_ID==x]) ; for (h in head){ind <- df.RET$SAMPLE_ID==x & df.RET$HEAD != h; if(sum(ind)>1){y<-mean(df.RET$THETA[ind])}else{y<-df.RET$THETA[ind]}}})

# add satwat from sp: no, it's already in there
# ind <- df.sp$SATWAT[nona]!=-9 & df.sp$SATWAT[nona]!=0
# satwat <-  cbind(df.BAS[ind,1:2],0,df.sp$SATWAT[ind],-999,-999,-999,-999,-999,-999,-999,-999,-999,-999)
# names(satwat)<-names(df.RET)
# df.RET1 <- rbind(df.RET,satwat)

# HEAD must be positive
df.RET$HEAD <- abs(df.RET$HEAD)
# remove NA or -999 profile and sample
df.RET <- df.RET[!is.na(df.RET$PROFILE_ID) & !is.na(df.RET$SAMPLE_ID) & df.RET$PROFILE_ID != -999 & df.RET$SAMPLE_ID != -999,]
# change NA to -999
df.RET[is.na(df.RET)] <- -999

# add MVG parameters supplied by contributor
df.RET0 <- df.RET
ind <- !is.na(df.sp$MVG_SAT)
mvg <- cbind(data.frame(PROFILE_ID=df.BAS$PROFILE_ID[ind],SAMPLE_ID=df.BAS$SAMPLE_ID[ind]),matrix(-999,nrow=sum(ind),ncol=3),df.sp[ind,17:21],matrix(-999,nrow=sum(ind),ncol=3),TH_INV_MOD=700,ID=paste(df.sp[ind,"LOCALNGR"],df.sp[ind,"HORIZON"]),stringsAsFactors=FALSE)
mvg[is.na(mvg)] <- -999
names(mvg) <- names(df.RET)
df.RET <- rbind(df.RET0,mvg)

## COND
df.COND <- as.data.frame(matrix(nrow=nrow(df.k),ncol=16,dimnames=list(NULL,c('PROFILE_ID','SAMPLE_ID','IND_VALUE','VALUE','COND','COND_M','K_INV_P1','K_INV_P2','K_INV_P3','K_INV_P4','K_INV_P5','K_INV_P6','K_INV_P7','K_INV_P8','K_INV_P9','K_INV_MOD'))))
df.COND[,1] <- df.BAS$PROFILE_ID[match(df.k$LOCALNGR,df.sp$LOCALNGR)]
df.COND[,2] <- df.BAS$SAMPLE_ID[match(paste(df.k$LOCALNGR,df.k$HORIZON),paste(df.sp$LOCALNGR,df.sp$HORIZON))]
df.COND[,3] <- grepl('head',df.k$IND_VAR)
df.COND[,4] <- df.k$VAR
df.COND[,5] <- df.k$COND
df.COND[,6:16] <- -999
# not remove duplicates: keep them all
# add column with original IDs
df.COND$ID <- paste(df.k$LOCALNGR,df.k$HORIZON,sep="__")
# add MVG parameters supplied by contributor and remove NA profile and samples
df.COND0 <- df.COND[!is.na(df.COND$PROFILE_ID) & !is.na(df.COND$SAMPLE_ID) & df.COND$PROFILE_ID != -999 & df.COND$SAMPLE_ID != -999,]
df.COND0[is.na(df.COND0)]<- -999
ind <- !is.na(df.sp$MVG_KS)
mvg <- cbind(data.frame(PROFILE_ID=df.BAS$PROFILE_ID[ind],SAMPLE_ID=df.BAS$SAMPLE_ID[ind]),1,matrix(-999,nrow=sum(ind),ncol=2),df.sp[ind,17:23],matrix(-999,nrow=sum(ind),ncol=3),TH_INV_MOD=900,ID=paste(df.sp[ind,"LOCALNGR"],df.sp[ind,"HORIZON"]),stringsAsFactors=FALSE)
mvg[is.na(mvg)] <- -999
names(mvg) <- names(df.COND)
df.COND <- rbind(df.COND0,mvg)


## METHOD
# methods can be derived from df.basic$comments
# table(tolower(gsub("^ +","",unlist(strsplit(df.basic$KEYWORDS,',')))))
# BGR
# THETA_M
bgr_th <- data.frame(CODE_M=600,METHOD='laboratory measurements of soil water retention by pressure membrane apparatus',METH_REF='ND',METH_PAR='THETA_M',stringsAsFactors=FALSE)
df.RET$THETA_M[df.RET$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('BGR',df.GEN$CONTACT_A)]]<-600
# COND_M
bgr_k <- data.frame(CODE_M=800,METHOD='horizontal saturated hydraulic conductivity by the falling head method on undisturbed soil cores',METH_REF='ND',METH_PAR='COND_M',stringsAsFactors=FALSE)
df.COND$COND_M[df.COND$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('BGR',df.GEN$CONTACT_A)]] <- 800
# POR_M
bgr_basic <- data.frame(CODE_M=c(100),METHOD=c("calculated from bulk density, assuming a constant particle density of 2.65 g/cm3"),METH_REF=c("ND"),METH_PAR=c("POR_M"),stringsAsFactors=FALSE)
df.BAS$POR_M[df.BAS$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('BGR',df.GEN$CONTACT_A)]] <- 100
# OC_M
bgr_oc <- data.frame(CODE_M=132,METHOD="dichromate oxidation with K2Cr2O7 in sulfuric acid",METH_REF="ND",METH_PAR="OC_M")
df.CHEM$OC_M[df.CHEM$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('BGR',df.GEN$CONTACT_A)]] <- 132
# P_M : pretreatment by hydrogen peroxide (H2O2) to destroy organic matter and by hydrochloric acid (HCl) to remove alkaline carbonates. After dispersion by sodium pyrophosphate (Na4P2O7) the contents of silt (2 – 63 µm) and clay (< 2 µm) fractions were measured by pipette sampling and contents of four sand fractions (63 – 125 µm, 125 – 200 µm, 200 – 630 µm, 630 – 2000 µm) were determined by wet sieving 
bgr_psd <- data.frame(CODE_M=500,METHOD='pretreatment by H2O2 and HCl, dispersion by a4P2O7, pipette method for silt (2 – 63 µm) and clay (< 2 µm) fractions and wet sieving for four sand fractions (63 – 125 µm, 125 – 200 µm, 200 – 630 µm, 630 – 2000 µm)',METH_REF='ND',METH_PAR='COND_M',stringsAsFactors=FALSE)
df.PS$P_M[df.PS$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('BGR',df.GEN$CONTACT_A)]] <- 500

# Netherlands: different sources and methods
# project number 6168
nl1 <- data.frame(CODE_M=c(601,801,802),METHOD=c('evaporation method according to Wind, high-tech version Boels; analysis Halbertsma; sample size 10 * 8 cm','evaporation method according to Wind, high-tech version Boels; analysis Halbertsma; sample size 10 * 8 cm','saturated conductivity using constant head (vertical downward), sample size 10 * 8 cm'),METH_REF='project number 6168; HYPRES',METH_PAR=c('THETA_M','COND_M','COND_M'),stringsAsFactors=FALSE)
df.RET$THETA_M[df.RET$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('project number 6168',df.GEN$COMMENTS3)]] <- 601
df.COND$COND_M[df.COND$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('project number 6168',df.GEN$COMMENTS3)] & df.COND$VALUE!=0] <- 801
df.COND$COND_M[df.COND$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('project number 6168',df.GEN$COMMENTS3)] & df.COND$VALUE==0] <- 802
# Staring Series 1987
nl2 <- data.frame(CODE_M=c(101,133,602,803),METHOD=c('calculated from bulk density, assuming a constant particle density of 2.65 g/cm3, corrected for OM content','standard loss of ignition','evaporation (moisture release to approx. 800cm tension), pressure cells applied on undisturbed stone free samples', 'column method (ksat), crust method (to 50 cm tension), sorptivity (sands) and hot air applied on undisturbed stone free samples. 13 points were interpolated from a hand drawn curve fitted to the data'), METH_REF='Staring Series 1987; HYPRES',METH_PAR=c('POR_M','OC_M','THETA_M','COND_M'),stringsAsFactors=FALSE)
df.BAS$POR_M[df.BAS$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('Staring Series 1987',df.GEN$methods)]] <- 101
df.CHEM$OC_M[df.CHEM$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('Staring Series 1987',df.GEN$methods)]] <- 133
df.RET$THETA_M[df.RET$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('Staring Series 1987',df.GEN$methods)]] <- 602
df.COND$COND_M[df.COND$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('Staring Series 1987',df.GEN$methods)]] <- 803
# still a lot of samples but too much of work: can't do it now (25/01/2013: deadline for sending for QC)
# Stricker
nl3 <- data.frame(CODE_M=c(603,804),METHOD=c("evaporation","crust and then sorptivity"),METH_REF="ND",METH_PAR=c("THETA_M","COND_M"))
df.RET$THETA_M[df.RET$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('Stricker',df.GEN$CONTACT_P) & !grepl('UNSODA',df.GEN$COMMENTS1)]] <- 603
df.COND$COND_M[df.COND$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('Stricker',df.GEN$PROFILE_ID) & !grepl('UNSODA',df.GEN$COMMENTS1)]] <- 804
# Booltink
nl4 <- data.frame(CODE_M=c(805,806),METHOD=c("onestep and crust method", "multistep and crust method"),METH_REF="ND",METH_PAR="COND_M")
df.COND$COND_M[df.COND$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('onestep',df.GEN$COMMENTS2)]] <- 805
df.COND$COND_M[df.COND$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl('multistep',df.GEN$COMMENTS2) & !grepl('Gerrit',df.GEN$CONTACT_P)]] <- 806


# SK
sk <- data.frame(CODE_M=c(110,134,604,605,807),
METHOD=c('Core method (ISO 11272) with 100 mL steel cylinders, oven-dried at 105°C for 48 hours','Dry combustion at 900°C','lab water retention by pressure plate and air drying','field water retention using TDR and tensiometers','field hydraulic conductivity by Guelph permeameter'),
METH_REF=c('ISO 11272','ISO 10694','ND','ND','ND'),
METH_PAR=c('BD_M','OC_M','THETA_M','THETA_M','COND_M'),stringsAsFactors=FALSE)
df.BAS$POR_M[df.BAS$PROFILE_ID %in% df.GEN$PROFILE_ID[df.GEN$ISO_COUNTRY=='SK' & grepl('pressure plate',df.GEN$COMMENTS2)]] <- 100
df.BAS$BD_M[df.BAS$PROFILE_ID %in% df.GEN$PROFILE_ID[df.GEN$ISO_COUNTRY=='SK' & grepl('pressure plate',df.GEN$COMMENTS2)]] <- 110
df.CHEM$OC_M[df.CHEM$PROFILE_ID %in% df.GEN$PROFILE_ID[df.GEN$ISO_COUNTRY=='SK' & grepl('pressure plate',df.GEN$COMMENTS2)]] <- 134
df.RET$THETA_M[df.RET$PROFILE_ID %in% df.GEN$PROFILE_ID[df.GEN$ISO_COUNTRY=='SK' & grepl('pressure plate',df.GEN$COMMENTS2)]] <- 604
df.RET$THETA_M[df.RET$PROFILE_ID %in% df.GEN$PROFILE_ID[df.GEN$ISO_COUNTRY=='SK' & grepl('TDR',df.GEN$COMMENTS2)]] <- 605
df.COND$COND_M[df.COND$PROFILE_ID %in% df.GEN$PROFILE_ID[df.GEN$ISO_COUNTRY=='SK' & grepl('Guelph',df.GEN$COMMENTS2)]] <- 807

# Schindler
sch <- data.frame(CODE_M=c(131,606,808),METHOD=c(
"Woesthoff method",
"evaporation method on 250cm3 undisturbed soil cores with tensions measured at two points through time",
"evaporation method on 250cm3 undisturbed soil cores with tensions at two points. unsaturated conductivity calculated using Darcy's law and assuming quasi steady state"),
METH_REF=c("ND","Schindler, U. 1980. Ein Schnellverfahren zur Messung der Wasserleitfähigkeit im teilgesättigten Boden an Stechzylinderproben.","Schindler, U. 1980. Ein Schnellverfahren zur Messung der Wasserleitfähigkeit im teilgesättigten Boden an Stechzylinderproben."),
METH_PAR=c("OC_M","THETA_M","COND_M"),stringsAsFactors=FALSE)
df.CHEM$OC_M[df.CHEM$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl("Schindler",df.GEN$CONTACT_P)]] <- 131
df.RET$THETA_M[df.RET$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl("Schindler",df.GEN$CONTACT_P)]] <- 606
df.COND$COND_M[df.COND$PROFILE_ID %in% df.GEN$PROFILE_ID[grepl("Schindler",df.GEN$CONTACT_P)]] <- 808

# Romano
rom <- data.frame(CODE_M=c(607,608,609,610,611,809,810),
METHOD=c("laboratory water retention data obtained using sand-kaolin box and the Richards plate apparatus",
"laboratory water retention data obtained using method of Boels et al. (1978)",
"laboratory water retention data obtained using gamma-ray attenuation equipment and tensiometers",
"laboratory water retention data obtained using gamma-ray attenuation equipment, tensiometers and the Richards plate apparatus",
"field water retention data obtained using tensiometers and neutron thermalization method",
"laboratory hydraulic conductivity data obtained using the instantaneous profile method",
"field hydraulic conductivity data obtained using the instantaneous profile method"),
METH_REF="ND",
METH_PAR=c(rep("THETA_M",5),"COND_M","COND_M"))
# assign codes
pid <- df.GEN$PROFILE_ID[grepl("Romano",df.GEN$CONTACT_P) & (grepl("water .* box",df.GEN$COMMENTS2) | grepl("water .* box",df.GEN$COMMENTS3))]
df.RET$THETA_M[df.RET$PROFILE_ID %in% pid] <- 607
pid <- df.GEN$PROFILE_ID[grepl("Romano",df.GEN$CONTACT_P) & (grepl("water .* Boels",df.GEN$COMMENTS2) | grepl("water .* Boels",df.GEN$COMMENTS3))]
df.RET$THETA_M[df.RET$PROFILE_ID %in% pid] <- 608
pid <- df.GEN$PROFILE_ID[grepl("Romano",df.GEN$CONTACT_P) & (grepl("water.*gamma",df.GEN$COMMENTS2) | grepl("water.*gamma",df.GEN$COMMENTS3))]
df.RET$THETA_M[df.RET$PROFILE_ID %in% pid] <- 609
pid <- df.GEN$PROFILE_ID[grepl("Romano",df.GEN$CONTACT_P) & (grepl("water.*gamma.*Richards",df.GEN$COMMENTS2) | grepl("water.*gamma.*Richards",df.GEN$COMMENTS3))]
df.RET$THETA_M[df.RET$PROFILE_ID %in% pid] <- 610
pid <- df.GEN$PROFILE_ID[grepl("Romano",df.GEN$CONTACT_P) & (grepl("field.*water",df.GEN$COMMENTS2) | grepl("field.*water",df.GEN$COMMENTS3))]
df.RET$THETA_M[df.RET$PROFILE_ID %in% pid] <- 611
pid <- df.GEN$PROFILE_ID[grepl("Romano",df.GEN$CONTACT_P) & (grepl("lab.*conductivity",df.GEN$COMMENTS2) | grepl("lab.*conductivity",df.GEN$COMMENTS3))]
df.COND$COND_M[df.COND$PROFILE_ID %in% pid] <- 809
pid <- df.GEN$PROFILE_ID[grepl("Romano",df.GEN$CONTACT_P) & (grepl("field.*conductivity",df.GEN$COMMENTS2) | grepl("field.*conductivity",df.GEN$COMMENTS3))]
df.COND$COND_M[df.COND$PROFILE_ID %in% pid] <- 810

# for inversion methods, MVG parameters can be derived from df.sp[,c('MVG_SAT','MVG_RESID','MVG_ALPHA','MVG_N','MVG_M')]
mvg <- data.frame(CODE_M=c(700,900),METHOD=c(paste("van Genuchten model",paste(names(df.sp)[17:21],collapse=','),sep=':'),paste("van Genuchten model",paste(names(df.sp)[17:23],collapse=','),sep=':')),METH_REF="van Genuchten, 1980",METH_PAR=c("TH_INV_MOD","K_INV_MOD"),stringsAsFactors=FALSE)

# merge METHODS
df.METH <- rbind(bgr_th,bgr_k,bgr_basic,bgr_oc,bgr_psd,nl1,nl2,nl3,nl4,sk,sch,rom,mvg)
df.METH <- as.data.frame(df.METH,stringsAsFactors=FALSE)

# save output
#hypres_hydi<-list(general=df.GEN,basic=df.BAS.nd,chemical=df.CHEM.nd,psize=df.PS,ret=df.RET,cond=df.COND,meth=df.METH,tsermeta=NULL,tserdata=NULL)
hypres_hydi<-list(general=df.GEN,basic=df.BAS[nona,],chemical=df.CHEM,psize=df.PS,ret=df.RET,cond=df.COND,meth=df.METH,tsermeta=NULL,tserdata=NULL)

save(hypres_hydi,file='hypres_hydi.RData')


dup <- duplicated(hypres_hydi$basic[,c("PROFILE_ID","HOR1_NAME","BD")])
hypres_hydi$basic[dup,c("PROFILE_ID","SAMPLE_ID","HOR1_NAME","SAMPLE_DEP_TOP","SAMPLE_DEP_BOT","BD")]

