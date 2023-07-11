# cleanCranfield
# 
# Author: M.Weynants
# Date created: 2013/03/20
#####################################################################

# GENERAL
general0 <- general
general <- cbind(general0[,1:36],GENERAL[,37:50],general0[,41:55])
# # WRB1998
# general$WRB1998_RSG <- general0$WRB_GRP
# adj1 <- substr(general0$WRB_ADJ,1,2); adj[adj1=="lp"]<-"le"
# adj2 <- substr(general0$WRB_ADJ,5,6)
# spe <- c('d','c','n','p','h','w','o','r','t','b')[match(general0$WRB_SPE,c('bathi','cumuli','endo','epi','hyper','hypo','orthi','para','proto','thapto'))]
# spe[is.na(spe)] <- ""
# general$WRB1998_ADJSPE1 <- paste(adj1,spe,sep="")
# general$WRB1998_ADJSPE2 <- adj2
# #cbind(general0$WRB2006,general0$WRB_GRP,adj1,spe,adj2,general$WRB1998_RSG,general$WRB1998_ADJSPE1,general$WRB1998_ADJSPE2)
# wrb2006
source("wrb.r") # RSG, ADJ2006, ADJ1998, SPE1998
wrb.full <- general0$WRB2006
# corrections:
wrb.full<-gsub("Calcric","Calcaric",wrb.full)
wrb.full<-gsub("Stagic","Stagnic",wrb.full)
wrb.full<-gsub("Profundic","Profondic",wrb.full)
wrb.full <- gsub("Endolithic","Lithic",wrb.full)
fao<-strsplit(tolower(gsub(",","",wrb.full))," ")
tmp2 <- tmp <- matrix('ND',nrow=length(fao),ncol=3)
for (x in 1:length(fao)){
if (length(fao[[x]])>=2){
for (iadj in 1:(length(fao[[x]])-1)){
tmp[x,iadj] <- fao[[x]][iadj]
tmp2[x,iadj] <- ADJ2006[match(tmp[x,iadj], tolower(ADJ2006$name)),'code']
}
}}
#cbind(wrb.full,as.data.frame(tmp),as.data.frame(tmp2))
general$WRB2006_RSG <- general0$WRB_GRP
general[,c("WRB2006_PQ1","WRB2006_PQ2")] <- tmp2[,1:2]
# missing values to right format
general[,c(37:52,56:65)][is.na(general[,c(37:52,56:65)])] <- "ND"
general[,c(21,25,33)] <- -999
general[is.na(general)] <- -999
# assign new profile id
general$COMMENTS1 <- pid <- general$PROFILE_ID
# starting from max of Lilly (Scotland): 44
general$PROFILE_ID <- 82600044 +(1:nrow(general))
# LC_L1
lc <- general$LC_L1
general$LC_L1 <- paste(substr(lc,1,1),"00",sep="")
general$LC_L1[lc=="0"]<-"ND"
general$LC_L2 <- paste(substr(lc,1,2),"0",sep="")
general$LC_L2[lc=="0"]<-"ND";general$LC_L2[substr(lc,2,2)=="0"]<-"ND"
general$LC_L3 <- lc
general$LC_L3[lc=="0"]<-"ND";general$LC_L3[substr(lc,3,3)=="0"]<-"ND"
# PAR_MAT: keep first
#table(general$PAR_MAT[nchar(general$PAR_MAT)>4])
general$PAR_MAT <- as.numeric(substr(general$PAR_MAT,1,4))
general$PAR_MAT[general0$PAR_MAT =="5310 / 6110 / 6120 / 6210 / 6220"] <- 6000
general$PAR_MAT[general0$PAR_MAT =="7100 / 7200"] <- 7000
general$PAR_MAT[general0$PAR_MAT =="2114 / 2150"] <- 2150
general$COMMENTS2 <- paste("PAR_MAT ",general0$PAR_MAT,sep="")

# method
meth0 <- meth
meth$METH_REF <- "ND"
ind <- grepl("Avery",meth$METHOD)
meth[ind,"METH_REF"] <- meth[ind,"METHOD"]
meth[ind,"METHOD"] <- "ND"

# basic
basic0 <- basic
basic <- cbind(basic0[,1:2],basic0[,8],basic0[,3:4],basic0$HOR_NAME,basic0[,5:6],BASIC[,9:17],basic0[,12:17],stringsAsFactors=FALSE)
names(basic) <- names(BASIC)
# assign PROFILE_ID and SAMPLE_ID
basic$PROFILE_ID <- general$PROFILE_ID[match(basic0$PROFILE_ID,general0$PROFILE_ID)]
basic$SAMPLE_ID <- basic$PROFILE_ID *100 + basic0$HOR_POS
###################
# convert HOR_NAME
basic$HOR1_NAME <- substr(paste(basic0$HOR_NAME,"       "),start=1,stop=7)
# insert " " when two master horizon
ind <- grepl("^.[[:upper:]][[:upper:]]",basic$HOR1_NAME)
basic$HOR1_NAME[ind] <- paste(substr(basic$HOR1_NAME[ind],1,2)," ",substr(basic$HOR1_NAME[ind],3,6),sep="")
basic$HOR1_NAME[basic$HOR1_NAME=="  AB   "] <- " A B   "
# insert spaces when none |b| unique Master horizon and designation 
ind <- grepl("^.[[:upper:]][[:lower:]]",basic$HOR1_NAME)
basic$HOR1_NAME[ind] <- paste(substr(basic$HOR1_NAME[ind],1,2),"  ",substr(basic$HOR1_NAME[ind],3,5),sep="")
# insert spaces when master folowed by digit
ind <- grepl("^.[[:upper:]][[:digit:]]",basic$HOR1_NAME)
basic$HOR1_NAME[ind] <- paste(substr(basic$HOR1_NAME[ind],1,2),"    ",substr(basic$HOR1_NAME[ind],3,3),sep="")
# move horizon designation
ind <- grepl("^.[[:upper:]].[[:lower:]]",basic$HOR1_NAME)
basic$HOR1_NAME[ind] <- paste(substr(basic$HOR1_NAME[ind],1,3)," ",substr(basic$HOR1_NAME[ind],4,6),sep="")
# move vertical designation
ind <- grepl("^.[[:upper:]].[[:digit:]]",basic$HOR1_NAME)
basic$HOR1_NAME[ind] <- paste(substr(basic$HOR1_NAME[ind],1,3),"   ",substr(basic$HOR1_NAME[ind],4,4),sep="")
# move misplaced vertical designation
ind <- grepl("^.{5}[[:digit:]]",basic$HOR1_NAME)
basic$HOR1_NAME[ind] <- paste(substr(basic$HOR1_NAME[ind],1,5)," ",substr(basic$HOR1_NAME[ind],6,6),sep="")
# remove x as third sub horizon designation
ind <- grepl("^.{6}x",basic$HOR1_NAME)
basic$HOR1_NAME[ind] <- paste(substr(basic$HOR1_NAME[ind],1,6)," ",sep="")
#
basic[,c(9,12,15:17)] <- 'ND'
basic[,c(10,11,13,14,22,23)] <- -999
basic[is.na(basic)] <- -999
# Structure1
source("fao_structure.r")#grade,size,type
str1 <- str2 <- rep("ND",nrow(basic)); str_comb <- rep("0",nrow(basic))
ind <- basic0$STR_SHAPE=="platy and subangularblocky"
str2[ind]<-"WEMESB"
str_comb[ind]<-"+"
basic0$STR_SHAPE[ind]<-"platy"
str1 <- paste(grade$code[match(basic0$STR_GRADE,grade$name)],
		size$code[match(basic0$STR_SIZE,size$name)],
		type$code[match(basic0$STR_SHAPE,type$name)],sep="")
str1 <- gsub("NA","ND",str1)
tmp<-cbind(basic0[,9:11],str1)
# tmp[grepl("ND",str1) & !grepl("^[[:space:]]*$",tmp[[1]]) & !grepl("^[[:space:]]*$",tmp[[2]]) & !grepl("^[[:space:]]*$",tmp[[3]]),]
basic[,c("STRUCTURE1","STR_COMB","STRUCTURE2")] <- cbind(str1,str_comb,str2)

# chemical
chemical0 <- chemical
names(chemical)[2] <- 'SAMPLE_ID'
# assign PROFILE_ID and SAMPLE_ID
chemical$PROFILE_ID <- general$PROFILE_ID[match(chemical0$PROFILE_ID,general0$PROFILE_ID)]
chemical$SAMPLE_ID <- basic$SAMPLE_ID[match(chemical0$LAYER_ID,basic0$HORIZON_ID)]
#
chemical <- chemical[,!grepl('BASE_CATIONS_M',names(chemical))]
# set *-M of missing data to -999: done in QA_corr.r
#chemical$ACIDITY_NA4O_M[chemical$ACIDITY_NA4O==-999] <- -999
ind <- chemical$PH_KCL>10
chemical$PH_KCL[ind]<-chemical$PH_KCL[ind]/10

# psize
psize0 <- psize
names(psize)[2]<-'SAMPLE_ID'
# assign PROFILE_ID and SAMPLE_ID
psize$PROFILE_ID <- general$PROFILE_ID[match(psize0$PROFILE_ID,general0$PROFILE_ID)]
psize$SAMPLE_ID <- basic$SAMPLE_ID[match(psize0$LAYER_ID,basic0$HORIZON_ID)]
psize$P_PERCENT <- as.numeric(psize$P_PERCENT)
# 
# P_PERCENT < 1
psize[is.na(psize$P_PERCENT),"P_PERCENT"] <- 0
usid <- unique(psize$SAMPLE_ID)
p_sum <- sapply(usid,function(x){sum(psize$P_PERCENT[psize$SAMPLE_ID == x])})
# remove empty curves (organic soils : OC > 30%)
psize <- psize[!psize$SAMPLE_ID %in% usid[p_sum==0],]


# ret
ret0<-ret
names(ret)[2] <- "SAMPLE_ID"
# assign PROFILE_ID and SAMPLE_ID
ret$PROFILE_ID <- general$PROFILE_ID[match(ret0$PROFILE_ID,general0$PROFILE_ID)]
ret$SAMPLE_ID <- basic$SAMPLE_ID[match(ret0$LAYER_ID,basic0$HORIZON_ID)]
ret[is.na(ret)] <- -999

# cond
cond0<-cond
names(cond)[1:2] <- c("PROFILE_ID","SAMPLE_ID")
# assign PROFILE_ID and SAMPLE_ID
cond$PROFILE_ID <- general$PROFILE_ID[match(cond0$PROFILE_ID,general0$PROFILE_ID)]
cond$SAMPLE_ID <- basic$SAMPLE_ID[match(cond0$LAYER_ID,basic0$HORIZON_ID)]
cond[is.na(cond)] <- -999