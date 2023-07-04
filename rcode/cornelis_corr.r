# cornelis_corr
# 
# Author: M.Weynants
# Date created: 2013/01/18
#################################################################
# general
# wgs84: x and y inverted
tmp <- general1$X_WGS84
general1$X_WGS84 <- general1$Y_WGS84
general1$Y_WGS84 <- tmp
# LC_L1
s3 <- general1$LC_L1

# LU_L1

#
general1$NAT_CLAS_REF <- 'Belgian soil classification'


# method
meth1$METH_PAR[meth1$METH_PAR=='EC']<-'EC_M'

# basic
basic1[,c('SAMPLE_DEP_TOP','SAMPLE_DEP_BOT')]<-round(basic1[,c('SAMPLE_DEP_TOP','SAMPLE_DEP_BOT')])
# table(nchar(basic1$HOR1_NAME[!grepl('ND',basic1$HOR1_NAME)]))
basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==6] <- paste(substr(basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==6],1,2),' ',substr(basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==6],3,6),sep='')
basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==5] <- paste(substr(basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==5],1,2),' ',substr(basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==5],3,5),' ',sep='')
basic1$HOR1_NAME[basic1$HOR1_NAME=='Bhir']<-' B  hi '
basic1$HOR1_NAME[grepl('sub',basic1$HOR1_NAME)] <- 'ND'
basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==4] <- paste(substr(basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==4],1,2),' ',substr(basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==4],3,4),'  ',sep='')
basic1$HOR1_NAME[basic1$HOR1_NAME=='Bir']<-' B  ir '
basic1$HOR1_NAME[basic1$HOR1_NAME=='B2t']<-' B  t 2'
basic1$HOR1_NAME[basic1$HOR1_NAME==' AB']<-' A B   '
basic1$HOR1_NAME[basic1$HOR1_NAME=='2Eg']<-'2E  g  '
ind <- grepl("^[[:upper:]][[:digit:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind]<-paste(' ',substr(basic1$HOR1_NAME[ind],1,1),'    ',substr(basic1$HOR1_NAME[ind],2,2),sep='')
ind <- grepl("^[[:upper:]][[:lower:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind]<-paste(' ',substr(basic1$HOR1_NAME[ind],1,1),'  ',substr(basic1$HOR1_NAME[ind],2,2),'  ',sep='')
ind <- grepl("^ [[:upper:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind]<-paste(basic1$HOR1_NAME[ind],'     ',sep='')
basic1$HOR1_NAME[basic1$HOR1_NAME=='BC'] <- ' B C   '
basic1$HOR1_NAME[basic1$HOR1_NAME=='2C'] <- '2C     '
basic1$HOR1_NAME[basic1$HOR1_NAME=='e'] <- 'E'
basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==1]<-paste(' ',basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)==1],'     ',sep='')
# structure
basic1$STRUCTURE1<- gsub('-','to',basic1$STRUCTURE1)
grade <- data.frame(name=c('weak','moderate','strong','weak moderate','moderate strong'),code=c('WE','MO','ST','WM','MS'),stringsAsFactors=FALSE)
type <- data.frame(name=c('rock structure','stratified structure','single grain','massive','prismatic','blocky','angular blocky','parallelipiped','angular and subangular blocky','angular wedge','subangular and angular blocky','subangular blocky','nuty','prismatic','subangular prismatic','wedge-shaped','columnar','granular','worm casts','platy','cloddy','crumb','lumpy'),code=c('RS','SS','SG','MA','PM','BL','AB','AP','AS','AW','SA','SB','SN','PR','PS','WE','CO','GR','WC','PL','CL','CR','LU'),stringsAsFactors=FALSE)
size <- data.frame(name=c('very fine','fine','medium','coarse','very coarse','v.coarse','extremely coarse','very fine and fine','very fine to medium','fine and medium','fine to coarse','medium and coarse','medium to very coarse','coarse to very coarse'),code=c('VF','FI','ME','CO','VC','VC','EC','FF','VM','FM','FC','MC','MV','CV'),stringsAsFactors=FALSE)
#
gd <- sapply(grade$name,function(x){grepl(x,tolower(basic1$STRUCTURE1))})
gd1 <- sapply(1:nrow(basic1),function(x){if (any(gd[x,])){y<-grade$code[max(which(gd[x,]))]} else y<-'ND'})
#
ty <- sapply(type$name,function(x){grepl(x,tolower(basic1$STRUCTURE1))})
ty1 <- sapply(1:nrow(basic1),function(x){if (any(ty[x,])){y<-type$code[max(which(ty[x,]))]} else y<-'ND'})
# 
sz <- sapply(size$name,function(x){grepl(x,tolower(basic1$STRUCTURE1))})
sz1 <- sapply(1:nrow(basic1),function(x){if (any(sz[x,])){y<-size$code[max(which(sz[x,]))]} else y<-'ND'})
sz1[sz[,1]] <- 'VF'
#
#basic1$STRUCTURE1
struc <- paste(gd1,sz1,ty1,sep='')
struc[146]<-'NDMAND'
basic1$STRUCTURE1 <- struc
basic1$STR_COMB[146] <- '-'
basic1$STRUCTURE2[146] <- 'NDCOPL'

basic1$POR <- -999
basic1<-basic.checks(basic1,general1$PROFILE_ID,meth1$CODE_M)

# chemical
chemical1$CACO3_M[chemical1$CACO3 != -999] <- 140
chemical1$PH_H2O_M[chemical1$PH_H2O != -999] <- 150
chemical1$PH_KCL_M[chemical1$PH_KCL != -999] <- 160
chemical1$EC_M[chemical1$EC != -999] <- 170
chemical1$CEC_M[chemical1$CEC != -999] <- 180
chemical1 <- chemical.checks(chemical1,basic1$SAMPLE_ID,meth1$CODE_M)

# psize
psize1 <- psize.checks(psize1,basic1$SAMPLE_ID,meth1$CODE_M)

# ret
ret1[ret1$THETA>1,'THETA']<- -999
ret1 <- ret1[ret1$THETA != -999,]
# Correction of duplicate
ret1[ret1$SAMPLE_ID==560006101 & ret1$HEAD== 15300,1:2] <- c(5600060,5600061,560006001,560006101)
ret1 <- ret.checks(ret1,basic1$SAMPLE_ID,meth1$CODE_M)

#cond
cond1 <- cond.checks(cond1,basic1$SAMPLE_ID,meth1$CODE_M)

