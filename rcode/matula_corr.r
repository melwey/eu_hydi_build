# matula_corr
# 
# Author: M.Weynants
# Date created: 2013/01/17
################################################################
# general
# profile_id
# general[general$PROFILE_ID %in% general$PROFILE_ID[duplicated(general$PROFILE_ID)],1:5]
general1 <- general1[!duplicated(general$PROFILE_ID),]
lc <- general1$LC_L1
lc1 <- substr(lc,1,1)
lc2 <- substr(lc,1,2)
general1$LC_L1[!grepl('ND',lc)] <- paste(lc1[!grepl('ND',lc)],'00',sep='')
general1$LC_L2[substr(lc,2,2)!='0' & !grepl('ND',lc)] <- paste(lc2[substr(lc,2,2)!='0' & !grepl('ND',lc)],'0',sep='')
general1$LC_L3[substr(lc,3,3) != '0' & !grepl('ND',lc)] <- lc[substr(lc,3,3) != '0' & !grepl('ND',lc)]
# 2023/04/18 
general1$NAT_CLAS_REF[!grepl("ND",general1$NAT_CLAS_REF)] <- "Němeček, J., Macků, J., Vokoun, J., Vavříček, D., Novák, P. (2001): Taxonomic Classification System of Soils of the Czech Republic. ČZU Praha. 79 p. ISBN 80-238-8061-6. (in Czech)"

names(general1) <- gsub("SLOPE","SLOP",names(general1))

general1 <- general.checks(general1)

# method
# OK

# basic
#any(basic1$SAMPLE_DEP_TOP>basic1$SAMPLE_DEP_BOT)
hor <- basic1$HOR1_NAME
hor[nchar(hor)==1] <- paste(' ',hor[nchar(hor)==1],'     ',sep='')
hor[hor=='hP'] <- ' P  h  '
ind<-nchar(hor)==2 & !grepl('ND',hor)
hor[ind] <- paste(' ',substr(hor[ind],1,1),'  ',substr(hor[ind],2,2),'  ',sep='')
hor[hor=='Bm/Cg']  <- ' B/Cmg '
hor[hor=='Bv/C']   <- ' B/Cv  '
hor[hor=='Bvg/Cg'] <- ' B/Cvg '
hor[hor=='Bv/Ck']  <- ' B/Cvk '
hor[hor=='Bn/C']   <- ' B/Cn  '
hor[hor=='BCg']    <- ' B Cg  '
# M: mineral layer (extended topsoil), subject to bioturbation in tropical soil -> ?
# P: peat horizon -> H
hor<-gsub('P','H',hor)
# D:  soil material below the solum that is unlike the solum in general character, is not C horizon, and cannot be given reliable designation -> '1C     '
hor <- gsub(' D','1C',hor)

hor -> basic1$HOR1_NAME
basic1<-basic.checks(basic1,general1$PROFILE_ID,meth1$CODE_M)

# chemical
chemical1 <- chemical.checks(chemical1,basic1$SAMPLE_ID,meth1$CODE_M)

# psize
psize1 <- psize.checks(psize1,basic1$SAMPLE_ID,meth1$CODE_M)

# ret
ret1 <- ret.checks(ret1,basic1$SAMPLE_ID,meth1$CODE_M)

# cond
cond1 <- cond[cond$COND!=-999 & cond$K_INV_P1!=-999,]
cond1 <- cond.checks(cond1,basic1$SAMPLE_ID,meth1$CODE_M)
