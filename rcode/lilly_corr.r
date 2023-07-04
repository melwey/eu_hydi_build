# lilly_corr
#
# Author: M. Weynants
# Date created: 2012/12/19
######################################################################
# GENERAL
tmp <- general1$X_WGS84
general1$X_WGS84 <- general1$Y_WGS84
general1$Y_WGS84 <- tmp
# LC
general1$LC_L2[general1$LC_L2==''] <- 'ND'
general1$LC_L3[general1$LC_L3==''] <- 'ND'
# SITE_SLOPE_FORM
general1$SITE_SLOPE_FORM <- gsub('C-','C',general1$SITE_SLOPE_FORM)
general1$SITE_SLOPE_FORM <- gsub('----','ND',general1$SITE_SLOPE_FORM)
# SRF_SAL_COV
general1$SRF_SAL_COV <- -999
#WRB2006
general1$WRB2006_RSG <- gsub('Stagnosol','ST',general1$WRB2006_RSG)
general1$WRB2006_RSG <- gsub('Cambisol','CM',general1$WRB2006_RSG)
general1$WRB2006_PQ1 <- gsub('haplic','ha',general1$WRB2006_PQ1)
general1$WRB2006_PQ1 <- gsub('fragic','fg',general1$WRB2006_PQ1)
general1$WRB2006_PQ1 <- gsub('stagnic','st',general1$WRB2006_PQ1)
general1$WRB2006_PQ1 <- gsub('endogleyic','ng',general1$WRB2006_PQ1)
general1$WRB2006_PQ1 <- gsub('gleyic','gl',general1$WRB2006_PQ1)
general1$WRB2006_PQ1 <- gsub('anthic','am',general1$WRB2006_PQ1)
general1$WRB2006_PQ2[general1$WRB2006_PQ2 == ''] <- 'ND'
general1$WRB2006_PQ2 <- gsub('stagnic','ha',general1$WRB2006_PQ2)
general1$WRB2006_PQ3 <- 'ND'
general1$WRB2006_SQ1 <- gsub('albic','ab',general1$WRB2006_SQ1)
general1$WRB2006_SQ1 <- gsub('eutric','eu',general1$WRB2006_SQ1)
general1$WRB2006_SQ1 <- gsub('dystric','dy',general1$WRB2006_SQ1)
general1$WRB2006_SQ2[general1$WRB2006_SQ2 == ''] <- 'ND'
general1$WRB2006_SQ2 <- gsub('eutric','eu',general1$WRB2006_SQ2)
general1$WRB2006_SQ3 <- 'ND'

general1 <- general.checks(general1)

# METH
meth1$METH_PAR <- toupper(meth1$METH_PAR)
meth1 <- meth.checks(meth1)

# BASIC
# HOR1_NAME
# must be completely recoded!
old <- unique(basic1$HOR1_NAME)
new <- c(' A  pg ',' B  g  ',' B  gx ',' C  g  ',' B  gs ',' A  p  ',' A B   ',' B  gx ',' B  gx1',' B  gx2',' C  g  ',' A Bg  ','2C  g  ',' B  s  ',' B  xg ',' B  g 1',' B  g 2',' B  g  ',' B Cg  ',' E  g  ',' C  gx ',' A  pg1',' B Cg 1',' B     ',' B  x  ','2C     ',' A     ')
basic1$HOR1_NAME <- sapply(basic1$HOR1_NAME,function(x){new[match(x,old)]})
# STRUCTURE
basic1$STRUCTURE1[basic1$STRUCTURE1 == ''] <- 'ND'
basic1$STRUCTURE1[basic1$STRUCTURE1 == 'MA'] <- '    MA'
basic1$STRUCTURE1[basic1$STRUCTURE1 == 'SG'] <- '    SG'
basic1$STR_COMB[basic1$STR_COMB == "+'"] <- '+'
basic1$STR_COMB[basic1$STR_COMB == ""] <- '0'
basic1$STRUCTURE2[basic1$STRUCTURE2 == ''] <- 'ND'
basic1$STRUCTURE2[basic1$STRUCTURE2 == 'MA'] <- '    MA'
basic1$STRUCTURE2[basic1$STRUCTURE2 == 'SG'] <- '    SG'

basic1$STRUCTURE1 <- gsub('VWE','WE',basic1$STRUCTURE1)
basic1$STRUCTURE2 <- gsub('VW','WE',basic1$STRUCTURE2)

basic1$BD_M[basic1$BD == -9] <- -999
basic1$BD[basic1$BD == -9] <- -999

basic1 <- basic.checks(basic1,general$PROFILE_ID,meth1$CODE_M)

# CHEMICAL
chemical[chemical1$ACIDITY_NA4O > chemical$CEC,c(1:2,20,15,25:27)]
chemical1 <- chemical.checks(chemical1,basic1$SAMPLE_ID,meth1$CODE_M)

# PSIZE
psize1$P_PERCENT[psize$P_PERCENT == -9] <- -999
# sum check corrections (se Copy of LILLY PSIZE issues.xls)
#psize1[psize1$SAMPLE_ID %in% c(8260003202,8260001101,8260002003),]
psize1$P_PERCENT[psize1$SAMPLE_ID == 8260001101 & psize1$P_SIZE == 2000] <- 51
psize1$P_PERCENT[psize1$SAMPLE_ID == 8260003202 & psize1$P_SIZE == 60] <- 29
psize1$P_PERCENT[psize1$SAMPLE_ID == 8260002003 & psize1$P_SIZE == 2000] <- 52
psize1 <- psize.checks(psize1,basic1$SAMPLE_ID,meth1$CODE_M)

# COND
# remove samples that are not in basic
cond1 <- cond1[cond1$PROFILE_ID %in% chemical1$PROFILE_ID,]
cond1 <- cond.checks(cond1,basic1$SAMPLE_ID,meth1$CODE_M)
