# goncalves_corr.r
# Apply corrections on tables from Goncalves after running the checkss
#
# Author: M.Weynants
# Date created: 2012/11/28
#####################################################################

# BASIC corrections
# Run on basic1 produced by basic.checks(basic,general1[[1]]) for Portugal: PROFILE_ID must refer to Portugal
if (!exists('basic1')) stop('basic_goncalves_corr.r runs inside CreateHYDI.R and needs basic1 to be defined')
if (!grepl('620',substr(as.character(basic1[1,1]),start=1,stop=3))) stop('goncalves_corr.r runs data from Portugal')
# SAMPLE_POS must be recoded
# in each profile, sort sample top depth and recode sample position
sapply(unique(basic1[[1]]),function(x){
basic1[basic1[[1]]==x,'SAMPLE_POS'] <- order(basic1[basic1[[1]]==x,'SAMPLE_DEP_TOP'])})
# STR_COMB
basic1[691,16] <- '-'

# CHEMICAL corrections
if (!exists('chemical1')) stop('goncalves_corr.r runs inside CreateHYDI.R and needs chemical1 to be defined')
# CACO3_M
chemical1[474,'CACO3_M'] <- -999
# SALT
chemical1[119,'SALT'] <- -999
# EX_K: negative value are not realistic
chemical1[569,'EX_K'] <- 0
chemical1[569,'BASE_CATIONS'] <- sum(chemical1[569,c('EX_NA','EX_MG','EX_K','EX_CA')])

# corrections according to Goncalves
chemical1[chemical1$SAMPLE_ID==6200001701,c("EX_MG","BASE_CATIONS")]<-c(0.01,0.20)
chemical1[chemical1$SAMPLE_ID==6200025402,c("EX_K","BASE_CATIONS")]<-c(0.05,16.80)
# BASE_CATIONS should be removed from samples that do not have all major cations:
chemical1$BASE_CATIONS[chemical1$EX_CA==-999 | chemical1$EX_MG==-999 | chemical1$EX_K==-999 | chemical1$EX_NA==-999] <- -999
