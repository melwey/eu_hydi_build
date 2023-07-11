# cleanDaroussin
# fit data format to guidelines
#
# Author: M.Weynants
# Date created: 2012/12/14
###################################################################
print('Clean data')

print('basic')
# samples are duplicated because 2 methods of BD: 111 and 112
basic0<-basic
# remove duplicated samples with BD==-999
basic <- basic0[!(basic0$SAMPLE_ID %in% basic0$SAMPLE_ID[duplicated(basic0$SAMPLE_ID)] & basic0$BD==-999),]
# plot two methods
# basic[basic$SAMPLE_ID %in% basic$SAMPLE_ID[duplicated(basic[,c("SAMPLE_ID","BD_M")])],c("SAMPLE_ID","BD","BD_M")]
# x<-basic$BD[basic$SAMPLE_ID %in% basic$SAMPLE_ID[duplicated(basic$SAMPLE_ID)] & basic$BD_M==111 & basic$SAMPLE_ID != 2500002301]
# y<-basic$BD[basic$SAMPLE_ID %in% # plot(x,y,xlab="111",ylab="112",title="Two methods of BD measurement");abline(0,1)
# generally: 112 > 111
# remove remaing duplicates ? keep 111 (500 cm3) and put 112 in comments1
ind.112 <- (basic$SAMPLE_ID %in% basic$SAMPLE_ID[duplicated(basic$SAMPLE_ID)] & basic$BD_M==112)
p.112 <- unique(basic$PROFILE_ID[ind.112])
for (ip in p.112) {general$COMMENTS1[general$PROFILE_ID==ip] <- paste("alternative BD:", paste(basic$SAMPLE_ID[basic$PROFILE_ID==ip & ind.112],basic$BD[basic$PROFILE_ID==ip & ind.112],sep=":",collapse=";"))}
# check length of COMMENTS1
#any(nchar(general$COMMENTS1) > 255)

basic <- basic[!(basic$SAMPLE_ID %in% basic$SAMPLE_ID[duplicated(basic$SAMPLE_ID)] & basic$BD_M==112),] 
basic$BD_M <- as.numeric(basic$BD_M)
basic$BD_M[is.na(basic$BD_M)] <- -999

print('chemical')
chemical0<-chemical
# there are 3 duplicates: 43, 45 and 47
chemical <- chemical0[!duplicated(chemical0$SAMPLE_ID),]
chemical[,'CEC_M'] <- as.numeric(chemical[,'CEC_M'])
chemical[is.na(chemical)] <- -999
chemical[,names(chemical) %in% c('EX_NA_M','ACIDITY_NA4O_M','ACIDITY_KCL_M')] <- -999
# remove BASE_CATIONS_M
chemical <- chemical[,!grepl("BASE_CATIONS_M",names(chemical))]
#
