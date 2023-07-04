# cleanJavaux
# fit data format to guidelines
#
# Author: M.Weynants
# Date created: 2012/12/12
###################################################################
print('Clean data')
print(' general')
general <- general[,2:66]
#general$PROFILE_ID <- as.numeric(gsub('56','560',as.character(general$PROFILE_ID)))
general$REL_T_SER <- -999

print('basic')
#basic$PROFILE_ID <- as.numeric(gsub('56','560',as.character(basic$PROFILE_ID)))
#basic$SAMPLE_ID <- as.numeric(gsub('56','560',as.character(basic$SAMPLE_ID)))
basic$STR_COMB <- as.character(basic$STR_COMB)
basic$POR_M <- -999
names(basic)[22:23]<-c('COARSE','COARSE_M')

print('chemical')
#chemical$PROFILE_ID <- as.numeric(gsub('56','560',as.character(chemical$PROFILE_ID)))
#chemical$SAMPLE_ID <- as.numeric(gsub('56','560',as.character(chemical$SAMPLE_ID)))

# psize
#psize$PROFILE_ID <- as.numeric(gsub('56','560',as.character(psize$PROFILE_ID)))
#psize$SAMPLE_ID <- as.numeric(gsub('56','560',as.character(psize$SAMPLE_ID)))

# ret
#ret$PROFILE_ID <- as.numeric(gsub('56','560',as.character(ret$PROFILE_ID)))
#ret$SAMPLE_ID <- as.numeric(gsub('56','560',as.character(ret$SAMPLE_ID)))


# cond
#cond$PROFILE_ID <- as.numeric(gsub('56','560',as.character(cond$PROFILE_ID)))
#cond$SAMPLE_ID <- as.numeric(gsub('56','560',as.character(cond$SAMPLE_ID)))
names(cond)[15] <- "K_INV_P9"
print('done')