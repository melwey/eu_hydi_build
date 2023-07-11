# daroussin_corr
#
# Author: M.Weynants
# Date created: 2013/01/30
######################################################################
# general
names(general1) <- gsub("SLOPE","SLOP",names(general1))
# wrb2006_pq1: one "l"
general1$WRB2006_PQ1 <- sub("^l$","lv",general1$WRB2006_PQ1)
# wrb2006_sq1: one "sr"
general1$WRB2006_SQ1 <- sub("sr","st",general1$WRB2006_PQ1)

general1 <- general.checks(general1)

# method: OK

# basic
# HOR1_NAME
ind <- nchar(basic1$HOR1_NAME) != 7
basic1$HOR1_NAME[ind] <- gsub(' ','',basic1$HOR1_NAME[ind])
# unique(basic1$HOR1_NAME[nchar(basic1$HOR1_NAME)<7])
ind <- grepl("^[[:upper:]][[:lower:]][[:lower:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind] <- paste(' ',substr(basic1$HOR1_NAME[ind],1,1),'  ',substr(basic1$HOR1_NAME[ind],2,3),' ',sep='')
ind <- grepl("^[[:upper:]][[:lower:]][[:digit:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind] <- paste(' ',substr(basic1$HOR1_NAME[ind],1,1),'  ',substr(basic1$HOR1_NAME[ind],2,2),' ',substr(basic1$HOR1_NAME[ind],3,3),sep='')
ind <- grepl("^[[:upper:]]$",basic1$HOR1_NAME)
basic1$HOR1_NAME[ind] <- paste(' ',basic1$HOR1_NAME[ind],'     ',sep='')
basic1$HOR1_NAME[basic1$HOR1_NAME == "E/Bg"] <- " E/Bg  "
basic1$HOR1_NAME[basic1$HOR1_NAME == "2B1"] <- "2B    1"
basic1$HOR1_NAME[basic1$HOR1_NAME == "2Bg2"] <- "2B  g 2"
basic1$HOR1_NAME[basic1$HOR1_NAME == "B1"] <- " B    1"
basic1$HOR1_NAME[basic1$HOR1_NAME == "B/C"] <- " B/C   "

basic1<-basic.checks(basic1,general1$PROFILE_ID,meth1$CODE_M)

# chemical: OK
# psize: 20 data from PROFILE_ID 25000023 to be removed
psize1 <- psize1[psize1$PROFILE_ID != 25000023,]

# ret
# one THETA >1: obviously a factor 10
print( ret1[ret1$SAMPLE_ID %in% ret1$SAMPLE_ID[ret1$THETA > 1],1:5])
ret1$THETA[ret1$THETA > 1] <- 0.1*ret1$THETA[ret1$THETA > 1]
# remove THETA==-999
ret1 <- ret1[ret1$THETA != -999,]

ret1<-ret.checks(ret1,basic1$SAMPLE_ID,meth1$CODE_M)

# cond: no data
#