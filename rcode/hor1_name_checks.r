x<-basic#hydi$BASIC
(any(!substr(x[!grepl('ND',x[[6]]),6],start=1,stop=1) %in% c(' ',as.character(1:9),'b')))
# master horizon
mh <- c('H', 'O', 'A', 'E', 'B', 'C', 'R', 'I', 'L', 'W', ' ','')
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=2,stop=2) %in% mh)) warning ("HOR1_NAME's second character must be a valid master horizon")
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=3,stop=3) %in% c('/',' '))) {
table(x[!substr(x[!grepl('ND',x[[6]]),6],start=3,stop=3) %in% c('/',' '),6])}
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=4,stop=4) %in% mh)) warning ("HOR1_NAME's fourth character must be a valid master horizon")
# subhorizon designations
sd <- c(letters,'@',' ')
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=5,stop=5) %in% sd)) warning ("HOR1_NAME's fifth character must be a valid horizon designation")
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=6,stop=6) %in% sd)) warning ("HOR1_NAME's sixth character must be a valid horizon designation")
# vertical subdivision
if (any(!substr(x[!grepl('ND',x[[6]]),6],start=7,stop=7) %in% c(1:9,' '))) warning ("HOR1_NAME's seventh character must be a valid horizon's vertical subdivision designation")
