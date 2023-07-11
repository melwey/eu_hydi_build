
# example *.mdb
require("RODBC")
# integer for id's
# transform PROFILE_ID and SAMPLE_ID into character
V <- c("integer","character","numeric","logical")
getSqlTypeInfo()
A <- c("INTEGER","VARCHAR","DOUBLE","VARCHAR(5)")
varTypeR<-varTypeA<-list()
for (i in 1:length(hydi)){
	varTypeR[[i]]<- varTypeA[[i]] <- rep(NA,length(hydi[[i]]))
	for (j in 1:length(hydi[[i]])){
		if ((i==1 & j==1) | (i>1 & j%in%c(1,2))){hydi[[i]][[j]] <-as.character(hydi[[i]][[j]])}
		v <- class(hydi[[i]][[j]])
		varTypeR[[i]][j] <- v
		# transform from R into Access variable type
		a <- A[grepl(v,V)]
		if (is.character(hydi[[i]][[j]])){a<-paste(a,"(",min(max(nchar(hydi[[i]][[j]])),255),")",sep="")}
		varTypeA[[i]][j] <- a
	}
attr(varTypeA[[i]],"names") <- names(hydi[[i]])
}
names(varTypeA)<-names(hydi)

# # as.numeric RET$FLAG
# hydi$RET$FLAG <- as.numeric(hydi$RET$FLAG)

# open connection with database
# require("DBI")
# drv <- dbDriver("RODBC")
# con <- dbConnect(drv, dbname="myNewDb.sqlite")
# file.remove("../HYDI-v1.accdb")

# ch <- odbcDriverConnect("HYDI-v1.accdb")
ch <- odbcConnectAccess2007("../output/HYDI-v1.accdb")
for (k in 1:length(hydi)){
print("---------")
print(names(hydi)[k])
sqlDrop(ch, names(hydi)[k])
sqlSave(ch, hydi[[k]], tablename = names(hydi)[k], append = FALSE,
        rownames = FALSE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE,varTypes=varTypeA[[k]])
}
#}}
odbcClose(ch)

# compact database:
#system2("C:\\Program Files\\Microsoft Office\\Office14\\MSACCESS.EXE" , "E:\\weyname\\Documents\\Documents\\EU_HYDI\\ContributedData\\HYDI-v1.accdb /compact")
