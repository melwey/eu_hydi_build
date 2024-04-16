# Basic statistics on EU-HYDI
load("../output/EUHYDI_NA_v1_1.Rdata") # hydi.na
attach(hydi.na)

cat("# Basic statistics on EU-HYDI \n\n", file = "../euhydi_stats.md")

cat("## DATABASE COMPLETENESS (in %) \n\n", file = "../euhydi_stats.md", append = TRUE)
cat("### -- GENERAL -- \n\n", file = "../euhydi_stats.md", append = TRUE)
Np <- nrow(GENERAL)
cat(paste("Number of entries:",Np,"\n\n"), file = "../euhydi_stats.md", append = TRUE)
cat("*Minimal requirements* \n\n", file = "../euhydi_stats.md", append = TRUE)
# coordinates
ind <- !is.na(GENERAL$X_WGS84)
pcor <- sum(ind)/Np*100
cat(paste("Geographical coordinates (mostly degraded, hence not suitable for high resolution spatial analyses):",round(pcor,digit=2),"%\n\n"), file = "../euhydi_stats.md", append = TRUE)

cat("*Other data* \n\n", file = "../euhydi_stats.md", append = TRUE)
pG <- pcor
count=1
for (icol in 7:60){
count <- count +1
pG[count] <- round(sum(!is.na(GENERAL[,icol]))/Np*100,digit=2)
cat(paste(names(GENERAL)[icol],":",pG[count],"%\n\n"), file = "../euhydi_stats.md", append = TRUE)
}
attr(pG,"name") <- c("coordinates",names(GENERAL)[7:60])
#print(summary(GENERAL))

cat("### -- BASIC --\n\n", file = "../euhydi_stats.md", append = TRUE)
Ns <- nrow(BASIC)
cat(paste("Number of entries:",Ns, "\n\n"), file = "../euhydi_stats.md", append = TRUE)
cat("*Minimal requirements* \n\n", file = "../euhydi_stats.md", append = TRUE)
pB <- NA
for (icol in c(4,5,20:23)){
pB[icol] <- round(sum(!is.na(BASIC[,icol]))/Ns*100,digit=2)
cat(paste(names(BASIC)[icol],":",pB[icol],"%\n\n"), file = "../euhydi_stats.md", append = TRUE)
}
cat("*Other data* \n\n", file = "../euhydi_stats.md", append = TRUE)
for (icol in c(1:3,6:19)){
pB[icol] <- round(sum(!is.na(BASIC[,icol]))/Ns*100,digit=2)
cat(paste(names(BASIC)[icol],":", pB[icol],"%\n\n"), file = "../euhydi_stats.md", append = TRUE)
}
attr(pB,"name") <- names(BASIC)

cat("### -- CHEMICAL --\n\n", file = "../euhydi_stats.md", append = TRUE)
N <- nrow(CHEMICAL)
cat(paste("Number of entries:",N, "\n\n"), file = "../euhydi_stats.md", append = TRUE)
cat("*Minimal requirements* \n\n", file = "../euhydi_stats.md", append = TRUE)
pC <- NA
for (icol in c(3:10)){
pC[icol] <- round(sum(!is.na(CHEMICAL[,icol]))/N*100,digit=2)
cat(paste(names(CHEMICAL)[icol],":",pC[icol],"%\n\n"), file = "../euhydi_stats.md", append = TRUE)
}
cat("*Other data*\n\n", file = "../euhydi_stats.md", append = TRUE)
for (icol in c(1:2,11:ncol(CHEMICAL))){
pC[icol] <- round(sum(!is.na(CHEMICAL[,icol]))/N*100,digit=2)
cat(paste(names(CHEMICAL)[icol],":",pC[icol],"%\n\n"), file = "../euhydi_stats.md", append = TRUE)
}
attr(pC,"name") <- names(CHEMICAL)

cat("### -- PSIZE --\n\n", file = "../euhydi_stats.md", append = TRUE)
N <- nrow(PSIZE)
cat(paste("Total number of entries:",N, "\n\n"), file = "../euhydi_stats.md", append = TRUE)
cat(paste("Samples with PSIZE:",round(sum(BASIC$SAMPLE_ID %in% PSIZE$SAMPLE_ID)*100/Ns,digit=2),"%\n\n"), file = "../euhydi_stats.md", append = TRUE)
load("../output/psize_qa.rdata")# idn and sum_pn
cat(paste("PSIZE summing to 100%:",round(sum(sum_pn>=99 & sum_pn<=101)*100/length(sum_pn),digit=2),"%\n\n"), file = "../euhydi_stats.md", append = TRUE)
round(sum(sum_pn>=99 & sum_pn<=101)*100/Ns,digit=2)
pP <- round(sum(sum_pn>=99 & sum_pn<=101)*100/length(sum_pn),digit=2)

cat("### -- RET --\n\n", file = "../euhydi_stats.md", append = TRUE)
Nr <- nrow(RET)
cat(paste("Total number of entries:",Nr, "\n\n"), file = "../euhydi_stats.md", append = TRUE)
ret.count <- as.numeric(table(RET$SAMPLE_ID))
cat(paste("Samples with RET:",round(length(ret.count)*100/Ns,digit=2), "%\n\n"), file = "../euhydi_stats.md", append = TRUE)
hist(ret.count[ret.count<65])
cat(paste("Samples with at least 5 points:",round(sum(ret.count >=5)*100/Ns,digit=2),"%\n\n"), file = "../euhydi_stats.md", append = TRUE)


cat("### -- COND --\n\n", file = "../euhydi_stats.md", append = TRUE)
N <- nrow(COND)
cat(paste("Total number of entries:",N, "\n\n"), file = "../euhydi_stats.md", append = TRUE)
cat(paste("Samples with COND:",round(sum(BASIC$SAMPLE_ID %in% COND$SAMPLE_ID)*100/Ns,digit=2),"%\n\n"), file = "../euhydi_stats.md", append = TRUE)
cond.count <- as.numeric(table(COND$SAMPLE_ID))
cat(paste("Samples with only KSAT:",round(sum(cond.count == 1)*100/Ns,digit=2),"%\n\n"), file = "../euhydi_stats.md", append = TRUE)

print("DESCRIPTIVE STATISTICS")
print("-- BASIC --")
sB <- summary(BASIC[,c(4:5,7:8,18:23)])
#print(sB)
sB.df <- as.data.frame(matrix(as.numeric(substr(sB,9,max(nchar(sB)))),nrow=7,ncol=10))
names(sB.df) <- gsub(" ","",attr(sB,"dimnames")[[2]])
rownames(sB.df)<- gsub(" ","",substr(sB[1:7,1],1,7))
write.csv(t(sB.df),file="../output/basic_summary.csv")

print("-- CHEMICAL --")
sC <- summary(CHEMICAL[,c(seq(3,33,by=2),34,35)])
#print(sC)
sC.df <- as.data.frame(matrix(as.numeric(substr(sC,9,max(nchar(sC)))),nrow=7,ncol=18))
names(sC.df) <- gsub(" ","",attr(sC,"dimnames")[[2]])
rownames(sC.df)<- gsub(" ","",substr(sC[1:7,1],1,7))
write.csv(t(sC.df),file="../output/chemical_summary.csv")


# Meet minimum requirements: add a column to BASIC (logical)
# for each BASIC$SAMPLE_ID, must have: GENERAL:coordinates; ISO_COUNTRY; RC_L1; RC_L2; CONTACT_P; EMAIL; BASIC: SAMPLE_DEP_TOP; SAMPLE_DEP_BOT; BD; BD_M; COARSE; COARSE_M; CHEMICAL: OC, OC_M; PSIZE: sum=100+/-1, P_M; RET: HEAD, THETA; MEHTOD: CODE_M from other tables

# Minimal requirements fulfilment
# vG = c("coordinates","ISO_COUNTRY","RC_L1","RC_L2","CONTACT_P","EMAIL")
# vB = c("SAMPLE_DEP_TOP","SAMPLE_DEP_BOT","BD","BD_M","COARSE","COARSE_M")
# vC = c("OC","OC_M")
# vP = "PSIZE" # sum=100+/-1, P_M;
# vR = c("HEAD","THETA","TH_M","INV_TH_M")
# vM = c("CODE_M")
# mr <- data.frame(Variable = v, Proportion = p)

# BD, HOC, PSD, RET
sid <- BASIC$SAMPLE_ID
sum(!is.na(BASIC$BD) & !is.na(CHEMICAL$HOC[match(sid,CHEMICAL$SAMPLE_ID)]) & sid %in% RET$SAMPLE_ID & sid%in%idn[sum_pn>=99 & sum_pn<=101])

# Export basic stats


