if (all(psize1$ID == "ND")) psize1$ID<-psize1$SAMPLE_ID
id <- unique(psize1$ID)
p <- psize1$PROFILE_ID[match(id,psize1$ID)]
s <- psize1$SAMPLE_ID[match(id,psize1$ID)]
p.sum <- sapply(id,function(x){sum(psize1[psize1$ID==x #& psize1$P_PERCENT!=-999
,"P_PERCENT"])})
oc <- chemical1$OC[match(s,chemical1$SAMPLE_ID)]
df<- data.frame(PROFILE_ID=p,SAMPLE_ID=s,ID=id,PSIZE_SUM=p.sum,OC=oc)
df[df$PSIZE_SUM < 99 |df$PSIZE_SUM > 101,]

# from hypres with no psd data
sid.HYP.psize <- unique(hydi$PSIZE$SAMPLE_ID[grepl("HYP",hydi$PSIZE$SOURCE)])

sid.HYP.noPSD <- hydi$BASIC[! hydi$BASIC$SAMPLE_ID %in% sid.HYP.psize & grepl("HYP",hydi$BASIC$SOURCE),c("SAMPLE_ID","SOURCE")]
table(sid.HYP.noPSD[,2])
#  Houskova_HYPRES    Romano_HYPRES Schindler_HYPRES    Wosten_HYPRES 
#              58              216              144              216 
sid.HYP.noPSD.attila <- read.csv("../../QAmeeting/PSIZE/sid_HYP_noPSD.csv")
table(sid.HYP.noPSD.attila[,2])
# Houskova_HYPRES Schindler_HYPRES    Wosten_HYPRES 
#              58              140              356

# 
sid.attila <- sid.HYP.noPSD.attila[! sid.HYP.noPSD.attila[,1] %in% hydi$BASIC$SAMPLE_ID,]

sum(hypres_hydi$basic$SAMPLE_ID %in% sid.attila)

table(hypres_hydi$general$CONTACT_P[hypres_hydi$general$PROFILE_ID %in% round(sid.attila[,1]/100)])
