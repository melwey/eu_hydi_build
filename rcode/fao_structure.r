# fao_structure.r
grade <- data.frame(name=c('very weak', 'weak, adherent','weak','moderate','strong','very strong','weak to moderate','moderate to strong'),code=c('WE','WE','WE','MO','ST','ST','WM','MS'),stringsAsFactors=FALSE)

size <- data.frame(name=c('very fine','fine','medium','coarse','very coarse','v.coarse','extremely coarse','very fine and fine','very fine to medium','fine and medium','fine to medium','fine to coarse','medium and coarse','medium to coarse','medium to very coarse','coarse and very coarse'),code=c('VF','FI','ME','CO','VC','VC','EC','FF','VM','FM','FM','FC','MC','MC','MV','CV'),stringsAsFactors=FALSE)

type <- data.frame(name=c('rock structure','stratified structure','single grain','singlegrain','massive','prismatic','blocky','angular blocky','parallelipiped','angular and subangular blocky','angular and subangularblocky','angular wedge','subangular and angular blocky','subangular blocky','subangular','nuty','prismatic','subangular prismatic','wedge-shaped','columnar','granular','worm casts','platy','cloddy','crumb','crumbly','lumpy'),code=c('RS','SS','SG','SG','MA','PM','BL','AB','AP','AS','AS','AW','SA','SB','SB','SN','PR','PS','WE','CO','GR','WC','PL','CL','CR','CR','LU'),stringsAsFactors=FALSE)
#
