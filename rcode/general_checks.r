# general.checks
# checks that table meets requirements of GENERAL table in EU-HYDI database
# Author: M.Weynants
# Date created: 2012/11/27
#################################################################
general.checks <- function(x){
print('checking GENERAL...')
# check size: 65 fields
if (length(x) < 65) stop("Table GENERAL has not enough columns")
if (length(x) > 65) warning("Table GENERAL has too many columns and will be truncated")
# check fields' names
if (any(names(x[,1:65]) != c("PROFILE_ID","LOC_COOR_X","LOC_COOR_Y", "LOC_COOR_SYST","X_WGS84","Y_WGS84","ELEV","ISO_COUNTRY","RC_L1","RC_L2","LC_L1","LC_L2","LC_L3","LU_L1","LU_L2","SITE_LANDFORM","SITE_SLOP_POS","SITE_SLOP_FORM","SITE_SLOP_GRAD","SRF_ROCK_COV","SRF_ROCK_DIS","SRF_COAR_COV","SRF_COAR_SIZ","SRF_ERO_CAT","SRF_ERO_COV","SRF_ERO_DEG","SRF_ERO_ACT","SRF_SEAL_THIC","SRF_SEAL_CON","SRF_CRAC_WID","SRF_CRAC_DEP","SRF_CRAC_DIS","SRF_SAL_COV","SRF_SAL_THIC","PAR_MAT","AGE","WRB2006_RSG","WRB2006_PQ1","WRB2006_PQ2","WRB2006_PQ3","WRB2006_SQ1","WRB2006_SQ2","WRB2006_SQ3","WRB1998_RSG","WRB1998_ADJSPE1","WRB1998_ADJSPE2","WRB1998_ADJSPE3","WRB1998_ADJSPE4","WRB1998_ADJSPE5","WRB1998_ADJSPE6","NAT_CLAS","NAT_CLAS_REF","YEAR","MONTH","DAY","SURVEYOR_P","PUBL_REF","CONTACT_P","CONTACT_A","EMAIL","REL_ID","REL_T_SER","COMMENTS1","COMMENTS2","COMMENTS3"))){warning("Column names of GENERAL are wrong")}
# check each field
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {all(abs(x-round(x)) < tol)}
# PROFILE_ID should be unique integers
if (!is.wholenumber(x[[1]])) warning("PROFILE_ID should be integers")
if (any(duplicated(x[[1]]))) warning("PROFILE_ID should be unique")
if (any(nchar(as.character(x[[1]])) != 8 & nchar(as.character(x[[1]])) != 7)) warning("Unvalid PROFILE_ID!")
# LOC_COOR_X, LOC_COOR_Y 
# LOC_COOR_SYST should be text
if (!is.character(x[[4]])) warning("LOC_COOR_SYST should be text")
# X_WGS84 and Y_WGS84 should be numeric and in the right bounding box
if (!is.numeric(x[[5]]) | !is.numeric(x[[6]])) warning("X_WGS84 and Y_WGS84 should be numeric")
if (any(!((x[[5]] > -12 & x[[5]] < 38 & x[[6]] > 34 & x[[6]] < 71) | (x[[5]]>32 & x[[5]]< 180 & x[[6]]>43 & x[[6]]<75) | (x[[5]]==-999 & x[[6]]==-999)))) warning("WG84 coordinates should refer to a geographical position in Europe or Russia. Eastern longitudes should be entered as negative numbers.")
# ELEV should be integer
if (!is.wholenumber(x[[7]])) round(x[[7]])
# ISO_COUNTRY must be in iso 3166 codes
require(ISOcodes);data("ISO_3166_1")
if (any(!(x[[8]] %in% ISO_3166_1$Alpha_2))) warning("ISO_COUNTRY should be a valid ISO 3166 Alpha2 code")
# RC_L1 and RC_L2 should be NUT1 and NUT2 for EC countries
EU <- c('AT','BE','BG','CY','CZ','DK','EE','FI','FR','DE','GR','HU','IE','IT','LV','LT','LU','MT','NL','PL','PT','RO','SK','SI','ES','SE','GB')
if (any(x[[8]] %in% EU)) {
if (any(sapply(x[[9]],function(a){is.character(a) & length(a)==3}))) warning("RC_L1 should be a valid NUTS1 code")
if (any(sapply(x[[10]],function(a){is.character(a) & length(a)==4}))) warning("RC_L2 should be a valid NUTS2 code")}
# LC_1, LC_2, LC_3, LU_1, LU_2 should be valid LUCAS codes
LC1 <- c('A00','B00','C00','D00','E00','F00','G00','H00','ND')
LC2 <- c('A10','A20',sapply(c(1:5,7:8),function(a){paste('B',a,'0',sep='')}),'C10','C20','C30','CX0','D10','D20','E10','E20','E30','F00','G10','G20','G30','G50','H10','H20','ND')
LC3 <- c('A11','A12','A13','A21','A22',
sapply(1:9,function(a){paste('B1',a,sep='')}),
'B21','B22','B23',
sapply(1:7,function(a){paste('B3',a,sep='')}),
sapply(1:5,function(a){paste('B4',a,sep='')}),
sapply(1:5,function(a){paste('B5',a,sep='')}),
sapply(1:7,function(a){paste('B7',a,sep='')}),
sapply(1:4,function(a){paste('B8',a,sep='')}),
sapply(1:9,function(a){paste('CX',a,sep='')}),
sapply(LETTERS[1:5],function(a){paste('CX',a,sep='')}),
'H11','H12','H21','H22','H23','ND')
if (any(!x[[11]] %in% LC1)) warning('LC_1 must be a valid LUCAS level 1 land cover code')
if (any(!x[[12]] %in% LC2)) warning('LC_2 must be a valid LUCAS level 2 land cover code')
if (any(!x[[13]] %in% LC3)) warning('LC_3 must be a valid LUCAS level 3 land cover code')
LU1 <- c('U110','U120','U130','U140','U150','U210','U220','U310','U320','U340','U350','U360','U370','U400','ND')
LU2 <- c('U111','U112','U113','U120','U130','U140','U150','U210',
sapply(1:7,function(a){paste('U22',a,sep='')}),
sapply(1:8,function(a){paste('U31',a,sep='')}),
'U321','U322','U330','U340','U350',
sapply(1:4,function(a){paste('U36',a,sep='')}),
'U370','U400','ND')
if (any(!x[[14]] %in% LU1)) warning('LU_1 must be a valid LUCAS level 1 land use code')
if (any(!x[[15]] %in% LU2)) warning('LU_2 must be a valid LUCAS level 2 land use code')
## Site description: FAO guidelines
lf <- c('L','LP','LL','LD','LV','S','SE','SH','SM','SP','SV','T','TE','TH','TM','TV','CU','RI','IN','IM','WE','ND')
if (any(!x[[16]] %in% lf)) warning('SITE_LANDFORM should be a valid code from FAO guidelines for soil description (2006)')
sp <- c('CR','UP','MS','LS','TS','BO','HI','IN','LO','ND')
if (any(! x[[17]] %in% sp)) warning('SITE_SLOPE_POS should be a valid code from FAO guidelines for soil description (2006)')
sf <- c('S','C','T','V','X','SS','SV','SC','VS','VV','VC','CS','CV','CC','ND')
if (any(! x[[18]] %in% sf)) warning('SITE_SLOPE_FORM should be a valid code from FAO guidelines for soil description (2006)')
sg <- c(sapply(1:9,function(a){paste('0',a,sep='')}),'10','ND')
if (any(!(( x[[19]] %in% sg) | (x[[19]] %in% c(1:10,-999))))) warning('SITE_SLOPE_GRAD should be a valid code from FAO guidelines for soil description (2006)')
if (is.numeric(x[[19]])) {tmp<-rep('ND',nrow(x));tmp[x[[19]]<10 & x[[19]] !=-999]<-paste('0',as.character(x[[19]][x[[19]]<10 & x[[19]] !=-999]),sep='');tmp[x[[19]]==10] <- as.character(x[[19]][x[[19]]==10]); x[[19]]<-tmp}
# rock
rc <- c('N','V','F','C','M','A','D','ND')
if (any(! x[[20]] %in% rc)) warning('SRF_ROCK_COV should be a valid code from FAO guidelines for soil description (2006)')
rd <- c(1:5,-999)
# SRF_ROCK_DIS should be numeric
if (!is.numeric(x[[21]])) x[[21]][which(x[[21]] == 'ND')] <- -999; x[[21]]<- as.numeric(x[[21]])
if (any(! x[[21]] %in% rd)) warning('SRF_ROCK_DIS should be a valid code from FAO guidelines for soil description (2006)')
# coarse
if (any(! x[[22]] %in% rc)) warning('SRF_COAR_COV should be a valid code from FAO guidelines for soil description (2006)')
cs <- c('F','M','C','S','B','L','ND')
if (any(! x[[23]] %in% cs)) warning('SRF_COAR_SIZ should be a valid code from FAO guidelines for soil description (2006)')
# erosion
ec <- c('N','W','WS','WR','WG','WT','WD','WA','A','AD','AM','AS','AZ','M','NK','ND')
if (any(! x[[24]] %in% ec)) warning('SRF_ERO_CAT should be a valid code from FAO guidelines for soil description (2006)')
ecv <- c(0:5,-999)
if (any(! x[[25]] %in% ecv)) warning('SRF_ERO_COV should be a valid code from FAO guidelines for soil description (2006)')
ed <- c('S','M','V','E','ND')
if (any(! x[[26]] %in% ed)) warning('SRF_ERO_DEG should be a valid code from FAO guidelines for soil description (2006)')
ea <- c('A','R','H','N','X','ND')
if (any(! x[[27]] %in% ea)) warning('SRF_ERO_ACT should be a valid code from FAO guidelines for soil description (2006)')
# sealing
st <- c('N','F','M','C','V','ND')
if (any(! x[[28]] %in% st)) warning('SRF_SEAL_THIC should be a valid code from FAO guidelines for soil description (2006)')
sc <- c('S','H','V','E','ND')
if (any(! x[[29]] %in% sc)) warning('SRF_SEAL_CON should be a valid code from FAO guidelines for soil description (2006)')
# cracks
cw <- c('F','M','W','V','E','ND')
if (any(! x[[30]] %in% cw)) warning('SRF_CRAC_WID should be a valid code from FAO guidelines for soil description (2006)')
cp <- c('S','M','D','V','ND')
if (any(! x[[31]] %in% cp)) warning('SRF_CRAC_DEP should be a valid code from FAO guidelines for soil description (2006)')
cd <- c('C','D','M','W','V','ND')
if (any(! x[[32]] %in% cd)) warning('SRF_CRAC_DIS should be a valid code from FAO guidelines for soil description (2006)')
# salt
sc <- c(0:4,-999)
if (any(! x[[33]] %in% sc)) warning('SRF_SAL_COV should be a valid code from FAO guidelines for soil description (2006)')
st <- c('N','F','M','C','V','ND')
if (any(! x[[34]] %in% st)) warning('SRF_SAL_THIC should be a valid code from FAO guidelines for soil description (2006)')
# PAR_MAT
pm <- c(0,5000,5500,5800,5810,5820,5830,5831,6000,6100,6110,6120,6200,6220,6300,6310,7000,9000,9100,9200,9210,5100,5110,5111,5120,5121,5122,5300,5310,5311,5312,5320,5321,5322,5510,6210,7200,7210,7220,9110,5200,5210,5211,5212,5220,5221,5222,5323,5324,5400,5410,5411,5412,5420,5421,5422,5431,5520,5530,5600,5610,5611,5612,5620,5621,5700,5710,5711,5712,5713,5714,5715,5720,5721,6111,7100,7110,7120,9120,9220,9230,9240,8000,8100,8111,8112,8113,8120,8200,8210,8300,8310,8320,8330,9300,3000, 3700, 3710, 3711, 3712, 3713, 3720, 3721, 3722, 3723, 3730, 3740, 3750, 3760, 3600, 4000, 4100, 4110, 4120, 4121, 4600, 4610, 4611,  4620, 4630, 4700, 4710, 4720, 4730,3100, 3110, 3120, 3130, 3131, 3132, 3140, 3400, 3410, 3411, 3412, 3420, 3430, 3431, 3440, 3441, 3450, 3620, 4200, 4210, 4211, 4220, 4230, 4240, 4250, 4260,3200, 3210, 3300, 3310, 3320, 3500, 3510, 3520, 3530, 3610, 3630, 4300, 4310, 4311, 4312, 4313, 4320, 4330, 4400, 4410, 4411, 4500, 4510, 4520,1000, 1100, 1110, 1111, 1120, 1200, 1210, 1212, 1213, 1214, 1215,  1220, 1230, 1231, 1300, 1310, 1311, 1312, 1320, 1400, 1410, 1411, 1412, 1413, 1420, 2000, 2200, 2210, 2220, 2230, 2300, 2310, 2320,1211, 2100, 2110, 2111, 2112, 2113, 2114, 2115, 2116, 2117, 2118, 2119, 2120, 2121, 2122, 2130, 2140, 2141, 2142, 2150,-999)
if (any(! x[[35]] %in% pm)) warning('PAR_MAT should be a valid code from the SGDBE guidelines')
# AGE
age <- c('vYn', 'vYa', 'Yn', 'Ya', 'Hn', 'Ha', 'lPi', 'lPp', 'lPf', 'oPi', 'oPp', 'oPf', 'T', 'O','ND')
if (any (! x[[36]] %in% age)) warning('AGE should be a valid code from the FAO guidelines for soil description (2006)')
# WRB 2006
 RSG <- c('AC', 'AB', 'AL', 'AN', 'AT', 'AR', 'CL', 'CM', 'CH', 'CR', 'DU', 'FR', 'FL', 'GL', 'GY', 'HS', 'KS', 'LP' ,'LX', 'LV', 'NT', 'PH', 'PL', 'PT', 'PZ', 'RG', 'SC', 'SN', 'ST', 'TC', 'UM', 'VR','ND')
 q <- c('ap','ae','ac','ao','ab','ax','al','aa','au','an','aq','am','ar','ai','ad','az','br','ca',
 'cc','cm','cb','cn','cl','cr','ce','co','cy','ct','dn','dr','du','dy','ek','nd','ny','ne',
 'nf','ng','nl','ns','et','ed','ee','el','ea','ec','eu','es','fr','fr','fi','ft','fv','fo',
 'fp','fa','fg','fu','ga','ge','gt','gr','gi','gc','gl','gb','gs','gz','gm','gy','gp','ha',
 'hm','hi','ht','hu','hg','hy','hf','ha','hl','hc','hd','he','hp','ho','hs','hk','wc','wg',
 'wl','ws','wn','ir','ll','la','le','lg','lm','lc','li','lx','lv','mg','mf','mz','ml','ms',
 'mo','mi','na','ni','nv','nt','om','oc','os','oa','ph','pe','pt','pc','pd','py','pg','pp',
 'ps','px','pi','pa','pl','po','pf','pr','pu','ra','rd','rg','rz','rh','ro','ru','rp','rs',
 'sz','sa','sn','sl','sk','so','sc','sm','sd','sp','st','sq','su','ty','te','tf','tr','ba',
 'bv','ti','tp','td','tx','tn','tu','um','ug','ub','vm','vr','vt','vi','vo','xa','ye','ND')
if (any(!x[[37]] %in% RSG)) warning("WRB2006_RSG must be a valid WRB reference soil group code (FAO, 2006)")
# remove any space
x[,38:43]<-sapply(x[,38:43],function(v){sub(' ','',v)})
if (any(! unlist(x[,c(38:40)]) %in% q)) warning("WRB2006_PQ1, WRB2006_PQ2 and WRB2006_PQ3 must be valid WRB prefix qualifiers (FAO, 2006)")
if (any(! unlist(x[,c(41:43)]) %in% q)) warning("WRB2006_SQ1, WRB2006_SQ2 and WRB2006_SQ3 must be valid WRB suffix qualifiers (FAO, 2006)")
# WRB 1998
if (any(!x[[44]] %in% RSG[-c(29,30)])) warning("WRB1998_RSG must be a valid WRB reference soil group code (FAO, 1998)")
a <- c('ap','ae','ac','ao','ab','ax','al','au','an','aq','am','ah','ar','ai','ad','az','ca','cc','cb','cn','ch','cl','cr','cy','ct',
'dn','du','dy','et','eu','es','fl','fr','fi','fv','fo','fg','fu','ga','ge','gt','gr','gi','gc','gl','gs','gz','gm','gy','gp',
'ha','hi','ht','hu','hg','hy','hk','ir','ll','le','li','lx','lv','mg','mz','me','ms','mo','na','ni','oh','om','or','oa','ph',
'pe','pt','pc','pd','pg','pp','ps','pi','pa','pn','pl','po','pf','pr','rd','rg','rz','rh','ro','ru','rp','rs','sz','sa','si',
'sl','sk','so','sd','sp','st','su','ty','tf','tr','ti','tx','tu','um','ub','vm','vr','vt','vi','xa','ye','ND')
s <- c('',' ','d','c','n','p','h','w','o','r','t','b')
as <- paste(a[-length(a)],rep(s,each=length(a)-1),sep='')
if (any(! (unlist(x[,45:50]) %in% a | unlist(x[,45:50]) %in% as))) warning("WRB1998_ADJSPE1 to WRB1998_ADJSPE6 must be valid combinations of WRB soil unit adjectives and specifiers (FAO, 1998)")
# NAT_CLAS and NAT_CLAS_REF
if (!is.character(x[[51]])) warning("NAT_CLAS must be entered as text")
if (!is.character(x[[52]])) warning("NAT_CLAS_REF must be entered as text")
# YEAR
if (! is.wholenumber(x[[53]])) warning("YEAR must be an integer")
if (any(x[[53]] > as.numeric(format(Sys.time(),"%Y")))) warning("YEAR must be a valid year")
# MONTH
if (! is.wholenumber(x[[54]])) warning("MONTH must be an integer")
if (any(! x[[54]] %in% c(-999,1:12))) warning("MONTH must be an integer between 1 and 12 or -999")
# DAY
if (! is.wholenumber(x[[55]])) warning("DAY must be an integer")
if (any(! x[[55]] %in% c(-999,1:31))) warning("DAY must be an integer between 1 and 31 or -999")
# SURVEYOR_P
if (! is.character(x[[56]])) warning("SURVEYOR_P must be entered as text")
# PUBL_REF
if (! is.character(x[[57]])) warning("PUBL_REF must be entered as text")
# CONTACT_P *
if (! is.character(x[[58]])) warning("CONTACT_P must be entered as text")
if (any(grepl('ND',x[[58]]))) warning("CONTACT_P is a mandatory field")
# CONTACT_A
if (! is.character(x[[59]])) warning("CONTACT_A must be entered as text")
# EMAIL
if (! is.character(x[[60]])) warning("EMAIL must be entered as text")
if (any(grepl('ND',x[[60]]))) warning("EMAIL is a mandatory field")
# REL_ID
if (! is.character(x[[61]])) warning("REL_ID must be a comma separated list of linked PROFILE_ID")
id<-strsplit(x[[61]],',')
if (any(unlist(lapply(id,function(idi){any(! idi %in% c(x[[1]],"ND"))})))) warning("REL_ID must be a comma separated list of linked PROFILE_ID")
# REL_T_SER
x[[62]] <- as.numeric(x[[62]]); x[[62]][is.na(x[[62]])] <- -999
# COMMENTS
if (! is.character(unlist(x[,63:65]))) warning("COMMENTS must be entered as text")
# FUNCTION OUTPUT
print('... done')
x <- x[,1:65]
return(x)
}

