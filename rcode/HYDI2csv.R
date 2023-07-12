# HYDI2csv.R: export rdata to a set of csv files

# Author: M. Weynants
# Date created: 2023/07/02
######################################################################

# HYDY2mdb.r

load("../output/EUHYDI_v1_1.Rdata")

# clean hydi of unnecessary columns
hydi$PSIZE <- hydi$PSIZE[,-c(grep("^ID$",names(hydi$PSIZE)),grep("^FLAG$",names(hydi$PSIZE)))] 
hydi$RET <- hydi$RET[,-grep("^ID$",names(hydi$RET))]
hydi$COND <- hydi$COND[,-grep("^ID$",names(hydi$COND))]
# merge psd_est codes
psd <- hydi$PSD_EST
for(i in 7:9){
psd[,i] <- gsub(" in HYPRES","",psd[,i])
psd[,i] <- sub("no entry in PSIZE table","no data",psd[,i])
print(table(psd[,i]))}
psd <- psd[-grep("sample not in BASIC table",psd[,7]),]
psd -> hydi$PSD_EST

meta_gen <- matrix(c("PROFILE_ID","Profile identifier. 8 digits: 3 for the country, 5 for the profile (primary key).",
"LOC_COOR_X","X/longitude in local coordinate system",
"LOC_COOR_Y","Y/latitude in local coordinate system",
"LOC_COOR_SYST","Local coordinate system",
"X_WGS84","Longitude in decimal degrees",
"Y_WGS84","Latitude in decimal degrees",
"ELEV","Elevation in meters",
"ISO_COUNTRY","ISO 3166 alpha 2 country code",
"RC_L1","Region code, level 1: NUTS1 if available",
"RC_L2","Region code, level 2: NUTS2 if available",
"LC_L1","Land cover code, level 1: LUCAS",
"LC_L2","Land cover code, level 2: LUCAS",
"LC_L3","Land cover code, level 3: LUCAS",
"LU_L1","Land use code, level 1: LUCAS",
"LU_L2","Land use code, level 2: LUCAS",
"SITE_LANDFORM","Site landform code (FAO, 2006) ",
"SITE_SLOP_POS","Slope position code (FAO, 2006)",
"SITE_SLOP_FORM","Slope form code (FAO, 2006)",
"SITE_SLOP_GRAD","Slope gradient code (FAO, 2006)",
"SRF_ROCK_COV","Soil surface: rock outcrops: cover (FAO, 2006)",
"SRF_ROCK_DIS","Soil surface: rock outcrops: distance (FAO, 2006)",
"SRF_COAR_COV","Soil surface: coarse fragments: cover (FAO, 2006)",
"SRF_COAR_SIZ","Soil surface: coarse fragments: size (FAO, 2006)",
"SRF_ERO_CAT","Soil surface: erosion: category (FAO, 2006)",
"SRF_ERO_COV","Soil surface: erosion: cover (FAO, 2006)",
"SRF_ERO_DEG","Soil surface: erosion: degree (FAO, 2006)",
"SRF_ERO_ACT","Soil surface: erosion: activity (FAO, 2006)",
"SRF_SEAL_THIC","Soil surface: sealing: thickness (FAO, 2006)",
"SRF_SEAL_CON","Soil surface: sealing: consistence (FAO, 2006)",
"SRF_CRAC_WID","Soil surface: cracks: width (FAO, 2006)",
"SRF_CRAC_DEP","Soil surface: cracks: depth (FAO, 2006)",
"SRF_CRAC_DIS","Soil surface: cracks: distance (FAO, 2006)",
"SRF_SAL_COV","Soil surface: salt: cover (FAO, 2006)",
"SRF_SAL_THIC","Soil surface: salt: thickness (FAO, 2006)",
"PAR_MAT","Parent material code: 4 digits (ESDB)",
"AGE","Age of land surface (FAO, 2006)",
"WRB2006_RSG","WRB 2006 classification code ? reference soil group",
"WRB2006_PQ1","WRB 2006 classification code ? first prefix qualifier",
"WRB2006_PQ2","WRB 2006 classification code ? second prefix qualifier",
"WRB2006_PQ3","WRB 2006 classification code ? third prefix qualifier",
"WRB2006_SQ1","WRB 2006 classification code ? first suffix qualifier",
"WRB2006_SQ2","WRB 2006 classification code ? second suffix qualifier",
"WRB2006_SQ3","WRB 2006 classification code ? third suffix qualifier",
"WRB1998_RSG","WRB 1998 classification code ? reference soil group",
"WRB1998_ADJSPE1","WRB 1998 classification code ? first soil unit adjective with specifier",
"WRB1998_ADJSPE2","WRB 1998 classification code ? second soil unit adjective with specifier",
"WRB1998_ADJSPE3","WRB 1998 classification code ? third soil unit adjective with specifier",
"WRB1998_ADJSPE4","WRB 1998 classification code ? fourth soil unit adjective with specifier",
"WRB1998_ADJSPE5","WRB 1998 classification code ? fifth soil unit adjective with specifier",
"WRB1998_ADJSPE6","WRB 1998 classification code ? sixth soil unit adjective with specifier",
"NAT_CLAS","National classification name",
"NAT_CLAS_REF","National classification reference",
"YEAR","Year of sampling",
"MONTH","Month of sampling [1,12]",
"DAY","Day of sampling [1,31]",
"SURVEYOR_P","Surveyor",
"PUBL_REF","Publication reference",
"CONTACT_P","Contact person",
"CONTACT_A","Contact address",
"EMAIL","Contact e-mail",
"REL_ID","Related profiles in time series table (comma separated PROFILE IDs)",
"REL_T_SER","Related time series",
"COMMENTS1","Comment",
"COMMENTS2","Comment",
"COMMENTS3","Comment",
"SOURCE","Source of data: Author of chapter in EU-HYDI Report"),
ncol=2,byrow=TRUE)

meta_basic <- matrix(c(
"PROFILE_ID","Profile identifier",
"SAMPLE_ID","Sample identifier: PROFILE_ID + SAMPLE_POS (primary key)",
"SAMPLE_POS","Sample position",
"SAMPLE_DEP_TOP","Sample depth top (cm)",
"SAMPLE_DEP_BOT","Sample depth bottom (cm)",
"HOR1_NAME","Horizon designation: 7 characters (EU-HYDI, Annex 2)",
"HOR1_TOP","Top depth of the first horizon included in the sample (cm)",
"HOR1_BOT","Bottom depth of the first horizon included in the sample (cm)",
"HOR2_NAME","Horizon designation: 7 characters",
"HOR2_TOP","Top depth of the second horizon included in the sample (cm)",
"HOR2_BOT","Bottom depth of the second horizon included in the sample (cm)",
"HOR3_NAME","Horizon designation: 7 characters",
"HOR3_TOP","Top depth of the third horizon included in the sample (cm)",
"HOR3_BOT","Bottom depth of the third horizon included in the sample (cm)",
"STRUCTURE1","Primary structure grade, size and shape (FAO, 2006)",
"STR_COMB","Combination of soil structure (EU-HYDI, Annex 2, Table 12)",
"STRUCTURE2","Secondary structure grade, size and shape (FAO, 2006)",
"POR","Porosity volume percentage",
"POR_M","Porosity method code",
"BD","Bulk density (g/cm3)",
"BD_M","Bulk density method code",
"COARSE","Coarse fragments mass percentage (>2 mm)",
"COARSE_M","Coarse fragments (>2 mm) method code"
),ncol=2,byrow=TRUE)

meta_chem <- matrix(c(
"PROFILE_ID","Profile identifier",
"SAMPLE_ID","Sample identifier (primary key)",
"LOI","Organic matter content by loss on ignition (mass percentage)",
"LOI_M","Loss on ignition method code",
"OC","Organic carbon content (mass percentage)",
"OC_M","Organic carbon content method code",
"TC","Total carbon content (mass percentage)",
"TC_M","Total carbon content method code",
"HOC","Harmonized carbon content (EU-HYDI, Chapter 27)",
"CACO3","Calcium carbonate content (mass percentage)",
"CACO3_M","Calcium carbonate content method code",
"PH_H2O","pH in soil-water suspension",
"PH_H2O_M","pH in soil-water suspension method code",
"PH_KCL","pH in soil-KCl suspension",
"PH_KCL_M","pH in soil-KCl suspension method code",
"PH_CACL2","pH in soil-CaCl2 suspension",
"PH_CACL2_M","pH in soil-CaCl2 suspension method code",
"EC","Electrical conductivity (mS/cm)",
"EC_M","Electrical conductivity method code",
"SALT","Soluble salt content (mass percentage)",
"SALT_M","Soluble salt content method code",
"CEC","Cation exchange capacity",
"CEC_M","Cation exchange capacity method code",
"EX_NA","Exchangeable Na (meq/(100g))",
"EX_NA_M","Exchangeable Na method code",
"EX_MG","Exchangeable Mg (meq/(100g))",
"EX_MG_M","Exchangeable Mg method code",
"EX_K","Exchangeable K (meq/(100g))",
"EX_K_M","Exchangeable K method code",
"EX_CA","Exchangeable Ca (meq/(100g))",
"EX_CA_M","Exchangeable Ca method code",
"BASE_CATIONS","Sum of Na, K, Ca and Mg cations (meq/(100g))",
"ACIDITY_EXCH","Exchangeable acidity real (meq/(100g))", "ACIDITY_EXCH_M","Exchangeable acidity method code",
"ACIDITY_POT","Potential acidity real number (meq/(100g))", "ACIDITY_POT_M","Potential acidity method code"
),ncol=2,byrow=TRUE)

meta_psize <- matrix(c(
"PROFILE_ID","Profile identifier",
"SAMPLE_ID","Sample identifier",
"P_SIZE","Maximum particle size (1e-6 m)",
"P_PERCENT","Mass percentage of mineral material smaller tha P_SIZE and bigger than smaller P_SIZE with same SAMPLE_ID",
"P_M","Particle size measurement method code"
),ncol=2,byrow=TRUE)

meta_ret <- matrix(c(
"PROFILE_ID","Profile identifier",
"SAMPLE_ID","Sample identifier",
"HEAD","Matric potential in suction head (cm)",
"THETA","Volume water content (cm3/cm3)",
"TH_INV_P1","Water retention: Inverse modelling parameter 1",
"TH_INV_P2","Water retention: Inverse modelling parameter 2",
"TH_INV_P3","Water retention: Inverse modelling parameter 3",
"TH_INV_P4","Water retention: Inverse modelling parameter 4",
"TH_INV_P5","Water retention: Inverse modelling parameter 5",
"TH_INV_P6","Water retention: Inverse modelling parameter 6",
"TH_INV_P7","Water retention: Inverse modelling parameter 7",
"TH_INV_P8","Water retention: Inverse modelling parameter 8",
"TH_INV_MOD","Water retention: Model code",
"FLAG","Incomplete quality flag (TRUE/FALSE). FALSE: unreliable"
),ncol=2,byrow=TRUE)

meta_cond <- matrix(c(
"PROFILE_ID","Profile identifier",
"SAMPLE_ID","Sample identifier",
"IND_VALUE","Matric potential (1)/water content (0)",
"VALUE","Value of suction head (cm)/water content (cm3/cm3)",
"COND","Hydraulic conductivity (cm/d)",
"COND_M","Hydraulic conductivity measurement method code",
"K_INV_P1","Conductivity: Inverse modelling parameter 1",
"K_INV_P2","Conductivity: Inverse modelling parameter 2",
"K_INV_P3","Conductivity: Inverse modelling parameter 3",
"K_INV_P4","Conductivity: Inverse modelling parameter 4",
"K_INV_P5","Conductivity: Inverse modelling parameter 5",
"K_INV_P6","Conductivity: Inverse modelling parameter 6",
"K_INV_P7","Conductivity: Inverse modelling parameter 7",
"K_INV_P8","Conductivity: Inverse modelling parameter 8",
"K_INV_MOD","Conductivity: model code"
),ncol=2,byrow=TRUE)

meta_meth <- matrix(c(
"CODE_M","Method code(primary key)",
"METHOD","Method description",
"METH_REF","Method reference (Author name, year/ISO code)",
"METH_PAR","Name of parameter measurement method code"
),ncol=2,byrow=TRUE)

meta_psd <- matrix(c(
"PROFILE_ID","Profile identifier",
"SAMPLE_ID","Sample identifier",
"SOURCE","Data source (EU-HYDI chapter author)", 
"USCLAY","Clay content in mass percentage (<2 1e-6 m)",
"USSILT","Silt content in mass percentage ([2,50[ 1e-6 m)",
"USSAND","Sand content in mass percentage ([50,2000[ 1e-6 m)",
"USCLAY_CODE","Method for obtaining the data",
"USSILT_CODE","Method for obtaining the data",
"USSAND_CODE","Method for obtaining the data"
),ncol=2,byrow=TRUE)

metadata <- as.data.frame(rbind(cbind("GENERAL",meta_gen),
	cbind("BASIC",meta_basic),
	cbind("CHEMICAL",meta_chem),
	cbind("PSIZE",meta_psize),
	cbind("RET",meta_ret),
	cbind("COND",meta_cond),
	cbind("METHOD",meta_meth),
	cbind("PSD_EST",meta_psd)
	),stringsAsFactors=FALSE)
names(metadata) <- c("TABLE","FIELD","DESCR")

hydi$METADATA <- metadata

references = as.data.frame(matrix(c(
"FAO, 2006","FAO. 2006. Guidelines for Soil Description","ftp://ftp.fao.org/docrep/fao/009/a0541e/a0541e00.pdf",
"ESDB","European Soil Database","http://eusoils.jrc.ec.europa.eu/ESDB_Archive/ESDB/index.htm",
"EU-HYDI","EUR 26053 European Hydopedological Data Inventory"," doi:10.2788/5936"
),ncol=3,byrow=TRUE,dimnames=list(NULL,c("REF_CODE","REFERENCE","DOI_URL"))),stringsAsFactors=FALSE)

hydi$REFERENCES <- references

## ------ export to csv ## 2023/03/26 --------
# create export dir
export_dir <- "../output/EUHYDI_v1_1_csv"
dir.create(export_dir)
for (tbl in names(hydi)){
  readr::write_csv(hydi[[tbl]], file = paste(export_dir,"/",tbl,".csv", sep=""))
}
## -------
