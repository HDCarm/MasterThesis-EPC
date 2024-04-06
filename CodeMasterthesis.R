
#### SET UP WORKING ENVIRONMENT ####

# Delete the whole environment by deleting a list that includes everything
rm(list = ls())
# Get the current working directory
oldwd <- getwd()
# Get the path of the current .R file and set it as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Disable scientific notation
options(scipen = 999)
# Set default font family to Times New Roman
par(family = "Times New Roman")

# add function to calculate p-values of lm objects https://gist.github.com/stephenturner/722049
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}



#### ACTIVATE PACKAGES ####

# List of packages to install and activate
packages <- c("readxl","tidyverse","writexl","haven", "cowplot", "ggplot2", "corrplot",
              "openxlsx", "PerformanceAnalytics","regclass", "margins", "lmtest","cluster",
              "factoextra","NbClust","interactions","psych","maps")
# Loop through the packages
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
rm(packages, pkg)



#### IMPORT DATA ####

df_sdg <- read_excel("data/SDR2023-data.xlsx", "SDR 2023 Data")
  colnames(df_sdg) <- gsub(" ", "", colnames(df_sdg)) # deleting all spaces in the columnames
  df_sdg <- rename(df_sdg, id = "CountryCodeISO3") # align country id column
  names(df_sdg) <- tolower(names(df_sdg)) # turn all columnames to lower capitals
  df_sdg <- rename(df_sdg, sdgscore = "2023sdgindexscore") # rename sdg score column to sdgscore for easier use

df_culture <- read_excel("data/6-dimensions-for-website-2015-12-08-0-100.xls")
  df_culture <- rename(df_culture, lto = "ltowvs") # rename cultural variables according to hofstede 2010
  df_culture <- rename(df_culture, id = "ctr") # align country id column
  names(df_culture) <- tolower(names(df_culture)) # turn all columnames to lower capitals
  df_culture[df_culture==0] <- NA # replace 0 values with NA
  df_culture <- na.omit(df_culture) # delete all rows containing NAs
  
df_wgi <- read_dta("data/wgidataset.dta")
  df_wgi <- rename(df_wgi, id = "code") # align country id column
  names(df_wgi) <- tolower(names(df_wgi)) # turn all columnames to lower capitals
  df_wgi <- rename(df_wgi,va="vae",pv="pve",ge="gee",rq="rqe",rl="rle",cc="cce")

df_gdppc <- read_excel("data/P_Data_Extract_From_World_Development_Indicators.xlsx", "Data")
  df_gdppc <- df_gdppc[-c(218:222),] # delete empty rows in df_gdppc
  # delete redunant year in colnames
  names(df_gdppc) <- gsub("\\[.*\\]", "", names(df_gdppc)) # replace everything within [] - // are to use [ as regular text without function - .* means just any sign
  names(df_gdppc) <- sub("([0-9]+)", "gdppc\\1", names(df_gdppc)) # write gdppc before every number in columnames
  colnames(df_gdppc) <- gsub(" ", "", colnames(df_gdppc)) # deleting all spaces in the columnames
  df_gdppc <- rename(df_gdppc, id = "CountryCode") # align country id column
  names(df_gdppc) <- tolower(names(df_gdppc)) # turn all columnames to lower capitals
  # add the venezuelian gdp forecast :https://www.imf.org/en/Publications/WEO/weo-database/2022/October/weo-report?c=299,&s=NGDPDPC,&sy=2022&ey=2022&ssm=0&scsm=1&scc=1&ssd=1&ssc=0&sic=1&sort=country&ds=.&br=1
  #df_gdppc$gdppc2022[df_gdppc$id == "VEN"] <- 3051.738 # write into column GDPPC row (ctr=VEN)
  #df_gdppc$gdppc2022[df_gdppc$id == "LBN"] <- 3283.414 # write into column GDPPC row (ctr=LBN)
  df_gdppc$gdppc2022 <- as.numeric(df_gdppc$gdppc2022)
  df_gdppc$loggdppc <- log(df_gdppc$gdppc2022)
  
df_gdppcgrowth <- read_excel("data/P_Data_Extract_From_World_Development_Indicators growth.xlsx","Data")  
  df_gdppcgrowth <- rename(df_gdppcgrowth, id = "Country Code") # align country id column
  df_gdppcgrowth <- df_gdppcgrowth[-c(267:271),] # delete empty rows in df_gdppc
  names(df_gdppcgrowth) <- gsub("\\s.*","",names(df_gdppcgrowth)) # replacing everything after the first space with ""
  names(df_gdppcgrowth) <- sub("([0-9]+)", "gdppcgrowth\\1", names(df_gdppcgrowth)) 
  df_gdppcgrowth$gdppcgrowth2018 <- as.numeric(df_gdppcgrowth$gdppcgrowth2018)
  df_gdppcgrowth$gdppcgrowth2019 <- as.numeric(df_gdppcgrowth$gdppcgrowth2019)
  df_gdppcgrowth$gdppcgrowth2020 <- as.numeric(df_gdppcgrowth$gdppcgrowth2020)
  df_gdppcgrowth$gdppcgrowth2021 <- as.numeric(df_gdppcgrowth$gdppcgrowth2021)
  df_gdppcgrowth$gdppcgrowth2022 <- as.numeric(df_gdppcgrowth$gdppcgrowth2022)
  df_gdppcgrowth$avg5ygr <- rowMeans(df_gdppcgrowth[,c("gdppcgrowth2018", "gdppcgrowth2019", "gdppcgrowth2020", "gdppcgrowth2021","gdppcgrowth2022")])
  names(df_gdppcgrowth) <- tolower(names(df_gdppcgrowth)) # turn all columnames to lower capitals
  colnames(df_gdppcgrowth) <- gsub(" ", "", colnames(df_gdppcgrowth)) # deleting all spaces in the columnames
  
df_cluster <- read_excel("data/CLASS.xlsx","List of economies")
  df_cluster <- df_cluster[-c(219:267),] # delete empty rows in fd_cluster
  colnames(df_cluster) <- gsub(" ", "", colnames(df_cluster)) # deleting all spaces in the columnames
  df_cluster <- rename(df_cluster, id="Code") # align country id column
  names(df_cluster) <- tolower(names(df_cluster))
  df_cluster$incomegroup[df_cluster$id == "VEN"] <- "Lower middle income" # add incomegroup for venezuela
  df_cluster$incomegroup <- tolower(df_cluster$incomegroup)
  df_cluster$incomegroup <- gsub(" ", "-",df_cluster$incomegroup)
  cluster <- unique(df_cluster$incomegroup)
  
df_culturexl <- read_excel("data/List of countries.xlsx")
  df_culturexl <- df_culturexl[,c("Jurisdiction","PD","IND","MAS", "UNA", "LTO", "IVR")]
  df_culturexl <- rename (df_culturexl, countryname = "Jurisdiction") # rename 2020 SDG Index Score to sdgscore
  df_culturexl[df_culturexl==0] <- NA # replace 0 values with NA
  df_culturexl <- na.omit(df_culturexl) # delete all rows containing NAs
  #Check whether data remains unmatched by country  
  missing <- subset(df_culturexl, !(countryname %in% df_gdppc$countryname)) # are observations in data that are not in gdpcapita
  df_culturexl$countryname <- gsub("Czech Republic", "Czechia", df_culturexl$countryname) # Replacing Czeck Republic with Czechia
  df_culturexl$countryname <- gsub("Turkey", "Turkiye", df_culturexl$countryname) # Replacing Turkey with Turkiye
  rm(missing) # delete missing variable 
  colnames(df_culturexl) <- gsub(" ", "", colnames(df_culturexl)) # deleting all spaces in the columnames
  names(df_culturexl) <- tolower(names(df_culturexl))
  df_culturexl <- rename(df_culturexl, pdi = "pd") # rename cultural variables according to hofstede 2010
  df_culturexl <- rename(df_culturexl, idv = "ind") # rename cultural variables according to hofstede 2010
  df_culturexl <- rename(df_culturexl, uai = "una") # rename cultural variables according to hofstede 2010
  df_culturexl <- df_culturexl[df_culturexl$countryname!="South Africa",] # removing south africa by selecting everything that is not south africa

sdgmapping <- read_excel("data/sdgmapping.xlsx")
  sdgmapping$sdgcat <- str_replace_all(sdgmapping$sdgcat, c("economic"="Economy", "social"="Society", "environmental"="Biosphere")) # replacing the first with th second
  

  
#### MERGE DATA ####

#merge df_gdppc, df_cluster and df_gdppcgrowth data. as both come from world bank, no mismatches occur 
data <- merge(df_gdppc, df_cluster[,c("id","incomegroup")], by="id", suffixes = c("",".cluster"))
missing <- subset(df_cluster, !(id %in% df_gdppc$id)) #  no gdppc data for Taiwan
data <- merge(data, df_gdppcgrowth, by="id", suffixes = c("",".growth"))
missing <- subset(df_gdppcgrowth, !(id %in% data$id)) # many missing observations but none is a country


# merging data and df_wgi. df_wgi only for rows concerning the year 2022
data1 <- merge(data, df_wgi[df_wgi$year==2022,], by="id",suffixes = c("", ".wgi"))
missing <- subset(df_gdppc, !(id %in% data$id)) # are observations in df_gdppc that are not in data

##### missing data df_gdppc/data ####
# 1 Andorra                   AND  <- ADO
df_wgi$id <- gsub("ADO", "AND", df_wgi$id) # Replacing ADO with AND
# 2 British Virgin Islands    VGB  /
# 3 Channel Islands           CHI  /
# 4 Congo, Dem. Rep.          COD  <- ZAR
df_wgi$id <- gsub("ZAR", "COD", df_wgi$id)
# 5 Curacao                   CUW  /
# 6 Faroe Islands             FRO  /
# 7 French Polynesia          PYF  /
# 8 Gibraltar                 GIB  /
# 9 Isle of Man               IMN  /
# 10 Kosovo                    XKX <- KSV
df_wgi$id <- gsub("KSV", "XKX", df_wgi$id)
# 11 New Caledonia             NCL  /
# 12 Northern Mariana Islands  MNP  /
# 13 Romania                   ROU  <- ROM
df_wgi$id <- gsub("ROM", "ROU", df_wgi$id)
# 14 Sint Maarten (Dutch part) SXM  /
# 15 St. Martin (French part)  MAF  /
# 16 Timor-Leste               TLS  <- TMP
df_wgi$id <- gsub("TMP", "TLS", df_wgi$id)
# 17 Turks and Caicos Islands  TCA  /
# 18 West Bank and Gaza        PSE /

data1 <- merge(data, df_wgi[df_wgi$year==2022,], by="id",suffixes = c("", ".wgi"))

# merging data and df_sdg
data2 <- merge(data1, df_sdg, by = "id",suffixes = c("", ".sdg"))
missing <- subset(data, !(id %in% data2$id)) # are observations in data that are not in data2

##### missing data data/data2 ####
# id          Country Name
# 1   ABW                 Aruba /
# 9   ASM        American Samoa /
# 25  BMU               Bermuda /
# 46  CYM        Cayman Islands /
# 75  GRL             Greenland /
# 77  GUM                  Guam /
# 79  HKG  Hong Kong SAR, China /
# 114 MAC      Macao SAR, China /
# 150 PRI           Puerto Rico /
# 196 VIR Virgin Islands (U.S.) /
# 200 XKX                Kosovo /

#merging data2 and df_culture
data3 <- merge(data2, df_culture, by = "id",suffixes = c("", ".culture"))
missing <- subset(data2, !(id %in% data3$id)) # are observations in data2 that are not in data3

##### missing data2/data3 ####
# 1   AFG                    Afghanistan /
# 2   AGO                         Angola /
# 5   ARE           United Arab Emirates /
# 8   ATG            Antigua and Barbuda /
# 9   AUS                      Australia <- AUL
df_culture$id <- gsub("AUL", "AUS", df_culture$id)
# 12  BDI                        Burundi /
# 15  BFA                   Burkina Faso <- BUF
df_culture$id <- gsub("BUF", "BFA", df_culture$id)
# 16  BGD                     Bangladesh <- BAN
df_culture$id <- gsub("BAN", "BGD", df_culture$id)
# 17  BGR                       Bulgaria <- BUL
df_culture$id <- gsub("BUL", "BGR", df_culture$id)
# 18  BHR                        Bahrain /
# 19  BHS                   Bahamas, The /
# 20  BIH         Bosnia and Herzegovina <- BOS
df_culture$id <- gsub("BOS", "BIH", df_culture$id)
# 22  BLZ                         Belize /
# 23  BOL                        Bolivia /
# 25  BRB                       Barbados /
# 26  BRN              Brunei Darussalam /
# 27  BTN                         Bhutan /
# 28  BWA                       Botswana /
# 31  CHE                    Switzerland <- SWI
df_culture$id <- gsub("SWI", "CHE", df_culture$id)
# 33  CHN                          China <- CHI
df_culture$id <- gsub("CHI", "CHN", df_culture$id)
# 34  CIV                  Cote d'Ivoire /
# 35  CMR                       Cameroon /
# 36  COD               Congo, Dem. Rep. /
# 37  COG                    Congo, Rep. /
# 39  COM                        Comoros /
# 40  CPV                     Cabo Verde /
# 41  CRI                     Costa Rica <- COS
df_culture$id <- gsub("COS", "CRI", df_culture$id)
# 42  CUB                           Cuba /
# 45  DEU                        Germany <- GER
df_culture$id <- gsub("GER", "DEU", df_culture$id)
# 46  DJI                       Djibouti /
# 47  DMA                       Dominica /
# 48  DNK                        Denmark <- DEN
df_culture$id <- gsub("DEN", "DNK", df_culture$id)
# 50  DZA                        Algeria <- ALG
df_culture$id <- gsub("ALG", "DZA", df_culture$id)
# 51  ECU                        Ecuador <- ECa
df_culture$id <- gsub("ECA", "ECU", df_culture$id)
# 53  ERI                        Eritrea /
# 54  ESP                          Spain <- SPA
df_culture$id <- gsub("SPA", "ESP", df_culture$id)
# 58  FJI                           Fiji /
# 60  FSM          Micronesia, Fed. Sts. /
# 61  GAB                          Gabon /
# 65  GIN                         Guinea /
# 66  GMB                    Gambia, The /
# 67  GNB                  Guinea-Bissau /
# 68  GNQ              Equatorial Guinea /
# 69  GRC                         Greece GRE
df_culture$id <- gsub("GRE", "GRC", df_culture$id)
# 70  GRD                        Grenada /
# 71  GTM                      Guatemala <- GUA
df_culture$id <- gsub("GUA", "GTM", df_culture$id) # Replacing IS with SOLL
# 72  GUY                         Guyana /
# 73  HND                       Honduras /
# 74  HRV                        Croatia /
# 75  HTI                          Haiti /
# 77  IDN                      Indonesia <- IDO
df_culture$id <- gsub("IDO", "IDN", df_culture$id) # Replacing IS with SOLL
# 79  IRL                        Ireland <- IRE
df_culture$id <- gsub("IRE", "IRL", df_culture$id) # Replacing IS with SOLL
# 80  IRN             Iran, Islamic Rep. <- IRA
df_culture$id <- gsub("IRA", "IRN", df_culture$id) # Replacing IS with SOLL
# 82  ISL                        Iceland <- ICE
df_culture$id <- gsub("ICE", "ISL", df_culture$id) # Replacing IS with SOLL
# 88  KAZ                     Kazakhstan /
# 89  KEN                          Kenya /
# 90  KGZ                Kyrgyz Republicn <- KYR
df_culture$id <- gsub("KYR", "KGZ", df_culture$id) # Replacing IS with SOLL
# 91  KHM                       Cambodia /
# 92  KIR                       Kiribati /
# 93  KNA            St. Kitts and Nevis /
# 95  KWT                         Kuwait /
# 96  LAO                        Lao PDR /
# 97  LBN                        Lebanon /
# 98  LBR                        Liberia /
# 99  LBY                          Libya /
# 100 LCA                      St. Lucia /
# 101 LIE                  Liechtenstein /
# 102 LKA                      Sri Lanka /
# 103 LSO                        Lesotho /
# 104 LTU                      Lithuania <- LIT
df_culture$id <- gsub("LIT", "LTU", df_culture$id) # Replacing IS with SOLL
# 106 LVA                         Latvia <- LAT
df_culture$id <- gsub("LAT", "LVA", df_culture$id) # Replacing IS with SOLL
# 107 MAR                        Morocco /
# 108 MCO                         Monaco /
# 109 MDA                        Moldova <- MOL
df_culture$id <- gsub("MOL", "MDA", df_culture$id) # Replacing IS with SOLL
# 110 MDG                     Madagascar /
# 111 MDV                       Maldives /
# 113 MHL               Marshall Islands /
# 114 MKD                North Macedonia <- MAC
df_culture$id <- gsub("MAC", "MKD", df_culture$id) # Replacing IS with SOLL
# 117 MMR                        Myanmar /
# 118 MNE                     Montenegro <- MNG
df_culture$id <- gsub("MNG", "MNE", df_culture$id) # Replacing IS with SOLL
# 120 MOZ                     Mozambique /
# 121 MRT                     Mauritania /
# 122 MUS                      Mauritius /
# 123 MWI                         Malawi /
# 124 MYS                       Malaysia <- MAL
df_culture$id <- gsub("MAL", "MYS", df_culture$id) # Replacing IS with SOLL
# 125 NAM                        Namibia /
# 126 NER                          Niger /
# 127 NGA                        Nigeria <- NIG
df_culture$id <- gsub("NIG", "NGA", df_culture$id) # Replacing IS with SOLL
# 128 NIC                      Nicaragua /
# 129 NLD                    Netherlands <- NET
df_culture$id <- gsub("NET", "NLD", df_culture$id) # Replacing IS with SOLL
# 131 NPL                          Nepal /
# 132 NRU                          Nauru /
# 134 OMN                           Oman /
# 138 PHL                    Philippines <- PHI
df_culture$id <- gsub("PHI", "PHL", df_culture$id) # Replacing IS with SOLL
# 139 PLW                          Palau /
# 140 PNG               Papua New Guinea /
# 142 PRK      Korea, Dem. People's Rep. /
# 143 PRT                       Portugal <- POR
df_culture$id <- gsub("POR", "PRT", df_culture$id) # Replacing IS with SOLL
# 144 PRY                       Paraguay /
# 145 QAT                          Qatar /
# 146 ROU                        Romania <- ROM
df_culture$id <- gsub("ROM", "ROU", df_culture$id) # Replacing IS with SOLL
# 150 SDN                          Sudan /
# 151 SEN                        Senegal /
# 152 SGP                      Singapore <- SIN
df_culture$id <- gsub("SIN", "SGP", df_culture$id) # Replacing IS with SOLL
# 153 SLB                Solomon Islands /
# 154 SLE                   Sierra Leone /
# 156 SMR                     San Marino /
# 157 SOM                        Somalia /
# 158 SRB                         Serbia <- SER
df_culture$id <- gsub("SER", "SRB", df_culture$id) # Replacing IS with SOLL
# 159 SSD                    South Sudan /
# 160 STP          Sao Tome and Principe /
# 162 SVK                Slovak Republic <- SLK
df_culture$id <- gsub("SLK", "SVK", df_culture$id) # Replacing IS with SOLL
# 163 SVN                       Slovenia <- SLV
df_culture$id <- gsub("SLV", "SVN", df_culture$id) # Replacing IS with SOLL
# 165 SWZ                       Eswatini /
# 166 SYC                     Seychelles /
# 167 SYR           Syrian Arab Republic /
# 168 TCD                           Chad /
# 169 TGO                           Togo /
# 171 TJK                     Tajikistan /
# 172 TKM                   Turkmenistan /
# 173 TLS                    Timor-Leste /
# 174 TON                          Tonga /
# 175 TTO            Trinidad and Tobago <- TRI
df_culture$id <- gsub("TRI", "TTO", df_culture$id) # Replacing IS with SOLL
# 176 TUN                        Tunisia /
# 178 TUV                         Tuvalu /
# 179 TZA                       Tanzania <- TAN
df_culture$id <- gsub("TAN", "TZA", df_culture$id) # Replacing IS with SOLL
# 182 URY                        Uruguay <- URU
df_culture$id <- gsub("URU", "URY", df_culture$id) # Replacing IS with SOLL
# 184 UZB                     Uzbekistan /
# 185 VCT St. Vincent and the Grenadines /
# 187 VNM                        Vietnam <- VIE
df_culture$id <- gsub("VIE", "VNM", df_culture$id) # Replacing IS with SOLL
# 188 VUT                        Vanuatu /
# 189 WSM                          Samoa /
# 190 YEM                    Yemen, Rep. /
# 191 ZAF                   South Africa <- SAF/SAW
df_culture$id <- gsub("SAW", "ZAF", df_culture$id)
# south africe constists of SAF a later survey and SAW where only whites were participants)
df_culture$lto[df_culture$id == "ZAF"] <- df_culture$lto[df_culture$id == "SAF"]
df_culture$ivr[df_culture$id == "ZAF"] <- df_culture$ivr[df_culture$id == "SAF"]
df_culture <- filter(df_culture, id != "SAF")
# 192 ZMB                         Zambia <- ZAM
df_culture$id <- gsub("ZAM", "ZMB", df_culture$id) # Replacing IS with SOLL
# 193 ZWE                       Zimbabwe <- ZIM
df_culture$id <- gsub("ZIM", "ZWE", df_culture$id) # Replacing IS with SOLL

# merge data2 and df_culture again after adapting the id's
data3 <- merge(data2, df_culture, by = "id", suffixes = c("", ".culture"))



#### FINAL MERGES ####

#merging data2 and df_culture2
dataxl <- merge(data2, df_culturexl, by = "countryname", suffixes = c("", ".culture"))

# use only needed columns from data3
data4 <- data3[,c("countryname", "id","incomegroup", "gdppc2022", "loggdppc", "avg5ygr",#gdppc columns
                  "va","pv","ge", "rq", "rl", "cc",  # wgi columns
                  "sdgscore" , paste0("goal", 1:17, "score"),
                  "pdi", "idv", "mas", "uai", "lto", "ivr")]
data <- data4
dataextended <- data3

# use only needed columns from dataxl
dataextendedxl <- dataxl
dataxl <- dataxl[,c("countryname", "id","incomegroup", "gdppc2022", "loggdppc","avg5ygr",#gdppc columns
                  "va","pv","ge", "rq", "rl", "cc",# wgi columns
                  "sdgscore" , paste0("goal", 1:17, "score"),
                  "pdi", "idv", "mas", "uai", "lto", "ivr")]
rm(data1, data2, data3, data4, missing)

# look for mismatched data
subset_data <- dataextended %>%
  select(id, contains("country"))
#delete rows BEN (Benin <> Belgium French and CAF <> Canada French)
#data <- data[-which(data$id == "BEN" | data$id == "CAF"), ]
#delete working variables
rm(subset_data)

data <- data[!rowSums(is.na(data[, c("pdi", "idv", "mas", "uai", "lto", "ivr")])), ] # remove rows where cultural values are missing
dataxl <- dataxl[!is.na(dataxl$gdppc2022) & !is.na(dataxl$sdgscore),] # remove rows where gdpc2022 or sdgscore is.na (selecting the one where is.na is not true by ! reversing)

rm(dataextended, dataextendedxl) # delete unnecessary dfs



#### PRICIPAL COMPONENT ANALYSIS ####

row.names(dataxl) <- dataxl$id
wgipca <- prcomp(dataxl[,c("va", "pv", "ge", "rq", "rl", "cc")], scale. = TRUE) # create df with wgis only
sum(wgipca$rotation[,1]^2) # check that sum of squares of loadings is 1
pca <- data.frame(summary(wgipca)$importance)
pca <- add_column(pca,rownames(pca), .after = 0)
pcar2 <- round((wgipca$sdev^2/sum(wgipca$sdev^2))*100, digits = 1)
pc1lab <- paste("PC1 (",pcar2[1]," explained var.)", sep = "")
pc2lab <- paste("PC2 (",pcar2[2]," explained var.)", sep = "")

biplot(wgipca, col = c("black","red"), arrow.len = 0.1, cex = c(0.6,1), 
       xlab = pc1lab, ylab = pc2lab,
       #font.lab = 2, font.axis = 1, font.main = 2,
       family = "Times")
       abline(h = 0, v = 0, col = "gray", lty = "solid")
       grid(nx = NULL, ny = NULL, col = "gray", lty = "solid", lwd = 0.5)
fviz_pca_biplot(wgipca,
                  axes = c(1, 2),
                  geom = c("point","text"),
                  #geom.ind = geom,
                  geom.var = c("arrow", "text"),
                xlab = pc1lab, ylab = pc2lab,
                arrow.length = 0.1, #labelsize = c(2,1),
                  col.ind = "black",
                  col.var = "red",
                  label = "all",
                  repel = FALSE,
                  palette = NULL,
                  addEllipses = FALSE,
                font.family = "Times") + theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

# Manual calculation of PX1 Matrix
X <- scale(dataxl[, c("va", "pv", "ge", "rq", "rl", "cc")]) #export scaled X matrix
wgieigenvector <- as.data.frame(wgipca$rotation)# export rotation matrix --> Eigenvector Matrix
pc1eigenvector <- wgieigenvector[,1] # export PC1 Eigen/loading vector
pc1eigenvector <- as.numeric(pc1eigenvector) # transform to numeric
wgieigenvector <- add_column(wgieigenvector,rownames(wgieigenvector), .after = 0) # add row names for xls ecport
X <- as.matrix(X) # transform into matrix to use %*%
wgipcaX <- rowSums(X %*% pc1eigenvector) # multiply X with eigenvector
head(wgipcaX) # compare the calculation against the prcomp output
head(wgipca$x[,1])

#KMO kaiser meyer olkin
kmo <- KMO(X)
kmoextract <- t(as.data.frame(kmo$MSAi))
kmoextract <- as.data.frame(kmoextract)
kmoextract <- add_column(kmoextract, overall=kmo$MSA, .after = 0) 
kmoextract <- round(kmoextract, digits=3)
kmoextract <- add_column(kmoextract, variable="msa", .after = 0) 

# Correlation
wgi_scaled <- as.data.frame(cbind(wgipca$x[,c(1:2)],X)) # whether scaled or unscaled data is used does not affect the cor results
corpca <- cor(wgi_scaled) # create a corelation matrix using pearson
corpca <- round(corpca, digits = 3) # round the corelation coefficients
corpca <- as.data.frame(corpca[-c(1:2),c(1:2)]) # extract the first 2 columns
corpca <- add_column(corpca, rownames(corpca), .after=0) # add the rownames as column for later xls export

#Compute into dataxl
pc1x <- wgipca$x[,1] # store principal component scores
pc1x <- ((pc1x - min(pc1x)) / (max(pc1x) - min(pc1x))) * 100 # normalise the principal component to the culture scores
plot(pc1x, wgipca$x[,1]) # check whether data is transformed accordingly
dataxl$wgi <- pc1x

rm(wgi_scaled,wgipcaX, X, pc1x,pc1lab,pc2lab,pcar2,pc1eigenvector, kmo) # delete working variables



#### CORRELATION MATRICES ####

#creating covariance / correlation dataset
cordata <-dataxl[,c("pdi", "idv", "mas", "uai", "lto", "ivr","wgi","loggdppc","avg5ygr")]
cor_all <- cor(cordata) # creating correlation matrix
corrplot(cor_all, type = "lower") # for individdualisation: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

cor_all <- round(cor_all, digits = 3) # rounding the digits
cor_all <- as.data.frame(cor_all) # turning into dfs. Not usable for plots but for p.mat

cor_all_p <- cor.mtest(cordata)
cor_all_p <- cor_all_p$p
cor_all_p <- ifelse(cor_all_p[,] < 0.001, "***", ifelse(cor_all_p[,] < 0.01, "**", ifelse(cor_all_p[,] < 0.05, "*", ""))) #rebuilding the * significance display
cor_all_p <- as.data.frame(cor_all_p)
cor_all_p <- data.frame(mapply(function(x, y) paste(x, y, sep = " "), cor_all, cor_all_p))
cor_all_p[upper.tri(cor_all_p)] <- ""
row.names(cor_all_p) <- colnames(cor_all_p)
cor_all_p <- add_column(cor_all_p, rownames(cor_all_p), .after=0)

#chart.Correlation(cordata, histogram=TRUE, pch=19)

rm(cordata)

# only wgis
cordata <-dataxl[,c("va","pv","ge","rq","rl","cc")]
cor_wgi <- cor(cordata)
cor_wgi <- round(cor_wgi, digits = 3)
cor_wgi <- as.data.frame(cor_wgi)
cor_wgi_p <- cor.mtest(cordata)
cor_wgi_p <- cor_wgi_p$p
cor_wgi_p <- ifelse(cor_wgi_p[,] < 0.001, "***", ifelse(cor_wgi_p[,] < 0.01, "**", ifelse(cor_wgi_p[,] < 0.05, "*", ""))) #rebuilding the * significance display
cor_wgi_p <- as.data.frame(cor_wgi_p)
cor_wgi_p <- data.frame(mapply(function(x, y) paste(x, y, sep = " "), cor_wgi, cor_wgi_p))
cor_wgi_p[upper.tri(cor_wgi_p)] <- ""
row.names(cor_wgi_p) <- colnames(cor_wgi_p)

rm(cordata)



#### SUBSET CREATION ####

if (length(unique(dataxl$incomegroup))>3){ # if more than 3 unique values exist vor dataxl$incomegroup
  dataxl$incomegroup <- gsub("lower-middle-income|low-income","lower-middle-low-income",dataxl$incomegroup) # create aggregate lower group
}

cluster <- unique(dataxl$incomegroup) # extract unique income groups

subset_names <- list() # create an empty list for subsets to be created
columns <- c("incomegroup") # write a list with all columnnames that should be used to create filtered datasets in the following
list_df <- c("dataxl")

for (df in list_df) {
  # Get the subset data frame
  subset_df <- get(df)
  
  # loop through every column in the columns list
  for (column in columns) {
    for (i in cluster) {
      # creating a dataset that is filtered based on the current column and respective value
      subset_name <- paste0(df, "_", i) # create a name for the dataset based on the sued df
      assign(subset_name, subset_df[subset_df[, column] == i, ]) # assign the subset the filtered data
      
      # add the created dataset_name to the list of dataset_names
      subset_names <- c(subset_names, subset_name)
    }
  }
}
rm(column, columns, df, i, list_df, subset_name, subset_df)
# add the data- dataset name to the subset_names
subset_names <- append(subset_names, "dataxl", after = 0) # 1st position
new_order <- c(1, 4, 2, 3)
subset_names <- subset_names[new_order]
rm(new_order)



#### SUMMARY STATISTICS ####

# FOR USED VARIABLES

variables <- c("sdgscore",paste0("goal",1:17,"score"), # create list with all used variabeles
               "wgi",
               "pdi","idv","mas","uai", "lto","ivr",
               "loggdppc","avg5ygr")

sumstat <- data.frame() # create a data.frame to put the results

for (i in variables) { # loop through the variables
  descrstat <- c(i, # variable name
                 min(dataxl[[i]],na.rm = TRUE), # minimum
                 max(dataxl[[i]],na.rm = TRUE), # maximum
                 var(dataxl[[i]],na.rm = TRUE), # variance
                 sd(dataxl[[i]],na.rm = TRUE), # standard deviation
                 mean(as.numeric(dataxl[[i]]),na.rm = TRUE), # mean
                 length(dataxl[[i]])-sum(is.na(dataxl[[i]]))) # length - NAs --> number of observations
  sumstat <- rbind(sumstat, descrstat)
}

names(sumstat) <- c("variable", "min", "max", "var", "sd","mean", "n") # rename the colnames
sumstat$min <- as.numeric(sumstat$min) # transform values as numeric in order to round later
sumstat$max <- as.numeric(sumstat$max)
sumstat$var <- as.numeric(sumstat$var)
sumstat$sd <- as.numeric(sumstat$sd)
sumstat$mean <- as.numeric(sumstat$mean)
sumstat[, 2:6] <- round(sumstat[, 2:6], digits = 3) # round values in columns 2:6
sumstat <- select(sumstat, -var) # delete variance as redundant and aer uses SD

rm(variables)

# FOR INCOME CLUSTERS

list_df <- c("data","dataxl")
for (df in list_df) {
  # Get the subset data frame
  subset_df <- get(df)
  datadescrstat <- subset_df[!grepl("\\.\\.", subset_df$gdppc2022), ]
  
  datadescrstat$gdppc2022 <- as.numeric(datadescrstat$gdppc2022)
  sumdescrstat <- data.frame()
  
  # extract descr. stat for all observations
  descrstat <- c("all",
                 min(datadescrstat$gdppc2022,na.rm = TRUE),
                 max(datadescrstat$gdppc2022,na.rm = TRUE),
                 var(datadescrstat$gdppc2022,na.rm = TRUE),
                 sd(datadescrstat$gdppc2022,na.rm = TRUE),
                 mean(as.numeric(datadescrstat$gdppc2022),na.rm = TRUE),
                 length(datadescrstat$incomegroup))
  sumdescrstat <- rbind(sumdescrstat, descrstat)
  
  # extract descr. stat for cluster
  for (i in cluster) {
    descrstat <- c(paste0(i),
                   min(datadescrstat$gdppc2022[datadescrstat$incomegroup == i],na.rm = TRUE),
                   max(datadescrstat$gdppc2022[datadescrstat$incomegroup == i],na.rm = TRUE),
                   var(datadescrstat$gdppc2022[datadescrstat$incomegroup == i],na.rm = TRUE),
                   sd(datadescrstat$gdppc2022[datadescrstat$incomegroup == i],na.rm = TRUE),
                   mean(as.numeric(datadescrstat$gdppc2022[datadescrstat$incomegroup == i]),na.rm = TRUE),
                   length(datadescrstat$incomegroup[datadescrstat$incomegroup == i]))
    sumdescrstat <- rbind(sumdescrstat, descrstat)
  }
  
  names(sumdescrstat) <- c("incomegroup", "min", "max", "var", "sd","mean", "n") # rename the colnames
  sumdescrstat$min <- as.numeric(sumdescrstat$min) # transform values as numeric in order to round later
  sumdescrstat$max <- as.numeric(sumdescrstat$max)
  sumdescrstat$var <- as.numeric(sumdescrstat$var)
  sumdescrstat$sd <- as.numeric(sumdescrstat$sd)
  sumdescrstat$mean <- as.numeric(sumdescrstat$mean)
  sumdescrstat[, 2:6] <- round(sumdescrstat[, 2:6], digits = 2) # round values
  
  # reorder the values
  custom_order <- c("all", "high-income", "upper-middle-income", "lower-middle-low-income") # Create a custom order vector
  sumdescrstat <- sumdescrstat[match(custom_order, sumdescrstat$incomegroup), ] # Reorder the dataset based on the custom order
  
  sumstat_name <- paste0("sumdescrstat",df) # create a name for the dataset based on the used df
  assign(sumstat_name, sumdescrstat) # assign the subset the filtered data
  
  rm(i,datadescrstat, descrstat, custom_order, sumdescrstat, sumstat_name, subset_df) # delete working variabels
}
rm(list_df, df) 



#### REGRESSIONS ####

##### SDG ~ Culture + Institutions + Economic Development ####

dependent_variables <- c("sdgscore" , paste0("goal", 1:17, "score")) # all dependent variables
independent_variables <- "~ pdi + idv + mas + uai + lto + ivr + wgi + loggdppc + avg5ygr" # write the static independent part
result_sdg <- data.frame()

for (dep_var in dependent_variables) {
  for (subset_name in subset_names) {
    
    # Get the subset data frame
    subset_df <- get(subset_name) 
    
    # Skip regression if only NAs
    if (sum(!is.na(subset_df[[dep_var]])) == 0) { # check if the dependent variable in the subset has any non-NA values in the subset data frame
      next # Skip this regression if there are no non-NA values / if there are ONLY NAs
    }
    
    # Perform the regression
    formula <- as.formula(paste(dep_var, independent_variables)) # creating a formula by pasting together dependent and independent variable
    reg <- lm(formula, data = subset_df)
    sumreg <- summary(reg)
    
        # Extract and Transform Results
    coefficients <- coef(sumreg) # extract coefficients from the summary of the regression (more data tat coef)
    coefficient <- sprintf("%.3f", coefficients[,"Estimate"])
    significance <- ifelse(coefficients[, 4] < 0.001, "***", 
                           ifelse(coefficients[, 4] < 0.01, "**", 
                                  ifelse(coefficients[, 4] < 0.05, "*", ""))) #rebuilding the * significance display
    standard_errors <- sprintf("(se: %.3f)", coefficients[, "Std. Error"])
    coefficient_p_value <- sprintf("(p: %.3f)", coefficients[, "Pr(>|t|)"]) # stransforms into character strings keeping 4 digits
    extract <- cbind(coefficient, coefficient_p_value, significance, standard_errors) # put columns together
    extract <- as.data.frame(extract) # transform into dataframe
    extract <- unite(extract, reg, coefficient, coefficient_p_value, significance, standard_errors, sep = " ") # unites coefficient and significance column with sep " " into new column reg in the df extract
    extract <- t(extract) # transpose extract
    extract <- as.data.frame(extract) # transform into dataframe
      coefficient2 <- sprintf("%.3f", coefficients[,"Estimate"])
      standard_errors2 <- sprintf("(se: %.3f)", coefficients[, "Std. Error"])
      coefficient_p_value2 <- sprintf("(p: %.5f)", coefficients[, "Pr(>|t|)"])
      extract2 <- cbind(coefficient2, coefficient_p_value2, standard_errors2) # put columns together
      extract2 <- as.data.frame(extract2) # transform into dataframe
      extract2 <- unite(extract2, reg, coefficient2, coefficient_p_value2, standard_errors2, sep = " ") # unites coefficient and significance column with sep " " into new column reg in the df extract
      extract2 <- t(extract2) # transpose extract
      extract2 <- as.data.frame(extract2) # transform into dataframe
      colnames(extract2) <- paste0(colnames(extract),"_2")
    r_squared <- sumreg$r.squared # extracting the R^2
    r_squared <- round(r_squared, digits=3)# round the R^2 to 3 digits
    # if the fstatistic is not NA, we extract the latter (might be Null if completely insignificant or too little observations in a subset)
    if (!is.null(sumreg$fstatistic) && !any(is.na(sumreg$fstatistic))) {
      fstat <- t(sumreg$fstatistic)
      fstat <- round(fstat, digits=3) # round the fstat to 3 digits
      qf <- qf(0.95, fstat[1,2], fstat[1,3]) # calculate the critical F-value (sign. level, degrees of freedom numerator, degrees of freedom denumerator)
      qf <- round(qf, digits = 3) # round the critical F-value to 3 digits after the comma
      qf <- as.data.frame(qf)} # transform into dataframe
    name <- paste0(dep_var, "_", subset_name) # chain dependent variable and subset name
    name <- as.data.frame(name) # transform into dataframe
    res.std.error <- round(sumreg$sigma, digits=3) # extract std. error of the model
    adjusted_r_squared <- round(sumreg$adj.r.squared, digits = 3)
    p <- lmp(reg)
    n <- nobs(reg) # count rows (sample size) 
    ifelse(exists("fstat"), # if fstat exists (has been initialized earlier) include it in the cbind
           extract <- cbind(name, n, extract,r_squared, adjusted_r_squared, res.std.error, p, extract2,  fstat, qf), 
           extract <- cbind(name, n, extract, r_squared, adjusted_r_squared, res.std.error, p, extract2))
    
    result_sdg <- bind_rows(result_sdg, extract) # add the current extract to the result
    fstat <- c("We need to create a variable in case none was created bc NULL in order to delete it in the next step")
    qf <- c("We need to create a variable in case none was created bc NULL in order to delete it in the next step")
    rm(extract2, coefficient2,standard_errors2,coefficient_p_value2)
    rm(coefficients, coefficient, significance, r_squared, fstat, extract,coefficient_p_value, qf, name, n, res.std.error, adjusted_r_squared, sumreg, formula,standard_errors, p) # remove working variables from the loop
  }
}

rm(dep_var, dependent_variables, independent_variables, subset_name)


###### Result Manipulation ####
  
  # rename the column containing the F value accoridngly + create an F statistic summary
  result_sdg <- rename(result_sdg, F = "value")
  result_sdg$fstatsummary <- paste0("F (",result_sdg$numdf, ", ", result_sdg$dendf,") = ",result_sdg$F, 
                                    ifelse(result_sdg$p < 0.001, ", p < 0.001", 
                                           ifelse(result_sdg$p < 0.01, ", p < 0.01", 
                                                  ifelse(result_sdg$p < 0.05, ", p < 0.05", "")))) # 9 is numdf = the number of independent variables
  
  # extract income group from name
  result_sdg$income <- gsub("^.*_dataxl","",result_sdg$name) #replace everything before and including _dataxl with ""
  result_sdg$income <- gsub("_","",result_sdg$income) #Replace _ with ""
  result_sdg$income <- ifelse(grepl("income", result_sdg$income), result_sdg$income, "All")
  
  # delete _dataxl from name column 
  result_sdg$name <- gsub("_dataxl","",result_sdg$name)

  # enrich the result dataframe with sdg goal mapping
  result_sdg$sdgcat <- sdgmapping$sdgcat[match(gsub("\\D", "", result_sdg$name), sdgmapping$sdggoal)]# write into result$sdgcat the value that is found by matching the number in result$name with sdgmapping$sdggoal
  result_sdg$sdgname <- sdgmapping$sdgfullname[match(gsub("\\D", "", result_sdg$name), sdgmapping$sdggoal)] # gsub deletes all non-numeric values in result$name
  result_sdg$sdgcat <- replace(result_sdg$sdgcat, is.na(result_sdg$sdgcat), "overall") # overwrite NAs with "all"
  result_sdg$sdgname <- replace(result_sdg$sdgname, is.na(result_sdg$sdgname), "overall") # overwrite NAs with "all"
  
  #create result_sdg for backup
  result_sdg_unfiltered <- result_sdg
  
  #round p value to 3 digits
  result_sdg$p <- round(result_sdg$p,digits=5)

  # create a vector with all variables/coefficients for later use
  variables <- c("(Intercept)","pdi","idv","mas","uai","lto","ivr","wgi","loggdppc","avg5ygr")
  
  #preparecoefficient output
  for (i in variables) {
    result_sdg[[i]] <- gsub("se: ","",result_sdg[[i]])
    result_sdg[[i]] <- gsub("\\s*\\(p:[^)]*\\)", "", result_sdg[[i]])
    result_sdg[[i]] <- gsub("  ", " ", result_sdg[[i]])
  } 
  rm(i)
  
  # create an aggregate column with sign. predictors only
  variables <- c("pdi","idv","mas","uai","lto","ivr","wgi","loggdppc","avg5ygr") # excluding the intercept
  result_sdg$significant_predictors <- ""
      for (i in variables) {
        result_sdg$significant_predictors <- 
          paste0(result_sdg$significant_predictors, ifelse(grepl("\\*", result_sdg[[i]]), paste0(i, " ",result_sdg[[i]], ", "), ""))
      } 
  rm(i)
      result_sdg$significant_predictors <- sub(",([^,]*)$", "", result_sdg$significant_predictors) # looks for a comma followed by any number of non-comma characters at the end of the string. We then replace this with an empty string
  
  # filter out insignificant models according to different criteria
  result_sdg_p_filtered <- subset(result_sdg, p < 0.05) # create subset with models p>0.05
  #result_sdg_f_filtered <- subset(result_sdg, F > qf)  # create a subset with F > critical value
  result_sdg_star_filtered <- result_sdg # copy original
    for (i in variables) { #für alle variables
      result_sdg_star_filtered[[i]][!grepl("\\*", result_sdg_star_filtered[[i]])] <- "" # ersetze Zeilen ohne * mit ""
    }
    rm(i)
    result_sdg_star_filtered <- subset(result_sdg_star_filtered,significant_predictors !="")
  
  result_sdg_star_minus_p <- subset(result_sdg_star_filtered, !(name %in% result_sdg_p_filtered$name))
  
  result_sdg_slim <- result_sdg  
  for (i in variables) { # running a loop over all coefficient columns
    result_sdg_slim[[i]] <- ifelse(grepl("\\*", result_sdg_slim[[i]]), ifelse(result_sdg_slim[[i]] > 0,paste0("+",result_sdg_slim$cluster),paste0("-",result_sdg_slim$cluster)), "")
    #if the value contains * then next loop or "". next loop, if positiv -> "+" and cluster otherwise "-" and cluster
  }
  rm(i)


  
#### INTERACTIONS ####

  # defining possible interaction
  interact_vars <- c("wgi","pdi","idv","mas","uai", "lto","ivr","loggdppc","avg5ygr")  # Replace with your actual variable names
  interact_combinations <- combn(interact_vars, 2, simplify = TRUE)
  interact_combinations <- t(interact_combinations)
  interact_combinations <- as.data.frame(interact_combinations)
  interact_combinations$formula <- paste0(interact_combinations$V1,"+",interact_combinations$V2,"+",interact_combinations$V1,"*",interact_combinations$V2)
  interact_combinations$I <- paste(interact_combinations$V1,interact_combinations$V2, sep = "*")
  interaction <- as.list(interact_combinations$I)
  #interaction <- as.list(interact_combinations$formula)
  
  # Original regression model
  original_formula <- ("sdgscore ~ pdi + idv + mas + uai + lto + ivr + wgi + loggdppc + avg5ygr +")
  #original_formula <- ("sdgscore ~ ")
  result_interact <- data.frame()
  
  for (i in interaction) {
    #interact_formula <- as.formula(paste(original_formula,"+" ,i))
    interact_formula <- as.formula(paste(original_formula ,i))
    reg <- lm(interact_formula, data = dataxl)
    sumreg <- summary(reg)
    
    #RESULT EXTRACTION
    coefficients <- coef(sumreg) # extract coefficients from the summary of the regression (more data tat coef)
    coefficient <- sprintf("%.3f", coefficients[,"Estimate"])
    significance <- ifelse(coefficients[, 4] < 0.001, "***", ifelse(coefficients[, 4] < 0.01, "**", ifelse(coefficients[, 4] < 0.05, "*", ""))) #rebuilding the * significance display
    standard_errors <- sprintf("(se: %.3f)", coefficients[, "Std. Error"])
    coefficient_p_value <- sprintf("(p: %.3f)", coefficients[, "Pr(>|t|)"]) # stransforms into character strings keeping 4 digits
    extract <- cbind(coefficient, coefficient_p_value,significance, standard_errors) # put columns together
    extract <- as.data.frame(extract) # transform into dataframe
    extract <- unite(extract, reg, coefficient, coefficient_p_value, significance, standard_errors, sep = " ") # unites coefficient and significance column with sep " " into new column reg in the df extract
    extract <- t(extract) # transpose extract
    extract <- as.data.frame(extract) # transform into dataframe
    #extract <- paste0(colnames(extract)," ",extract)
    cols_to_rename <- grepl(":", colnames(extract))# Rename columns in 'extract' data frame containing ":" to "I"
    colnames(extract)[cols_to_rename] <- "I"
      coefficient2 <- sprintf("%.3f", coefficients[,"Estimate"])
      standard_errors2 <- sprintf("(se: %.3f)", coefficients[, "Std. Error"])
      coefficient_p_value2 <- sprintf("(p: %.5f)", coefficients[, "Pr(>|t|)"])
      extract2 <- cbind(coefficient2, coefficient_p_value2, standard_errors2) # put columns together
      extract2 <- as.data.frame(extract2) # transform into dataframe
      extract2 <- unite(extract2, reg, coefficient2, coefficient_p_value2, standard_errors2, sep = " ") # unites coefficient and significance column with sep " " into new column reg in the df extract
      extract2 <- t(extract2) # transpose extract
      extract2 <- as.data.frame(extract2) # transform into dataframe
      colnames(extract2) <- paste0(colnames(extract),"_2")
    r_squared <- sumreg$r.squared # extracting the R^2
    r_squared <- round(r_squared, digits=3)# round the R^2 to 3 digits
    # if the fstatistic is not NUll, we extract the latter (might be Null if completely insignificant or too little observations in a subset)
    if (!is.null(sumreg$fstatistic)) {
      fstat <- t(sumreg$fstatistic)
      fstat <- round(fstat, digits=3) # round the fstat to 3 digits
      qf <- qf(0.95, fstat[1,2], fstat[1,3]) # calculate the critical F-value (sign. level, degrees of freedom numerator, degrees of freedom denumerator)
      qf <- round(qf, digits = 3) # round the critical F-value to 3 digits after the comma
      qf <- as.data.frame(qf)} # transform into dataframe
    name <- paste0(i) # chain dependent variable and subset name
    name <- as.data.frame(name) # transform into dataframe
    res.std.error <- round(sumreg$sigma, digits=3) # extract std. error of the model
    adjusted_r_squared <- round(sumreg$adj.r.squared, digits = 3)
    p <- lmp(reg)
    n <- nobs(reg) # count rows (sample size) 
    ifelse(exists("fstat"), # if fstat exists (has been initialized earlier) include it in the cbind
           extract <- cbind(name, n, extract, r_squared, adjusted_r_squared, res.std.error, p,extract2, fstat, qf), 
           extract <- cbind(name, n, extract, r_squared, adjusted_r_squared, res.std.error, p,extract2))
    result_interact <- bind_rows(result_interact, extract) # add the current extract to the result
    fstat <- c("We need to create a variable in case none was created bc NULL in order to delete it in the next step")
    #print(paste0(i, " - " ,name))
    rm(extract2, coefficient2,standard_errors2,coefficient_p_value2)
    rm(coefficients, coefficient, significance, r_squared, fstat, extract, qf, name, n, res.std.error, adjusted_r_squared, sumreg, cols_to_rename, interact_formula, standard_errors, p) # remove working variables from the loop
  }
  rm(i, interact_vars, interact_combinations, original_formula, interaction)
  
  
  ##### Result Manipulation ####
  
  result_interact <- rename(result_interact, F = "value")
  result_interact$fstatsummary <- paste0("F(",9, ", ", result_interact$n-(9+1),") =",result_interact$F, 
                                         ifelse(result_interact$p < 0.001, ", p < 0.001", 
                                                ifelse(result_interact$p < 0.01, ", p < 0.01", 
                                                       ifelse(result_interact$p < 0.05, ", p < 0.05", "")))) # 9 is numdf = the number of independent variables
  
  # create a vector with all variables/coefficients for later use
  variables <- c("(Intercept)","pdi","idv","mas","uai","lto","ivr","wgi","loggdppc","avg5ygr","I")
  
  #preparecoefficient output
  for (i in variables) {
    result_interact[[i]] <- gsub("se: ","",result_interact[[i]])
    result_interact[[i]] <- gsub("\\s*\\(p:[^)]*\\)", "", result_interact[[i]])
    result_interact[[i]] <- gsub("  ", " ", result_interact[[i]])
  } 
  rm(i)
  # create a vector with all variables/coefficients for later use
  variables <- c("pdi","idv","mas","uai","lto","ivr","wgi","loggdppc","avg5ygr","I")
  
  # create an aggregate column with sign. predictors only
  result_interact$significant_predictors <- ""
  for (i in variables) {
    result_interact$significant_predictors <- 
      paste0(result_interact$significant_predictors, ifelse(grepl("\\*", result_interact[[i]]), paste0(i, " ",result_interact[[i]], ", "), ""))
  } 
  rm(i)
  result_interact$significant_predictors <- sub(",([^,]*)$", "", result_interact$significant_predictors) # looks for a comma followed by any number of non-comma characters at the end of the string. We then replace this with an empty string
  
  # filter out insignificant models according to different criteria
  result_interact_p_filtered <- subset(result_interact, p < 0.05) # create subset with models p>0.05
  #result_interact_f_filtered <- subset(result_interact, F > qf)  # create a subset with F > critical value
  result_interact_star_filtered <- result_interact # copy original
  for (i in variables) { #für alle variables
    result_interact_star_filtered[[i]][!grepl("\\*", result_interact_star_filtered[[i]])] <- "" # ersetze Werte ohne * mit ""
  }
  rm(i, variables)
  result_interact_star_filtered <- subset(result_interact_star_filtered,significant_predictors !="")
  
  #extract results with significant interaction
  result_interact_significant_I <- subset(result_interact, grepl("\\*", I))
  
  
  ##### Interaction visualisation ####
  
  print(result_interact_significant_I$name) # print significant interaction terms
  #[1] "wgi*loggdppc" "idv*uai"      "ivr*avg5ygr" 
  
  reg_wgi_loggdppc <- lm(sdgscore ~ pdi + idv + mas + uai + lto + ivr + wgi + loggdppc + avg5ygr + wgi*loggdppc, dataxl)
  reg_idv_uai <-lm(sdgscore ~ pdi + idv + mas + uai + lto + ivr + wgi + loggdppc + avg5ygr +idv*uai, dataxl)
  reg_ivr_avg5ygr <- lm(sdgscore ~ pdi + idv + mas + uai + lto + ivr + wgi + loggdppc + avg5ygr +ivr*avg5ygr, dataxl)
  
  # create ggpllt theme specifying times new roman as font for everything
  tnr <- theme(text = element_text(family = "Times New Roman"),
               legend.text = element_text(family = "Times New Roman"),
               legend.title = element_text(family = "Times New Roman"),
               axis.text.x = element_text(family = "Times New Roman"),
               axis.text.y = element_text(family = "Times New Roman"),
               axis.title.x = element_text(family = "Times New Roman"),
               axis.title.y = element_text(family = "Times New Roman"))
  size <- theme(legend.text = element_text(size = 6))
  # creating ggplot theme with several adjustments
  spec <- theme(legend.position = "bottom",
                legend.key.width= unit(0.8, 'cm'),
                legend.key.height= unit(0.5, 'cm'),
                aspect.ratio = 1)
  
  #interact_plot(reg_ivr_avg5ygr, pred = ivr, modx = avg5ygr, plot.points = TRUE, interval = TRUE) 
  interact_reg <- c("reg_wgi_loggdppc","reg_idv_uai","reg_ivr_avg5ygr")
  interact_plot_list <- c()
  result_sim_slope <- data.frame()
  for (i in interact_reg) {
    x <- gsub("^reg_|_[^.]*$", "",i) # extract the first term
    z<- gsub("^.*_", "", i) # extract the second term
    I <- get(i) # retrieve thge lm object
    
    # create plots for both variables as moderation and predictor respectively
    assign(paste0("interact_",x,"_",z,"_interact_plot"), 
           interact_plot(I, pred = !!x, modx = !!z, plot.points = TRUE, point.alpha = 0.8, interval = TRUE, rug = TRUE, rug.sides = "bl", colors = c("#F8766D", "#00BA38", "#619CFF")) + spec + tnr ) # assign the plot to the paste0 variable
    assign(paste0("interact_",z,"_",x,"_interact_plot"), 
           interact_plot(I, pred = !!z, modx = !!x, plot.points = TRUE, point.alpha = 0.8, interval = TRUE, rug = TRUE, rug.sides = "bl", colors = c("#F8766D", "#00BA38", "#619CFF")) + spec + tnr ) # assign the plot to the paste0 variable
    # assign(paste0("interact_",x,"_",z,"_ggpredict_plot"), 
    #        ggpredict(I, terms = c(x,z)) |> plot(show_data=TRUE, colors = c("#ffa630", "#4da1a9", "#996888")))
    # assign(paste0("interact_",z,"_",x,"_ggpredict_plot"), 
    #        ggpredict(I, terms = c(z,x)) |> plot(show_data=TRUE, colors = c("#ffa630", "#4da1a9", "#996888")))
    # 
    # create a list with all the plots names
    interact_plot_list <- c(interact_plot_list, 
                    paste0("interact_", x, "_", z, "_interact_plot"),
                    paste0("interact_", z, "_", x, "_interact_plot")#,
                    # paste0("interact_", x, "_", z, "_ggpredict_plot"),
                    # paste0("interact_", z, "_", x, "_ggpredict_plot")
                    )
    
    # retrieve simple slope analysis data
    simslopxz <- sim_slopes(I, pred = !!x, modx = !!z) %>% #create the sim_slopes object
      .$slopes %>% #use its summary
      cbind(c("- 1 SD","Mean","+ 1 SD"),.) %>% #bind the c() onto it
      mutate(pred=x, mod=z) #add columns pred and mod
    colnames(simslopxz)[2] <- "Value of mod"
    result_sim_slope <- rbind(result_sim_slope, simslopxz)
    
    simslopzx <- sim_slopes(I, pred = !!z, modx = !!x) %>% #create the sim_slopes object
      .$slopes %>% #use its summary
      cbind(c("- 1 SD","Mean","+ 1 SD"),.) %>% #bind the c() onto it
      mutate(pred=z, mod=x)#add columns pred and mod
    colnames(simslopzx)[2] <- "Value of mod"
    result_sim_slope <- rbind(result_sim_slope, simslopzx)

    assign(paste0("interact_",x,"_",z,"_sim_slopes"), 
           sim_slopes(I, pred = !!x, modx = !!z))
    assign(paste0("interact_",z,"_",x,"_sim_slopes"), 
           sim_slopes(I, pred = !!z, modx = !!x))
  }
  rm(x, z, I) # delete working variables
  
  for (i in interact_plot_list) {
    plot <- get(i)
    path <- paste0("plots/",i,".png")
    ggsave(path,plot, width = 10, height = 11, units = "cm")
  }
  
  result_sim_slope[,2:7] <- round(result_sim_slope[,2:7], digits = 3)
  result_sim_slope$p <- round(result_sim_slope$p, digits = 5)
  colnames(result_sim_slope)[1] <- "name"
  result_sim_slope$summary <- paste0("(beta: ",result_sim_slope$Est.,", se = ",result_sim_slope$S.E.,", p = ",result_sim_slope$p,")") 
  result_sim_slope <- result_sim_slope[,c("pred", "mod", "Value of mod","name", "summary")]
  
#interact_plot(I, pred = !!x, modx = !!z, plot.points = TRUE, interval = TRUE, colors = c("lightgrey", "black", "darkgrey"))


rm(result_interact_significant_I, simslopxz, simslopzx, plot, path, i, reg_wgi_loggdppc, reg_idv_uai, reg_ivr_avg5ygr)



#### CLUSTER ANALYSIS ####

dataxl_scaled <- data.frame() # initiated dataframe
#create a dataframe with relevant variables only
dataxl_scaled <- dataxl[,c("countryname","sdgscore","pdi","idv","mas","uai", "lto","ivr","wgi","loggdppc","avg5ygr")] 
dataxl_scaled_use <- dataxl[,c("sdgscore","pdi","idv","mas","uai", "lto","ivr","wgi","loggdppc","avg5ygr")]
rownames(dataxl_scaled_use) <- dataxl$countryname # import rownames from countryname column
dataxl_scaled_use <-  scale(dataxl_scaled_use) # scale the data
dataxl_scaled_use <- as.data.frame(dataxl_scaled_use) # transform to dataframe

d <- dist(dataxl_scaled_use) # create a dissimilarities matrix using euclidian (default)
dataxl_hclust <- hclust(d, method = "ward.D2") # use hclust to create a clustering with Ward.D2
coef.hclust(dataxl_hclust) # agglomerate coefficients

# Optimal number of clusters
plot_silhouette <- fviz_nbclust(dataxl_scaled_use, FUN = hcut, method = "silhouette", 
                                k.max=10, linecolor = "#F8766D") + tnr + labs(title=NULL) + theme(text = element_text(size = 10))
plot_wss <- fviz_nbclust(dataxl_scaled_use, FUN = hcut, method = "wss", 
                         linecolor = "#F8766D") + tnr + labs(title=NULL) + theme(text = element_text(size = 10))
plot_gapstats <- fviz_nbclust(dataxl_scaled_use, FUN = hcut, method = "gap_stat", 
                              k.max=10,linecolor = "#F8766D") + tnr + labs(title=NULL) + theme(text = element_text(size = 10))


plot_cluster_optimal <- plot_grid(plot_wss, plot_silhouette, plot_gapstats)
ggsave("plots/plot_cluster_optimal.png", width = 16, height = 5, units = "cm") # save the gird_plot

ncluster <- 9 # define number of clusters to be created
plot(dataxl_hclust, cex=0.6, hang=-1)
plot(dataxl_hclust, cex=1, hang=-1)
#rect.hclust(dataxl_hclust, k = ncluster, border = c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#00B9E3","#619CFF","#DB72FB","#FF61C3")) #colours do not match boxplots
ggsave("plots/plot_cluster_dendrogram.png", width = 1400, height = 600, units = "px")
dataxl_groups <- cutree(dataxl_hclust, ncluster) # store cluster affiliation

dataxl_scaled$mas[dataxl_groups == 6] # check members of a certain group

# first third threshold 
firstthird <- quantile(dataxl$sdgscore, 2/3)

# Extract Values 
result_cluster_mean <- aggregate(dataxl_scaled[,-c(1)],list(dataxl_groups),mean) %>% round(., digits = 3) #%>% t()
#result_cluster_var <- aggregate(dataxl_scaled[,-c(1)],list(dataxl_groups),var) %>% round(., digits = 3) #%>% t()
#result_cluster_sd <- aggregate(dataxl_scaled[,-c(1)],list(dataxl_groups),sd) %>% round(., digits = 3) #%>% t()
#result_cluster_aggregate <- data.frame(mapply(function(x, y) paste(x, y, sep = " / "), result_cluster_mean, result_cluster_median))
result_cluster_length <- aggregate(dataxl_scaled[,-c(1)],list(dataxl_groups),length)#%>% t()
colnames(result_cluster_length)[2] <- "n"
result_cluster_mean <- cbind(result_cluster_mean, result_cluster_length[,2])
result_cluster_mean <- result_cluster_mean[order(result_cluster_mean$sdgscore),] # order cluster by sdgscore
result_cluster_mean <- t(result_cluster_mean)
# need to add rownames

##### CLuster Plots ####

dataxl_cluster <- as.data.frame(dataxl_groups) # fromat as dataframe for later merge
dataxl_cluster$countryname <- rownames(dataxl_cluster) # add rownames for later merge
dataxl_cluster<- merge(dataxl_scaled,dataxl_cluster, by="countryname") # merge by countryname
dataxl_cluster$dataxl_groups <- as.factor(dataxl_cluster$dataxl_groups) # convert to factor for plotting by categories

regressors <- c("sdgscore","pdi","idv","mas","uai", "lto","ivr","wgi","loggdppc","avg5ygr") # create list of variables to plot
# create plot list for boxplots
plot_list_cluster_box <- lapply(regressors, function(regressors) {
  ggplot(dataxl_cluster, aes(x = reorder(dataxl_groups,sdgscore), y = get(regressors), color = dataxl_groups)) +
    #geom_hline(yintercept = mean(dataxl[[regressors]]), color = "red", linetype = "dashed") + 
    geom_boxplot() +
    theme_classic() +
    labs(x = NULL, y = regressors, color = NULL) +
    theme(text = element_text(family = "Times New Roman"),
          axis.text.x = NULL,
          axis.text.y = element_text(angle = 90),
          panel.background = element_rect(fill = "white", color = "black"),
          legend.position = "none") 
})

plot_cluster_box <- plot_grid(plotlist = plot_list_cluster_box, ncol = 4)
#plot_cluster_box <- plot_grid(plotlist = plot_list_cluster_box, ncol = 5)
ggsave("plots/plot_cluster_box.png", plot_cluster_box, width = 16, height = 20, units = "cm")

#### EXPORT xls FILES ####

# Get all objects in the environment
all_objects <- ls() 
# Filter the list to include only data frames
#data_frame_names <- all_objects[sapply(all_objects, function(x) is.data.frame(get(x)))]
# only use fdataframes that do not contain the words df or data
data_frame_names <- all_objects[sapply(all_objects, function(x) {
  is.data.frame(get(x)) && !grepl("data|df", x)
})]

# Create a workbook
wb <- createWorkbook() 

# Loop through data frames and add them to sheets
for (name in data_frame_names) {
  sheet_name <- name  # Use the data frame name as the sheet name
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x = get(name))
}
stamp <- format(Sys.time(), "%Y%m%d%H%M%S") # get current day and time
# Save the workbook
saveWorkbook(wb, paste0("output/", stamp,"result.xlsx"), overwrite = TRUE)
rm(all_objects,data_frame_names,name, stamp, wb, sheet_name)



#### GRAPHS ####

##### World Plot ####
dataxl_groups <- as.data.frame(dataxl_groups)
world <- map_data("world")
dataxl_groups$region <- rownames(dataxl_groups)
diff <- setdiff(dataxl_groups$region, world$region)
world$region <- gsub("Czech Republic", "Czechia", world$region) # replacing first with second argument
world$region <- gsub("Egypt", "Egypt, Arab Rep.", world$region) # replacing first with second argument
world$region <- gsub("Iran", "Iran, Islamic Rep.", world$region) # replacing first with second argument
world$region <- gsub("South Korea", "Korea, Rep.", world$region) # replacing first with second argument
world$region <- gsub("Russia", "Russian Federation", world$region) # replacing first with second argument
world$region <- gsub("Slovakia", "Slovak Republic", world$region) # replacing first with second argument
world$region <- gsub("Trinidad", "Trinidad and Tobago", world$region) # replacing first with second argument
world$region <- gsub("Tobago", "Trinidad and Tobago", world$region) # replacing first with second argument
world$region <- gsub("Turkey", "Turkiye", world$region) # replacing first with second argument
world$region <- gsub("USA", "United States", world$region) # replacing first with second argument
world$region <- gsub("UK", "United Kingdom", world$region) # replacing first with second argument
worldSubset <- left_join(world, dataxl_groups, by ="region")

worldcluster <- ggplot(data=worldSubset, 
                       mapping = aes(x=long, y=lat, group=group)) +
                         coord_fixed(1.3) + 
                         geom_polygon(col="lightgrey",linewidth = 0.01,aes(fill=factor(dataxl_groups))) +
                         labs(fill = "cluster") +
                          theme(legend.position = "bottom",
                                text = element_text(family = "Times New Roman", size = 10),  # Set font familylegend.position = "bottom",
                                axis.title = element_blank(),  # Remove x-axis label
                                axis.text = element_blank(),
                                axis.ticks = element_blank(),
                                panel.background = element_rect(fill = "white"),   # Set background color to white
                                panel.grid = element_blank())
ggsave("plots/worldcluster.png", width = 16, height = 14, units = "cm")
                          #scale_fill_manual(values = c("1" = "#ffa630", "2" = "#d7e8ba", "3" = "#4da1a9", "4" = "#996888", "NA" = "grey")) 
rm(world, worldcluster, worldSubset)

#c("#F8766D","#D39200","#93AA00","#00BA38","#00C19F","#00B9E3","#619CFF","#DB72FB","#FF61C3")

##### Plot France/Germany values ####
germany_france_data <- subset(dataxl, countryname %in% c("Germany", "France"))
germany_france_data <- germany_france_data[,c("countryname", "pdi", "idv", "mas", "uai", "lto", "ivr")]
germany_france_data <- gather(germany_france_data, dimension, score, pdi:ivr, factor_key=TRUE)
ggplot(germany_france_data, aes(x=dimension, y=score, fill=countryname)) +
  geom_bar(stat="identity", width = 0.7, position = position_dodge(width=0.9)) + 
  scale_y_continuous(breaks = seq(0,100,by = 10)) +
  theme_minimal_hgrid() +
  theme(text = element_text(family = "Times New Roman")) +  
  guides(fill = guide_legend(title = NULL)) 
ggsave("plots/FRA_GER.png", width = 16, height = 6, units = "cm")
rm(germany_france_data)



#### COMPARE (UN)FILTERED DATA ####
result_compare <- data.frame()
# sdg score abhängig von vsm
reg <- lm(sdgscore ~ pdi + idv + mas + uai + lto + ivr, dataxl)
coefficients <- coef(summary(reg)) # extract coefficients from the dummary of the regression (more data tat coef)
coefficient <- round(coefficients[,1], 3) # round the coeffients to 2 digits after the comma
standard_errors <- sprintf("(se: %.3f)", coefficients[, "Std. Error"])
significance <- ifelse(coefficients[, 4] < 0.001, "***", ifelse(coefficients[, 4] < 0.01, "**", ifelse(coefficients[, 4] < 0.05, "*", ""))) #rebuilding the * significance display
adjusted_r_squared <- round(summary(reg)$adj.r.squared, digits = 3)
# if the fstatistic is not NUll, we extract the latter (might be Null if completely insignificant or too little observations in a subset)
if (!is.null(summary(reg)$fstatistic)) {
  fstat <- t(summary(reg)$fstatistic)
  fstat <- round(fstat, digits=3) # round the fstat to 3 digits
  qf <- qf(0.95, fstat[1,2], fstat[1,3]) # calculate the critical F-value (sign. level, degrees of freedom numerator, degrees of freedom denumerator)
  qf <- round(qf, digits = 3) # round the critical F-value to 3 digits after the comma
  qf <- as.data.frame(qf)} # transform into dataframe
extract <- cbind(coefficient, standard_errors, significance) # put columns together
extract <- as.data.frame(extract) # transform into dataframe
extract <- unite(extract, reg, coefficient, significance, standard_errors, sep = " ") # unites coefficient and significance column with sep " " into new column reg in the df extract
extract <- t(extract) # transpose extract
extract <- as.data.frame(extract) # transform into dataframe
n <- nobs(reg) 
ifelse(exists("fstat"), # if fstat exists (has been initialized earlier) include it in the cbind
       extract <- cbind(n, extract,adjusted_r_squared, fstat, qf), 
       extract <- cbind(n, extract, adjusted_r_squared))
result_compare <- bind_rows(result_compare, extract) # add the current extract to the result
fstat <- c("We need to create a variable in case none was created bc NULL in order to delete it in the next step")
rm(coefficients, coefficient, significance, adjusted_r_squared, standard_errors, fstat, extract, qf) 

reg <- lm(sdgscore ~ pdi + idv + mas + uai + lto + ivr, data)
coefficients <- coef(summary(reg)) # extract coefficients from the dummary of the regression (more data tat coef)
coefficient <- round(coefficients[,1], 3) # round the coeffients to 2 digits after the comma
standard_errors <- sprintf("(se: %.3f)", coefficients[, "Std. Error"])
significance <- ifelse(coefficients[, 4] < 0.001, "***", ifelse(coefficients[, 4] < 0.01, "**", ifelse(coefficients[, 4] < 0.05, "*", ""))) #rebuilding the * significance display
adjusted_r_squared <- round(summary(reg)$adj.r.squared, digits = 3)
# if the fstatistic is not NUll, we extract the latter (might be Null if completely insignificant or too little observations in a subset)
if (!is.null(summary(reg)$fstatistic)) {
  fstat <- t(summary(reg)$fstatistic)
  fstat <- round(fstat, digits=3) # round the fstat to 3 digits
  qf <- qf(0.95, fstat[1,2], fstat[1,3]) # calculate the critical F-value (sign. level, degrees of freedom numerator, degrees of freedom denumerator)
  qf <- round(qf, digits = 3) # round the critical F-value to 3 digits after the comma
  qf <- as.data.frame(qf)} # transform into dataframe
extract <- cbind(coefficient, standard_errors, significance) # put columns together
extract <- as.data.frame(extract) # transform into dataframe
extract <- unite(extract, reg, coefficient, significance, standard_errors, sep = " ") # unites coefficient and significance column with sep " " into new column reg in the df extract
extract <- t(extract) # transpose extract
extract <- as.data.frame(extract) # transform into dataframe
n <- nobs(reg) 
ifelse(exists("fstat"), # if fstat exists (has been initialized earlier) include it in the cbind
       extract <- cbind(n, extract,adjusted_r_squared, fstat, qf), 
       extract <- cbind(n, extract, adjusted_r_squared))
result_compare <- bind_rows(result_compare, extract) # add the current extract to the result
fstat <- c("We need to create a variable in case none was created bc NULL in order to delete it in the next step")
rm(n, coefficients, coefficient, significance, adjusted_r_squared, standard_errors, fstat, extract, qf, sumstat, reg) 

result_compare <- rename(result_compare, F = "value")
result_compare$fstatsummary <- paste0("F (",result_compare$numdf, ", ", result_compare$dendf,") = ",result_compare$F, 
                                      ifelse(result_compare$p < 0.001, ", p < 0.001", 
                                             ifelse(result_compare$p < 0.01, ", p < 0.01", 
                                                    ifelse(result_compare$p < 0.05, ", p < 0.05", "")))) # 9 is numdf = the number of independent variables

write_xlsx(result_compare,"output/ResultCompare.xlsx")

#### RESET OPTIONS ####
rm(regressors, d, diff, interact_reg, size, spec, subset_df, subset_names, sumdescrstatdata, sumdescrstatdataxl, tnr, result_cluster_length, dataxl_scaled, dataxl_scaled_use, dataxl_cluster, dataxl_hclust, data, cor_all, cor_wgi, coefficient_p_value, interact_plot_list, ncluster, cluster) #delete obsolet objects
options(scipen = 0) # enable scientifix notation
setwd(oldwd) # set the working directory to the initial value
rm(oldwd)



#### USED PACKAGE VERSIONS ####

# cluster_2.1.6	
# corrplot_0.92	
# cowplot_1.1.3	
# factoextra_1.0.7	
# ggplot2_3.5.0	
# haven_2.5.4	
# interactions_1.1.5	
# lmtest_0.9-40	
# maps_3.4.2
# margins_0.3.26.1	
# NbClust_3.0.1	
# openxlsx_4.2.5.2	
# PerformanceAnalytics_2.0.4	
# psych_2.4.3	
# readxl_1.4.3	
# regclass_1.6	
# tidyverse_2.0.0	
# writexl_1.5.0	

