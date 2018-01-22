path <- 'E:/vanhack/'

setwd(path)
source(paste(path,'/ETL/utils.R', sep = '') )
source(paste(path,'/ETL/dataMining.R', sep = '') )

#### **** COLECT DATA **** ####
#scrapGlassDoor()

dados <- read.csv('data/GlassdoorTest.csv', stringsAsFactors = F)
#load(postJobGlassdoor.RData)

#### **** PRE-PROCESS DATA **** ####
dadosClean <- cleanText(data = dados,column = 'descrip', stopWords = stopwords('en'))

dadosClean$skills     <- getSkills(dadosClean,column = 'descrip')
dadosClean$education  <- getEducation(dadosClean,'descrip')
dadosClean$language   <- getLanguage(dadosClean,'descrip')

dadosClean$JobClear   <- clearPostJob(dadosClean,'postJob')

dadosClean[is.na(dadosClean$company),]$company <- 'NI'
dadosClean$companyClear <- clearCompany(dadosClean,'company')

dadosClean$cityClear    <- clearCity(dadosClean,'city_state')
dadosClean$cityClear    <- clearCity(dadosClean,'cityClear')

dadosClean$stateClear   <- clearState(dadosClean,'city_state')

## GET THE PROVINCE OF THE CANADA
canadaCity <- data.frame(jsonlite::fromJSON('data/maps/cities.json'), stringsAsFactors = F)
colnames(canadaCity) <- c('city','prov')
dadosClean[dadosClean$urlDF.country.i. == 'Canada',]$stateClear <- sapply(dadosClean[dadosClean$urlDF.country.i. == 'Canada',]$cityClear, function(x){
  r <- canadaCity[tolower(canadaCity$city) == tolower(x),'prov']
  if( length(r) > 0){
    r[1] 
  }else{
    x
  }
})

dadosClean$dateClean    <- ""
dadosClean              <- clearDate(data = dadosClean)

dadosClean <- dadosClean[,c('JobClear','companyClear','dateClean', 'skills','education','language','dateColect','urlDF.country.i.','cityClear','stateClear','url')]
#data.table::fwrite(dadosClean, 'data/dadosFinal.csv')

#### **** DATA MINING (CLUSTER) **** ####

clusterReq <- clusterDM(dadosClean,c('JobClear', 'skills', 'education', 'language','stateClear'))
data.table::fwrite('data/clusterReq.csv')


