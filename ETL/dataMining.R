#### **** DATA MINING (CLUSTER) **** ####
clusterDM <- function(data,columns = ''){
  dados_2 <- data[,columns]
  
  dados_3 <- dados_2 %>%
              unnest_tokens(lang, language, token = stringr::str_split, pattern = ' ') %>%
              unnest_tokens(edu, education, token = stringr::str_split, pattern = ' ') %>%
              unnest_tokens(skill, skills,  token = stringr::str_split, pattern = ' ')
  
  dados_3[dados_3$lang == '',]$lang   <- 'NI'
  dados_3[dados_3$edu == '',]$edu     <- 'NI'
  dados_3[dados_3$skill == '',]$skill <- 'NI'
  
  dados_4 <- data.frame(apply(dados_3, 2, function(x){
    as.numeric(as.factor(x))
  }))
  
  
  cluster         <- kmeans(dados_4, centers = 4)
  dados_3$cluster <- cluster$cluster
  
  tableClusterReq <- data.frame(table(dados_3$stateClear, dados_3$cluster), stringsAsFactors = F)
  
  tableClusterReq$Var1 <- as.character(tableClusterReq$Var1) 
  tableClusterReq$Var2 <- as.character(tableClusterReq$Var2)
  
  
  clusterReq <- tableClusterReq %>%
                  dplyr::group_by(Var1) %>%
                  dplyr::mutate(value = max(Freq)) %>%
                  filter(Freq == value) %>% select(Var1, Var2)
  
  colnames(clusterReq) <- c('state', 'Cluster')
  
  #data.table::fwrite(clusterReq, 'data/clusterReq.csv')
  return(clusterReq)
}
