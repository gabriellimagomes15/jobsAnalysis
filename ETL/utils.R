library(rvest)
library(plyr)
library(data.table)
library(tm)
library(stringr)
library(stringdist)
library(dplyr)
library(tidytext)


scrapGlassDoor <- function(){
  urlRoot <- 'https://www.glassdoor.com'
  urls    <- c('https://www.glassdoor.com/Job/canada-data-scientist-jobs-SRCH_IL.0,6_IN3_KO7,21_IP','https://www.glassdoor.com/Job/us-data-scientist-jobs-SRCH_IL.0,2_IN1_KO3,17_IP')
  country <- c('Canada','US')
  urlDF   <- data.frame(urls, country, stringsAsFactors = F)
  dataDF  <- data.frame()
  
  
  for(i in 1:dim(urlDF)[1]){
    urlPage    <- urlDF$urls[i]
    TotalPages <- ifelse(urlDF$country[i] == 'Canada', 28,450)
    
    tryCatch({
      ## FOR TO READ THE PAGES WITH ALL JOB POST. TotalPages+1 is because the first page doesn't work with this url
      for( pag in 1:TotalPages+1){
        cat('\n Page = ', pag)
        urlPage2 <- paste(urlPage,pag,'.htm', sep = '')
        cat(' ',urlPage2)
        page     <- read_html(urlPage2)
        links    <- page %>% html_nodes('.jlGrid.hover') %>% html_nodes('.jl .flexbox a') %>% html_attr('href')
        
        tryCatch({    
          ## FOR TO READ A JOB POST
          for(href in links){
            url        <- paste(urlRoot, href, sep = '')
            pageJob    <- read_html(url)
            
            postJob    <- pageJob %>% html_nodes('.empInfo.tbl h2') %>% html_text()
            company    <- pageJob %>% html_nodes('.empInfo .ib') %>% html_text() %>% .[1]
            company    <- ifelse(is.na(company) , 'NA', company)
            
            city_state <- pageJob %>% html_nodes('.empInfo .subtle.ib') %>% html_text()
            city_state <- ifelse(identical(city_state,character(0)), '--', city_state)
            
            date <- pageJob %>% html_nodes('.empLinks .minor') %>% html_text()
            date <- ifelse(identical(date,character(0)), 'NA', date)
            
            descrip <- pageJob %>% html_nodes('.jobDescriptionContent') %>% html_text()
            descrip <- ifelse(identical(descrip,character(0)), 'NA', descrip)
            
            dateColect <- Sys.Date()
            
            data   <- data.frame(postJob, company, city_state, date, descrip, dateColect, country = urlDF$country[i], url)
            dataDF <- rbind.fill(dataDF,data)
            
            fwrite(dataDF, 'postJobGlassdoor.csv')
            
            Sys.sleep(4) # DELAY
          }## END FOR LINKS
        }, error = function(e){
          print(paste('ERROR READ LINKS: ', e , sep = ' ') )
        })
      }## END FOR TOTALPAGE
    }, error = function(e){
      print(paste('ERROR: ', e , sep = ' ') )
    })
  }## END FOR URLPAGE
}

#### **** DICTONARY OF SKILL IN DATA SCIENCE **** ####
dictonarySkills <- function(){
  skills <- c()
  url    <- 'http://www.datascienceglossary.org/#algorithm'
  page   <- read_html(url)
  skil   <- page %>% html_nodes('.row .col-md-3') %>% html_text()
  skills <- c(skills,skil)
  
  url    <- 'http://bigdata-madesimple.com/big-data-a-to-zz-a-glossary-of-big-data-terminology/'
  page   <- read_html(url)
  skil   <- page %>% html_nodes('.ptb60 strong') %>% html_text()
  skills <- c(skills,skil)
  
  write(skills, 'dictonarySkills.txt')
  
  a <- read.table('t.txt',sep = '\n', stringsAsFactors = F)
  skills <- c(skills,a$V1)
  
  write(skills, 'dictonarySkills.txt')
  skills <- tolower(skills)
  skills <- unique(skills)
  
  write(skills, 'dictonarySkills.txt')
}



#### **** PRE-PROCESS **** ####
getSkills <- function(data,column = ''){
  vocSkill   <- read.table('dictonarySkills.txt',sep = '\n', stringsAsFactors = F)
  skillFinal <- c()
  apply(data, 1, function(x){
    skill  <- c('')
    words  <- unlist(strsplit(x[column], ' '))
    for(token in words){
      a <- 1-stringdist(tolower(token), vocSkill$V1, method = 'jw')
      if( max(a)  > 0.9){
        skill <- paste(skill, vocSkill[which.max(a),])
      }
    }
    skillFinal <<- c(skillFinal, skill)
  })
  return(skillFinal)
}


getEducation <- function(data,column = ''){
  vocEduc <- read.table('education.txt',sep = '\n')
  eduFinal <- c()
  apply(data, 1, function(x){
    educ  <- c('')
    words <- unlist(strsplit(x[column], ' '))
    for(token in words){
      a <- 1-stringdist(tolower(token), vocEduc$V1, method = 'jw')
      if( max(a)  > 0.9){
        educ <- paste(educ, vocEduc[which.max(a),])
      }
    }
    eduFinal <<- c(eduFinal, educ)
  })
  return(eduFinal)
}

getLanguage <- function(data,column = ''){
  vocLang <- read.table('languages.txt',sep = '\n', stringsAsFactors = F)
  langFinal <- c()
  apply(data, 1, function(x){
    lang  <- c('')
    words <- unlist(strsplit(x[column], ' '))
    for(token in words){
      a <- 1-stringdist(tolower(token), vocLang$V1, method = 'jw')
      if( max(a)  > 0.9){
        lang <- paste(lang, vocLang[which.max(a),])
      }
    }
    langFinal <<- c(langFinal, lang)
  })
  return(langFinal)
}

clearPostJob <- function(data, column = ''){
  postJobClean <- sapply(data[,column], function(x){
    x <- gsub("[[:punct:]]", " ", x) # remove punctuation
    x <- gsub("[[:digit:]]", " ", x) # remove numbers
    x <- gsub("http\\w+", " ", x)    # remove html links
    x <- gsub("\\W"," ", x) # remove not word
    x <- gsub("[ \t]{2,}", " ", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x
  })  
  return(postJobClean)
}


clearCompany <- function(data, column = ''){
  companyClean <- sapply(data[,column], function(x){
    x <- gsub("[[:punct:]]", " ", x) # remove punctuation
    x <- gsub("[[:digit:]]", " ", x) # remove numbers
    x <- gsub("^[ \t]{2,}", " ", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x
  })
  return(companyClean)
}

clearCity <- function(data, column = ''){
  cityClean <- sapply(data[,column], function(x){
    x <- gsub("--", 'ND',x)
    x <- gsub("–", '',x) ## CLEAR ESPECIAL CHAR IN LINUX
    x <- gsub('^A\\sa\\W+A|^a\\W+A','',iconv( enc2native(x), to = "ASCII//TRANSLIT")) ##CLEAR ESPECIAL CHAR IN WINDOWS
    x <- gsub('^A\\sa\\W+A|^a\\W+A','',x)
    x <- gsub("[ \t]{2,}", "", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x <- gsub(",\\s+\\w+","", x) # 
    x <- gsub("^\\W+|\\W+$","", x) # remove not word
    x
  })
  return(cityClean)
}


clearState <- function(data, column = ''){
  stateClean <- sapply(data[,column], function(x){
    pos <- regexpr(',',x)[1]
    if(pos > 0){
      x   <- substr(x, pos+1, str_length(x))
    }else{
      x <- 'ND'
    }
    x   <- gsub("\\W+", "", x) # remove double space
    x
  })
  return(stateClean)
}


clearDate <- function(data){
  dados2 <- data.frame(t(apply(data, 1, function(x){
                          if (grepl('days', x['date'])){
                            days  <- as.numeric( gsub("([0-9]+).*$", "\\1", x['date']) )
                            x['dateClean'] <- as.character(as.Date(x['dateColect']) - days)
                            
                          }else{
                            x['dateClean'] <- x['dateColect']
                          }
                          x
                        })
                      ),stringsAsFactors = F
                    )
  return(dados2)
}

## Function to clear comments. Need be a data frame
## Obs: Para eliminar a duplicidade de registro, será utilizada a primeira coluna do parametro 'column';
cleanText <- function(data,column = '', stopWords = "",stemming = F,specialChar = F,delDup = F){
  if(column[1] == ''){
    warning('ATENÇÃO!!!!\n A coluna (atributo) selecionada para limpeza é: ', colnames(data)[1])
    column <- colnames(data)[1]
  }
  if(class(data)[1] == "list"){
    textDF = do.call("rbind", lapply(data, as.data.frame));
  }else{
    textDF <- data
  }
  #commentsDF = subset(commentsDF, select = c(text));
  # Text Cleasing
  retorno  <- lapply(textDF[,column],function(x){
    x <- gsub('http.* *', '', x)
    x <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", x)
    x <- gsub(":", "", x)
    x <- gsub("@\\w+", "", x)       # remove at people
    x <- gsub("[[:punct:]]", "", x) # remove punctuation
    x <- gsub("[[:digit:]]", "", x) # remove numbers
    x <- gsub("http\\w+", "", x)    # remove html links
    x <- gsub("\\W"," ", x) # remove not word
    x <- gsub("[ \t]{2,}", " ", x) # remove double space
    x <- gsub("^\\s+|\\s+$", "", x)# remove space in begin and final
    x <- tolower(x)
    x <- removeWords(x,c(stopWords,stopwords("pt"))) ## remove stop words
    x
  }
  );
  
  # remove special character
  if(specialChar){
    retorno <- lapply(retorno,function(x){iconv( enc2native(x), to = "ASCII//TRANSLIT") })
  }
  
  if(stemming){
    retorno <- lapply(retorno,function(x){stemDocument(x,language = "portuguese")})
  }
  
  # verificar quantas colunas. Se o retorno vai ser convertido em dataframe ou um vetor
  if(length(column) > 1){
    retorno <- data.frame(retorno, stringsAsFactors = F)
  }else{
    retorno <- unlist(retorno)
  }
  
  textDF[,column] <- retorno
  
  if(delDup){
    # Removing Duplicate tweets and Removing null line
    warning('\n ===> ATENÇÃO!!!\n Será verificado a duplicidade de registros na coluna : ', toupper(column[1]),'\n')
    
    textDF$DuplicateFlag <- duplicated(textDF[,column[1]]);
    textDF <- subset(textDF, textDF$DuplicateFlag =="FALSE");
    textDF <- subset(textDF, select = -c(DuplicateFlag))  
  }
  
  return(textDF)
}
