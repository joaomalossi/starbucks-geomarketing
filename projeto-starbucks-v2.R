rm(list=ls())
require(rgdal)
require(dplyr)
require(rgeos)
require(sp)

## Leitura de todos os arquivos de shapefile de são paulo
if(file.exists("sp_2.shp"))
{
  sp_ <- readOGR("sp_2.shp") 
} else {
  folderp1 <- "SP_relevante/"
  folderp2 <- "3550308"
  folderp3 <- "_setor"
  folderp4 <- ".shp"
  sp_ <- readOGR("SP_relevante/35503080100/35503080100_setor.shp")
  for (i in seq(200,9600,100)) {
    if(isTRUE(i<1000)){
      folder <- paste(folderp1,folderp2,"0",i,"/",folderp2,"0",i,folderp3,folderp4,sep="")
    }
    else{
      folder <- paste(folderp1,folderp2,i,"/",folderp2,i,folderp3,folderp4,sep="")
    }
    if(file.exists(folder)){
      temp_ <- readOGR(folder)
      sp_ <- rbind(sp_,temp_)
    }
  }
  ## escreve um shapefile já agregado para não precisar ler novamente os arquivos
  writeOGR(obj=sp_, dsn="sp_2.shp", layer = "sp_2", driver="ESRI Shapefile")
}

## leitura do arquivo de satrbucks geocodificados
starbucks <- readOGR("Starbucks Brasil\\points7.shp")

## define o esquema de coordenadas dos shapefiles
proj4string(starbucks) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(sp_) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

## primeira parte do join espacial (descobre quais pontos estão em quais polígonos)
setores_com_starbucks <- over(starbucks,sp_)

## limpa a memória
rm(starbucks)

## gera um dataset com valores 1 para regiões que possuem starbucks
CD_GEOCODI <- setores_com_starbucks$CD_GEOCODI
POSSUI_STARBUCKS <- rep.int(1,length(CD_GEOCODI))
tabela_temp <- data.frame(CD_GEOCODI, POSSUI_STARBUCKS)

## remove linhas que possuem NA e remove linhas repetidas
tabela_temp <- tabela_temp[complete.cases(tabela_temp),]
tabela_temp <- unique(tabela_temp)

## segunda parte do join espacial, inclui a informação de POSSUI_STARBUCKS na tabela de polígonos
tabela_full <- sp_
tabela_full <- sp::merge(tabela_full, tabela_temp, by.x = "CD_GEOCODI", by.y = "CD_GEOCODI", all.x = TRUE, sort=F)

## limpa a memória
rm(tabela_temp)
rm(CD_GEOCODI)
rm(POSSUI_STARBUCKS)

## leitura dos arquivos de estação de metro do GEOSAMPA
est_metro <- readOGR("EST_METRO\\points2.shp")

## ajuste do esquema de coordenadas
proj4string(est_metro) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

## primeira parte do join espacial
setores_com_metro <- over(est_metro,tabela_full)

## gera um dataset com valores 1 para regiões que possuem estação de metro
CD_GEOCODI <- setores_com_metro$CD_GEOCODI
POSSUI_METRO <- rep.int(1,length(CD_GEOCODI))
tabela_temp <- data.frame(CD_GEOCODI, POSSUI_METRO)

## remove linhas que possuem NA e remove linhas repetidas
tabela_temp <- tabela_temp[complete.cases(tabela_temp),]
tabela_temp <- unique(tabela_temp)

## segunda parte do join espacial
tabela_full <- sp::merge(tabela_full, tabela_temp, by.x = "CD_GEOCODI", by.y = "CD_GEOCODI", all.x = TRUE, sort=F)

## limpa a memória
rm(sp_)
rm(setores_com_starbucks)
rm(tabela_temp)
rm(setores_com_metro)
rm(CD_GEOCODI)
rm(POSSUI_METRO)


## leitura dos arquivos do censo relevantes
## busca o nome de todos os arquivos dentro da pasta especificada 
list.filenames <- list.files(path="SP_Capital_20171016/SP Capital/Base informaçoes setores2010 universo SP_Capital/CSV/",pattern = ".csv$")
list.data <- list()

## leitura de todos os arquivos em uma lista
for (i in 1:length(list.filenames)) {
  folder <- paste("SP_Capital_20171016/SP Capital/Base informaçoes setores2010 universo SP_Capital/CSV/", list.filenames[i], sep="")
  list.data[[i]] <- read.csv(folder)
}
names(list.data)<-list.filenames

## faz o merge de todos os arquivos em 1 único data set
full_dataset <- list.data[[1]]

for (i in 2:length(list.filenames)) {
  full_dataset <- sp::merge(full_dataset,list.data[[i]], by = "Cod_setor")
}

## limpa a memória
rm(list.data)

## ajuste de classe para realização de um left join
full_dataset$Cod_setor <- as.factor(full_dataset$Cod_setor)

## ajusta o nome da primary key
names(full_dataset)[1] <- "CD_GEOCODI"

## salva o arquivo para avaliação de resultados
write.csv(full_dataset,"full_dataset.csv") 

## join das informações do censo no shapefile completo
tabela_full <- sp::merge(tabela_full, full_dataset, by.x = "CD_GEOCODI", by.y = "CD_GEOCODI", all.x = TRUE, sort=F)

## leitura dos arquivos de empresas e serviços do GEOSAMPA
empresas <- read.csv("empresas.csv")
servicos <- read.csv("servicos.csv")

## join das informações do GEOSAMPA no shapefile completo
tabela_full <- sp::merge(tabela_full, empresas, by.x = "CD_GEOCODI", by.y = "CD_GEOCODI", all.x = TRUE, sort=F)
tabela_full <- sp::merge(tabela_full, servicos, by.x = "CD_GEOCODI", by.y = "CD_GEOCODI", all.x = TRUE, sort=F)

## salva o arquivo completo em shapefile, para qualquer nova análise
writeOGR(obj=tabela_full, dsn="result.shp", layer = "result", driver="ESRI Shapefile")

## salva o arquivo para validação das informações
write.csv(tabela_full@data, "valores.csv")

## leitura do arquivo salvo, isso ajuda a ajustar as classes de cada coluna
re_log <- read.csv("valores.csv", stringsAsFactors=FALSE)

## ajuste do dataset: remove valores NA, remove linhas indesejadas. Este proccesso foi desenvolvido considerando
## que alguns valores NA são relevantes e outros não.
re_log[is.na(re_log[,15]),15] <- 0
re_log[is.na(re_log[,16]),16] <- 0
re_log[is.na(re_log[,62]),62] <- 0
re_log[is.na(re_log[,63]),63] <- 0
re_log[is.na(re_log[,64]),64] <- 0
re_log <- re_log[complete.cases(re_log[,15:64]),]


## ajuste das classes
for(i in 15:length(re_log)) {
  re_log[,i] <- as.numeric(as.character(re_log[,i]))
}

re_log[is.na(re_log)] <- 0

## write.csv(re_log, "teste1.csv")

## matriz de correlação linear
mat_cor <- cor(re_log[,15:64])

## regressão logaritmica
mod1 = glm(POSSUI_STARBUCKS ~ V002 + 
             V004 +
             X16_21 +
             X22_30 +
             X31_45 +
             X45_60 +
             Target_2_10_salarios_per_capita + 
             target+ V022 + 
             X5a15salarios + 
             X15oumais + 
             POSSUI_METRO +
             Sum...ATACADO_MIC +
             Sum...ATACADO_PEQ +
             Sum...ATACADO_MED +
             Sum...ATACADO_GDE +
             ATACADO +
             Sum...VAREJO_MIC +
             Sum...VAREJO_PEQ +
             Sum...VAREJO_MED +
             Sum...VAREJO_GDE +
             VAREJO + 
             Sum...ENSINO_MIC +
             Sum...ENSINO_PEQ +
             Sum...ENSINO_MED +
             Sum...ENSINO_GDE +
             ENSINO + 
             Sum...FINANC_MIC +
             Sum...FINANC_PEQ +
             Sum...FINANC_MED +
             Sum...FINANC_GDE +
             FINANC + 
             Sum...SAUDE_MIC +
             Sum...SAUDE_PEQ +
             Sum...SAUDE_MED +
             Sum...SAUDE_GDE +
             SAUDE + 
             Sum...TRANSP_MIC +
             Sum...TRANSP_PEQ +
             Sum...TRANSP_MED +
             Sum...TRANSP_GDE +
             TRANSP +
             SHOPPING +
             TERMINAL +
             wifi, family="binomial", data=re_log)
summary(mod1)



## filtro dos setores que não possuem estarbucks
setor_sem_starbucks <- subset(re_log, re_log$POSSUI_STARBUCKS==0)

## utiliza o modelo identificado na regressão para as regiões que não possuem starbucks
result <- predict(mod1, setor_sem_starbucks, type = "response")

## apresenta o setor que possui o maior valor
nova_loja <- setor_sem_starbucks[which.max(result),]

