library(tidyverse)
library(magrittr)
options(scipen = 999)
library(reshape2)
library(plotly)
library(DT)

vote_map <- readRDS("data/vote_map.rds")
centraltown <- readRDS("data/centraltown.rds")
referendum <- readRDS("data/2018 taiwan referedum rate of supported(eng).rds")
#row.names(referendum) <- referendum$X
#referendum %<>% select(., -X)

corr_referendum <- round(cor(referendum, method = "spearman"), 4)
corr_referendum_up <- corr_referendum
corr_referendum_up[upper.tri(corr_referendum_up)] <- NA


var_position <- scale(corr_referendum, T, T)%*%eigen(cor(scale(corr_referendum, T, T), 
                                                         method = "spearman"))$vectors[, 1:2]
var_position %<>% round(., 4)
var_position %<>% as.data.frame()
var_position %<>% mutate(., case = paste0("case", 7:16))

samples_position <- (scale(referendum, T, T))%*%eigen(cor(scale(referendum, T, T)))$vectors[, 1:2] #pca$scores or #predict(pca)
samples_position %<>% as.data.frame()

samples_position_with_vote <- referendum
samples_position_with_vote %<>% mutate(., V1 = samples_position$V1, V2 = samples_position$V2)
row.names(samples_position_with_vote) <- row.names(referendum)



