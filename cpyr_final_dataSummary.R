############################################################
##          documentation of Meta-analysis                ##
############################################################

##____________________Libraries___________________________##

library(ggplot2)
library(dplyr)
library(plyr)
library(gridExtra)
library(lme4)
library(MCMCglmm)
library(orthopolynom)
library(stringr)

##_______________set working directory____________________##

##setwd("/Users/Yoni/Documents/ZhangLab")

##__________________set options___________________________##

options(dplyr.width = Inf)
options(width = 100)
##_____________source: data dicts functions_______________##

#source("R/assemble_&_massage_data.R")
load("~/Dropbox/ZhangLabData/cpyrMAdata.rda")
load("~/Dropbox/ZhangLabData/pestDictionary.rda")
## Gives: dat, ins_table, response_tables,
## response_tables_lookup, & pestDict
source("~/Documents/Coding/R/R_convenience/helper_functions.R")
## Gives: hf (helper functions)
source("~/Documents/ZhangLab/R/Chlorpyrifos/table_operations.R")
## Gives: to (table operations)


#### Data summaries
prodsToAis <- ins_table %>%
    group_by(Pesticide) %>%
    arrange(AI) %>%
    dplyr::summarise(
        ais = paste(substr(unique(AI), 1, 6), collapse = " + ")
    ) %>% ungroup()

singleAIprods <- ins_table %>%
    group_by(Pesticide) %>%
    dplyr::summarise(
        num_ais = length(unique(AI))
    ) %>%
    filter(num_ais == 1)

## quick helper function to return all ais within a certain pesticide
p2ai <- function(p){
    (prodsToAis %>% filter(Pesticide == p))$ais
}

## quick helper function to compress a single treatment (a pesticide with
## multiple ais, a multi-product combination, or both) into one data point
g <- function(.data){
    if(all(is.na(.data$MPNkey))){
        return(data.frame(AI = sapply(.data$Pesticide, p2ai),
                          PDF = .data$PDF.file.name,
                          experiment = .data$V1,
                          Pest = .data$Pest,
                          ef_y = .data$LnR1,
                          SEMestMax = .data$SEMestMax,
                          SEMestMin = .data$SEMestMin,
                          AS = .data$AS,
                          stringsAsFactors = FALSE))
    } else {
        return(data.frame(AI = paste(sort(unique(hf$trim(unlist(strsplit(sapply(
                                  .data$Pesticide, p2ai), "\\+"))))),
                              collapse = " + "),
                          PDF = unique(.data$PDF.file.name),
                          experiment = unique(.data$V1),
                          Pest = unique(.data$Pest),
                          ef_y = na.omit(unique(.data$LnR1)),
                          SEMestMax = na.omit(unique(.data$SEMestMax)),
                          SEMestMin = na.omit(unique(.data$SEMestMin)),
                          AS = ifelse(all(is.na(.data$AS)), NA, paste(na.omit(.data$AS), collapse = ", ")),
                          stringsAsFactors = FALSE))
    }
}

## quick helper function to retrieve a pest name based on its abbreviation.
h <- function(pestabbv){
    is.na(pestabbv) && return(NA)
    hf$trim(unlist(strsplit((get(pestabbv, pestDict))[1], "\\("))[1])
}

pestsAI <- ins_table %>%
    group_by(SampleID) %>%
    sample_n(1) %>%
    ungroup() %>%
    filter(Pesticide != "UTC") %>%
    group_by(MPNkey) %>%
    do(g(.)) %>% ungroup()

pestsAI$PestName <- sapply(pestsAI$Pest, h)

pestCounts <- pestsAI %>%
    group_by(Pest, PestName) %>%
    dplyr::summarise(num_pdfs = length(unique(PDF)),
                     num_exp = length(unique(experiment)),
                     num_ais = length(unique(AI)),
                     num_samp = n())

aiCounts <- pestsAI %>%
    group_by(AI) %>%
    dplyr::summarise(num_pdfs = length(unique(PDF)),
                     num_exp = length(unique(experiment)),
                     num_pests = length(unique(Pest)),
                     num_samp = n()) %>%
    ungroup() %>%
    mutate(cpyr_f = grepl("chlorp", AI))

pestAi_Counts <- pestsAI %>%
    group_by(Pest,AI) %>%
    dplyr::summarise(num_pdfs = length(unique(PDF)),
                     num_exp = length(unique(experiment)),
                     num_samp = n()) %>%
    arrange(desc(num_samp)) %>%
    ungroup() %>%
    mutate(cpyr_f = grepl("chlorp", AI))

## helper function to bin sample count values.
f <- function(x){
    is.na(x) && return(NA)
    bools <- unlist(llply(list(1:5, 6:10, 11:30, 31:50, 51:61),
                          function(range){x %in% range}))
    range <- unlist(ranges[bools])
    paste0(min(range), "-", max(range))
}

pestAi_Counts$Pest <- factor(pestAi_Counts$Pest,
                             levels = unique(pestAi_Counts$Pest))
pestAi_Counts$AI <- factor(pestAi_Counts$AI,
                             levels = unique(pestAi_Counts$AI))
pestAi_Counts$bin <- factor(sapply(pestAi_Counts$num_samp, f),
                            levels = c("1-5", "6-10", "11-30", "31-50", "51-61"))


## for saving images
l_ply(c("num_pdfs", "num_exp", "num_pests", "num_samp"), function(x){
    l_ply(c(TRUE,FALSE), function(y){
        p <- ggplot(aiCounts %>% filter(cpyr_f == y), aes_string(x = "AI", weight = x)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
            geom_histogram() +
            ggtitle(paste(x, "; cpyr formulations = ", y, sep = "")) +
            theme(axis.text.x = element_text(size = 6)) +
            theme(axis.text.y = element_text(size = 6))
        ggsave(plot = p, filename = paste0("~/Dropbox/ZhangLabData/cpyrPlots/AIHist_by_",
                                           x, "_cpyr_", y, ".pdf")) 
    })
})

## ai vs insect:
g <- ggplot(pestAi_Counts %>% filter(cpyr_f == TRUE), aes(x = Pest, y = AI, fill = bin)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    geom_tile() +
    theme(axis.text.x = element_text(size = 6)) +
    theme(axis.text.y = element_text(size = 4))
ggsave(plot = g, filename = "~/Dropbox/ZhangLabData/cpyrPlots/SampleCount_by_Pest&AI_cpyr.pdf")

g <- ggplot(pestAi_Counts %>% filter(cpyr_f == FALSE), aes(x = Pest, y = AI, fill = bin)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    geom_tile() +
    theme(axis.text.x = element_text(size = 6)) +
    theme(axis.text.y = element_text(size = 4))
ggsave(plot = g, filename = "~/Dropbox/ZhangLabData/cpyrPlots/SampleCount_by_Pest&AI_no_cpyr.pdf")
