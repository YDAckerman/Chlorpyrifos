############################################################
##          Assemble the final data set                   ##
############################################################

## This file can be sourced to bring in the object finalDataSet
## (which I highly recommend you rename for convenience). It
## differs significantly from the ins_table data frame found
## in many of my other analyses. In particular, all multi-
## product and multi-ai formulations have been consolidated into
## a single row. It also contains only variables that directly pertain
## to this analysis, thereby making head() or tail() calls
## useful. The Major shortcoming of this redaction is with regards
## to the lbs of active ingredient variable. Whereas previously it
## worked as a numeric modifier to the single active ingredient of
## the datapoint to which it belonged, the consolidation of
## data points precludes a simple numerical conversion/
## Thus instead the LbsAI variable is now a character with "+" signs
## separating the values of individual ais. These can still easily
## be used to form l-s-h labels (as has been our intention) if need be.

library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(metafor)

summary <- readline(prompt = "summarize? yes/no:")
while(!(summary %in% c('yes', 'no'))){
    summary <- readline(prompt = "summarize? yes/no")
}
summary <- switch(summary,
                  'yes' = TRUE,
                  'no' = FALSE)

clear <- readline(prompt = "clear? yes/no:")
while(!(clear %in% c('yes', 'no'))){
    clear <- readline(prompt = "summarize? yes/no")
}
clear <- switch(clear,
                  'yes' = TRUE,
                  'no' = FALSE)

## source("R/assemble_&_massage_data.R")
load("~/Dropbox/ZhangLabData/cpyrMAdata.rda")
load("~/Dropbox/ZhangLabData/pestDictionary.rda")
## Gives: dat, ins_table, response_tables,
## response_tables_lookup, & pestDict
source("~/Documents/Coding/R/R_convenience/helper_functions.R")
## Gives: hf (helper functions)
source("~/Documents/ZhangLab/R/Chlorpyrifos/table_operations.R")
## Gives: to (table operations)
source("~/Documents/ZhangLab/R/Chlorpyrifos/massage_functions.R")
## Gives: mf (massage functions)

#### Data summaries
prodsToAis <- ins_table %>%
    group_by(Pesticide) %>%
    arrange(AI) %>%
    dplyr::summarise(
        ais = paste(unique(AI), collapse = " + ")
    ) %>% ungroup()

singleAIprods <- ins_table %>%
    group_by(Pesticide) %>%
    dplyr::summarise(
        num_ais = length(unique(AI))
    ) %>%
    filter(num_ais == 1)

## quick helper function to return all ais within a certain pesticide
p2ai <- function(p){
    p == "UTC" && return(NA)
    (prodsToAis %>% filter(Pesticide == p))$ais
}

## quick helper function to consolidate multi-row treatments into
## a single row while leaving the single-row treatments alone:
g <- function(.data){
    if(all(is.na(.data$MPNkey))){
        return(data.frame(
            Product = .data$Pesticide,
            LbsAI = as.character(.data$AILbsAcre),
            AI = sapply(.data$Pesticide, p2ai),
            PDF = .data$PDF.file.name,
            Experiment = .data$V1,
            Year = .data$Year,
            Pest = .data$Pest,
            Replicates = .data$Number.of.replicates,
            Imputed = .data$is_imputed,
            lnRR = .data$LnR1,
            smd = .data$stdMnDiff,
            SEMestMax = .data$SEMestMax,
            SEMestMin = .data$SEMestMin,
            AS = .data$AS,
            stringsAsFactors = FALSE))
    } else {
        return(data.frame(
            Product = collProds(.data$Pesticide),
            LbsAI = collProds(.data$AILbsAcre),
            AI = compAI(.data$Pesticide),
            PDF = unique(.data$PDF.file.name),
            Experiment = unique(.data$V1),
            Year = unique(.data$Year),
            Pest = unique(.data$Pest),
            Replicates = unique(.data$Number.of.replicates),
            Imputed = all(.data$is_imputed),
            lnRR = na.omit(unique(.data$LnR1)),
            smd = na.omit(unique(.data$stdMnDiff)),
            SEMestMax = ifelse(all(is.na(.data$SEMestMax)), NA,
                na.omit(unique(.data$SEMestMax))),
            SEMestMin = ifelse(all(is.na(.data$SEMestMin)), NA,
                na.omit(unique(.data$SEMestMin))),
            AS = ifelse(all(is.na(.data$AS)), NA,
                paste(na.omit(.data$AS), collapse = ", ")),
            stringsAsFactors = FALSE))
    }
}

compAI <- function(pesticides){
    paste(sort(unique(hf$trim(unlist(strsplit(sapply(
        pesticides, p2ai), "\\+"))))),
                collapse = " + ")
}

collProds <- function(pesticides){
    paste(sort(pesticides), collapse = " + ")
}

## quick helper function to retrieve a pest name based on its abbreviation.
h <- function(pestabbv){
    is.na(pestabbv) && return(NA)
    hf$trim(unlist(strsplit((get(pestabbv, pestDict))[1], "\\("))[1])
}

finalDataSet <- ins_table %>%
    group_by(SampleID) %>%
    sample_n(1) %>%
    ungroup() %>%
    ##filter(Pesticide != "UTC") %>%
    group_by(MPNkey) %>%
    do(g(.)) %>%
    ungroup() %>%
    arrange(Experiment)

finalDataSet$PestName <- sapply(finalDataSet$Pest, h)

## count number of treatments in each experiment
expTrt <- finalDataSet %>%
    group_by(Experiment) %>%
    dplyr::summarise(num_treat = n()) %>%
    select(Experiment, num_treat) %>%
    ungroup() %>%
    filter(num_treat > 4)

## remove studies that would lead to unstable variance estimates
finalDataSet <- semi_join(finalDataSet, expTrt, by = "Experiment")

## calculate this only after you've paired down to your desired dataset:
## vcov_mat <- bldiag(lapply(split(finalDataSet, finalDataSet$Experiment), mf$vcov))

i <- which(finalDataSet$SEMestMax == 0)

if(summary){

    pestCounts <- finalDataSet %>%
        group_by(Pest, PestName) %>%
            dplyr::summarise(num_pdfs = length(unique(PDF)),
                             num_exp = length(unique(Experiment)),
                             num_ais = length(unique(AI)),
                             num_samp = n())

    aiCounts <- finalDataSet %>%
        group_by(AI) %>%
            dplyr::summarise(num_pdfs = length(unique(PDF)),
                             num_exp = length(unique(Experiment)),
                             num_pests = length(unique(Pest)),
                             num_samp = n()) %>%
                                 ungroup() %>%
                                     mutate(cpyr_f = grepl("chlorp", AI))

    pestAi_Counts <- finalDataSet %>%
        group_by(Pest,AI) %>%
            dplyr::summarise(num_pdfs = length(unique(PDF)),
                             num_exp = length(unique(Experiment)),
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

    ## distribution of year
    qplot(finalDataSet$Year, geom = "histogram")

    ## distribution of smd
    qplot(finalDataSet$smd, geom = "density")

    ## smd with year
    ggplot(finalDataSet, aes(x = Year, y = smd)) + geom_point()

    ## look at effect with year by ai
    tmp_ai <- finalDataSet %>% group_by(AI) %>%
        dplyr::summarise(count = n()) %>%
            arrange(desc(count)) %>%
                head(5)
    tmp_pest <- semi_join(finalDataSet, tmp_ai, by = "AI") %>%
        group_by(AI, Pest) %>%
            dplyr::summarise(count = n()) %>%
                arrange(desc(count)) %>%
                    top_n(n = 3) %>%
                        ungroup()

    tmp <- semi_join(finalDataSet, tmp_pest, by = c("AI","Pest"))
    pestAI <- tmp %>% select(AI, Pest) %>% distinct(AI, Pest)

    grobs <- m_ply(pestAI, function(AI, Pest){
        pest <- Pest
        ai <- AI
        g <- ggplot(tmp %>% filter(AI == ai & Pest == pest),
               aes(x = as.numeric(Year),
                   y = as.numeric(lnRR),
                   color = as.numeric(LbsAI))) +
            geom_point() +
                theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
                    geom_smooth(method = "lm") +
                        ggtitle(paste(Pest, AI, sep = " - "))
        ggsave(g, filename = paste0(Pest, AI, "Yearly.pdf"))
    })

    grobs <- m_ply(pestAI, function(AI, Pest){
        pest <- Pest
        ai <- AI
        g <- ggplot(tmp %>% filter(AI == ai & Pest == pest),
               aes(x = as.numeric(LbsAI),
                   y = as.numeric(lnRR))) +
            geom_point() +
                theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
                        ggtitle(paste(Pest, AI, sep = " - "))
        ggsave(g, filename = paste0(Pest, AI, "DoseResponse.pdf"))
    })
    
    tmp <- finalDataSet %>%
        group_by(AI) %>%
            dplyr::summarise(count = n(), posSmd = na.omit(sum(smd > 0))) %>%
                dplyr::mutate(frac_bad = posSmd / count) %>%
                    arrange(desc(frac_bad))

    cpyr <- finalDataSet %>% filter(AI == "chlorpyrifos")


    ## OneTwoPints <- 473.176 * c(1, 2) * 0.00220462 * 1.1222
    
    cpyr_L <- cpyr %>%
        filter(grepl("lorsban", Product, ignore.case = TRUE)) %>%
            group_by(Product, Pest) %>%
                dplyr::summarise(count = n()) %>%
                    filter(count > 5)
    cpyr_L <- semi_join(cpyr, cpyr_L, by = c("Product", "Pest"))
    cpyr_L <- melt(cpyr_L, id = setdiff(colnames(cpyr), "Product")) %>%
        mutate(LbsAI = as.numeric(LbsAI))
    ggplot(cpyr_L, aes(x = LbsAI, y = smd)) +
        geom_point() +
            facet_grid(Pest~value, scale = "free")

    
}

if(clear){
    rm(list = setdiff(ls(), c("finalDataSet", "vcov_mat")))
}

