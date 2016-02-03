############################################################
##          Assemble the final data set                   ##
############################################################

## This file can be sourced to bring in the object finalDataSet
## (which I highly recommend you rename for convenience). It
## differs significantly from the ins_table data frame found
## in many of my other analyses. In particular, all multi-
## product and multi-ai formulations have been consolidated into
## a single row. It also contains only variables that directly pertain
## to this analysis, thereby making head() or tail() calls more
## useful. The major shortcoming of this redaction is with regards
## to the lbs of active ingredient variable. Whereas previously it
## worked as a numeric modifier to the single active ingredient of
## the datapoint to which it belonged, the consolidation of
## data points precludes a simple numerical conversion/
## Thus instead the LbsAI variable is now a character with "+" signs
## separating the values of individual ais. These can still easily
## be used to form l-s-h labels (as has been our intention) if need be.

summary <- readline(prompt = "summarize? yes/no:")
while(!(summary %in% c('yes', 'no'))){
    summary <- readline(prompt = "please answer yes or no")
}
summary <- switch(summary,
                  'yes' = TRUE,
                  'no' = FALSE)

clear <- readline(prompt = "clear? yes/no:")
while(!(clear %in% c('yes', 'no'))){
    clear <- readline(prompt = "please answer yes or no")
}
clear <- switch(clear,
                'yes' = TRUE,
                'no' = FALSE)

outliers <- readline(prompt = "remove outliers? yes/no:")
while(!(outliers %in% c('yes', 'no'))){
    outliers <- readline(prompt = "please answer yes or no")
}
outliers <- switch(outliers,
                  'yes' = TRUE,
                   'no' = FALSE)


library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(metafor)
library(wesanderson)

## source("R/assemble_&_massage_data.R")

## if not running from cpyr_main, uncomment:
## load("~/Dropbox/ZhangLabData/cpyrMAdataJan27.rda")
## Gives: dat, ins_table, response_tables, response_tables_lookup

load("~/Dropbox/ZhangLabData/pestDictionary.rda") ## gives pestDict

source("~/Documents/Coding/R/R_convenience/helper_functions.R")## Gives: hf (helper functions)

source("~/Documents/ZhangLab/R/Chlorpyrifos/table_operations.R")## Gives: to (table operations)

source("~/Documents/ZhangLab/R/Chlorpyrifos/massage_functions.R")## Gives: mf (massage functions)

ins_table <- ins_table %>%
    filter(!grepl("proprietary blend", AI, ignore.case = TRUE) &
           !grepl("petroleum distillates", AI, ignore.case = TRUE))

ins_table$DPR.Label.Database.name <- tolower(ins_table$DPR.Label.Database.name)

#### Data summaries
prodsToAis <- ins_table %>%
    group_by(DPR.Label.Database.name) %>%
    arrange(AI) %>%
    dplyr::summarise(
        ais = paste(na.omit(unique(AI)), collapse = " + ")
    ) %>% ungroup()

singleAIprods <- ins_table %>%
    group_by(DPR.Label.Database.name) %>%
    dplyr::summarise(
        num_ais = length(unique(AI))
    ) %>%
    filter(num_ais == 1)

## quick helper function to return all ais within a certain pesticide
p2ai <- function(p){
    p == "UTC" && return(NA)
    (prodsToAis %>% filter(DPR.Label.Database.name == p))$ais
}

## quick helper function to consolidate multi-row treatments into
## a single row while leaving the single-row treatments alone:
g <- function(.data){
    if(all(is.na(.data$MPNkey))){
        return(data.frame(
            Product = .data$DPR.Label.Database.name,
            LbsAI = as.character(.data$AILbsAcre),
            AI = sapply(.data$DPR.Label.Database.name, p2ai),
            PDF = .data$PDF.file.name,
            Experiment = .data$V1,
            Year = .data$Year,
            Pest = .data$Pest,
            Replicates = .data$Number.of.replicates,
            Imputed = any(.data$is_imputed),
            lnRR = .data$LnR1,
            smd = .data$stdMnDiff,
            SEMestMax = .data$SEMestMax,
            SEMestMin = .data$SEMestMin,
            AS = .data$AS,
            stringsAsFactors = FALSE))
    } else {
        return(data.frame(
            Product = collProds(.data$DPR.Label.Database.name),
            LbsAI = collProds(.data$AILbsAcre),
            AI = compAI(.data$DPR.Label.Database.name),
            PDF = unique(.data$PDF.file.name),
            Experiment = unique(.data$V1),
            Year = unique(.data$Year),
            Pest = unique(.data$Pest),
            Replicates = unique(.data$Number.of.replicates),
            Imputed = any(.data$is_imputed),
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

compAI <- function(stuff){
    paste(sort(unique(hf$trim(unlist(strsplit(sapply(
        stuff, p2ai), "\\+"))))),
                collapse = " + ")
}

collProds <- function(stuff){
    paste(sort(stuff), collapse = " + ")
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

## a little pest-name cleanup
finalDataSet$PestName <- tolower(unlist(
    llply(strsplit(sapply(finalDataSet$Pest, h), ":"), function(x){x[1]})
    ))

## calculate some summary statistics
PestAISummary <- finalDataSet %>%
    filter(!is.na(AI)) %>%
    group_by(PestName, AI) %>%
    dplyr::summarise(variance = var(smd),
                     mean = mean(smd),
                     samples = n()) %>%
    ungroup() %>%
    dplyr::mutate(stdev = sqrt(variance)) %>%
    dplyr::mutate(SEM = 1.96 * stdev / sqrt(samples)) %>%
    arrange(PestName)

## write.csv(PestAISummary, file = "~/Dropbox/ZhangLabData/cpyrPlots/pestAISummaries.csv")

if(outliers){
## and let's remove all of the samples that are more than 2std's away from diptheria... I mean the mean:
finalDataSet <- left_join(finalDataSet,
                          PestAISummary, by = c("PestName", "AI")) %>%
    dplyr::mutate(outside = (smd > 2*stdev + mean) |
                  (smd < mean - 2*stdev)) %>%
                      filter(!outside | Product == "UTC")

PestAISummary <- finalDataSet %>%
    filter(!is.na(AI)) %>%
    group_by(PestName, AI) %>%
    dplyr::summarise(variance = var(smd),
                     mean = mean(smd),
                     samples = n()) %>%
    ungroup() %>%
    dplyr::mutate(stdev = sqrt(variance)) %>%
    dplyr::mutate(SEM = 1.96 * stdev / sqrt(samples)) %>%
    arrange(PestName)

## write.csv(PestAISummary, file = "~/Dropbox/ZhangLabData/cpyrPlots/pestAISummariesSansOutliers.csv")
}

## count number of treatments in each experiment
expTrt <- finalDataSet %>%
    group_by(Experiment) %>%
    dplyr::summarise(num_treat = n()) %>%
    select(Experiment, num_treat) %>%
    ungroup() %>%
    filter(num_treat > 4)

## remove studies that would lead to unstable variance estimates
finalDataSet <- semi_join(finalDataSet, expTrt, by = "Experiment")

## weird "NA" cropping up, simple solution:
finalDataSet$AI[finalDataSet$AI == "NA" | finalDataSet$AI == ""] <- NA


## QUICK FIX! the original finalDataSet had UTC, not utc:
finalDataSet$Product[which(finalDataSet$Product == "utc")] <- "UTC"

## also, remove any products with unresolved AI's:
finalDataSet <- finalDataSet %>%
    filter(Product == "UTC" | !is.na(AI))

## calculate varsmd:
finalDataSet <- finalDataSet %>%
    group_by(Experiment) %>%
    do(mf$VarSMD(.)) %>%
    ungroup() %>%
    dplyr::mutate(SEMsmd = sqrt(varsmd))

## get an estimate for sample size:
sampleEsts <- as.numeric(gsub("[^0-9]", "", dat$Pest.units))
dat$SampleEstimates <- sampleEsts

est_df <- dat %>% distinct(PDF.file.name, Source.Fig.or.Table.number) %>%
    select(PDF.file.name, Source.Fig.or.Table.number, SampleEstimates)

pdf_source_v1 <- ins_table %>%
    distinct(PDF.file.name, Source.Fig.or.Table.number, V1) %>%
    dplyr::select(PDF.file.name, Source.Fig.or.Table.number, V1)

est_df <- left_join(pdf_source_v1, est_df, by = c("PDF.file.name", "Source.Fig.or.Table.number")) %>%
    dplyr::select(-Source.Fig.or.Table.number) %>%
    dplyr::rename(Experiment = V1,
                  PDF = PDF.file.name)

est_df$SampleEstimates[which(est_df$SampleEstimates == 103)] <- 10

tmp <- left_join(finalDataSet, est_df, by = c("PDF", "Experiment"))

## i <- which(finalDataSet$SEMestMax == 0)

if(summary){

    ## merge crop into data
    crop_dat <- dat %>%
        dplyr::select(PDF.file.name, Source.Fig.or.Table.number, Crop) %>%
            dplyr::distinct(PDF.file.name, Source.Fig.or.Table.number, Crop)

    crop_dat <- left_join(ins_table, crop_dat, by = c("PDF.file.name", "Source.Fig.or.Table.number")) %>%
        dplyr::select(PDF.file.name, V1, Source.Fig.or.Table.number, Crop) %>%
            dplyr::distinct(PDF.file.name, V1, Source.Fig.or.Table.number, Crop) %>%
                dplyr::rename(PDF = PDF.file.name, Experiment = V1) %>%
                    dplyr::mutate(Crop = tolower(Crop))
    
    finalDataSet <- left_join(finalDataSet, crop_dat, by = c("PDF", "Experiment"))

    ## write.csv(finalDataSet, file = "~/Dropbox/ZhangLabData/cpyrPlots/finalCpyrData.csv")

    ## sample size for all crops
    cropCounts <- finalDataSet %>%
            group_by(Crop) %>%
                dplyr::summarise(num_pdfs = length(unique(PDF)),
                                 num_exp = length(unique(Experiment)),
                                 num_pests = length(unique(Pest)),
                                 num_samp = n()) %>%
                                     ungroup()

    tmp_crop <- cropCounts %>%
        select(Crop, num_samp) %>%
            dplyr::rename(Number.of.Samples = num_samp) %>%
                arrange(desc(Number.of.Samples))

    ##write.csv(tmp_crop, file = "~/Dropbox/ZhangLabData/cpyrPlots/cropCounts.csv")
    tmp_crop <- finalDataSet %>%
        filter(AI == "chlorpyrifos") %>%
            mutate(usage_alfalfa = ifelse(LbsAI <= .94 & LbsAI >= .23, "s",
                       ifelse(LbsAI < .23, "l", "h")),
                   usage_orange = ifelse(LbsAI <= 5.64 & LbsAI >= .94, "s",
                       ifelse(LbsAI < .94, "l", "h")))

    tmp_orange <- tmp_crop %>% filter(Crop == "orange") %>%
        group_by(usage_orange) %>%
            dplyr::summarise(count = n()) %>%
                dplyr::rename(Use.Range = usage_orange,
                              Number.of.Samples = count)

    ##write.csv(tmp_orange, file = "~/Dropbox/ZhangLabData/cpyrPlots/usageOrange.csv")

    tmp_alfalfa <- tmp_crop %>% filter(Crop == "alfalfa") %>%
        group_by(usage_alfalfa) %>%
            dplyr::summarise(count = n()) %>%
                dplyr::rename(Use.Range = usage_alfalfa,
                              Number.of.Samples = count)

    ##write.csv(tmp_alfalfa, file = "~/Dropbox/ZhangLabData/cpyrPlots/usageAlfalfa.csv")
    
    ## sample size for all pests
    pestCounts <- finalDataSet %>%
        group_by(Pest, PestName) %>%
            dplyr::summarise(num_pdfs = length(unique(PDF)),
                             num_exp = length(unique(Experiment)),
                             num_ais = length(unique(AI)),
                             num_samp = n())

    tmp_pest <- pestCounts %>%
        ungroup() %>%
            filter(!is.na(Pest)) %>%
                select(PestName, num_samp) %>%
                    dplyr::rename(Pest.Common.Name = PestName, Number.of.Samples = num_samp) %>%
                        arrange(desc(Number.of.Samples))

    ##write.csv(tmp_pest, file = "~/Dropbox/ZhangLabData/cpyrPlots/pestCounts.csv")

    ## sample size for all ai
    aiCounts <- finalDataSet %>%
        filter(!is.na(AI)) %>%
        group_by(AI) %>%
            dplyr::summarise(num_pdfs = length(unique(PDF)),
                             num_exp = length(unique(Experiment)),
                             num_pests = length(unique(Pest)),
                             num_samp = n()) %>%
                                 ungroup() %>%
                                     mutate(cpyr_f = grepl("chlorp", AI))

    tmp_ai <- aiCounts %>%
        select(AI, num_samp) %>%
            dplyr::rename(Active.Ingredient = AI, Number.of.Samples = num_samp) %>%
                arrange(desc(Number.of.Samples))

    ## write.csv(tmp_ai, file = "~/Dropbox/ZhangLabData/cpyrPlots/aiCounts.csv")

    ## Sample size for cpyr-containing treatments
    cpyr_counts <- aiCounts %>%
        filter(cpyr_f == TRUE)
    cpyr_counts$AI <- unlist(llply(cpyr_counts$AI, function(x){
        paste(unlist(strsplit(x, "\\+")), collapse = "+ \n ")
    }))

    p <- ggplot(cpyr_counts, aes(x = AI, y = num_samp)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
            geom_bar(stat = "identity") +
                ylab("Number of Samples") +
                    xlab("Active Ingredient") +
                        theme(axis.text.x = element_text(size = 8, hjust = 1)) +
                    theme(axis.text.y = element_text(size = 10))
    
    ## ggsave(plot = p, filename = "~/Dropbox/ZhangLabData/cpyrPlots/CpyrAICounts.pdf") 

    ## Sample size for non-cpyr treatments
    non_cpyr_counts <- aiCounts %>% filter(cpyr_f == FALSE & !is.na(AI))
    p <- ggplot(non_cpyr_counts, aes(x = AI, y = num_samp)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
            geom_bar(stat = "identity") +
                ylab("Number of Samples") +
                    xlab("Active Ingredient") +
                        theme(axis.text.x = element_text(size = 6, hjust = 1)) +
                    theme(axis.text.y = element_text(size = 10))
    
    ## ggsave(plot = p, filename = "~/Dropbox/ZhangLabData/cpyrPlots/NonCpyrAICounts.pdf") 

    ## AI vs Pest:
    pestAi_Counts <- finalDataSet %>%
        filter(!is.na(AI) & !is.na(PestName)) %>%
            group_by(PestName,AI) %>%
                dplyr::summarise(num_pdfs = length(unique(PDF)),
                                 num_exp = length(unique(Experiment)),
                                 num_samp = n()) %>%
                                     ungroup() %>%
                                         arrange(desc(num_samp)) %>%
                                             mutate(cpyr_f = grepl("chlorp", AI))


    ## helper function to bin sample count values.
    f <- function(x){
        is.na(x) && return(NA)
        ranges <- list(1:10, 11:30, 31:50, 51:60, 61:80)
        bools <- unlist(llply(ranges,
                              function(range){x %in% range}))
        range <- unlist(ranges[bools])
        paste0(min(range), "-", max(range))
    }

    pestAi_Counts$PestName <- factor(pestAi_Counts$PestName,
                                 levels = unique(pestAi_Counts$Pest))
    pestAi_Counts$AI <- factor(pestAi_Counts$AI,
                               levels = unique(pestAi_Counts$AI))
    pestAi_Counts$bin <- factor(sapply(pestAi_Counts$num_samp, f),
                                levels = c("1-10", "11-30", "31-50", "51-60", "61-80"))


    g <- ggplot(pestAi_Counts, aes(x = PestName, y = AI, fill = bin)) +
        geom_tile() +
            scale_fill_manual(values = wes_palette("Zissou", 5)[c(2,1,3,4,5)]) +
                xlab("Pest Common Name") +
                    ylab("Active Ingredient") +
                        guides(fill=guide_legend(title="Number of Samples")) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
                            theme(axis.text.y = element_text(size = 4))

    ## ggsave(g, filename = "~/Dropbox/ZhangLabData/cpyrPlots/PestAIPlot.pdf")


    ## Dose Response 'Curves'
    ai_thresh <- finalDataSet %>%
        filter(!is.na(AI) & !is.na(Pest) & !is.na(smd)) %>%
        group_by(AI, Pest) %>%
            dplyr::summarise(num_samp = n()) %>%
                ungroup() %>%
                    dplyr::arrange(desc(num_samp)) %>%
                        top_n(16)

    ai_tmp <- semi_join(finalDataSet, ai_thresh, by = c("AI", "Pest"))
    g <- ggplot(ai_tmp, aes(x = LbsAI, y = smd, colour = Experiment, group = Experiment)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
            geom_point() +
                stat_smooth(method = "lm", se = FALSE) +
                xlab("Lbs of AI per Acre") +
                    ylab("Standard Mean Difference from Control") +
                theme(legend.position="none") +
                facet_wrap(AI~PestName, scale = "free")

    ## ggsave(g, filename = "~/Dropbox/ZhangLabData/cpyrPlots/DoseResponseTop16ColoredByExperiment.pdf")

    g <- ggplot(ai_tmp, aes(x = LbsAI, y = smd)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
            geom_point() +
                xlab("Lbs of AI per Acre") +
                    ylab("Standard Mean Difference from Control") +
                theme(legend.position="none") +
                facet_wrap(AI~PestName, scale = "free")

    ## ggsave(g, filename = "~/Dropbox/ZhangLabData/cpyrPlots/DoseResponseTop16.pdf")


    ## Cpyr formulation:
    cpyr_tmp <- finalDataSet %>%
        dplyr::filter(grepl("chlorpyrifos", AI) & !grepl("\\+", AI))
    cpyr_thresh <- cpyr_tmp %>%
        filter(!is.na(Product) & !is.na(AI) & !is.na(Pest)) %>%
            group_by(Product, Pest) %>%
                dplyr::summarise(num_samp = n()) %>%
                    ungroup() %>%
                        arrange(desc(num_samp)) %>%
                            filter(num_samp > 2) %>%
                                group_by(Pest) %>%
                                    dplyr::summarise(num_prods = length(unique(Product))) %>%
                                        ungroup() %>%
                                            filter(num_prods > 1)


    g <- ggplot(semi_join(cpyr_tmp, cpyr_thresh, by = c("Pest")), aes(x = Product, y = smd, color = Experiment)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
            theme(legend.position="none") +
            xlab("Pesticide Commercial Name") +
                ylab("Standard Mean Difference from Control") +
                    geom_point() +
                        facet_wrap(~PestName, scale = "free")

    ## ggsave(g, filename = "~/Dropbox/ZhangLabData/cpyrPlots/CpyrFormulationComparison.pdf")


    ## Cpyr-only vs cpyr+ effects
    cpyr_tmp <- finalDataSet %>%
        dplyr::filter(grepl("chlorpyrifos", AI))
    cpyr_thresh <- cpyr_tmp %>%
        filter(!is.na(Product) & !is.na(Pest)) %>%
            group_by(Product, Pest) %>%
                dplyr::summarise(num_samp = n()) %>%
                    ungroup() %>%
                        arrange(desc(num_samp)) %>%
                            filter(num_samp > 2) %>%
                                group_by(Pest) %>%
                                    dplyr::summarise(num_prods = length(unique(Product))) %>%
                                        ungroup() %>%
                                            arrange(desc(num_prods)) %>%
                                                filter(num_prods > 2)

    cpyr_tmp <- semi_join(cpyr_tmp, cpyr_thresh, by = c("Pest"))
    cpyr_tmp$Product <- unlist(llply(cpyr_tmp$Product, function(x){
        paste(unlist(strsplit(x, "\\+")), collapse = "+ \n ")
    }))
        
    g <- ggplot(cpyr_tmp, aes(x = Product, y = smd, color = Experiment)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
            theme(legend.position="none") +
                xlab("Pesticide Commercial Name") +
                    ylab("Standard Mean Difference from Control") +
                        geom_point() +
                            facet_wrap(~PestName, scale = "free")

    ## ggsave(g, filename = "~/Dropbox/ZhangLabData/cpyrPlots/CpyrPlusOtherFormulationComparison.pdf")
    
    ## ai vs insect:
    g <- ggplot(pestAi_Counts %>% filter(cpyr_f == TRUE), aes(x = Pest, y = AI, fill = bin)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
            geom_tile() +
                theme(axis.text.x = element_text(size = 6)) +
                    theme(axis.text.y = element_text(size = 4))
    ## ggsave(plot = g, filename = "~/Dropbox/ZhangLabData/cpyrPlots/SampleCount_by_Pest&AI_cpyr.pdf")

    g <- ggplot(pestAi_Counts %>% filter(cpyr_f == FALSE), aes(x = Pest, y = AI, fill = bin)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
            geom_tile() +
                theme(axis.text.x = element_text(size = 6)) +
                    theme(axis.text.y = element_text(size = 4))
    ## ggsave(plot = g, filename = "~/Dropbox/ZhangLabData/cpyrPlots/SampleCount_by_Pest&AI_no_cpyr.pdf")

    ## distribution of year
    ## qplot(finalDataSet$Year, geom = "histogram")

    ## distribution of smd
    ## qplot(finalDataSet$smd, geom = "density")

    ## smd with year
    ## ggplot(finalDataSet, aes(x = Year, y = smd)) + geom_point()

    ## are the pest-specific smd values 'relatively' normal:
    tmp_pests <- finalDataSet %>%
        group_by(Pest) %>%
            dplyr::summarise(count = n()) %>%
                filter(count >= 10)
    tmp_pests <- semi_join(finalDataSet, tmp_pests, by = "Pest")
    grobs <- llply(unique(tmp_pests$PestName),
                   function(pest){
                       tmp <- tmp_pest %>% filter(AI != "UTC" & PestName == pest) 
                       qqnorm(tmp$smd)
                   }
                   )
    names(grobs) <- unique(tmp_pests$PestName)
    ## to show:
    ## grobs[["alfalfa weevil"]]
}

if(clear){
    rm(list = setdiff(ls(), c("finalDataSet", "dat", "ins_table",
           "response_tables", "response_tables_lookup" )))
}

