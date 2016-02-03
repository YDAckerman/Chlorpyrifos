############################################################
## workflow to assemble and massage data. Since so much
## happens in this file, I tried to be more modular with
## my commenting. 
## author: Yoni Ackerman
## contact: jdackerma@ucdavis.edu
############################################################

# bring in the libraries
library(plyr); library(dplyr)
library(reshape2)

##____________set__working__ directory____________________##

setwd("~/Documents/ZhangLab/R/Chlorpyrifos")

##_________________set__options___________________________##

options(dplyr.width = Inf)

##__________source:__data__dicts__functions_______________##

source("addUniformRateToData.R")
## gives "full_dat", which is a clean up and
## merged version of all the cpyr data sets.

## source("R/create_pest_name_dict.R") yields:
load("~/Dropbox/ZhangLabData/pestDictionary.rda")
## Gives: pestDict

## if not run from cpyr_main, uncomment:
## source("R/export_multitable_excel_to.R") yields:
## load("~/Dropbox/ZhangLabData/response_data.rda")
## Gives: response_tables_lookup & response_tables

source("massage_functions.R") ## Gives: environment mf
source("table_operations.R") ## Gives: environment to
source("create_unique_multiprod_keys.R") ## Gives: addMultiProdKeysToData
source("stack.R"); set.seed(0) ## gives the class stack

##_______________________workflow_________________________##
############################################################

##__________________Get__Data_____________________________##

dat <- addMultiProdKeysToData(full_dat) %>%
    arrange(Original.sequence) 

## these are the tables who were 'entered', i.e. their
## v1-values aren't NA
entered <- response_tables_lookup %>%
    filter(!is.na(V1)) %>% 
    do(mf$correctDfCols(.)) ## get correct column names

##__________________make ins_table_________________________##
## llply has been giving me some trouble -> email hadley??

## initiate stack.
## We'll use this to give every row in the dataset we're about
## to generate a unique id.
keys <- replicate(6000, paste(sample(c(letters,1:26), 12, replace = TRUE),
                              collapse = ""))
stack <- new_stack()
stack$set(keys)

## this is for debugging, if an error
##get's thrown we'll know the index where it
## occured.
j <- 0
place <- 1

## This loop will end up building an intermediary dataset
## between the main/raw data and the finalDataSet data. It's
## going to go through each entered 'raw' table and:
## - calculate insect day measurements for each row
## - determine how long the study went on
## - calculate the SEM of the insect day measurement
## - and estimate the insect day SEM if necessary
## It will then combine these new columns with the
## group columns matching the 'raw' tables V1-id to form
## a new dataframe that can be seamlessly combined with all
## the other newly transformed 'raw' dataframes
## the last step (though it's in the first line), will be the
## do.call(rbind(  which will bind all the new dataframes
## together.
ins_table <- do.call(rbind, lapply(1:nrow(entered), function(x){

    row <- entered[x, ]
    hash <- row$V1
    pre_ins_table <- response_tables[[hash]]
    tmp <- dat %>%
        dplyr::mutate(
            Source.Fig.or.Table.number = hf$trim(Source.Fig.or.Table.number),
            PDF.file.name = hf$trim(PDF.file.name)
            ) %>%
        filter(PDF.file.name == row$PDF.file.name &
               Source.Fig.or.Table.number == row$Source.Fig.or.Table.number)
    stopifnot(nrow(tmp) != 0)
    col <- "StatTest"
    method <- unique(tmp[, col])
    method <- unique(ifelse(grepl("duncan", method, ignore.case = TRUE),
                            "Duncan", "Fisher"))
    ## do all table operations, then merge
    operations <- c("insectDays", "studyDuration","insectDaysSEM", "EstimateSEM")
    pre_ins_tables <- llply(operations, function(operation){
        to$tableOperation(pre_ins_table, operation, method = method)
    }, .inform = TRUE)
    ## before reducing, pull out the pretreatment column
    pretreatCols <- attr(pre_ins_table, "info")$pretreatCols
    ## Reduce! combine all the tables
    pre_ins_table <- Reduce(function(...) merge(..., all = TRUE), pre_ins_tables)
    ## pull from the merged tables the columns we want
    AScol <- setdiff(grep("adjuv", colnames(pre_ins_table), ignore.case = TRUE),
                     grep("rate", colnames(pre_ins_table), ignore.case = TRUE))
    ASrateCol <- intersect(grep("adjuv", colnames(pre_ins_table),
                                ignore.case = TRUE),
                           grep("rate", colnames(pre_ins_table),
                                ignore.case = TRUE))

    ## deal with pretreatment columns
    if(is.na(pretreatCols)){
        ## if there are no pretreat columns, creat a new NA-filled column
        ## named PreTreat
        pre_ins_table$PreTreat <- NA
    } else {
        ## if there are pretreat columns, excise them, sum them along rows
        ## and then assign the result to a new column called PreTreat
        pretreatVals <- pre_ins_table[, pretreatCols]
        if(length(pretreatCols) == 1){
            pre_ins_table$PreTreat <- pretreatVals
        } else {
            pre_ins_table$PreTreat <- rowSums(pretreatVals)
        }
    }

    ## select/rename the important rows of the 'raw' table
    slice <-
        pre_ins_table %>%
        select(Pesticide = 1,
               Rate = 2,
               RateRaw = 2,
               insectDays,
               studyDuration,
               stdEr_LB,
               stdEr_UB,
               insectDaysSEM,
               MultProdNum = intersect(
                   contains("mult", ignore.case = TRUE),
                   contains("prod", ignore.case = TRUE)),
               PreTreatmentPop = PreTreat
               )
    
    if(length(AScol) == 1){ slice$AS <- pre_ins_table[, AScol] }
    else { slice$AS <- NA }
    if(length(ASrateCol) == 1){ slice$ASrate <- pre_ins_table[, ASrateCol] }
    else { slice$ASrate <- NA }
    ## update j for 'debugging'
    j <- get("j", envir = globalenv())
    assign("j", j + 1, envir = globalenv())
    slice$Rate <- as.character(slice$Rate)
    slice$SampleID <- replicate(nrow(slice), stack$pop())

    ## TODO: this is a work in progress - an attempt to get an
    ## 'original sequence' 
    place <- get("place", envir = globalenv())
    slice$Original.sequence <- place:(place + nrow(slice) - 1)
    assign("place", place + nrow(slice), globalenv())
    ## attach the columns to the identifying values from row
    ## and return.
    cbind(row, slice, row.names = NULL)
}))

##___________________standardize trtmnts_____________________##
i <- hf$mgrep(c("untreated", "check", "utc", "control", "cehck", "water"),
              ins_table$Pesticide, ignore.case = TRUE)
ins_table$Pesticide[i] <- "UTC"



## for some reason or another, data was allowed to be entered multiple
## times. Let's fix that problem now:

##_________________Remove Repeat Data________________________##

## if we can find spots where the UTC is repeated, we'll
## know the whole table is a duplicate:
ins_table_utc <- ins_table %>% filter(Pesticide == "UTC")

tmp <- ldply(1:nrow(ins_table_utc), function(i){
    ## pretend its a loop and go through by rows
    row <- ins_table[i,]
    ## We're looking for repeats, so search ins_table for
    ## any matches with itself in pesticide and insectdays,
    ## but non-matches with the V1 table hash
    slice <- ins_table %>% filter(Pesticide == row$Pesticide,
                                  insectDays == row$insectDays,
                                  V1 != row$V1)
    ## if the slice is empty, we're alright
    if(empty(slice)){
        return(NULL)
    }
    ## otherwise, get the info:
    data.frame(ID = row$V1,
               Pesticide = row$Pesticide,
               insectDays = row$insectDays,
               pdf = row$PDF.file.name,
               tbl = row$Source.Fig.or.Table.number,
               repPDFs = paste(unique(slice$PDF.file.name), collapse = ";"),
               reptbls = paste(unique(slice$Source.Fig.or.Table.number),
                   collapse = ";"),
               repIDs = paste(unique(slice$V1), collapse = ";"),
               repInsectDays = paste(unique(slice$insectDays), collapse = ";"),
               repPesticides = paste(unique(slice$Pesticide), collapse = ";"),
               stringsAsFactors = FALSE)
}, .progress = "text")

repV1 <- c()
for (i in 1:nrow(tmp)) {
    V1 <- tmp$ID[i]
    reps <- unlist(strsplit(tmp$repIDs[i], ";"))
    b <- any(reps %in% repV1)
    if(!b){
        repV1 <- c(repV1, V1)
    }
}

redundantV1s <- setdiff(unlist(strsplit(tmp$repIDs, ";")),repV1)

ins_table <- ins_table %>% filter(!(V1 %in% redundantV1s))


## Rate is included for future joining purposes, so let's convert
## it to numeric:
ins_table$Rate <- gsub("[^0-9\\.\\+]", "", ins_table$Rate)


##________________edit number of replicates__________________##
ins_table$Number.of.replicates <- as.numeric(ins_table$Number.of.replicates)

##__________________create Effect ratio______________________##
ins_table <-
    ins_table %>%
    group_by(V1) %>%
    do(mf$calcPercent(., "UTC", "Pesticide", "insectDays"))


##__________________standardize insect names_________________##
standardPestNames <- unlist(sapply(ins_table$Pest.common.name, function(x){
    if (is.na(x)){ return(NA) }
    bools <- sapply(ls(pestDict), function(y){
        x %in% pestDict[[y]]
    })
    if(sum(bools) == 0){return(NA)}
    ls(pestDict)[bools]
}))

ins_table['Pest'] <- standardPestNames

## let's remove all the  UTC lines from ins_table in preparation
## for merging

ins_table_utc <- filter(ins_table, Pesticide == "UTC")
ins_table_treated <- filter(ins_table, Pesticide != "UTC")

##________________get 'official' pesticide name_______________##

ins_table_treated <- mf$cPesticideMatchCol(ins_table_treated)
ins_table_utc["Pesticide.commercial.name"] <- NA

##_________________match mpn's between dfs__________________##

ins_table_treated <- mf$cMultiProdMatchCol(ins_table_treated)
ins_table_utc["Multiple.product.numbers"] <- NA
ins_table_utc["MPNkey"] <- NA

## how to test?
## test <- sapply(1:nrow(ins_table_treated), function(i){
##     row <- ins_table_treated[i,]
##     is.na(row$Multiple.product.numbers) == is.na(row$MultProdNum)
## })

##_______________create 'official' rate cols_____________________##

tmp <- mf$cRate(ins_table_treated)
## test
## test <- data.frame(ins_table_treated$RateRaw, tmp$Application.rate, tmp$Application.rate.units, tmp$Uniform.application.rate, tmp$Uniform.application.rate.units)
## write.csv(test, "test.csv")

ins_table_treated <- data.frame(ins_table_treated, tmp, stringsAsFactors = FALSE)

## use this to find the problematic entries
## test <- sapply(1:nrow(ins_table_treated), function(i){
##     row <- ins_table_treated[i,]n
##     if(is.na(row$Rate) | is.na(row$Application.rate)){ return(NA) }
##     a <- sum(as.numeric(unlist(strsplit(row$Rate, "\\+")))) == sum(as.numeric(unlist(strsplit(row$Application.rate, "\\+"))))
##     if(is.na(a)){ return(NA) }
##     b <- row$RateRaw == row$Application.rate
##     if(a | b){ return(TRUE) } else { return(FALSE) }
## })

ins_table_utc[,colnames(tmp)] <- NA

## now that we have all the AI info in dat, it is easy to transfer it to
## ins_table:

ai_dat <- dat %>%
    dplyr::select(DPR.Label.Database.name, Pesticide.commercial.name,
           AI.1, X..AI1,
           AI.2, X..AI2,
           AI.3, X..AI3,
           AI.4, X..AI4) %>%
    dplyr::distinct(Pesticide.commercial.name,
           AI.1, X..AI1,
           AI.2, X..AI2,
           AI.3, X..AI3,
           AI.4, X..AI4)

ins_table_treated <- left_join(ins_table_treated, ai_dat, by = "Pesticide.commercial.name")

## make a better MPNkey
ins_table_treated <- ins_table_treated %>%
    dplyr::mutate(MPNkey = ifelse(is.na(MultProdNum), NA,
                      paste(V1, MultProdNum, sep = "-")))


##____________standardize AIs________________________##

ins_table_treated[,c("AI.1", "AI.2", "AI.3", "AI.4")] <-
    llply(c("AI.1", "AI.2", "AI.3", "AI.4"), function(x){
        tolower(gsub("chlorantraniliprole\\+lambda\\-cyhalothrin",
                     "lambda-cyhalothrin + chlorantraniliprole",
                     gsub("chlorpyifos",
                          "chlorpyrifos",
                          ins_table_treated[, x])))
    })

ins_table_treated <- ins_table_treated %>%
    dplyr::mutate(AI.1 = ifelse(AI.1 == "na", NA, AI.1),
           AI.2 = ifelse(AI.2 == "na", NA, AI.2),
           AI.3 = ifelse(AI.3 == "na", NA, AI.3),
           AI.4 = ifelse(AI.4 == "na", NA, AI.4))

##lets get rid of these ahead of time!
ins_table_utc$DPR.Label.Database.name <- "UTC"
ins_table_utc$AI <- NA
ins_table_utc$AIperc <- NA

## let's melt the data so that each ai has it's own row:
ins_table_treated <- ldply(1:nrow(ins_table_treated), function(i){
    ## specify the columns we want to melt
    ai_cols <- c( "AI.1", "AI.2", "AI.3", "AI.4")
    ai_perc_cols <- c("X..AI1", "X..AI2", "X..AI3", "X..AI4")
    ## get the sample in question
    row <- ins_table_treated[i,]

    ## if for some reason there are no AI's, return
    ## the row with empty AI and AIperc variables
    if(is.na(row$AI.1)){
        row <- row[, setdiff(colnames(row), c(ai_cols, ai_perc_cols))]
        row$AI <- NA
        row$AIperc <- NA
        return(row)
    }
    ## melt
    ais <- melt(row[, c("SampleID",ai_cols)], id = "SampleID") %>%
        dplyr::rename(AI = value) %>%
            dplyr::select(-variable)
    ai_percs <- melt(row[, c("SampleID",ai_perc_cols)], id = "SampleID") %>%
        dplyr::rename(AIperc = value) %>%
            dplyr::select(-variable)
    ## form df from the melted data
    new_cols <- data.frame(AI = ais$AI, AIperc = ai_percs$AIperc, stringsAsFactors = FALSE) %>%
        filter(!is.na(AI))
    ## remove the melted columns from row
    row <- row[, setdiff(colnames(row), c(ai_cols, ai_perc_cols))]
    ## column bind and return
    cbind(row, new_cols)
})

## bind the two pieces back together
ins_table <- rbind(ins_table_utc, ins_table_treated)
ins_table <- as.data.frame(ins_table) %>% arrange(Original.sequence)

##___________________Calc Uniform AI units__________________##
tmp <- ins_table %>% select(Uniform.application.rate,
                            Uniform.application.rate.units,
                            Density, Application.Counts,
                            AIperc)

tmp <- mdply(tmp, mf$convertRate, .expand = TRUE, .inform = TRUE)

ins_table$AILbsAcre <- tmp$AILbsAcre


##_____________________form the ln ratio___________________________##
ins_table['LnR'] <- log(ins_table$PercControl)
ins_table['LnR1'] <- log(ins_table$PercControl1)

##________________________Fix up Locality___________________________##
ins_table <- mf$standardizeLocality(ins_table)

##_______________________Obtain the slope___________________________##
##                        of the control                            ##

tmp <- ldply(unique(ins_table$V1), function(v1){
    tab <- response_tables[[v1]]
    tab <- to$tableOperation(tab, "ctrlSlope")
    data.frame(V1 = v1, ctrlSlope = unique(tab$ctrlSlope),
               stringsAsFactors = FALSE)
})

ins_table <- merge(ins_table, tmp, by = "V1")

##_______________________Convert SEM values_________________________##
##                        and estimates                             ##
##                       by effect transform                        ##

## I also want to impute some SEM values here. That is, use the variance
## of the non-control insectdays measures as the SEM for each of those
## measurements:

imputedSEMs <- ins_table %>%
    filter(Pesticide != "UTC") %>%
    group_by(V1) %>%
    dplyr::summarise(imputedSEM = sqrt(var(insectDays, na.rm = TRUE))) %>%
    ungroup()

ins_table <- left_join(ins_table, imputedSEMs, by = "V1")

ins_table <-
    ins_table %>%
    group_by(V1) %>%
    do(mf$convertSEMbyTransform(.)) %>%
    ungroup()

##_______________________StdMeanDiff________________________________##
##                                                                  ##
ins_table <- ins_table %>%
    group_by(V1) %>%
    do(mf$calcStdMeanDiff(.))

## Select Best stdMeanDif
stdMnDiff <- ldply(1:nrow(ins_table), function(i){
    row <- ins_table[i,]
    imputed <- FALSE
    ifelse(!is.na(row$di_t_sem), (TRUE &&  (smd <- row$di_t_sem)),
           ifelse(!is.na(row$di_t_est), (TRUE && (smd <- row$di_t_est)),
                  ((smd <- row$di_t_imp) && (imputed <- TRUE))))
    data.frame(stdMnDiff = smd, is_imputed = imputed)
})

ins_table <- cbind(ins_table, stdMnDiff)


##___________________________Weights________________________________##
##                                                                  ##

## The estimated and given SEMs needed to be converted into
## log ratio SEMs ... that is why things  looked so strange.

## add best estimates to ins_table as columns:
weights1 <- dnorm(ins_table$ctrlSlope, 0, sqrt(1))

weights2 <- unlist(alply(ins_table %>% select(trSEM_LB, trSEM_UB, trSEM, trSEMimp), 1,
                  function(x){
                      x <- unlist(x)
                      if(sum(is.na(x)) %in% 3:4){return(x[4])}
                      x <- x[1:3]
                      bool <- !is.na(x) & !is.infinite(x)
                      if(!any(bool)){
                          return(NA)
                      } else {
                          return(max(x[bool]))
                      }
                  }))

weights3 <- unlist(alply(ins_table %>% select(trSEM_LB, trSEM_UB, trSEM, trSEMimp), 1,
                  function(x){
                      x <- unlist(x)
                      if(sum(is.na(x)) %in% 3:4){return(x[4])}
                      x <- x[1:3]
                      bool <- !is.na(x) & !is.infinite(x)
                      if(!any(bool)){
                          return(NA)
                      } else {
                          return(min(x[bool]))
                      }
                  }))

ins_table['slopeWeights'] <- weights1
ins_table['SEMestMax'] <- weights2
ins_table['SEMestMin'] <- weights3


##_________Change_Pesticide_Commercial_Name_to_DPR_Name_____________##
ins_table <- ins_table %>%
    dplyr::mutate(DPR.Label.Database.name = ifelse(is.na(DPR.Label.Database.name),
                      Pesticide.commercial.name, DPR.Label.Database.name))

##_______________________add indicated ais__________________________##
i <- tolower(ins_table$Pesticide) %in% tolower(ins_table$AI)
ais <- tolower(ins_table$Pesticide[i])
ins_table$AI[i] <- ais
gPerc <- unlist(llply(which(i), function(x){
    ifelse(is.na(ins_table$AIperc[x]), "100", ins_table$AIperc[x])
}))
ins_table$AIperc[i] <- gPerc
## TODO: probably need to insert density as well.

##__________________fix mispellings, etc____________________________##

i <- ins_table$AI == "(s)-cypermethrin"
ins_table$AI[i] <- "zeta-cypermethrin"
i <- ins_table$AI == "chlorpyifos"
ins_table$AI[i] <- "chlorpyrifos"
##__________________add standard use ranges_________________________##

ranges <- data.frame(
    AI = c("methoxyfenozide","zeta-cypermethrin","beta-cyfluthrin",
        "carbofuran","chlorpyrifos","cyfluthrin","flubendiamide",
        "gamma-cyhalothrin","indoxacarb","lambda-cyhalothrin","spinetoram",
        "spinosad", "thiamethoxam", "acetamiprid", "bifenthrin", 
        "abamectin", "etoxazole", "fenpropathrin", "milbemectin"),
    minLbs_ai_a = c(.06,.008,.0065, 0.1336668,.5, .013,
        0.06256145,.0075,.045,.0075,.0156,.023, .023,.028,.033,0.0046875,.03,.05,NA),
    maxLbs_ai_a = c(.38,.025,.025, 1.069335,4, .05, .156,.24,.11,
        .48,.0938,.156,.125,.250,.2,0.01875,.18,.4,NA),
    stringsAsFactors = FALSE
    )

##___________________________save___________________________________##
## if not run from main, uncomment:
## save(ins_table, ranges, response_tables, response_tables_lookup, dat,
##      file = "~/Dropbox/ZhangLabData/cpyrMAdataJan27.rda")

##___________________________clean up_______________________________##
rm(list = setdiff(ls(), c("ins_table", "ranges", "response_tables",
       "response_tables_lookup", "dat"))

######################################################################
######################################################################
######################################################################

