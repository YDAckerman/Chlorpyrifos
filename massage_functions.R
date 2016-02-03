############################################################
## Helper functions for massaging data (in general? - no, ##
## only in particular... *sigh*)                          ##
############################################################

## set working directory, load sources & libraries
setwd("~/Documents/ZhangLab/R/Chlorpyrifos")

library(stringdist)
source("~/Documents/Coding/R/R_convenience/helper_functions.R")

mf <- new.env()

## calculate the percentage of values in a column against
## some defined baseline. Must give text names.
mf$calcPercent <- function(.data, baseName, baseColumn, valueColumn){
    val <- .data[[valueColumn]][which(.data[[baseColumn]] == baseName)]
    ## add 1 to valueColumn to remove Zeros (ok?)
    data.frame(.data,
               PercControl = .data[[valueColumn]] / val,
               PercControl1 = (.data[[valueColumn]] + 1) / val, stringsAsFactors = FALSE)
}

mf$calcStdMeanDiff <- function(.data){
    control <- .data %>% filter(Pesticide == "UTC") %>%
        mutate(stdEr_est =
               ifelse(is.infinite(stdEr_LB) | is.infinite(stdEr_UB),
                      NA, stdEr_UB))

    .data <- .data %>%
        mutate(di_t_sem = mf$a_i(control$Number.of.replicates) *
               (insectDays - control$insectDays) /
               control$insectDaysSEM,
               di_t_est = mf$a_i(control$Number.of.replicates) *
               (insectDays - control$insectDays) /
               control$stdEr_est,
               di_t_imp = mf$a_i(control$Number.of.replicates) *
               (insectDays - control$insectDays) /
               (sqrt(control$Number.of.replicates) * control$imputedSEM)
               )
    .data
}

mf$a_i <- function(n_i){
    is.na(n_i) && return(NA)
    (4 * n_i - 8) / (4 * n_i - 5)
}

mf$cov_dij <- function(di_t1, di_t2, ni_c){
    part1 <- mf$a_i(ni_c) ^ 2
    part2 <- (ni_c - 1) * ((ni_c - 3) ^ (-1)) *
        ((ni_c) ^ (-1))
    part3 <- (di_t1 * di_t2) * (((mf$a_i(ni_c)) ^ 2) * (ni_c - 1) *
                         ((ni_c - 3) ^ (-1)) - 1)
    part1 * part2 + part3
}

mf$cov_dij_imp <- function(di_t1, di_t2, ni, ki){
    part1 <- mf$a_i(ki) ^ 2
    part2 <- (ki - 1) * ((ki - 3) ^ (-1)) *
        ((ni) ^ (-1))
    part3 <- (di_t1 * di_t2) * (((mf$a_i(ki)) ^ 2) * (ki - 1) *
                         ((ki - 3) ^ (-1)) - 1)
    part1 * part2 + part3
}

mf$var_dij <- function(di, ni_c, ni_t){
    part1 <- mf$a_i(ni_c) ^ 2
    part2 <- (ni_c - 1) * ((ni_c - 3) ^ (-1)) * (ni_t + ni_c) *
        ((ni_c * ni_t) ^ (-1))
    part3 <- (di ^ 2) * (((mf$a_i(ni_c)) ^ 2) * (ni_c - 1) *
                         ((ni_c - 3) ^ (-1)) - 1)
    part1 * part2 + part3
}

mf$var_dij_imp <- function(di, ni, ki){
    part1 <- mf$a_i(ki) ^ 2
    part2 <- (ki - 1) * ((ki - 3) ^ (-1)) * (2) *
        ((ni) ^ (-1))
    part3 <- (di ^ 2) * (((mf$a_i(ki)) ^ 2) * (ki - 1) *
                         ((ki - 3) ^ (-1)) - 1)
    part1 * part2 + part3    
}


mf$VarSMD <- function(.data){
    imputed <- unique(.data$Imputed)
    ctrl <- .data %>% filter(Product == "UTC")
    ntreat <- nrow(.data) - 1
    varsmd <- unlist(llply(1:nrow(.data), function(i){
        if(.data$Product[i] == "UTC"){return(NA)}
        if(imputed){
            mf$var_dij_imp(.data$smd[i], .data$Replicates[i], ntreat)
        } else {
            mf$var_dij(.data$smd[i], ctrl$Replicates, .data$Replicates[i])
        }
    }))
    .data$varsmd <- varsmd
    .data
}

mf$vcov <- function(.data){
    
    imputed <- unique(.data$Imputed)
    ctrl <- .data %>% filter(Product == "UTC")
    dat <- .data %>% filter(Product != "UTC")
    nrows <- nrow(dat)
    indices <- expand.grid(i = 1:nrows, j = 1:nrows)
    indices <- indices %>% arrange(i, j)
    mat <- mdply(indices, function(i, j){
        if(i > j){ 0 }
        else if(imputed){
            if(i == j){
                mf$var_dij_imp(dat$smd[i], dat$Replicates[i],
                               nrows)
            } 
            else {
                mf$cov_dij_imp(dat$smd[i], dat$smd[j],
                               dat$Replicates[i], nrows)
            }
        } else {
            if(i == j){
                mf$var_dij(dat$smd[i], ctrl$Replicates,
                           dat$Replicates[i])
            }
            else {
                mf$cov_dij(dat$smd[i], dat$smd[j], ctrl$Replicates)
            }
        }
    }, .expand = TRUE)
    mat <- matrix(mat$V1, nrows, nrows, byrow = TRUE)
    mat <- mat + t(mat)
    mat - mat * diag(.5, nrows, nrows)
}

mf$convertSEMbyTransform <- function(.data){

    ## following the propagation of error given by:
    ## x = ln((a + 1) / b) ->
    ## sigma_x^2 = (1/(a + 1)) ^ 2 * sigma_a ^ 2 + (1/b) ^ 2 * sigma_b  ^ 2
    
    erCols <- c("stdEr_LB", "stdEr_UB", "insectDaysSEM", "imputedSEM")
    sems <- .data %>% select_(.dots = erCols)

    ## since sigma_a == sigma_b we'll call it sigma_sem
    sigma_sem_sq <- sems ^ 2

    ## sigma_a is the treatment's insect days
    sigma_a_sq <- (1 / (.data$insectDays + 1)) ^ 2
    
    ctrl <- .data %>% filter(Pesticide == "UTC")

    ## sigma_b is the control's insect days
    sigma_b_sq <- ( 1 / ctrl$insectDays) ^ 2

    ## sqrt(sigma_x^2) = sigma_x
    total <- sqrt(sigma_sem_sq * (sigma_a_sq + sigma_b_sq))
    
    colnames(total) <- c("trSEM_LB", "trSEM_UB", "trSEM", "trSEMimp")
    cbind(.data, total, stringsAsFactors = FALSE)
}

## wrapper function for addPesticideMatch
mf$cPesticideMatchCol <- function(df){
    ## group df by pdf, table in pdf, and pesticide
    ## in table, then perform addPesticideMatch on
    ## each group.
    df %>%
        group_by(PDF.file.name,
                 Source.Fig.or.Table.number,
                 Pesticide) %>%
                     do(mf$addPesticideMatch(.))
}

## find the product in dat that matches the pesticide
## in each row of ins_table (for future joining)
mf$addPesticideMatch <- function(.data){

    ## pull out relevant values
    pesticide <- unique(.data$Pesticide)
    pdf <- unique(.data$PDF.file.name)
    table <- unique(.data$Source.Fig.or.Table.number)
    slice <-
        dat %>%
            dplyr::mutate(PDF.file.name = hf$trim(PDF.file.name),
                          Source.Fig.or.Table.number = hf$trim(Source.Fig.or.Table.number)) %>%
                filter(PDF.file.name == pdf &
                       Source.Fig.or.Table.number == table)
    ## pull products and rates from the subset
    products <- slice$Pesticide.commercial.name
    ## find most likely product
    p_dists <- stringdist(tolower(pesticide), tolower(products))
    i <- which(p_dists == min(p_dists, na.rm = TRUE))
    .data["Pesticide.commercial.name"] <- products[i[1]]
    .data
    
}

## convert the 'uniform application rate/unit' to total mass of AI
mf$convertRate <- function(Uniform.application.rate,
                           Uniform.application.rate.units,
                           Density, Application.Counts, AIperc){
    
    unif_rate <- as.numeric(Uniform.application.rate)
    aiperc <- as.numeric(unlist(strsplit(gsub("\\.\\.", "\\.", AIperc), "~")))
    dens <- as.numeric( unlist( strsplit( gsub("[^0-9\\.-]", "", gsub(
        "\"13-17 lb/cu ft. (loose)\"", "", Density)), "-")))
    
    perc <- mean(aiperc) / 100
    k1 <- prod(as.numeric(c(Application.Counts, unif_rate)))
    dens <- mean(dens)
    if(!is.na(dens)) {
        if(dens > 2){
            dens <- dens * 453.592 * .000264172
        }
    }

    ## TODO: make sure AI/acre really means AI per acre...
    if (is.na(Uniform.application.rate.units)){
        return(data.frame(AILbsAcre = NA))
    }
    if (Uniform.application.rate.units == "lb AI/acre"){
        ret <- k1
    } else if (Uniform.application.rate.units == "fl oz product/acre"){
        ## fl oz / acre -> ml /acre -> g / acre -> lbs /acre
        ret <- k1 * 29.5735 * .00220462 * dens * perc
    } else if (Uniform.application.rate.units == "dry oz product/acre"){
        ## dry oz / acre -> lbs / acre
        ret <- k1 * perc * .0625
    } else if (Uniform.application.rate.units == "dry oz AI/acre"){
        ret <- k1 * .0625
    } else if (Uniform.application.rate.units == "lb AI/tree"){
        ret <- NA
    }

    ret <- as.data.frame(as.list(round(ret, 3)))
    colnames(ret) <- c("AILbsAcre")
    ret
}

## wrapper function for addMprodMatch
mf$cMultiProdMatchCol <-  function(df){

    ## group data by pdf, table in pdf, pest units in table, and
    ## mprod# in table, then perform addMprodMatch on each group
    df %>%
            group_by(PDF.file.name,
                     Source.Fig.or.Table.number,
                     Pest.units,
                     MultProdNum) %>%
                         do(mf$addMprodMatch(.))
}

## for a group of data from the same pdf, table, pest unit, and mprod#
## find the corresponding mprod# for that group in the cpyr data set
## and add those numbers as a new column to the data. then return the data.
mf$addMprodMatch <- function(.data){

    ## gather up all the necessary identifiers:
    pesticides <- unique(.data$Pesticide.commercial.name)
    pdf <- unique(.data$PDF.file.name)
    table <- unique(.data$Source.Fig.or.Table.number)
    unit <- unique(.data$Pest.units)
    rates <- as.numeric(.data$Rate)
    adjsur <- .data$AS
    adjsurR <- as.numeric(gsub("[^0-9\\.]", "", .data$ASrate))
    mprod <- unique(.data$MultProdNum)
    MPNkey <- NA

    ## if there is no  mprod# for this group, let MPNkey remain NA,
    ## otherwise proceed
    if (!all(is.na(mprod))){

        ## filter the cpyr data set using the identifiers
        tmp <-
            dat %>% filter(PDF.file.name == pdf &
                           Source.Fig.or.Table.number == table &
                           Pest.units == unit)

        ## group tmp by multiple.product.numbers (the cypr equiv of mprod#)
        ## and compare each group to the pesticide usage identifiers.
        ## (why'd I use ddply? was dplyr being weird or something?)
        tmp <-
            ddply(tmp, .(Multiple.product.numbers), function(x){
                mf$checkGroup(x, pesticides, rates, adjsur, adjsurR)
            })

        ## if the results are a dataframe, pull out the original multiple
        ## product number and the unique key and reset mprod and mpnkey
        if(is.data.frame(tmp)){
            mprod <- paste(unique(tmp$Multiple.product.numbers), collapse = ";")
            MPNkey <- paste(unique(tmp$key), collapse = ";")
        }
    }

    ## create new columns filled with the results and return
    .data["MPNkey"] <- MPNkey
    .data["Multiple.product.numbers"] <- mprod
    .data
}

## group undstandardized location names using string intersect
mf$standardizeLocality <- function(.data){

    ## load states df
    source("~/Documents/ZhangLab/R/Chlorpyrifos/stateAbbr.R")

    Locs <- unique(.data$Locality)

    ## figure out which state is in which loc for all  locs
    tmp <- mdply(states, function(state, stateAbbr){
        hf$mgrepl(
            c(state, paste0(" ",stateAbbr)),
            hf$removeParens(Locs),
            ignore.case = FALSE)
    })
    
    standardLocs <- unlist(llply(.data$Locality, function(loc){
        j <- which(Locs == loc)
        j <- which(tmp[, j + 2])
        if(length(j) == 0){ return(NA) }
        return(paste(states[j,], collapse = " "))
    }))
    
    ## create a new and aptly named col in data and return data
    .data["standardLocs"] <- standardLocs
    .data
}

## takes a group of data with shared pdf, table, pest units, and mprod
## number, and compares its pesticide-use variables to given pesticide-use
## identifiers. Returns the group if there's a match, null otherwise.
mf$checkGroup <- function(.data, pesticides, rates, adjsur, adjsurR){

    pcides <- unique(.data$Pesticide.commercial.name)
    arate <- as.numeric(unique(gsub("[^0-9\\.]", "",.data$Application.rate)))

    as <- unique(.data$Surfactant.Adjuvant..if.any.)
    asr <- as.numeric(unique(gsub("[^0-9\\.]", "",
                       .data$Surfactant.Adjuvant.application.rate)))

    ## use some fuzzy matching helper functions to see if there's a
    ## match
    if (setequal(pcides, pesticides) &
        setequal(arate , rates) &
        all(hf$aIn(as , adjsur)) &
        all(hf$aIn(asr , adjsurR))
        ){
        return(.data)
    } else {
        NULL
    }
}

## uses bruteforce identifiers to filter cpyr according to each
## row in df. pulls out the application rate and application rate units
## from the results and returns the collection of all results  merged
## into df. 
mf$cRate <- function(df){
    ldply(1:nrow(df), function(i){
        
        row <- df[i, ]

        rate <- row$Rate
        sour <- row$Source.Fig.or.Table.number
        pdf <- row$PDF.file.name
        cide <- row$Pesticide.commercial.name

        if(is.na(rate)){
            return(data.frame(Application.rate = NA,
                              Application.rate.units =  NA,
                              Uniform.application.rate = NA,
                              Uniform.application.rate.units = NA,
                              Density = NA,
                              Application.Counts = NA
                              ))
        }
        
        tmp <- dat %>%
            dplyr::mutate(Source.Fig.or.Table.number = hf$trim(Source.Fig.or.Table.number)) %>%
                filter(PDF.file.name == pdf,
                       Source.Fig.or.Table.number == sour,
                       Pesticide.commercial.name == cide) %>%
                           ## create a new column, Rate, that will
                           ## be easier to match on.
                           mutate(Rate = as.character(gsub("[^0-9\\.\\+]", "",
                                      Application.rate)))

        if( grepl("\\+", rate) ){
            j <- which.min(stringdist(rate, tmp$Rate))
        } else {
            j <- which.min(stringdist(as.character(as.numeric(rate)), as.character(as.numeric(tmp$Rate))))
        }
        
        ##if(empty(tmp) | nrow(tmp) > 1){print(i)}
        data.frame(Application.rate = tmp$Application.rate[j],
                   Application.rate.units = tmp$Application.rate.units[j],
                   Uniform.application.rate = tmp$Uniform.application.rate[j],
                   Uniform.application.rate.units = tmp$Uniform.application.rate.units[j],
                   Density = tmp$Density[j],
                   Application.Counts = tmp$Application.Counts[j],
                   stringsAsFactors = FALSE)

    })
}

## converts the application rate and application rate units
## to a uniform value that we can then convert into total
## mass of AI. used for "new" data sets

mf$convertToUniformRate <- function(Application.rate, Application.rate.units){

    ## Application.rate <- dat_new[1,]$Application.rate
    ## Application.rate.units <- dat_new[1,]$Application.rate.units

    if(is.na(Application.rate.units) |
       is.na(Application.rate) |
       hf$mgrepl(c("%", "\\^", "\\(", "\\)"), Application.rate) |
       hf$mgrepl(c("tree", "conidia", "kg seed", "v/v"), Application.rate.units)){
        return(data.frame(Uniform.application.rate = NA,
                          Uniform.application.rate.units = NA,
                          stringsAsFactors = FALSE))
    }

    ## when multiple rates are given (separated by +), take the
    ## average - the product w/ Application count will come out to
    ## the same thing
    ap_rate <- mean(as.numeric(unlist(strsplit(Application.rate, "\\+"))))
    is_ai <- grepl("AI", Application.rate.units)
    num_conversion <- c(16,16,32,0.035274,128,33.814,.033814,1,1,1)
    den_conversion <- c(.404686, 1, 1)
    den_units <- c("ha", "A$", "acre")
    is_fluid <- c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)
    num_units <- c("pt","lb","qt","g","gal","L","ml", "dry oz", "fl oz", "^oz")

    den <- sapply(den_units, function(x) grepl(x, hf$removeParens(Application.rate.units)))
    num <- sapply(num_units, function(x) grepl(x, hf$removeParens(Application.rate.units)))

    if(setequal(num[4:5], c(TRUE, TRUE))){
        num[4:5] <- c(FALSE, TRUE)
    }

    if(all(!den)){
        return(data.frame(Uniform.application.rate = NA,
                          Uniform.application.rate.units = NA,
                          stringsAsFactors = FALSE))
    }

    unif_ap_rate <- ap_rate * num_conversion[num] / den_conversion[den]
    unit <- ifelse(is_fluid[num], "fl oz ", "dry oz ")
    type <- ifelse(is_ai, "AI", "product")

    ret <- data.frame(Uniform.application.rate = round(unif_ap_rate, 2),
                      Uniform.application.rate.units = paste0(unit, type, "/acre"),
                      stringsAsFactors = FALSE)

    if(nrow(ret) > 1){
        print(paste(Application.rate, Application.rate.units, sep = " "))
    } else {
        return(ret)
    }
    
}

mf$correctDfCols <- function(df){

    corrections <- list(
        Entered.by = list(c("enter", "by"), c("data", "entry")),
        Proofed.by = c("proof"),
        Locality = c("loca"),
        Crop = c("crop"),
        Variety = c("variety"),
        Number.of.replicates = list(c("num", "replica"), c("replicate(?!\\.)"),
            c("replications")),
        Study.design = list(c("study", "des"), c("rcbd")),
        Replicate.size = c("size"),
        Application.rate = list(c("pesticide", "usage"),
            c("(?<!.)Appl\\.rate(?!.)")),
        Multiple.product.numbers = list(c("Multi\\.prod\\.no"),
            c("mult", "prod", "number")),
        Application.rate.units = list(c("(?<!.)appl", "rate", "unit"),
            c("pesticide", "unit")),
        Uniform.application.rate = c("uniform", "application", "rate(?!\\.)"),
        Uniform.application.rate.units = c("uniform", "application",
            "rate", "units"),
        Density = c("density"),
        Application.volume = list(c("total\\.applied\\.amount"),
            c("appl\\.vol(?!.)"), c("appl\\.volume(?!.)"),
            c("Application\\.volume\\.\\.just\\.a\\.number\\.")),
        Application.volume.units = c("appl", "vol", "unit"),
        Treatment.medium = list(c("treat", "medium"), c("treat", "media")),
        Treatment.dates = list(c("treat", "dates", "incl"),
            c("Treatment\\.dates"), c("Treatment\\.date"), c("appl", "date")),
        Dates.checked = list(c("dates_checked"), c("date", "meas")),
        Pest.common.name = list(c("pest\\.", "name"), c("Pest\\.pathogn\\.name"),
            c("(?<!.)pest(?!.)")),
        Pest.units = list(c("pest\\.", "unit"), c("measured\\.endpoint\\.units")),
        Life.stage.tested = c("life", "stage"),
        Application.notes = c("app", "note"),
        Source.Fig.or.Table.number = list(c("source", "fig", "table"),
            c("data", "source"), c("fig", "table")),
        PDF.file.name = list(c("pdf", "file", "name"), c("pdf")),
        Field.or.lab = c("field", "lab"),
        Adjuvant.Surfactant.rate = list(c("adj", "surf", "rate"),
            c("a\\.", "s\\.", "rate"), c("adj", "appl", "rate")),
        StatTest = c("stat", "test"),
        Adjuvant.Surfactant = list(c("adj", "surfactant(?!\\.)"),
            c("aju"), c("surf", "adjuvant\\.\\.if"), c("adjuvant(?!.)")),
        VarType = list(c("vartype"),
            c("Variance\\.type\\.\\.StDev\\.\\.SEM\\.\\.etc\\.\\."),
            c("vartiance", "ype"), c("variance", "type")),
        Measurement.notes = list(c("Measured\\.endpoint\\.notes"),
            c("notes", "methodology", "meas")),
        Treatment.Var = list(c("(?<!.)var(?!.)"), c("t\\.var"),
            c("Pesticide\\.treatment\\.variance"), c("t\\.variance"), c("treat", "var")),
        Treatment.Value = list(c("treat(?!.)"), c("meas", "results"),
            c("Pesticide\\.treatment\\.value"), c("treatment(?!\\.)")),
        Endpoint.Dates = c("endpt", "date"),
        Treatment.StGr = list(c("(?<!\\.)stgr(?!.)"),
            c("Pesticide\\.treatment\\.StGr\\.letter"),
            c("t\\.", "stat", "group"), c("t\\.stgr"), c("treat", "stgr")),
        Plot.size = c("plot", "size"),
        Year = list(c("study", "year"), c("year(?!\\.)")),
        Pesticide.commercial.name = c("Product(?!.)"),
        UTC.Var = list(c("u\\.var"),
            c("UTC\\.variance\\.\\.st\\.dev\\.\\.SEM\\.\\.when\\.given\\."),
            c("u\\.variance")),
        UTC.Value = list(c("Untreated\\.Control\\.Value"),
            c("untreated", "control(?!\\.)"), c("utc(?!\\.)")),
        UTC.StGr = list(c("UTC\\.StGr\\.letter"),
            c("u", "stat", "group", "let"), c("u\\.stgr")),
        AI = c("active", "ingredient"),
        PercentAI = c("(?<!.)ai\\.\\."),
        TotalMassAI = c("total", "mass", "ai")
        )

    for (correction_name in names(corrections)) {
        correction <- corrections[[correction_name]]
        ## some corrections are lists, others are
        ## individual vectors.
        if(is.list(correction)){
            colnum <- na.omit(unlist(llply(correction, function(x){
                hf$mgrep(x, colnames(df), ignore.case = TRUE,
                         strict = length(x) - 1, perl = TRUE)
            })))
            colnames(df)[colnum] <- correction_name
        } else {
            colnum <- hf$mgrep(correction, colnames(df), ignore.case = TRUE,
                               strict = length(correction) - 1, perl = TRUE)
            colnames(df)[colnum] <- correction_name
        }
    }
    df
}

############################################################
############################################################
