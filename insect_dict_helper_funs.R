########################################
## helper functions for insect dict prep
########################################


source("~/Documents/Coding/R/R_convenience/helper_functions.R")

idhf <- new.env()

## check to see if string is a possible key
idhf$isKey <- function(s){
    grepl("^[[:upper:]]+$", s) | grepl("3CAH", s)
}

## pull contents of parentheses from string
idhf$extractParens <- function(s){
    regmatches(s, gregexpr("(?<=\\().*?(?=\\))", s, perl=T))[[1]]
}

## remove all but the common name from the pest
idhf$getCommonName <- function(s){
    s <- unlist(strsplit(s, ":"))[1]
    s <- unlist(strsplit(s, ","))[1]
    s <- unlist(strsplit(s, "\\("))[1]
}

## pull the key from the pest (if it has one)
idhf$getKey <- function(s){
    key <- NA
    pk <- idhf$extractParens(s)
    if (length(pk) > 0){
        kb <- sapply(pk, idhf$isKey)
        if(sum(kb) > 0){
            key <- pk[kb]
        }
    }
    key
}

## find the key most likely to match the current pest
idhf$findAKey <- function(s){

    require(stringdist)
    
    ## if no key is present, lets search off common name:
    common_name <- idhf$getCommonName(s)
    common_name <- gsub("3-", "three", common_name)
    common_name <- tolower(hf$trim(common_name))
    ## exceptions
    if (common_name  %in% c("aphids", "swa", "benificial insects", NA)){
        return(NULL)
    }

    dists <- sapply(pests, function(pest){
        testcn <- tolower(hf$trim(idhf$getCommonName(pest)))
        stringdist(common_name, testcn)
    })

    matches <- pests[dists < 3]
    if (length(matches) == 0){return(NULL)}
    keys <- sapply(matches, idhf$getKey)
    if (sum(!is.na(keys)) == 0){
        return(NULL)
    }
    unique(na.omit(keys))
    
}

########################################
########################################
