library(dplyr)
library(plyr)
library(stringdist)
library(chron)

source("~/Documents/Coding/R/R_convenience/helper_functions.R")
source("~/Documents/ZhangLab/R/Chlorpyrifos/massage_functions.R")

files = list("~/Dropbox/ZhangLabData/ExcelData/Data-allCpyr-withAI.csv",
        "~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-May2015-Mike.csv",
        "~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-Oct2014-Jess.csv")

dat <- llply(files, function(file){
    d <- read.csv(file,
                  stringsAsFactors = FALSE,
                  na.strings = c("", "uncertain", "unknown",
                      "unknown formulation", "unclear",
                      "not specified", "not stated", "seed treatment date not specified"))
})

## check to see if all products in the new data are in the old:
## products_new <- unique(c(dat[[2]]$Pesticide.commercial.name, dat[[3]]$Pesticide.commercial.name))
## products_old <- unique(dat[[1]]$Pesticide.commercial.name)

## guesses <- ldply(products_new, function(x){
##     guess <- grep(x, products_old, ignore.case = TRUE, value = TRUE)
##     is.na(x) && return(data.frame(product = x, guess = NA))
##     if(length(guess) == 0){
##         i <- which.min(stringdist(tolower(x), tolower(products_old)))
##         if(length(i) == 0){
##             d <- data.frame(product = x, guess = NA)
##         } else {
##             d <- data.frame(product = x, guess = products_old[i])
##         }
##     } else {
##         i <- which.min(stringdist(tolower(x), tolower(guess)))
##         d <- data.frame(product = x, guess = guess[i])    
##     }
##     d
## })

## write.xlsx(guesses, file = "comparisons.xlsx")

## ## went through by hand to find comparisons that don't look work:
## guesses <- read.xlsx(file = "~/Documents/ZhangLab/comparisons.xlsx", sheetIndex = 1)

## now go on abound and do further searching... Saved results as prodInfo.rda

load("~/Dropbox/ZhangLabData/prodInfo.rda")

## the number after the product name can be one of two things: part of the name or an indication of
## the amount of active ingredient in the product. To deal with this, in stead of doing string matching
## I should do string intersections.

completed_cases <- prods %>% filter(check == "z")

## incomplete_cases <- prods %>% filter(check != "z")

## guesses <- ldply(1:nrow(incomplete_cases), function(i){
##     row <- incomplete_cases[i,]
##     original <- gsub("Mustang Max", "MustangMax", row$original)
##     prod_reduc <- hf$trim(hf$removeParens(gsub("[%\\.]", "", gsub("\\w*\\d\\w*", "", original))))
##     guesses <- grep(prod_reduc, pur_prod$product_name, ignore.case = TRUE, value = TRUE)
##     if(length(guesses) == 0 | length(guesses) == nrow(pur_prod)){
##         return(data.frame(row$original, best_guess = NA, num_matches = NA, stringsAsFactors = FALSE))
##     }
##     ranks <- unlist(llply(guesses, function(product){
##         length(hf$stringIntersect(row$original, product))
##     }))
##     if(all(is.na(ranks))){
##         return(data.frame(original, best_guess = NA, num_matches = NA, stringsAsFactors = FALSE))
##     }
##     top_rank <- max(ranks, na.rm = TRUE)
##     sum(ranks == top_rank, na.rm = TRUE)
##     guess <- guesses[which.max(ranks)]
##     data.frame(row$original, best_guess = guess,
##                num_matches = sum(ranks == top_rank, na.rm = TRUE),
##                stringsAsFactors = FALSE)
## })

## write.csv(guesses, file = "guesses.csv")

## I had to do a  lot of editing by hand...
incomplete_cases <- read.csv(file = "~/Dropbox/ZhangLabData/prod_guesses.csv",
                             header = TRUE, na.strings = c("NA", "Err:512")) %>%
    select(original, best_guess)

completed_cases <- completed_cases %>%
    mutate(original = ifelse(original == "Err:512", NA, original),
           product_name = ifelse(product_name == "Err:512", NA, product_name)) %>%
    select(original, product_name)

incomplete_cases <- ldply(1:nrow(incomplete_cases), function(i){
    row <-incomplete_cases[i,]
    original <- row$original
    best_guess <- row$best_guess
    if(is.na(original) | is.na(best_guess)){
        return(data.frame(row, density = NA, prodno = NA))
    } 
    guesses <- pur_prod %>%
        filter(product_name == best_guess)
    last_updates <- as.Date(guesses$lastup_dt, format = "%d%m%Y")
    if(all(is.na(last_updates))){
        guesses <- guesses %>%
            distinct(density, prodno) %>%
                select(density, prodno)
        if(nrow(guesses) > 1){
            if(all(is.na(guesses$density))){
                guesses <- guesses %>%
                    select(density, prodno) %>%
                        sample_n(1)
            } else {
                guesses <- guesses %>%
                    filter(!is.na(density)) %>%
                        select(density, prodno) %>%
                            sample_n(1)
                }
        } else if(nrow(guesses) < 1){
            stop(paste(best_guess, " and ", original, " doesn't provide enough information"))
        }
    } else {
        guesses <- guesses[which.max(last_updates),] %>% select(density, prodno)
    }
    ## now output the right info
    ##
    return(data.frame(row, guesses, stringsAsFactors = FALSE))
})

incomplete_cases <- left_join(incomplete_cases, pur_udc, by = "prodno")
incomplete_cases <- left_join(incomplete_cases, pur_chem %>% select(chem_code, chemname), by = "chem_code")

incomplete_cases <- incomplete_cases %>%
    dplyr::select(-prodno, -chem_code) %>%
    dplyr::rename(Density = density,
                  AI.. = prodchem_pct,
                  Active.Ingredient..AI. = chemname,
                  product_name = best_guess)

completed_cases <- ldply(1:nrow(completed_cases), function(i){

    i <- 9
    row <- completed_cases[i,]
    original <- row$original
    product_name <- row$product_name
    
    if(is.na(original) | is.na(product_name)){
        return(data.frame(row, Density = NA, Active.Ingredient..AI. = NA, AI.. = NA))
    }
    
    guesses <- dat[[1]] %>%
        mutate(Pesticide.commercial.name = hf$trim(Pesticide.commercial.name),
               Active.Ingredient..AI. = tolower(Active.Ingredient..AI.)) %>%
                   filter(Pesticide.commercial.name == product_name)
    guesses <- guesses %>%
        dplyr::distinct(Density, Active.Ingredient..AI., AI..) %>%
        dplyr::select(Density,Active.Ingredient..AI.,AI..)
    
    if(nrow(guesses) != 1){
        stop(paste(product_name, " and ", original, " doesn't provide enough information"))
    }
    return(data.frame(row, guesses, stringsAsFactors = FALSE))
}, .inform = TRUE)

## be careful: each active ingredient has its own row in incomplete_cases, whereas in completed_cases
## all active ingredients are in one row (only 9 are like this, though...)
new_data_pesticide_chem <-
    rbind(completed_cases, incomplete_cases) %>%
    dplyr::select(Pesticide.commercial.name = original,
                  Density, Active.Ingredient..AI., AI..)
    

dat[[3]]$Plot.size = NA
dat[[3]]$Subset.within.source..usually.column.or.panel. = NA

dat_new <- rbind(dat[[2]], dat[[3]])
dat <- dat[[1]]

## tmp <- new_data_pesticide_chem %>%
##     group_by(product_name) %>%
##     dplyr::summarise(num_ai = length(unique(Active.Ingredient..AI.)))

## surprise! there are no application counts for this data..........
## [23] "Application.Counts"
## luckily, there are application dates...

Application.counts <-  unlist(llply(dat_new$Treatment.dates..incl..year.in.all.dates., function(val){
    if(is.na(val)){return(NA)}
    if(grepl("2\\*", val)) {return(2)}
    if(grepl("and", val)){
        return(length(unlist(strsplit(val, "and"))))
    } else {
        return(length(unlist(strsplit(val, ","))))
    }
}))

dat_new$Application.Counts <- Application.counts

tmp <- mdply(dat_new %>% select(Application.rate, Application.rate.units), 
             mf$convertToUniformRate, .expand = FALSE, .inform = TRUE) %>%
    dplyr::select(Uniform.application.rate, Uniform.application.rate.units)

dat_new <- cbind(dat_new, tmp)

## dat_new now has application counts and Uniform.application.rate/units. Is it ready for a left_join?
dat_new <- left_join(dat_new, new_data_pesticide_chem, by = "Pesticide.commercial.name")

## make both data sets have the same columns:
l_ply(setdiff(colnames(dat), colnames(dat_new)), function(x){
    dat_new[,x] <<- NA
})
l_ply(setdiff(colnames(dat_new), colnames(dat)), function(x){
    dat[,x] <<- NA
})

## and in the darkness bind them
full_dat <- rbind(dat, dat_new)

save(full_dat, file = "full_data_set.rda")
