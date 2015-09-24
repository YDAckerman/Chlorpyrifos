## a function to add multi-product keys to the data

addMultiProdKeysToData <- function(){

    ##require(xlsx)
    require(dplyr)
    require(plyr)
    require(pryr)
    
    setwd("/Users/Yoni/Documents/ZhangLab")

    ## source("R/insect_dict_helper_funs.R")
    ## source("R/export_helper_funs.R")
    source("R/stack.R")

    ## load data
    load("~/Documents/ZhangLab/full_data_set.rda")

    dat <- full_dat
    
    ## files = list("~/Documents/ZhangLab/ExcelData/Data-allCpyr-withAI.csv",
    ##     "~/Documents/ZhangLab/ExcelData/Data-Cpyr-May2015-Mike.csv",
    ##     "~/Documents/ZhangLab/ExcelData/Data-Cpyr-Oct2014-Jess.csv")
    
    
    ## columns <- c("Entered.by", "Proofed.by", "PDF.file.name", "Locality", "Crop", 
    ##              "Variety", "Study.design..usually..random.complete.block.design..for.field.studies.", 
    ##              "Number.of.replicates", "X.Field..or..lab..study",
    ##              "Treatment.medium..e.g....whole.tree..or..food..sprayed.fruit...", 
    ##              "Source..Fig.or.Table.number.", "Pesticide.commercial.name", 
    ##              "Application.rate", "Application.rate.units", "Multiple.product.numbers", 
    ##              "Year", "Uniform.application.rate", "Uniform.application.rate.units", "Density", "Active.Ingredient..AI.",
    ##              "Application.Counts",
    ##              "Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.",
    ##              "Life.stage.tested..if.stated..egg..larva..pupa..adult.",
    ##              "Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf..", 
    ##              "Notes")
    
    ## dat <- ldply(files, function(file){
    ##     d <- read.csv(file,
    ##                   stringsAsFactors = FALSE,
    ##                   na.strings = c("", "uncertain", "unknown", "unknown formulation", "unclear"))
    ##     d[,columns]
    ## })

    ## get the number of multiple product numbers
    num_mprods <- dat %>%
        dplyr::group_by(PDF.file.name, Source..Fig.or.Table.number.) %>%
            dplyr::select(Multiple.product.numbers) %>%
                dplyr::summarise(
                    nums = length(na.omit(unique(Multiple.product.numbers)))
                    )

    ## create keys randomly (set seed for reproducibility)
    set.seed(0)
    keys <- unique(replicate(sum(num_mprods$nums),
            paste(sample(c(letters,1:26), 12, replace = TRUE), collapse = "")))

    stack <- new_stack()
    stack$set(keys)
    
    ## create df matching pdf-table-mpn to a unique key
    key_table <- ddply(dat, .(PDF.file.name,
                              Source..Fig.or.Table.number.,
                              Multiple.product.numbers), function(tab){
                                  if (is.na(unique(tab$Multiple.product.numbers))){
                                      val <- NA
                                  } else {
                                      val <- stack$pop()
                                  }
                                  data.frame('key' = val)
                              })

    ## merge key_table into dat for the desired result
    merge(dat, key_table, by = c("PDF.file.name",
                              "Source..Fig.or.Table.number.",
                              "Multiple.product.numbers"))
}


## CHECK:


## ## old:
## ## dat <- read.csv("ExcelData/Data-allCpyr-withAI-csv.csv",
## ##                     stringsAsFactors = FALSE,
## ##                     header = TRUE)

## ## new:
## load("~/Documents/ZhangLab/full_data_set.rda")
## dat <- full_dat
## dat2 <- addMultiProdKeysToData()

## dat2 %>%
##     dplyr::filter(PDF.file.name == "AMT-26-D21") %>%
##     dplyr::select(
##         PDF.file.name,
##         Source..Fig.or.Table.number.,
##         Multiple.product.numbers,
##         key)

## dat %>%
##     dplyr::filter(PDF.file.name == "AMT-26-D21") %>%
##     dplyr::select(
##         PDF.file.name,
##         Source..Fig.or.Table.number.,
##         Multiple.product.numbers)

