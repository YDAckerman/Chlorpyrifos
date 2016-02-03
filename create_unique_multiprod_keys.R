## a function to add multi-product keys to the data

addMultiProdKeysToData <- function(full_dat){

    ##require(xlsx)
    require(plyr); require(dplyr)
    require(pryr)
    
    setwd("~/Documents/ZhangLab/R/Chlorpyrifos")

    ## source("R/insect_dict_helper_funs.R")
    ## source("R/export_helper_funs.R")
    source("stack.R")

    dat <- full_dat

    ## get the number of multiple product numbers
    num_mprods <- dat %>%
        dplyr::group_by(PDF.file.name, Source.Fig.or.Table.number) %>%
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
                              Source.Fig.or.Table.number,
                              Multiple.product.numbers), function(tab){
                                  if (is.na(unique(tab$Multiple.product.numbers))){
                                      val <- NA
                                  } else {
                                      val <- stack$pop()
                                  }
                                  data.frame('key' = val)
                              })

    ## merge key_table into dat for the desired result
    ## and return it
    merge(dat, key_table, by = c("PDF.file.name",
                              "Source.Fig.or.Table.number",
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
##         Source.Fig.or.Table.number,
##         Multiple.product.numbers,
##         key)

## dat %>%
##     dplyr::filter(PDF.file.name == "AMT-26-D21") %>%
##     dplyr::select(
##         PDF.file.name,
##         Source.Fig.or.Table.number,
##         Multiple.product.numbers)

