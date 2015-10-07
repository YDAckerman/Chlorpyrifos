############################################################
## Workflow to bring data from an excel file with multiple tables to R
## given a line in the CPYR dataset, I want to be able to locate and
## extract the raw insect data that corresponds to it into a df.
############################################################

library(openxlsx)
library(dplyr)
library(plyr)
library(digest)

source("~/Documents/ZhangLab/R/Chlorpyrifos/export_helper_funs.R")

## there are additional data files that need to be added
dat <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Data-allCpyr-withAI.csv",
                stringsAsFactors = FALSE, na.strings = c(""))

dat1 <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-May2015-Mike.csv",
                 stringsAsFactors = FALSE)

dat2 <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-Oct2014-Jess.csv",
                stringsAsFactors = FALSE)

input_files <- c("~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-Oct2014-Raw.csv",   
                "~/Dropbox/ZhangLabData/ExcelData/Data-FromSunny-23Sep-Raw.csv",
                 "~/Dropbox/ZhangLabData/ExcelData/Data-Katy-Cpyr-All-Raw.csv",
                 "~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-May2015-MikeRaw.csv")

## get all pdf names from the data set
pdfs <- as.list(unique(unlist(llply(list(dat, dat1, dat2), function(d){
    d$PDF.file.name
}))))

names(pdfs) <- unlist(pdfs)

## read lines from each file
files <- llply(input_files, readLines)
names(files) <- input_files

pdfs_in_file_not_in_dataset <- setdiff(unlist(llply(files, function(file){
    tmp <- setdiff(unlist(llply(file, function(line){
        unlist(strsplit(line, ";"))[1]
    })), "")
    tmp <- tmp[!hf$mgrepl(c("table", "skip"), tmp, ignore.case = TRUE)]
    tmp <- tmp[!(tmp %in% as.character(1:21))]
})), pdfs)

convPDFS <- ldply(pdfs_in_file_not_in_dataset, function(x){
    file <- names(files)[unlist(llply(files, function(f) {grepl(x,paste(f, collapse = " "))}))]
    data.frame(o = x,
               n = unlist(pdfs)[which.min(stringdist(x, pdfs))],
               file = file,
               stringsAsFactors = FALSE)
})

## see which files contain info for which pdfs
pdf_locs_in_files <- llply(files, function(file){
    llply(pdfs, function(pdf){
        pdf == "" && return(NA)
        if(pdf %in% convPDFS$n){
            pdf <- convPDFS$o[convPDFS$n == pdf]
        }
        i <- grep(gsub("[^[:alnum:]]", "", pdf), gsub("[^[:alnum:]]", "", file))
        if (length(i) > 1){ stop(paste("pdf appears too many times", pdf, sep = ": "))} else {i}
    }) })


## dat <- dat %>%
##     dplyr::filter( Entered.by == "Jerry") %>%
## dplyr::select( PDF.file.name,
##               Source..Fig.or.Table.number.,
##               Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf..,
##               Life.stage.tested..if.stated..egg..larva..pupa..adult.,
##               Locality,
##               Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.)

## pdfs <- unique(dat_proxy$PDF.file.name)
## pdfs <- as.list(pdfs)
## names(pdfs) <- unique(dat_proxy$PDF.file.name)

## these are the files in which we can find pdfs:
pdfs_to_file <- llply(pdfs, function(pdf){
    file <- files[unlist(llply(pdf_locs_in_files, function(x){
        length(x[[pdf]]) != 0
    }))]
    if (length(names(file)) == 0) { NA } else { names(file) }
})

## these are the pdfs that aren't in any file, and pdfs listed in files, but not
## found in the "non-raw" datasets:
pdfs_in_dataset_not_in_file <- names(pdfs_to_file[which(is.na(pdfs_to_file))])


## dat <- unique(dat)

## dat_proxy <- filter(dat_proxy, PDF.file.name == "AMT32-D04")
## dat_proxy <- dplyr::sample_n(dat_proxy, 30)

group_cols <- c("PDF.file.name",
                "Source..Fig.or.Table.number.",
                "Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf..",
                "Life.stage.tested..if.stated..egg..larva..pupa..adult.",
                "Locality",
                "Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.")

## test getResponseTables on subset
f <- ehf$getResponseTables
response_tables <- new.env()

response_tables_lookup <- ldply(list(dat, dat1, dat2), function(d) {
    ddply(d, group_cols,
          function(x){
              row <- unique(x[, group_cols])
              val <- try(f(row), silent = FALSE)
              ret <- NA
              if (identical(class(val), "try-error")) {
                  ret <- NA
              } else {
                  hash <- digest(row)
                  response_tables[[hash]] <- val
                  ret <- hash
              }
              ret
          },
          .inform = FALSE,
          .progress = "text")})

## clean up the mess
rm(dat, dat1, dat2, input_files, pdf_locs_in_files, pdfs, files, pdfs_in_dataset_not_in_file,
   pdfs_in_file_not_in_dataset, pdfs_to_file, group_cols, ehf, f)

save(response_tables, response_tables_lookup, file = "~/Dropbox/ZhangLabData/response_data.rda")
############################################################
############################################################
