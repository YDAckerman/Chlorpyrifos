############################################################
## Workflow to bring data from an excel file with multiple tables to R
## given a line in the CPYR dataset, I want to be able to locate and
## extract the raw insect data that corresponds to it into a df.
## author: Yoni Ackerman
## contact: jdackerma@ucdavis.edu
############################################################

library(openxlsx)
library(plyr); library(dplyr)
library(digest)
library(stringdist)

source("export_helper_funs.R")

## Read in the following datasets, renaming columns as necessary
dat <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Data-allCpyr-withAI.csv",
                stringsAsFactors = FALSE, na.strings = c("", "???"))

dat1 <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-May2015-Mike.csv",
                 stringsAsFactors = FALSE, na.strings = c("", "???"))

dat2 <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-Oct2014-Jess.csv",
                stringsAsFactors = FALSE, na.strings = c("", "???"))

dat3 <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Data-TwoMoreCPYR.csv",
                 stringsAsFactors = FALSE, na.strings = c("", "???")) %>%
    dplyr::rename(
        Source..Fig.or.Table.number. = Data.source,
        Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf.. = Measured.endpoint.or.pest.units,
        Life.stage.tested..if.stated..egg..larva..pupa..adult. = Life.stage..if.applicable.,
        Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article. = Pest.pathogn.name,
        Year = Study.year
        )

## list of the 'raw' data files
input_files <- c("~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-Oct2014-Raw.csv",   
                "~/Dropbox/ZhangLabData/ExcelData/Data-FromSunny-23Sep-Raw.csv",
                 "~/Dropbox/ZhangLabData/ExcelData/Data-Katy-Cpyr-All-Raw.csv",
                 "~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-May2015-MikeRaw.csv",
                 "~/Dropbox/ZhangLabData/ExcelData/Data-TwoMoreCPYR-raw.csv"
                 )

## get all pdf names from the data set
pdfs <- as.list(unique(unlist(llply(list(dat, dat1, dat2, dat3), function(d){
    d$PDF.file.name
}))))

names(pdfs) <- unlist(pdfs)

## read in the lines from each file
files <- llply(input_files, readLines)
names(files) <- input_files

## find any pdfs in the raw file that are not represented
## in any rows of the dataset.
pdfs_in_file_not_in_dataset <- setdiff(
    unlist(
        llply(files, function(file){
            tmp <- setdiff(unlist(llply(file, function(line){
                unlist(strsplit(line, ";"))[1]
            })), "")
            tmp <- tmp[!hf$mgrepl(c("table", "skip"), tmp, ignore.case = TRUE)]
            tmp <- tmp[!(tmp %in% as.character(1:21))]
        })), pdfs)

## build a dataframe that suggests closes matches between pdf names
## in the 'main' datasets and the 'raw' data sets.
convPDFS <- ldply(pdfs_in_file_not_in_dataset, function(x){
    file <- names(files)[unlist(llply(files, function(f) {grepl(x,paste(f, collapse = " "))}))]
    data.frame(o = x,
               n = unlist(pdfs)[which.min(stringdist(x, names(pdfs)))],
               file = file,
               stringsAsFactors = FALSE)
}, .inform = TRUE)

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


## the following are the column names (common to each dataset) that
## we'll need to accurately extract the data from the raw file:
group_cols <- c("PDF.file.name",
                "Source..Fig.or.Table.number.",
                "Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf..",
                "Life.stage.tested..if.stated..egg..larva..pupa..adult.",
                "Locality",
                "Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.",
                "Year",
                "Number.of.replicates")

## abbreviate the main function as f
f <- ehf$getResponseTables

## create an environment to contain all of the tables we extract
## from the 'raw' files
response_tables <- new.env()

## loop through each dataset, and each combination of the above
## group columns in each data set. Use the unique group columns
## from each iteration to extract the relevant table from the raw
## data sheet.
response_tables_lookup <- ldply(list(dat, dat1, dat2, dat3), function(d) {
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

## remove all trash from global environment so that this can
## be run as a script without cluttering up the workspace
rm(list = setdiff(ls(), c("response_tables", "response_tables_lookup")))

## save the extracted data:
## response_tables: a list linking an id string (V1) to a data table
##                  extracted from the raw file.
## response_tables_lookup: a dataframe linking the group columns to the id string
##                         assigned to the table they correspond to.
## save(response_tables, response_tables_lookup, file = "~/Dropbox/ZhangLabData/response_data.rda")
############################################################
############################################################
