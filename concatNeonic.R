library(plyr)
library(dplyr)
library(reshape2)
library(data.table)

source("~/Documents/Coding/R/R_convenience/helper_functions.R")

### Neonic Data

## get all of the file names in one place
files <-  unlist(llply(c("~/Dropbox/ZhangLabData/ExcelData/NeonicDataFiles",
                         "~/Dropbox/ZhangLabData/ExcelData/4Jan2016",
                         "~/Dropbox/ZhangLabData/ExcelData/CpyrData",
                         "~/Dropbox/ZhangLabData/ExcelData/BeanData7Jan15"),
                       function(directory){
                           files <- list.files(directory, full.names = TRUE)
                           files <- files[grepl("csv", files)]
                           files[!grepl("raw", files, ignore.case = TRUE)]
                       }))

## use them to import the data
dfs <- llply(files, function(f){ read.csv(f, stringsAsFactors = FALSE,
                                          na.strings = c("", "not specified", "???", "uncertain",
                                              "unknown", "unknown formulation", "unclear",
                                              "not specified", "not stated",
                                              "seed treatment date not specified")) }, .progress = "text")

## remove NA columns
dfs <- llply(dfs, function(df){
    df[, !grepl("NA\\.", colnames(df))]
})

## remove repeat columns, RowNum, and 'X' columns (update, I think
## I'll keep X cols - I don't know what their deal is)
dfs <- llply(dfs, function(df){
    df <- df[, !grepl("[1-9](?!.)", colnames(df), perl = TRUE)]
    df[, !grepl("RowNum", colnames(df))]
    ## df[, !grepl("X", colnames(df))]
})

## alter REALLY problematic columns by hand:
colnames(dfs[[1]])[14] <- "Application.volume.units"
colnames(dfs[[5]])[18] <- "Application.rate.units"


## I think the most efficient way to do these all at once / reduce errors /
## make bugs easier to find / reproduce is to make a dict of column names I want corresponding
## to key words to match them on.

## there's probably a nice r package to be had from this problem.
## yep, I got it.

corrections <- list(
    Entered.by = list(c("enter", "by"), c("data", "entry")),
    Proofed.by = c("proof"),
    Number.of.replicates = list(c("num", "replica"), c("replicate(?!\\.)")),
    Study.design = c("study", "des"),
    Replicate.size = c("size"),
    Application.rate = list(c("pesticide", "usage"), c("(?<!.)Appl\\.rate(?!.)")),
    Multiple.product.numbers = list(c("Multi\\.prod\\.no"), c("mult", "prod", "number")),
    Application.rate.units = list(c("(?<!.)appl", "rate", "units"), c("pesticide", "unit")),
    Uniform.application.rate = c("uniform", "application", "rate(?!\\.)"),
    Uniform.application.rate.units = c("uniform", "application", "rate", "units"),
    Density = c("density"),
    Application.volume = list(c("total\\.applied\\.amount"), c("appl\\.vol(?!.)"), c("appl\\.volume(?!.)"),
        c("Application\\.volume\\.\\.just\\.a\\.number\\.")),
    Application.volume.units = c("appl", "vol", "units"),
    Treatment.medium = list(c("treat", "medium"), c("treat", "media")),
    Treatment.dates = list(c("treat", "dates", "incl"), c("Treatment\\.dates"), c("Treatment\\.date")),
    Dates.checked = list(c("dates_checked"), c("date", "meas")),
    Pest.common.name = list(c("pest\\.", "name"), c("Pest\\.pathogn\\.name")),
    Pest.units = c("pest\\.", "unit"),
    Life.stage.tested = c("life", "stage"),
    Application.notes = c("app", "note"),
    Source.Fig.or.Table.number = list(c("source", "fig", "table"), c("data", "source")),
    PDF.file.name = c("pdf", "file", "name"),
    Field.or.lab = c("field", "lab"),
    Adjuvant.Surfactant.rate = list(c("adj", "surf", "rate"), c("a\\.", "s\\.", "rate")),
    StatTest = c("stat", "test"),
    Adjuvant.Surfactant = list(c("adj", "surfactant(?!\\.)"), c("aju"), c("surf", "adjuvant\\.\\.if")),
    VarType = list(c("vartype"), c("Variance\\.type\\.\\.StDev\\.\\.SEM\\.\\.etc\\.\\."), c("vartiance", "ype")),
    Measurement.notes = list(c("Measured\\.endpoint\\.notes"), c("notes", "methodology", "meas")),
    Treatment.Var = list(c("(?<!.)var(?!.)"), c("t\\.var"), c("Pesticide\\.treatment\\.variance"), c("t\\.variance")),
    Treatment.Value = list(c("treat(?!.)"), c("meas", "results"), c("Pesticide\\.treatment\\.value"), c("treatment(?!\\.)")),
    Treatment.StGr = list(c("(?<!\\.)stgr(?!.)"), c("Pesticide\\.treatment\\.StGr\\.letter"),
        c("t\\.", "stat", "group"), c("t\\.stgr")),
    Plot.size = c("plot", "size"),
    Year = list(c("study", "year"), c("year(?!\\.)")),
    Pesticide.commercial.name = c("Product(?!.)"),
    UTC.Var = list(c("u\\.var"), c("UTC\\.variance\\.\\.st\\.dev\\.\\.SEM\\.\\.when\\.given\\."), c("u\\.variance")),
    UTC.Value = list(c("Untreated\\.Control\\.Value"), c("untreated", "control(?!\\.)"), c("utc(?!\\.)")),
    UTC.StGr = list(c("UTC\\.StGr\\.letter"), c("u", "stat", "group", "let"), c("u\\.stgr")),
    AI = c("active", "ingredient"),
    PercentAI = c("(?<!.)ai\\.\\."),
    TotalMassAI = c("total", "mass", "ai")
    )


## for each dataframe, loop through and make each correction
tmp <- llply(dfs, function(df){
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
})

## and in the darkness:
df_full <- rbindlist(tmp, fill = TRUE, use.names = TRUE)

write.csv(df_full, file = "~/Dropbox/ZhangLabData/ExcelData/AllDataCombined20Jan.csv")
