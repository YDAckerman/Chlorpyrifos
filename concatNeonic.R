library(plyr)
library(dplyr)
library(reshape2)
library(xlsx)
library(data.table)

source("~/Documents/Coding/R/R_convenience/helper_functions.R")

### Neonic Data

## get all of the file names in one place
files <- list.files("~/Dropbox/ZhangLabData/ExcelData/NeonicDataFiles", full.names = TRUE)
files <- files[!grepl("raw", files, ignore.case = TRUE)]

## use them to import the data
dfs <- llply(files, function(f){ read.xlsx(f, 1) }, .progress = "text")

## remove NA columns
dfs <- llply(dfs, function(df){
    df[, !grepl("NA\\.", colnames(df))]
})

## remove repeat columns
dfs <- llply(dfs, function(df){
    df[, !grepl("[1-9](?!.)", colnames(df), perl = TRUE)]
})

## alter REALLY problematic columns by hand:
colnames(dfs[[1]])[14] <- "Application.volumn.units"
colnames(dfs[[5]])[18] <- "Application.rate.units"

## unlist(llply(dfs, function(df){length(colnames(df))}))
## number of columns:
##  1  2  3  4  5  6  7  8  9 10
## 35 37 35 36 37 36 36 33 37 40

## scope out who is most like whom
df_mat <- matrix(rep(0, 100), 10, 10)
combinations <- data.frame(t(combn(1:10, 2)))
colnames(combinations) <- c("x", "y")

m_ply(combinations, function(x, y){
    mat <- get("df_mat", env = globalenv())
    mat[x, y] <- length(setdiff(colnames(dfs[[x]]), colnames(dfs[[y]])))
    mat[y, x] <- length(setdiff(colnames(dfs[[y]]), colnames(dfs[[x]])))
    assign("df_mat", mat, env = globalenv())
})


## difference matrix
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
##  [1,]    0    8   14   18   17   18   18   19   19    17
##  [2,]   10    0   14   13   12   14   12   15   13     9
##  [3,]   14   12    0   22   22   22   22   22   22    20
##  [4,]   19   12   23    0    2    2    1    3    2     8
##  [5,]   19   12   24    3    0    3    2    5    3     8
##  [6,]   19   13   23    2    2    0    2    3    3     9
##  [7,]   19   11   23    1    1    2    0    3    1     7
##  [8,]   17   11   20    0    1    0    0    0    0     7
##  [9,]   21   13   24    3    3    4    2    4    0     9
## [10,]   22   12   25   12   11   13   11   14   12     0

## 4-9 are very similar, 1 and 2 are sort of similar, 10 and 3 are quite
## different from everyone.

## I think the most efficient way to do these all at once / reduce errors /
## make bugs easier to find / reproduce is to make a dict of column names I want corresponding
## to key words to match them on.

## there's probably a nice r package to be had from this problem.
## yep, I got it.

corrections <- list(
    Entered.by = c("enter", "by"),
    Proofed.by = c("proof", "by"),
    Number.of.replicates = list(c("num", "replica"), c("replicate(?!\\.)")),
    Study.design = c("study", "des"),
    Replicate.size = c("size"),
    Application.rate = c("pesticide", "usage"),
    Application.rate.units = list(c("appl", "rate", "units"), c("pesticide", "unit")),
    Application.volume = list(c("total\\.applied\\.amount"), c("appl\\.vol(?!.)"), c("appl\\.volume(?!.)")),
    Application.volume.units = c("appl", "vol", "units"),
    Treatment.medium = list(c("treat", "medium"), c("treat", "media")),
    Treatment.dates = list(c("treat", "dates", "incl"), c("Treatment.dates")),
    Dates.checked = list(c("dates_checked"), c("date", "meas")),
    Pest.common.name = c("pest\\.", "name"),
    Pest.units = c("pest\\.", "unit"),
    Life.stage.tested = c("life", "stage"),
    Application.notes = c("app", "note"),
    Source.Fig.or.Table.number = c("source", "fig", "table"),
    PDF.file.name = c("pdf", "file", "name"),
    Field.or.lab = c("field", "lab"),
    Adjuvant.Surfactant.rate = list(c("adj", "surf", "rate"), c("a\\.", "s\\.", "rate")),
    StatTest = c("stat", "test"),
    Adjuvant.Surfactant = c("adj", "surfactant(?!\\.)"),
    VarType = c("vartype"),
    UTC.Var = c("u\\.var"),
    Treatment.Var = list(c("(?<!\\.)var(?!.)"), c("t\\.var")),
    Treatment.Value = list(c("treat(?!.)"), c("meas", "results")),
    T.StGr = c("(?<!\\.)stgr(?!.)")
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

## make another matrix, this time to show how well our corrections did:
df_mat <- matrix(rep(0, 100), 10, 10)
combinations <- data.frame(t(combn(1:10, 2)))
colnames(combinations) <- c("x", "y")

m_ply(combinations, function(x, y){
    mat <- get("df_mat", env = globalenv())
    mat[x, y] <- length(setdiff(colnames(tmp[[x]]), colnames(tmp[[y]])))
    mat[y, x] <- length(setdiff(colnames(tmp[[y]]), colnames(tmp[[x]])))
    assign("df_mat", mat, env = globalenv())
})

## much better:
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
##  [1,]    0    1    4    1    2    3    1    4    1     1
##  [2,]    3    0    4    1    2    3    1    4    1     0
##  [3,]    4    2    0    2    3    3    2    3    2     2
##  [4,]    2    0    3    0    1    2    0    3    0     0
##  [5,]    2    0    3    0    0    2    0    3    0     0
##  [6,]    2    0    2    0    1    0    0    1    0     0
##  [7,]    2    0    3    0    1    2    0    3    0     0
##  [8,]    2    0    1    0    1    0    0    0    0     0
##  [9,]    3    1    4    1    2    3    1    4    0     1
## [10,]    6    3    7    4    5    6    4    7    4     0

## all of the disagreements above are due to an extra column, etc.

## see any discrepancies?
unique(unlist(llply(tmp, colnames)))

## and in the darkness:
df_neonic <- rbindlist(tmp, fill = TRUE, use.names = TRUE)


### 4Jan2016 data

## pull names of the files
files <- list.files("~/Dropbox/ZhangLabData/ExcelData/4Jan2016", full.names = TRUE)
files <- files[grepl("csv", files, ignore.case = TRUE)]
files <- files[!grepl("raw", files, ignore.case = TRUE)]

## use them to import the data
dfs <- llply(files, function(f){ read.csv(f, 1) }, .progress = "text")
## remove repeat columns
dfs <- llply(dfs, function(df){
    df[, !grepl("[1-9](?!.)", colnames(df), perl = TRUE)]
})


corrections <- list(
    Entered.by = list(c("enter", "by"), c("data", "entry")),
    Proofed.by = c("proof"),
    Number.of.replicates = list(c("num", "replica"), c("replicate(?!\\.)")),
    Study.design = c("study", "des"),
    Replicate.size = c("size"),
    Application.rate = list(c("pesticide", "usage"), c("(?<!.)Appl\\.rate(?!.)")),
    Multiple.product.numbers = c("Multi\\.prod\\.no"),
    Application.rate.units = list(c("appl", "rate", "units"), c("pesticide", "unit")),
    Application.volume = list(c("total\\.applied\\.amount"),
        c("appl\\.vol(?!.)"), c("appl\\.volume(?!.)"),
        c("Application\\.volume\\.\\.just\\.a\\.number\\.")),
    Application.volume.units = c("appl", "vol", "units"),
    Treatment.medium = list(c("treat", "medium"), c("treat", "media")),
    Treatment.dates = list(c("treat", "dates", "incl"), c("Treatment.dates")),
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
    Adjuvant.Surfactant = list(c("adj", "surfactant(?!\\.)"), c("aju")),
    VarType = list(c("vartype"), c("Variance\\.type\\.\\.StDev\\.\\.SEM\\.\\.etc\\.\\.")),
    Measurement.notes = c("Measured\\.endpoint\\.notes"),
    UTC.Var = list(c("u\\.var"), c("UTC\\.variance\\.\\.st\\.dev\\.\\.SEM\\.\\.when\\.given\\.")),
    Treatment.Var = list(c("(?<!.)var(?!.)"), c("t\\.var"), c("Pesticide\\.treatment\\.variance")),
    Treatment.Value = list(c("treat(?!.)"), c("meas", "results"), c("Pesticide\\.treatment\\.value")),
    T.StGr = list(c("(?<!\\.)stgr(?!.)"), c("Pesticide\\.treatment\\.StGr\\.letter")),
    Plot.size = c("plot", "size"),
    Year = c("study", "year"),
    Pesticide.commercial.name = c("Product(?!.)"),
    UTC = c("Untreated\\.Control\\.Value"),
    U.StGr = c("UTC\\.StGr\\.letter")
    )


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

df_4jan15 <- rbindlist(tmp, fill = TRUE, use.names = TRUE)

full_df <- rbindlist(list(df_4jan15, df_neonic), fill = TRUE, use.names = TRUE)

write.csv(full_df, file = "~/Dropbox/ZhangLabData/ExcelData/Neonic4JanCombined.csv")
