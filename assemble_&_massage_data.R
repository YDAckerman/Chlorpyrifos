############################################################
##       workflow to assemble and massage data            ##
############################################################

##____________set__working__ directory____________________##

setwd("/Users/Yoni/Documents/ZhangLab")

##_________________set__options___________________________##

options(dplyr.width = Inf)

##__________source:__data__dicts__functions_______________##

## source("R/create_pest_name_dict.R") yields:
load("~/Documents/ZhangLab/pestDictionary.rda")
## Gives: pestDict

## source("R/export_multitable_excel_to.R") yields:
load("~/Documents/ZhangLab/response_data.rda")
## Gives: response_tables_lookup & response_tables

source("R/massage_functions.R")
## Gives: environment mf
source("R/table_operations.R")
## Gives: environment to
source("R/create_unique_multiprod_keys.R")
## Gives: addMultiProdKeysToData

##_______________________workflow_________________________##
############################################################

##__________________Get_______Data________________________##

dat <- addMultiProdKeysToData()

##__________________get all tables________________________##

entered <- response_tables_lookup %>% filter(!is.na(V1))

##__________________make ins_table_________________________##
## llply has been giving me some serious shit.
j <- 0
ins_table <- do.call(rbind, lapply(1:nrow(entered), function(x){
    row <- entered[x, ]
    hash <- row$V1
    pre_ins_table <- response_tables[[hash]]
    tmp <- dat %>%
        dplyr::mutate(Source..Fig.or.Table.number. = hf$trim(Source..Fig.or.Table.number.),
                      PDF.file.name = hf$trim(PDF.file.name)) %>%
        filter(PDF.file.name == row$PDF.file.name &
               Source..Fig.or.Table.number. == row$Source..Fig.or.Table.number.)
    stopifnot(nrow(tmp) != 0)
    col <- "StatTest..Duncan.s..Tukey..etc....incl..signif..level"
    col <- paste0(col, "..and.any.data.transformation..e.g...log.x.1..")
    method <- unique(tmp[, col])
    method <- unique(ifelse(grepl("duncan", method, ignore.case = TRUE), "Duncan", "Fisher"))
    ## do all table operations, then merge
    operations <- c("insectDays", "studyDuration","insectDaysSEM", "EstimateSEM")
    pre_ins_tables <- llply(operations, function(operation){
        to$tableOperation(pre_ins_table, operation, method = method)
    })
    ## what is this doing? - oh Reduce!
    pre_ins_table <- Reduce(function(...) merge(..., all = TRUE), pre_ins_tables)
     ## pull from the merged tables the columns we want
    AScol <- setdiff(grep("adjuv", colnames(pre_ins_table), ignore.case = TRUE),
                     grep("rate", colnames(pre_ins_table), ignore.case = TRUE))
    ASrateCol <- intersect(grep("adjuv", colnames(pre_ins_table), ignore.case = TRUE),
                           grep("rate", colnames(pre_ins_table), ignore.case = TRUE))
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
                   contains("prod", ignore.case = TRUE))
               )
    if(length(AScol) == 1){ slice$AS <- pre_ins_table[, AScol] } else { slice$AS <- NA}
    if(length(ASrateCol) == 1){ slice$ASrate <- pre_ins_table[, ASrateCol] } else { slice$ASrate <- NA }
    j <- get("j", envir = globalenv())
    assign("j", j + 1, envir = globalenv())
    slice$Rate <- as.character(slice$Rate)
    ## attach the columns to the identifying values from row
    ## and return.
    cbind(row, slice, row.names = NULL)
}))

## Rate is included for future joining purposes, so let's convert
## it to numeric:
ins_table$Rate <- gsub("[^0-9\\.\\+]", "", ins_table$Rate)

##___________________standardize trtmnts_____________________##
i <- hf$mgrep(c("untreated", "check", "utc", "control", "cehck"),
              ins_table$Pesticide, ignore.case = TRUE)
ins_table$Pesticide[i] <- "UTC"

##__________________create Effect ratio______________________##
ins_table <-
    ins_table %>%
    group_by(V1) %>%
    do(mf$calcPercent(., "UTC", "Pesticide", "insectDays"))

## ## plyr
## system.time(test1 <- ddply(ins_table, .(V1), function(x){
##     mf$calcPercent(x, "UTC", "Treatment", "insectDays")
## }))
## ## dplyr
## system.time(test2 <-
##             ins_table %>%
##             group_by(V1) %>%
##             do(mf$calcPercent(., "UTC", "Treatment", "insectDays"))
##             )

##__________________standardize insect names_________________##
standardPestNames <- sapply(ins_table$Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article., function(x){
    if (is.na(x)){ return(NA) }
    bools <- sapply(ls(pestDict), function(y){
        x %in% pestDict[[y]]
    })
    ls(pestDict)[bools]
})

ins_table['Pest'] <- standardPestNames


## let's remove all the  UTC lines from ins_table in preparation
## for merging

ins_table_utc <- filter(ins_table, Pesticide == "UTC")
ins_table_treated <- filter(ins_table, Pesticide != "UTC")

##________________get 'official' pesticide name_______________##

ins_table_treated <- mf$cPesticideMatchCol(ins_table_treated)
ins_table_utc["Pesticide.commercial.name"] <- NA

##________________add unique multi prod #s_____________________##

## dat <- addMultiProdKeysToData()

##______________add Uniform rate info to dat___________________##

## update: this is already present in the "newly revised edition of" the data set

## dat <- dat %>%
##     arrange(desc(Original.sequence)) %>%
##     select(-Uniform.application.rate,
##            -Uniform.application.rate.units,
##            -Density,
##            -Application.Counts)

## dat_supplement <- read.csv("ExcelData/Data-allCpyr-withAIForCalc.csv",
##                            stringsAsFactors = FALSE,
##                            na.strings = c("", "uncertain",
##                                "unknown formulation", "unknown", "unclear")) %>%
##     arrange(desc(Original.sequence)) %>%
##     select(Uniform.application.rate,
##            Uniform.application.rate.units,
##            Density,
##            Application.Counts)

## dat <- cbind(dat, dat_supplement)

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

## use this to find the problem children
## test <- sapply(1:nrow(ins_table_treated), function(i){
##     row <- ins_table_treated[i,]
##     if(is.na(row$Rate) | is.na(row$Application.rate)){ return(NA) }
##     a <- sum(as.numeric(unlist(strsplit(row$Rate, "\\+")))) == sum(as.numeric(unlist(strsplit(row$Application.rate, "\\+"))))
##     if(is.na(a)){ return(NA) }
##     b <- row$RateRaw == row$Application.rate
##     if(a | b){ return(TRUE) } else { return(FALSE) }
## })

ins_table_utc[,colnames(tmp)] <- NA

## now change te typos since we're done with matching
ins_table_treated$Pesticide.commercial.name <-
    gsub("Beseige", "Besiege", ins_table_treated$Pesticide.commercial.name)

## check:
## tab_new <-
##     ins_table_treated %>%
##     group_by(PDF.file.name, Source..Fig.or.Table.number., Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf..) %>%
##     dplyr::summarise(
##         mpn = paste(unique(Multiple.product.numbers), collapse = ";"),
##         key = paste(unique(MPNkey), collapse = ";"))
## tab_old <-
##     dat %>%
##     group_by(PDF.file.name, Source..Fig.or.Table.number., Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf..) %>%
##     dplyr::summarise(
##         mpn = paste(unique(Multiple.product.numbers), collapse = ";"),
##         key = paste(unique(key), collapse = ";"))
## tab <- dplyr::left_join(tab_new, tab_old,
##                         by = c("PDF.file.name","Source..Fig.or.Table.number.", "Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf..")) %>%
##     group_by(PDF.file.name, Source..Fig.or.Table.number.) %>%
##     dplyr::mutate(
##         eq1 = setequal(unlist(strsplit(mpn.x, split = ";")),
##             unlist(strsplit(mpn.y, split = ";"))),
##         eq2 = setequal(unlist(strsplit(key.x, split = ";")),
##             unlist(strsplit(key.y, split = ";")))
##         )
## filter(tab, !eq2 | !eq1)


## let's remove the erroneous individuals for now
## TODO, get a better fix for this
i <- grep(";", ins_table_treated$MPNkey)
ins_table_treated <- ins_table_treated[-i, ]

## TODO: The next sections need to change to incorporate the new data

##_______________________add AI & AI%_________________________________##
j <- ""
AI_table <- ldply(unique(ins_table_treated$Pesticide.commercial.name),
                  function(x){
                      j <- get("j", envir = globalenv())
                      assign("j", x, envir = globalenv())
                      tmp <- dat %>% dplyr::filter(Pesticide.commercial.name == x)
                      ais <- hf$trim(tolower(tmp$Active.Ingredient..AI.))
                      aiperc <- tmp$AI..
                      if (all(is.na(ais))) {
                          ais <- unique(ais)
                          aiperc <- unique(aiperc)
                      } else {
                          ais <- unlist(strsplit(unique(na.omit(ais)), "\\+"))
                          aiperc <- unlist(strsplit(unique(na.omit(aiperc)), "\\+"))
                          if(length(aiperc) == 0){
                              aiperc <- rep(NA, length(ais))
                          }
                      }
                      data.frame(
                          Pesticide.commercial.name = x,
                          AIperc = aiperc,
                          AI = ais,
                          stringsAsFactors = FALSE)
                  })

ins_table_treated <- merge(ins_table_treated, AI_table,
                           by = c("Pesticide.commercial.name"))

## give the untreated rows the same cols
ins_table_utc["AI"] <- NA
ins_table_utc["AIperc"] <- NA

##____________standardize AIs________________________##

ins_table_treated["AI"] <- gsub("chlorpyifos",
                                "chlorpyrifos",
                                ins_table_treated$AI)

ins_table_treated <- ins_table_treated %>%
    mutate(AI = hf$trim(AI))

## convert multiple active ingredients into multiple columns
## tmp <- llply(ins_table_treated[, c("AI", "AIperc")],
##             function(col){
##                 t <- ldply(col,
##                       function(x){
##                           if (is.na(x)){
##                               tmp <- NA
##                           } else {
##                               tmp <- hf$trim(unlist(strsplit(unlist(strsplit(x,"\\+")), "~")))
##                           }
##                           if(length(tmp) == 1){
##                               q <- data.frame(X = tmp[1], Y = NA,
##                                               stringsAsFactors = FALSE)
##                           } else {
##                               q <- data.frame(X = tmp[1], Y = tmp[2],
##                                               stringsAsFactors = FALSE)
##                           }
##                           q
##                       })
##                 if("naled" %in% col){
##                     colnames(t) <- c("AI1", "AI2")
##                 } else {
##                     colnames(t) <- c("AIperc1", "AIperc2")
##                     t <- data.frame(llply(t, as.numeric))
##                 }
##                 t
##             })

## tmp <- do.call(cbind, tmp)
## colnames(tmp) <- c("AI1", "AI2", "AIperc1", "AIperc2")

## ins_table_treated <- data.frame(ins_table_treated, tmp, stringsAsFactors = FALSE)

## ## add these cols to the utc set
## ins_table_utc[, colnames(tmp)] <- NA

## bind the two pieces back together
ins_table <- rbind(ins_table_utc, ins_table_treated)

##___________________Calc Uniform AI units__________________##
attr(ins_table, "vars") <- NULL

tmp <- ins_table %>% select(Uniform.application.rate,
                            Uniform.application.rate.units,
                            Density, Application.Counts,
                            AIperc)

tmp <- mdply(tmp, mf$convertRate, .expand = TRUE, .inform = TRUE)

ins_table$AILbsAcre <- tmp$AILbsAcre

##_________________Lab or field?___________________##
dat_tmp <- dat %>% transmute(
    PDF.file.name = PDF.file.name,
    Source..Fig.or.Table.number. = Source..Fig.or.Table.number.,
    Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf.. = Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf..,
    X.Field..or..lab..study = hf$trim(tolower(X.Field..or..lab..study)),
    Crop = tolower(Crop),
    Variety = Variety
    )

ins_table <- left_join(ins_table, unique(dat_tmp),
                       by = c("PDF.file.name",
                           "Source..Fig.or.Table.number.",
                           "Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf.."))

##_________________Remove Repeat Data________________________##

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
               tbl = row$Source..Fig.or.Table.number.,
               repPDFs = paste(unique(slice$PDF.file.name), collapse = ";"),
               reptbls = paste(unique(slice$Source..Fig.or.Table.number.),
                   collapse = ";"),
               repIDs = paste(unique(slice$V1), collapse = ";"),
               repInsectDays = paste(unique(slice$insectDays), collapse = ";"),
               repPesticides = paste(unique(slice$Pesticide), collapse = ";"),
               stringsAsFactors = FALSE)
}, .progress = "text")

## TODO: this is a quick fix, it won't be right if more
## repeated tables are added. So either: precheck tables, or
## make the filtering-out of repeated tables automatic.

## better method:
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

## tmp <- tmp[order(tmp$pdf,tmp$tbl),]
## primary <- tmp[1:11,]

## primary <- ldply(1:nrow(primary), function(i){
##     row <- primary[i,]
##     redun <- unlist(strsplit(row$repIDs, ";"))
##     data.frame(pdf = row$pdf,
##                tbl = row$tbl,
##                ID = row$ID,
##                redundantV1s = redun, stringsAsFactors = FALSE)
## })

ins_table <- ins_table %>% filter(!(V1 %in% redundantV1s))

## ## also check that no tables appear twice because of
## ## errors in code/files
## overEntered <- tmp %>%
##     mutate(bool = pdf == repPDFs) %>%
##     filter(bool) %>%
##     select(pdf, tbl, repPDFs, reptbls, ID, repIDs)

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
ins_table <-
    ins_table %>%
    group_by(V1) %>%
    do(mf$convertSEMbyTransform(.))

##_______________________remove Vars attr___________________________##

attr(ins_table, "vars") <- NULL

##___________________________Weights________________________________##
##                                                                  ##

## The estimated and given SEMs needed to be converted into
## log ratio SEMs ... that is why things  looked so strange.

## add best estimates to ins_table as columns:
weights1 <- dnorm(ins_table$ctrlSlope, 0, sqrt(1))

weights2 <- unlist(alply(ins_table %>% select(trSEM_LB, trSEM_UB, trSEM), 1,
                  function(x){
                      x <- unlist(x)
                      bool <- !is.na(x) & !is.infinite(x)
                      if(!any(bool)){
                          return(NA)
                      } else {
                          return(max(x[bool]))
                      }
                  }))

weights3 <- unlist(alply(ins_table %>% select(trSEM_LB, trSEM_UB, trSEM), 1,
                  function(x){
                      x <- unlist(x)
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

##_______________________add indicated ais__________________________##
i <- tolower(ins_table$Pesticide) %in% tolower(ins_table$AI)
ais <- tolower(ins_table$Pesticide[i])
ins_table$AI[i] <- ais

##__________________add standard use ranges_________________________##
i <- ins_table$AI == "(s)-cypermethrin"
ins_table$AI[i] <- "zeta-cypermethrin"

ranges <- data.frame(
    AI = c("methoxyfenozide","zeta-cypermethrin","beta-cyfluthrin",
        "carbofuran","chlorpyrifos","cyfluthrin","flubendiamide",
        "gamma-cyhalothrin","indoxacarb","lambda-cyhalothrin"),
    minLbs_ai_a = c(.06,.008,.0065, 0.1336668,.5, .013,
        0.06256145,.0075,.045,.0075),
    maxLbs_ai_a = c(.38,.025,.025, 1.069335,4, .05, .156,.24,.11,.48)
    )

##___________________________save___________________________________##
save(ins_table, ranges, response_tables, response_tables_lookup, dat,
    file = "cpyrMAdata.rda")

##___________________________clean up_______________________________##

rm(addMultiProdKeysToData, AI_table, entered, hf, i, ins_table_treated,
   ins_table_utc, mf, new_stack, pestDict, standardPestNames, tmp, to,
   dat_tmp, ins_table, response_tables, response_tables_lookup, dat,
   weights1, weights2, weights3, redundantV1s)

######################################################################
######################################################################
######################################################################

                   AI    mn    mx
1    (s)-cypermethrin 0.011 0.212
2     beta-cyfluthrin 0.012 0.024
3          carbofuran 0.100 1.500
4        chlorpyrifos 0.250 7.842
5          cyfluthrin 0.017 0.060
6       flubendiamide 0.031 0.125
7   gamma-cyhalothrin 0.004 0.100
8          indoxacarb 0.025 0.220
9  lambda-cyhalothrin 0.008 0.263
10    methoxyfenozide 0.106 0.150
11  zeta-cypermethrin 0.019 0.051
