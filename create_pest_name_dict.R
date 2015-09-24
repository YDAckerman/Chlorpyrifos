############################################################
##       script to build pest name dictionary             ##
############################################################

## regex help courtesy of stackoverflow:
## http://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r-regex
## http://stackoverflow.com/questions/21992491/checking-if-string-is-in-uppercase-in-r
## should one wish to do a web check 
## url <- "http://www.entsoc.org/common-names?title=XXX&field_sciname_value=&tid=&tid_1=&tid_2=&tid_3=&tid_4="

## Libraries:
library(stringdist)

## wd
setwd("/Users/Yoni/Documents/ZhangLab")

## source functions
source("/Users/Yoni/Documents/ZhangLab/R/insect_dict_helper_funs.R")

##_______________________workflow_________________________##

## pull dataframe into R
dat <- read.csv("ExcelData/Data-allCpyr-withAI.csv",
                stringsAsFactors = FALSE)

dat1 <- read.csv("ExcelData/Data-Cpyr-May2015-Mike.csv",
                 stringsAsFactors = FALSE)

dat2 <- read.csv("ExcelData/Data-Cpyr-Oct2014-Jess.csv",
                stringsAsFactors = FALSE)

## pull individual pests into vec: (what a col name...)
pests <- unique(unlist(llply(list(dat,dat1,dat2), function(d){
    d$Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.
})))

## use environment as dict object:
pestDict <- new.env()

## populate the dict
## (since we'll be assinging things to an external object I'll
## use a for loop for convenience)
for (i in 1:length(pests)){
    pest <- pests[i]
    keys <- idhf$findAKey(pest)
    if (!is.null(keys)){
        key <- keys[1]
    } else {
        print(pest)
        next
    }
    if (exists(key, pestDict)){
        value <- pestDict[[key]]
        value <- c(value, pest)
        pestDict[[key]] <- value
        next
    } else {
        pestDict[[key]] <- pest
        next
    }
}

## exceptions not worth building in:
s <- pestDict[["AW"]][1]                                     ## armyworm
pestDict[["AW"]] <- pestDict[["AW"]][-1]
pestDict[["ARW"]] <- s
pestDict[["CEW"]] <- c(pestDict[["CEW"]], pestDict[["CBW"]]) ## bollworm
rm(CBW, envir = pestDict)
s <- pestDict[["LB"]][1]                                     ## lygus hesp
pestDict[["LB"]] <- pestDict[["LB"]][-1]
pestDict[["WTPB"]] <- s

pestDict[["VMB"]] <- "Vine Mealybug: Planococcus ficus (Signoret)"
pestDict[["CRWP"]] <- "Aphytis melinus DeBach"
pestDict[["SFA"]] <- "Southern fire ant: Solenopsis xyloni McCook"
pestDict[["CPAP"]] <- "Cowpea aphid parasitoids: Lysiphlebis testaceipes (Cresson)"
pestDict[["DCFB"]] <- "Desert corn flea beetle: Chaetocnema ectypa Horn"
pestDict[["BI"]] <- "Benificial insects"
pestDict[["MB"]] <- "Mealybug: Ferrisia gilli Gullan"
pestDict[["CL"]] <- c("Cabbage looper: Trichoplusia ni (Hubner)", "Cabbage looper")
pestDict[["SWA"]] <- "SWA (The species associated with this acronym was not revealed in the study.)"
pestDict[["A"]] <- c("Aphids", "Blue alfalfa aphids + Pea aphids")
pestDict[["CB&TB"]] <- "Cotton bollworm: Helicoverpa zea Boddie and Tobacco budworm: Heliothis virescens (F.)"
pestDict[["PSB"]] <- "Palestriped flea beetle"
pestDict[["SP"]] <- "Spiders"
pestDict[["AL"]] <- "Alfalfa looper"
pestDict[["LH"]] <- c("Empoasca sp. (Empoasca fabae and Empoasca mexara main pests in the region)", pestDict[["LH"]])
pestDict[["APW"]] <- "Aphid parasitic wasp"
pestDict[["LaBe"]] <- "Lady beetle"
pestDict[["LB"]] <- c("Lygus", pestDict[["LB"]])
pestDict[["LFB"]] <- "Leaffooted bug: Leptoglossus phyllopus (Linnaeus)"
pestDict[["CA"]] <- "Cotton aphid: Aphis gossypii"
pestDict[["SLW"]] <- "Silverleaf whitefly: Bemisia tabaci, biotype B (=Bemisia argentifolii)"
pestDict[["GSB"]] <- "Green stink bug: Acrosternum hilare (Say)"
pestDict[["TBW"]] <- "Heliothis virescens; Helicoverpa zea"
pestDict[["G"]] <- "Grasshoppers (species not specified)"
pestDict[["BB"]] <- "Blister beetles (species not specified)"
pestDict[["MSB"]] <- "Meadow spittlebug (species not specified)"
pestDict[["TP"]] <- "Parasitoid: Trioxys pallidus"
pestDict[["SA"]] <- "Hyperparasitoid: Syrphophagus aphidivorus"
pestDict[["CPB"]] <- "Clouded plant bug: Neurocolpus nubilus"

## see that everyone has been seen to:
## tmp <- sapply(pests, function(x){any(sapply(ls(pestDict), function(y){ x %in% pestDict[[y]]}))})
## tmp[!tmp]

rm(s, pests, idhf, pest, i, key, keys, value, dat, dat1, dat2)

save(pestDict, file = "pestDictionary.rda")
########################################
########################################

