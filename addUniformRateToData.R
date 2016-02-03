############################################################
## work flow to concetenate all of the cpyr main files
## author: Yoni Ackerman
## contact: jdackerma@ucdavis.edu
############################################################

## import libraries
library(dplyr)
library(plyr)
library(stringdist)
library(chron)
library(data.table)

## source massage functions and helper functions environments
source("~/Documents/Coding/R/R_convenience/helper_functions.R")
source("~/Documents/ZhangLab/R/Chlorpyrifos/massage_functions.R")

## these are all the cpyr main files:
files = list(
    "~/Dropbox/ZhangLabData/ExcelData/Data-allCpyr-withAI.csv",
    "~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-May2015-Mike.csv",
    "~/Dropbox/ZhangLabData/ExcelData/Data-Cpyr-Oct2014-Jess.csv",
    "~/Dropbox/ZhangLabData/ExcelData/Data-TwoMoreCPYR.csv"
    )

## read in the pur-cpyr product data files (there are multiples
## because they were created as the datasets themselves arose, hence:
## TODO: concatenate into one efficient product file)

## original
prod_data_cpyr <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Products-CPYR.csv",
                           stringsAsFactors = FALSE, na.strings = c("", "?????", "not known",
                                                         "not specified", "product name ambiguous", "unknown")) %>%
    dplyr::rename(DPR.Label.Database.name = DPR.Name,
                  SpecGravOrDensity = Density,
                  AI.1 = AI1,
                  AI.2 = AI2,
                  AI.3 = AI3,
                  chem_prod_notes = Notes,
                  ChemicalType = Type,
                  DPR.Prodno = ProdNo
                  )

## non-cpyr, just in case it has anything the original couldn't find
prod_data <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Products-nonCPYR.csv",
                      stringsAsFactors = FALSE, na.strings = c("", "?????", "not known",
                                                    "not specified", "product name ambiguous", "unknown"))  %>%
    dplyr::rename(AI.2 = AI.number.2,
                  chem_prod_notes = Notes,
                  AI.1 = AI1,
                  SpecGravOrDensity = Sp.Grav..liq.,
                  Pesticide.commercial.name = Product.name..from.article.)

## the two additional tables added at the end
prod_data_two_more <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Products-TwoMoreCPYR.csv",
                      stringsAsFactors = FALSE, na.strings = c("", "?????", "not known",
                                                    "not specified", "product name ambiguous", "unknown"))  %>%
    dplyr::rename(DPR.Label.Database.name = DPR.Name,
                  SpecGravOrDensity = Density,
                  AI.1 = AI1,
                  AI.2 = AI2,
                  AI.3 = AI3,
                  chem_prod_notes = Notes,
                  DPR.Prodno = Prodno,
                  Pesticide.commercial.name = Product.name.from.article
                  )

## look at that - easy! now they're one dataframe
prod_data <- rbindlist(list(prod_data, prod_data_cpyr, prod_data_two_more), use.names = TRUE, fill = TRUE)

## import all the main files into a list
dat <- llply(files, function(f){ read.csv(f, stringsAsFactors = FALSE,
                                          na.strings = c("", "not specified", "???", "uncertain",
                                              "unknown", "unknown formulation", "unclear",
                                              "not specified", "not stated",
                                              "seed treatment date not specified",
                                              "Not specified", "Not Spec", "not known",
                                              "[could not find MSDS only label]")) }, .progress = "text")

## for each dataframe, loop through and make each correction
tmp <- llply(dat, mf$correctDfCols)

## bind the dataframes into one
dat <- as.data.frame(rbindlist(tmp, fill = TRUE, use.names = TRUE))


####### Corrections and cleaning:

i <- grepl("oz/100 gal Product/Acre", dat$Application.rate.units)
dat$Application.rate.units[i] <- "oz product/100 gal"
## Adjuvant units had no errors of this type

## Extra commas at end of either “application” or “measurement” date list
for (column in c("Treatment.dates", "Dates.checked")) {
    dat[, column] <- gsub(",(?!.)", "", dat[, column], perl = TRUE)
}

## remove/replace:
## trailing whitespace and newline chars from product names
## trailing commas
## mispellings
dat$Pesticide.commercial.name <- hf$trim(dat$Pesticide.commercial.name)
dat$Pesticide.commercial.name <- gsub("\\\n", "", dat$Pesticide.commercial.name)
dat$Pesticide.commercial.name <- gsub(",(?!.)", "", dat$Pesticide.commercial.name, perl =  TRUE)
dat$Pesticide.commercial.name <- gsub("Dispress", "Disperss", dat$Pesticide.commercial.name)
dat$Pesticide.commercial.name <- gsub("xx", "x", dat$Pesticide.commercial.name)
dat$Pesticide.commercial.name <- gsub("Imidian", "Imidan", dat$Pesticide.commercial.name)
dat$Pesticide.commercial.name <- gsub("Weather Stick", "weather stik", dat$Pesticide.commercial.name)
dat$Pesticide.commercial.name <- gsub("\\’", "\\'", dat$Pesticide.commercial.name)
dat$Pesticide.commercial.name <- gsub("\\\"", "", dat$Pesticide.commercial.name)
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Check" |
                                  dat$Pesticide.commercial.name == "Water control" |
                                  dat$Pesticide.commercial.name == "Water only")] <- NA
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "none")] <- NA

## remove dupicate rownumber column
dat <- dat %>% dplyr::select(-X.1)

## Add AI column and change AI = experimental where Pesticide is any variation of exp
dat$AI[grep("^exp", dat$Pesticide.commercial.name, ignore.case = TRUE, perl = TRUE)] <- "experimental"

####### End corrections and Cleaning

## now we want to merge the pur product information into the dataframes.
## todo this, we'll create a temporary dataframe with a 'smushed' variable.
## Any product names that differ only by spaces/letter cases will then be
## the same. This works pretty well, unless there is an egregious error
## in two product strings that should be the same

prods <- as.character(unique(dat$Pesticide.commercial.name))
prods_df <- data.frame(Pesticide.commercial.name = prods,
                       smushed = tolower(gsub(" ","", gsub("-","", prods))),
                       stringsAsFactors = FALSE) %>%
    filter(smushed != "")

## use prods_df to select one variation for each unique 'smushed' value,
## choosing by length of the variation
fixed_prod_names <- prods_df %>%
    mutate(len = nchar(Pesticide.commercial.name)) %>%
    dplyr::group_by(smushed) %>%
    dplyr::arrange(len) %>%
    dplyr::top_n(1) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(standardized_product_name = Pesticide.commercial.name)

## fold prods_df back on itself to get a new dataframe (of the same name)
## that contains each product and it's standardized name (the 'new' variable)
prods_df <- left_join(prods_df, fixed_prod_names, by = "smushed") %>%
    dplyr::select(Pesticide.commercial.name, standardized_product_name)

## join prods_df into d
tmp <- left_join(dat, prods_df, by = "Pesticide.commercial.name")

## change Pesticide.commercial.name to it's standardized equivalent
dat <- tmp %>%
    dplyr::rename(Dirty.Old.Pesticide.name = Pesticide.commercial.name,
           Pesticide.commercial.name = standardized_product_name
           )


####### MORE CLEANING AND EDITING - these are the egregious errors mentioned above

## individual text fixes. 
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "A Pristine 38WDG")] <- "Pristine 38WDG"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "B5028 20.0 oz")] <- "B5028"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Bloomtime Bio 10^7")] <- "Bloomtime Bio"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Bloomtime Bio 10^8")] <- "Bloomtime Bio"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Beseige 1.25ZC")] <- "Besiege 1.25ZC"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Beseige")] <- "Besiege"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Captan 50 WP (2x)")] <- "Captan 50 WP"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Carpovirusine 1x10^13 AS")] <- "Carpovirusine AS"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Carpo-virusine 1×10^13 AS")] <- "Carpovirusine AS"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Carzol 90SP2")] <- "Carzol 90SP"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Carzol 90SP2")] <- "Carzol 90SP"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Carzol 90SP2")] <- "Carzol 90SP"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Cobalt 2.54EC")] <- "Cobalt 2.55EC"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Phyton-016-B")] <- "Phyton 016-B"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Cyd-X 3.3×10^13 AS")] <- "Cyd-X"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "F-0570 0.8EW")] <- "F-0570 0.8EC"

i <- which(dat$Pesticide.commercial.name == "Eco 4000 0.05% (v/v)")
dat$Pesticide.commercial.name[i] <- "Eco 4000"
dat$Application.rate[i] <- "0.05% (v/v)"
i <- which(dat$Pesticide.commercial.name == "Eco 4000 0.075% (v/v)")
dat$Pesticide.commercial.name[i] <- "Eco 4000"
dat$Application.rate[i] <- "0.075% (v/v)"

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Endigo ZCX")] <- "Endigo ZC"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "f/b Success")] <- "Success"

i <- which(dat$Pesticide.commercial.name == "Harpin 24-hr feeding time")
dat$Pesticide.commercial.name[i] <- "Harpin"
dat$Application.rate.units[i] <- "24-hr feeding time"
i <- which(dat$Pesticide.commercial.name == "Harpin 48-hr feeding time")
dat$Pesticide.commercial.name[i] <- "Harpin"
dat$Application.rate.units[i] <- "48-hr feeding time"

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Isopropyl alcohol.")] <- "Isopropyl alcohol"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Kaoil")] <- "Kaolin"

i <- which(dat$Pesticide.commercial.name == "Kaoil 24-hr feeding time")
dat$Pesticide.commercial.name[i] <- "Kaolin"
dat$Application.rate.units[i] <- "24-hr feeding time"
i <- which(dat$Pesticide.commercial.name == "Kaolin 48 h feeding time")
dat$Pesticide.commercial.name[i] <- "Kaolin"
dat$Application.rate.units[i] <- "48-hr feeding time"

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Maneb 75DF 2.0")] <- "Maneb 75DF"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Maneb 75DF 1.5")] <- "Maneb 75DF"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "MBI-203")] <- "MBI 203"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "MBI-206")] <- "MBI 206"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Merit 75WP 0.20")] <- "Merit 75WP"

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Micro 108 (1exp7 cfu/ml)")] <- "Micro 108"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "MSMA (?) (18.0%)")] <- "MSMA (18.0%)"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "MSMA (?) (9.81%)")] <- "MSMA (9.81%)"
                            
i <- which(dat$Pesticide.commercial.name == "Pristine 12.5 oz")
dat$Pesticide.commercial.name[i] <- "Pristine"
dat$Application.rate[i] <- "12.5 oz"

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Proclaim + Dyne-amic")] <- "Proclaim"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Purespray Green oi")] <- "Purespray Green"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "PureSpray Green oil")] <- "Purespray Green"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Prev-Am")] <- "Prev AM"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "QRD 146 (3x)")] <- "QRD 146"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "QRD-146 (3x)")] <- "QRD 146"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Rovral (2x)")] <- "Rovral"

i <- which(dat$Pesticide.commercial.name == "Saf-T-Side 1% V/V")
dat$Pesticide.commercial.name[i] <- "Saf-T-Side"
dat$Application.rate[i] <- "1% V/V"

i <- which(dat$Pesticide.commercial.name == "Silwet L-77 0.023% v/v")
dat$Pesticide.commercial.name[i] <- "Silwet L-77"
dat$Application.rate[i] <- "0.023% v/v"

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Sesamin EC 9.34")] <- "Sesamin EC"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Silwet L-77 control")] <- "Silwet L-77"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Sonalan HFX")] <- "Sonalan HFP"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Sonata ASO at")] <- "Sonata ASO"

i <- which(dat$Pesticide.commercial.name == "Sporan 38% 96 fl oz")
dat$Pesticide.commercial.name[i] <- "Sporan 38% EC"
dat$Application.rate[i] <- "96 fl oz"

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Spotlight (fgluroxypyr 26.2%)")] <- "Spotlight (fluroxypyr 26.2%)"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Stylet-oil (before bloom)")] <- "Stylet-oil"

i <- which(dat$Pesticide.commercial.name == "Stylet Oil 1% V/V")
dat$Pesticide.commercial.name[i] <- "Stylet Oil"
dat$Application.rate[i] <- "1% V/V"

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Topgurad")] <- "Topguard"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Tourney G 3")] <- "Tourney G"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Topsin M 70W P")] <- "Topsin M 70WP"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Topsin-M")] <- "Topsin M"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "26 GT 2SC")] <- "26GT 2SC"

i <- which(dat$Pesticide.commercial.name == "Trilogy 1% V/V")
dat$Pesticide.commercial.name[i] <- "Trilogy"
dat$Application.rate[i] <- "1% V/V"

i <- which(dat$Pesticide.commercial.name == "LI 6365, 10 fl oz")
dat$Pesticide.commercial.name[i] <- "LI 6365"
dat$Application.rate[i] <- "10 fl oz"

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "TrisStar 70WSP")] <- "TriStar 70WSP"

i <- which(dat$Pesticide.commercial.name == "Valero 1% V/V")
dat$Pesticide.commercial.name[i] <- "Valero"
dat$Application.rate[i] <- "1% V/V"

i <- which(dat$Pesticide.commercial.name == "Volck Supreme Oil 97.95% 1% V/V")
dat$Pesticide.commercial.name[i] <- "Volck Supreme Oil 97.95%"
dat$Application.rate[i] <- "1% V/V"

i <- which(dat$Pesticide.commercial.name == "Volck Supreme Oil 1% V/V")
dat$Pesticide.commercial.name[i] <- "Volck Supreme Oil"
dat$Application.rate[i] <- "1% V/V"

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Window 2F")] <- "Widow 2F"
dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "Ziram 75DF")] <- "Ziram 76DF"

prod_data$Pesticide.commercial.name <- tolower(prod_data$Pesticide.commercial.name)
dat$Pesticide.commercial.name <- tolower(dat$Pesticide.commercial.name)

dat$Pesticide.commercial.name[which(dat$Pesticide.commercial.name == "movento 2sc")] <- "movento 2 sc"

####### END CLEANING AND EDITING

## ## perform a quality check, the ers data frame will contain any
## ## products in the main data sheet that did not find their twin in
## ## the pur-product dataframe:
## bools <- ldply(unique(dat$Pesticide.commercial.name), function(x){
##     data.frame(name = x, bool = any(x == prod_data$Pesticide.commercial.name),
##                num = sum(x == prod_data$Pesticide.commercial.name), stringsAsFactors = FALSE)
## })

## ers <- bools %>% filter(!bool | num > 1)

## now that we're comfortable with all the corrections,
## merge the product data into the main cpyr dataframe
dat <- left_join(dat, prod_data, by = "Pesticide.commercial.name")

####### MORE CLEANING AND EDITING
dat$Treatment.dates[which(dat$Treatment.dates == "Two unspecified dates (so this cell needs one comma: ,) among 4 stated treatment dates: 20 Jun 2013/8 Jul 2013/31 Jul 2013/23 Aug 2013")] <- "unspec, unspec, 20 Jun 2013, 8 Jul 2013, 31 Jul 2013, 23 Aug 2013"
dat$Treatment.dates[which(dat$Treatment.dates == "8, 25 Sep, 2, 9, Oct 2009")] <- "8, 25 Sep, 2, 9 Oct 2009"
dat$Treatment.dates[which(dat$Treatment.dates == "8, 21, 23, 25, 28, 30 Sep, 2, 5, 7, 9, 12 Oct 2009")] <-
    "8, 21, 23, 25, 28, 30 Sep, 2, 5, 7, 9, 12 Oct 2009"
dat$Treatment.dates[which(dat$Treatment.dates == "Article only mentions only that various treatments were done in \"pre-bloom\" \"during bloom\" and/or \"post-bloom\" times.")] <- NA
####### END CLEANING AND EDITING


## deduce the number of applications applied from the application
## dates give in dat.
Application.counts <-  unlist(llply(dat$Treatment.dates, function(val){
    if(is.na(val)){return(NA)}
    if(grepl("2\\*", val)) {return(2)}
    if(grepl("and", val)){
        return(length(unlist(strsplit(val, "and"))))
    } else {
        return(length(unlist(strsplit(val, ","))))
    }
}))

## fill in using the deduced application counts where the
## application counts variable in dat is empty
dat$Application.Counts.tmp <- Application.counts
dat <- dat %>%
    dplyr::mutate(Application.Counts = ifelse(!is.na(Application.Counts), Application.Counts, Application.Counts.tmp)) %>%
    dplyr::select(-Application.Counts.tmp)

## rename SpecGravOrDensity to Density 
dat <- dat %>%
    dplyr::mutate(Density = ifelse(!is.na(Density), as.character(Density), as.character(SpecGravOrDensity))) %>%
    dplyr::select(-SpecGravOrDensity)

## Convert the application rate and its units to their uniform counterparts
tmp <- mdply(dat %>% select(Application.rate, Application.rate.units), 
             mf$convertToUniformRate, .expand = FALSE, .inform = TRUE) %>%
    dplyr::select(Uniform.application.rate,  Uniform.application.rate.units)

## bind the uniform rate/units back with dat into a new
## data frame (since I use dat so often)
full_dat <- cbind(dat %>%
                  dplyr::select(-Uniform.application.rate,
                         -Uniform.application.rate.units), tmp)

## clean up
rm(list = setdiff(ls(), "full_dat"))

## and save
## save(full_dat, file = "~/Dropbox/ZhangLabData/full_data_set.rda")
