## library(RPostgreSQL)
## library(DBI)
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
## library(stringdist)

source("~/Documents/Coding/R/R_convenience/helper_functions.R")

## con <- src_postgres(dbname="california",
##                   host="ziram.lawr.ucdavis.edu",
##                    user="jonathan"
##                    ## ,options="-c search_path=bee"
##                    )

## prod_adj <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.dpr_prod_adjuvant")))
## ai_cats <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.dpr_ai_categories")))
## pur_prod <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.product")))
## dpr_adj <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.dpr_prod_adjuvant")))

## load in the data:
d <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Neonic4JanCombined.csv", stringsAsFactors = FALSE)

## In either pesticide or adjuvant units “oz/100 gal Product/Acre” should be “oz product/100 gal”

bools <- grepl("oz/100 gal Product/Acre", d$Application.rate.units)
d$Application.rate.units[bools] <- "oz product/100 gal"
## Adjuvant units had no errors of this type

## Extra commas at end of either “application” or “measurement” date list
for (column in c("Treatment.dates", "Dates.checked")) {
    d[, column] <- gsub(",(?!.)", "", d[, column], perl = TRUE)
}

## remove/replace:
## trailing whitespace and newline chars from product names
## trailing commas
## mispellings
d$Pesticide.commercial.name <- hf$trim(d$Pesticide.commercial.name)
d$Pesticide.commercial.name <- gsub("\\\n", "", d$Pesticide.commercial.name)
d$Pesticide.commercial.name <- gsub(",(?!.)", "", d$Pesticide.commercial.name, perl =  TRUE)
d$Pesticide.commercial.name <- gsub("Dispress", "Disperss", d$Pesticide.commercial.name)
d$Pesticide.commercial.name <- gsub("xx", "x", d$Pesticide.commercial.name)
d$Pesticide.commercial.name <- gsub("Imidian", "Imidan", d$Pesticide.commercial.name)
d$Pesticide.commercial.name <- gsub("Weather Stick", "weather stik", d$Pesticide.commercial.name)
d$Pesticide.commercial.name <- gsub("\\’", "\\'", d$Pesticide.commercial.name)
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Check" |
                                  d$Pesticide.commercial.name == "Water control" |
                                  d$Pesticide.commercial.name == "Water only")] <- NA
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "none")] <- NA

## remove dupicate rownumber column
d <- d %>% select(-X.1)

## Add AI column and change AI = experimental where Pesticide is any variation of exp
d$ActiveIngredient <- NA
d$ActiveIngredient[grep("^exp", d$Pesticide.commercial.name, ignore.case = TRUE, perl = TRUE)] <- "experimental"


## Figure out which Products are actually adjuvants
## (the bigger problem is that there is no direct connection to purdb, so
## first I'm going to see if I can get a prodno in there...)
prods <- as.character(unique(d$Pesticide.commercial.name))

## create a temporary dataframe with a 'smushed' variable. Any product
## names that differ only by spaces/letter cases will then be the same
prods_df <- data.frame(Pesticide.commercial.name = prods,
                       smushed = tolower(gsub(" ","", gsub("-","", prods))),
                       stringsAsFactors = FALSE) %>%
    filter(smushed != "")

## use prods_df to select one variation for each unique 'smushed' value,
## choosing by length of the variation
fixed_prod_names <- prods_df %>%
    mutate(len = nchar(Pesticide.commercial.name)) %>%
    dplyr::group_by(smushed) %>%
    arrange(len) %>%
    top_n(1) %>%
    slice(1) %>%
    ungroup() %>%
    rename(standardized_product_name = Pesticide.commercial.name)

## fold prods_df back on itself to get a new dataframe (of the same name)
## that contains each product and it's standardized name (the 'new' variable)
prods_df <- left_join(prods_df, fixed_prod_names, by = "smushed") %>%
    select(Pesticide.commercial.name, standardized_product_name)

## join prods_df into d
tmp <- left_join(d, prods_df, by = "Pesticide.commercial.name")

## change Pesticide.commercial.name to it's standardized equivalent
d <- tmp %>%
    select(-Pesticide.commercial.name) %>%
    rename(Pesticide.commercial.name = standardized_product_name)

## individual text fixes. 
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "A Pristine 38WDG")] <- "Pristine 38WDG"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "B5028 20.0 oz")] <- "B5028"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Bloomtime Bio 10^7")] <- "Bloomtime Bio"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Bloomtime Bio 10^8")] <- "Bloomtime Bio"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Captan 50 WP (2x)")] <- "Captan 50 WP"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Carpovirusine 1x10^13 AS")] <- "Carpovirusine AS"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Carzol 90SP2")] <- "Carzol 90SP"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Carzol 90SP2")] <- "Carzol 90SP"
## d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Dimension (dithiopyr 12.7%)")] <- "Dimension"

i <- which(d$Pesticide.commercial.name == "Eco 4000 0.05% (v/v)")
d$Pesticide.commercial.name[i] <- "Eco 4000"
d$Application.rate[i] <- "0.05% (v/v)"
i <- which(d$Pesticide.commercial.name == "Eco 4000 0.075% (v/v)")
d$Pesticide.commercial.name[i] <- "Eco 4000"
d$Application.rate[i] <- "0.075% (v/v)"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Endigo ZCX")] <- "Endigo ZC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "f/b Success")] <- "Success"

i <- which(d$Pesticide.commercial.name == "Harpin 24-hr feeding time")
d$Pesticide.commercial.name[i] <- "Harpin"
d$Application.rate.units[i] <- "24-hr feeding time"
i <- which(d$Pesticide.commercial.name == "Harpin 48-hr feeding time")
d$Pesticide.commercial.name[i] <- "Harpin"
d$Application.rate.units[i] <- "48-hr feeding time"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Isopropyl alcohol.")] <- "Isopropyl alcohol"

i <- which(d$Pesticide.commercial.name == "Kaoil 24-hr feeding time")
d$Pesticide.commercial.name[i] <- "Kaoil"
d$Application.rate.units[i] <- "24-hr feeding time"
i <- which(d$Pesticide.commercial.name == "Kaolin 48 h feeding time")
d$Pesticide.commercial.name[i] <- "Kaolin"
d$Application.rate.units[i] <- "48-hr feeding time"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Maneb 75DF 2.0")] <- "Maneb 75DF"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Maneb 75DF 1.5")] <- "Maneb 75DF"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Merit 75WP 0.20")] <- "Merit 75WP"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Micro 108 (1exp7 cfu/ml)")] <- "Micro 108"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "MSMA (?) (18.0%)")] <- "MSMA (18.0%)"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "MSMA (?) (9.81%)")] <- "MSMA (9.81%)"
                            
i <- which(d$Pesticide.commercial.name == "Pristine 12.5 oz")
d$Pesticide.commercial.name[i] <- "Pristine"
d$Application.rate[i] <- "12.5 oz"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Proclaim + Dyne-amic")] <- "Proclaim"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Purespray Green oi")] <- "Purespray Green"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "QRD 146 (3x)")] <- "QRD 146"
## d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Radiant 2SC")] <- "Radiant SC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Rovral (2x)")] <- "Rovral"

i <- which(d$Pesticide.commercial.name == "Saf-T-Side 1% V/V")
d$Pesticide.commercial.name[i] <- "Saf-T-Side"
d$Application.rate[i] <- "1% V/V"

## d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Serenade 18WP")] <- "Serenade 10WP"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Sesamin EC 9.34")] <- "Sesamin EC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Silwet L-77 control")] <- "Silwet L-77"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Sonalan HFX")] <- "Sonalan HFP"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Sonata ASO at")] <- "Sonata ASO"

i <- which(d$Pesticide.commercial.name == "Sporan 38% 96 fl oz")
d$Pesticide.commercial.name[i] <- "Sporan 38% EC"
d$Application.rate[i] <- "96 fl oz"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Spotlight (fgluroxypyr 26.2%)")] <- "Spotlight (fluroxypyr 26.2%)"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Stylet-oil (before bloom)")] <- "Stylet-oil"

i <- which(d$Pesticide.commercial.name == "Stylet Oil 1% V/V")
d$Pesticide.commercial.name[i] <- "Stylet Oil"
d$Application.rate[i] <- "1% V/V"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Topgurad")] <- "Topguard"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Tourney G 3")] <- "Tourney G"

i <- which(d$Pesticide.commercial.name == "Trilogy 1% V/V")
d$Pesticide.commercial.name[i] <- "Trilogy"
d$Application.rate[i] <- "1% V/V"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "TrisStar 70WSP")] <- "TriStar 70WSP"


i <- which(d$Pesticide.commercial.name == "Valero 1% V/V")
d$Pesticide.commercial.name[i] <- "Valero"
d$Application.rate[i] <- "1% V/V"

i <- which(d$Pesticide.commercial.name == "Volck Supreme Oil 97.95% 1% V/V")
d$Pesticide.commercial.name[i] <- "Volck Supreme Oil 97.95%"
d$Application.rate[i] <- "1% V/V"

i <- which(d$Pesticide.commercial.name == "Volck Supreme Oil 1% V/V")
d$Pesticide.commercial.name[i] <- "Volck Supreme Oil"
d$Application.rate[i] <- "1% V/V"


d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Window 2F")] <- "Widow 2F"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Ziram 75DF")] <- "Ziram 76DF"

## this is the pur-verified data file that mike made of all the products and their
## ais, etc:
prod_data <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Products-nonCPYR.csv",
                   stringsAsFactors = FALSE, na.strings = c("", "?????"))

prod_data <- prod_data %>%
    dplyr::rename(AI.2 = AI.number.2,
                  chem_prod_notes = Notes,
                  AI.1 = AI1,
                  Pesticide.commercial.name = Product.name..from.article.) %>%
    dplyr::select(-Seq)


bools <- unlist(llply(prod_data$Pesticide.commercial.name, function(x){
    x %in% d$Pesticide.commercial.name
}))

d <- left_join(d, prod_data, by = "Pesticide.commercial.name")

## write.csv(d, file = "Neonic19JanCombined.csv")


## save the full main dataframe
#write.csv(d, file = "~/Dropbox/ZhangLabData/ExcelData/AllDataCombined.csv")
