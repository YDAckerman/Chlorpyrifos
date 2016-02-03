library(plyr)
library(dplyr)
library(reshape2)
library(data.table)

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
d <- read.csv("~/Dropbox/ZhangLabData/ExcelData/AllDataCombined.csv", stringsAsFactors = FALSE)

## cpyr product data
prod_data_cpyr <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Products-CPYR.csv",
                           stringsAsFactors = FALSE, na.strings = c("", "?????")) %>%
    dplyr::rename(DPR.Label.Database.name = DPR.Name,
                  SpecGravOrDensity = Density,
                  AI.1 = AI1,
                  AI.2 = AI2,
                  AI.3 = AI3,
                  chem_prod_notes = Notes,
                  ChemicalType = Type,
                  DPR.Prodno = ProdNo
                  )

## this is the pur-verified data file that mike made of all the products and their
## ais, etc:
prod_data <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Products-nonCPYR.csv",
                   stringsAsFactors = FALSE, na.strings = c("", "?????"))  %>%
    dplyr::rename(AI.2 = AI.number.2,
                  chem_prod_notes = Notes,
                  AI.1 = AI1,
                  SpecGravOrDensity = Sp.Grav..liq.,
                  Pesticide.commercial.name = Product.name..from.article.)

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
                  Pesticide.commercial.name = Product.name.from.article,
                  Pesticide.Type = Pesticide.type
                  )

prod_data_amt <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Products-AMTfile.csv",
                      stringsAsFactors = FALSE, na.strings = c("", "?????", "not known",
                                                    "not specified", "product name ambiguous", "unknown"))  %>%
    dplyr::rename(SpecGravOrDensity = Sp.Grav..liq.,
                  AI.1 = AI,
                  AI.2 = AI2,
                  AI.3 = AI3,
                  X..AI1 = X.AI,
                  X..AI2 = X.AI2,
                  X..AI3 = X.AI3,
                  chem_prod_notes = Notes,
                  Pesticide.commercial.name = Product.name..from.article.
                  )

prod_data <- rbindlist(list(prod_data, prod_data_cpyr,
                            prod_data_amt, prod_data_two_more),
                       use.names = TRUE, fill = TRUE)

prod_data <- prod_data %>%
    dplyr::mutate(Pesticide.commercial.name = tolower(gsub("[^a-zA-Z0-9]", "", Pesticide.commercial.name))) %>%
    group_by(Pesticide.commercial.name) %>%
    slice(1) %>%
    ungroup

## In either pesticide or adjuvant units “oz/100 gal Product/Acre” should be “oz product/100 gal”
i <- grepl("oz/100 gal Product/Acre", d$Application.rate.units)
d$Application.rate.units[i] <- "oz product/100 gal"
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
d$AI[grep("^exp", d$Pesticide.commercial.name, ignore.case = TRUE, perl = TRUE)] <- "experimental"


## create a temporary dataframe with a 'smushed' variable. Any product
## names that differ only by spaces/letter cases will then be the same
prods <- as.character(unique(d$Pesticide.commercial.name))
prods_df <- data.frame(Pesticide.commercial.name = prods,
                       smushed = tolower(gsub(" ","", gsub("-","", prods))),
                       stringsAsFactors = FALSE) %>%
    filter(smushed != "")

## use prods_df to select one variation for each unique 'smushed' value,
## choosing by length of the variation
fixed_prod_names <- prods_df %>%
    dplyr::mutate(len = nchar(Pesticide.commercial.name)) %>%
    dplyr::group_by(smushed) %>%
    dplyr::arrange(len) %>%
    dplyr::top_n(1) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(standardized_product_name = Pesticide.commercial.name)

## fold prods_df back on itself to get a new dataframe (of the same name)
## that contains each product and it's standardized name (the 'new' variable)
prods_df <- left_join(prods_df, fixed_prod_names, by = "smushed") %>%
    select(Pesticide.commercial.name, standardized_product_name)

## join prods_df into d
tmp <- left_join(d, prods_df, by = "Pesticide.commercial.name")

## change Pesticide.commercial.name to it's standardized equivalent
d <- tmp %>%
    dplyr::rename(Original.Pesticide.commercial.name = Pesticide.commercial.name) %>%
    dplyr::rename(Pesticide.commercial.name = standardized_product_name)

## individual text fixes. 
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "A Pristine 38WDG")] <- "Pristine 38WDG"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "B5028 20.0 oz")] <- "B5028"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Bloomtime Bio 10^7")] <- "Bloomtime Bio"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Bloomtime Bio 10^8")] <- "Bloomtime Bio"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Beseige 1.25ZC")] <- "Besiege 1.25ZC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Beseige")] <- "Besiege"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Captan 50 WP (2x)")] <- "Captan 50 WP"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Carpovirusine 1x10^13 AS")] <- "Carpovirusine AS"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Carpo-virusine 1×10^13 AS")] <- "Carpovirusine AS"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Carzol 90SP2")] <- "Carzol 90SP"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Carzol 90SP2")] <- "Carzol 90SP"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Cobalt 2.54EC")] <- "Cobalt 2.55EC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Phyton-016-B")] <- "Phyton 016-B"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Cyd-X 3.3×10^13 AS")] <- "Cyd-X"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "F-0570 0.8EW")] <- "F-0570 0.8EC"
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
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Kaoil")] <- "Kaolin"

i <- which(d$Pesticide.commercial.name == "Kaoil 24-hr feeding time")
d$Pesticide.commercial.name[i] <- "Kaolin"
d$Application.rate.units[i] <- "24-hr feeding time"
i <- which(d$Pesticide.commercial.name == "Kaolin 48 h feeding time")
d$Pesticide.commercial.name[i] <- "Kaolin"
d$Application.rate.units[i] <- "48-hr feeding time"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Maneb 75DF 2.0")] <- "Maneb 75DF"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Maneb 75DF 1.5")] <- "Maneb 75DF"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "MBI-203")] <- "MBI 203"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "MBI-206")] <- "MBI 206"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Merit 75WP 0.20")] <- "Merit 75WP"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Micro 108 (1exp7 cfu/ml)")] <- "Micro 108"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "MSMA (?) (18.0%)")] <- "MSMA (18.0%)"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "MSMA (?) (9.81%)")] <- "MSMA (9.81%)"
                            
i <- which(d$Pesticide.commercial.name == "Pristine 12.5 oz")
d$Pesticide.commercial.name[i] <- "Pristine"
d$Application.rate[i] <- "12.5 oz"

d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Proclaim + Dyne-amic")] <- "Proclaim"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Purespray Green oi")] <- "Purespray Green"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "PureSpray Green oil")] <- "Purespray Green"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Prev-Am")] <- "Prev AM"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "QRD 146 (3x)")] <- "QRD 146"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "QRD-146 (3x)")] <- "QRD 146"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "A16422 (A) SC")] <- "A16422 SC (A)"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Agri-Med 0.15EC")] <- "Agri-Mek 0.15EC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Carpovirusine (Cydia pomonella graulosis virus)")] <- "Carpovirusine"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Cyd-X (Cydia pomonella granulosis virus)")] <- "Cyd-X"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Nixter 75W")] <- "Nexter 75W"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Captuer 2EC")] <- "Capture 2EC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "AntGuart")] <- "AntGuard"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Oberon 2SC d")] <- "Oberon 2SC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Oberon 2SCd")] <- "Oberon 2SC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Zeal 72WGDc")] <- "Zeal"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Yield Enhancher")] <- "Yield Enhancer"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "TopFilm SEb")] <- "TopFilm SE"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Ecotrol1 10EC")] <- "Ecotrol 10EC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Envidor 2SC3")] <- "Envidor"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Envidor 2SC2")] <- "Envidor"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Asana XL (Grower Standard)")] <- "Asana XL"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Volium Xpress 1.25 ZC")] <- "Voliam Xpress 1.25 ZC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "MBI 203DF")] <- "MBI 203 DF2"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Torac 1SEC")] <- "Torac 15EC"
## d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Radiant 2SC")] <- "Radiant SC"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Rovral (2x)")] <- "Rovral"

i <- which(d$Pesticide.commercial.name == "Saf-T-Side 1% V/V")
d$Pesticide.commercial.name[i] <- "Saf-T-Side"
d$Application.rate[i] <- "1% V/V"

i <- which(d$Pesticide.commercial.name == "Silwet L-77 0.023% v/v")
d$Pesticide.commercial.name[i] <- "Silwet L-77"
d$Application.rate[i] <- "0.023% v/v"


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
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Topsin M 70W P")] <- "Topsin M 70WP"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "Topsin-M")] <- "Topsin M"
d$Pesticide.commercial.name[which(d$Pesticide.commercial.name == "26 GT 2SC")] <- "26GT 2SC"

i <- which(d$Pesticide.commercial.name == "Trilogy 1% V/V")
d$Pesticide.commercial.name[i] <- "Trilogy"
d$Application.rate[i] <- "1% V/V"

i <- which(d$Pesticide.commercial.name == "LI 6365, 10 fl oz")
d$Pesticide.commercial.name[i] <- "LI 6365"
d$Application.rate[i] <- "10 fl oz"

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


d <- d %>%
    dplyr::mutate(Pesticide.commercial.name = tolower(gsub("[^a-zA-Z0-9]", "", Pesticide.commercial.name)))

bools <- ldply(unique(d$Pesticide.commercial.name), function(x){
    any(x == prod_data$Pesticide.commercial.name)
    data.frame(name = x, bool = any(x == prod_data$Pesticide.commercial.name),
               num = sum(x == prod_data$Pesticide.commercial.name), stringsAsFactors = FALSE)
})

ers <- bools %>% filter(!bool | num > 1)

d <- left_join(d, prod_data, by = "Pesticide.commercial.name") %>%
    dplyr::select(-Pesticide.commercial.name) %>%
    dplyr::rename(Pesticide.commercial.name = Original.Pesticide.commercial.name)


## save the full main dataframe
## write.csv(d, file = "~/Dropbox/ZhangLabData/ExcelData/AllDataCombinedwithAI.csv")
