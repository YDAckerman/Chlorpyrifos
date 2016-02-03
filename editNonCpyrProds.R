## we need to verify a few portions of the Products-nonCPYR.csv datasheet agains
## the PUR database.
## Where a product number is given:
## - verify the product name
## - double check its ais and perc-ais
## - retrieve the pesticide type
## - retrieve the ai chemical class
## 
## Where no product number is given, but an AI is listed:
## - retrieve the pesticide type
## - retrieve the ai chemical class

library(plyr)
library(dplyr)
library(data.table)
source("~/Documents/Coding/R/R_convenience/helper_functions.R")

con <- src_postgres(dbname="california",
                  host="ziram.lawr.ucdavis.edu",
                   user="jonathan"
                   )

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
                  gPesticide.Type = Pesticide.type
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

d_full <- rbindlist(list(prod_data, prod_data_cpyr), use.names = TRUE, fill = TRUE)

## get prod types
prodnos <- unique(d_full$DPR.Prodno)
prod_typs <- ldply(prodnos, function(prodno){
    if(is.na(prodno)){
        return(
            data.frame(prodno = NA, Pesticide.Type = NA, stringsAsFactors = FALSE)
            )
    }
    prodnos_sql <- paste(paste0("prodno = '", hf$trim(unlist(strsplit(prodno, ","))), "'"), collapse = " OR ")
    sql <- paste0("SELECT ai_type FROM (SELECT DISTINCT(chem_code) FROM pur.udc WHERE ",
                  prodnos_sql,") p LEFT JOIN pur.dpr_ai_categories pc ON p.chem_code = pc.chem_code")
    result <- collect(tbl(con, dplyr::sql(sql)))
    data.frame(prodno = prodno, Pesticide.Type = paste(na.omit(unique(result$ai_type)), collapse = "+"), stringsAsFactors = FALSE)
}, .progress = "text")

prod_typs <- prod_typs %>% dplyr::rename(DPR.Prodno = prodno)
d_full <- as.data.frame(d_full)
d_full <- dplyr::left_join(d_full, prod_typs, by = "DPR.Prodno")

## write.csv(d_full, file = "Product-Data-All-With-Prod-Type.csv")


d <- d_full %>% filter(grepl("[0-9]*", DPR.Prodno)) %>%
    select(DPR.Label.Database.name, DPR.Prodno) %>%
    distinct(DPR.Label.Database.name, DPR.Prodno)

ai_cats <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.dpr_ai_categories")))
udc <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.product LIMIT 10")))
ai_names <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.chemical")))
prod_adj <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.dpr_prod_adjuvant")))

## pur.dpr_prod_chem

tmp <- llply(1:nrow(d), function(i){
    prod <- d[i,]$DPR.Label.Database.name
    prodnos <- d[i,]$DPR.Prodno
    
    ## prodnos <- "65395, 53357"
    ## some of the products list multiple numbers
    if(grepl("[0-9]*", prodnos)){
        prodnos_sql <- paste(paste0("prodno = '", hf$trim(unlist(strsplit(prodnos, ","))), "'"), collapse = " OR ")
    } else {
        return(NULL)
    }
       
    ## get all the AIs, AI percs, AI classes, and Product types:
    sql <- paste0("SELECT * FROM (SELECT product_name, prodno as product_number FROM pur.product WHERE ", prodnos_sql,") p LEFT JOIN pur.dpr_prod_chem pc ON p.product_number = pc.prodno")
    result <- collect(tbl(con, dplyr::sql(sql)))
    if(empty(result)){
        return(data.frame(PUR.Database.name = prod, DPR.Prodno = prodnos))
    }
    result <- left_join(left_join(result, ai_cats, by = "chem_code"), ai_names, by = "chem_code")
    result <- left_join(result, prod_adj, by = "prodno")
    result$PUR.Database.name <- prod
    result$DPR.Prodno <- prodnos

    if("Y" %in% result$adjuvant){
        result %>%
            select(-product_number) %>%
            mutate(names_match = length(hf$stringIntersect(PUR.Database.name,
                       paste(unique(product_name), sep = " "))) > 0) %>%
                           filter(adjuvant == "Y") %>%
                               
                               slice(1)
    } else {
        result %>%
            select(-product_number) %>%
            mutate(names_match = length(hf$stringIntersect(PUR.Database.name,
                           paste(unique(product_name), sep = " "))) > 0)
    }
}, .inform = TRUE)


product_df <- rbindlist(tmp, fill = TRUE, use.names = TRUE) %>%
    filter(chemname != "INERT INGREDIENTS" & adjuvant == "N")
tmp <- left_join(d_full %>% filter(grepl("[0-9]*", DPR.Prodno)),
                 product_df, by = "DPR.Prodno")

tmp_ais <- tmp %>%
    select(DPR.Label.Database.name, prodno, chem_code,
           chemname, prodchem_pct,
           AI.1, X..AI1,
           AI.2, X..AI2,
           AI.3, X..AI3,
           AI.4, X..AI4)

check_ais <- ldply(1:nrow(tmp_ais), function(i){
    chem_check = with(tmp_ais[i, ],
        agrepl(chemname, AI.1, ignore.case = TRUE) | agrepl(AI.1, chemname, ignore.case = TRUE) |
        agrepl(chemname, AI.2, ignore.case = TRUE) | agrepl(AI.2, chemname, ignore.case = TRUE) |
        agrepl(chemname, AI.3, ignore.case = TRUE) | agrepl(AI.3, chemname, ignore.case = TRUE) |
        agrepl(chemname, AI.4, ignore.case = TRUE) | agrepl(AI.4, chemname, ignore.case = TRUE))
    perc_chem_check = with(tmp_ais[i, ],
        agrepl(prodchem_pct, X..AI1) |
        agrepl(prodchem_pct, X..AI2) |
        agrepl(prodchem_pct, X..AI3) |
        agrepl(prodchem_pct, X..AI4))
    data.frame(check_chem = chem_check, check_perc = perc_chem_check)
})

tmp_ais <- cbind(tmp_ais, check_ais)

ers <- tmp_ais %>% filter(!check_chem | !check_perc)

## write.csv(ers, file = "mismatch_chems.csv")

## I've reviewed the results with mike and we're satisfied: the data is now interchangeable with pur.

