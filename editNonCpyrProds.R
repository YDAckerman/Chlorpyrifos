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
source("~/Documents/Coding/R/R_convenience/helper_functions.R")

con <- src_postgres(dbname="california",
                  host="ziram.lawr.ucdavis.edu",
                   user="jonathan"
                   )

d_full <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Products-nonCPYR.csv",
                   stringsAsFactors = FALSE, na.strings = c("", "?????"))

d <- d_full %>% filter(grepl("[0-9]*", DPR.Prodno)) %>%
    select(DPR.Label.Database.name, DPR.Prodno) %>%
    distinct(DPR.Label.Database.name, DPR.Prodno)

ai_cats <- collect(tbl(con, dplyr::sql("SELECT chem_code, ai_class, ai_type FROM pur.dpr_ai_categories")))
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
        
    ## get product info from PUR corresponding to the given prodnos
    sql <- paste0("SELECT product_name FROM pur.product WHERE ", prodnos_sql)
    result <- collect(tbl(con, dplyr::sql(sql)))

    ## if nothing comes up, return ret as it is
    if(empty(result)){
        return(NULL)
    }

    prod_names_from_db <- result$product_name
    prod_sql <- paste(paste0("product_name = '", prod_names_from_db, "'"), collapse = " OR ")

    ## get all the AIs, AI percs, AI classes, and Product types:
    sql <- paste0("SELECT * FROM (SELECT product_name, prodno as product_number FROM pur.product WHERE ", prod_sql,") p LEFT JOIN pur.dpr_prod_chem pc ON p.product_number = pc.prodno")
    result <- collect(tbl(con, dplyr::sql(sql)))
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


product_df <- do.call(rbind, tmp) %>%
    filter(chemname != "INERT INGREDIENTS" & adjuvant == "N")
tmp <- left_join(d_full %>% filter(grepl("[0-9]*", DPR.Prodno)),
                 product_df, by = "DPR.Prodno")

tmp_ais <- tmp %>%
    select(DPR.Label.Database.name, prodno, chem_code,
           chemname, prodchem_pct, AI1, X..AI1,
           AI.number.2, X..AI2,
           AI.3, X..AI3,
           AI.4, X..AI4)

check_ais <- ldply(1:nrow(tmp_ais), function(i){
    chem_check = with(tmp_ais[i, ],
        agrepl(chemname, AI1, ignore.case = TRUE) | agrepl(AI1, chemname, ignore.case = TRUE) |
        agrepl(chemname, AI.number.2, ignore.case = TRUE) | agrepl(AI.number.2, chemname, ignore.case = TRUE) |
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

## I've review the results with mike and we're satisfied: the data is now interchangeable with pur.

