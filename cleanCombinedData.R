library(RPostgreSQL)
library(DBI)
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
library(stringdist)

source("~/Documents/Coding/R/R_convenience/helper_functions.R")

con <- src_postgres(dbname="california",
                  host="ziram.lawr.ucdavis.edu",
                   user="jonathan"
                   ## ,options="-c search_path=bee"
                   )

prod_adj <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.dpr_prod_adjuvant")))
ai_cats <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.dpr_ai_categories")))
pur_prod <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.product")))
dpr_adj <- collect(tbl(con, dplyr::sql("SELECT * FROM pur.dpr_prod_adjuvant")))

## load in the data:
d <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Neonic4JanCombined.csv")

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
    rename(new = Pesticide.commercial.name)

## fold prods_df back on itself to get a new dataframe (of the same name)
## that contains each product and it's standardized name (the 'new' variable)
prods_df <- left_join(prods_df, fixed_prod_names, by = "smushed") %>%
    select(Pesticide.commercial.name, new)

## join prods_df into d
tmp <- left_join(d, prods_df, by = "Pesticide.commercial.name")

## change Pesticide.commercial.name to it's standardized equivalent
d <- tmp %>%
    select(-Pesticide.commercial.name) %>%
    rename(Pesticide.commercial.name = new)

## now were going to do some shenanigans to determine
## who's an adjuvant and who ISN'T

## get the unique product names
prods <- as.character(unique(d$Pesticide.commercial.name))

## go through each product and find it's top
## five matches in PUR. if all of it's matches
## are insecticides, label it an insecticide.
## if all of its matches are adjuvants, label it an
## adjuvant. And if there's any ambiguity, label it ??
## for me to check it out by hand.
test <- llply(prods, function(prod){
    prod <- hf$removeParens(hf$trim(prod))
    if(prod == " " | is.na(prod)) {return(NA)}
    p <- unlist(strsplit(prod, " "))
    i <- grepl(p[1], pur_prod$product_name, ignore.case = TRUE)
    tmp <- pur_prod[i,]
    tmp <- left_join(tmp, dpr_adj, by = "prodno")
    tmp$dist <- stringdist(prod, tmp$product_name)
    adj <- tmp %>%
        arrange(dist) %>%
            head(5)
    if(empty(adj)){return(NA)}

    adj <- adj %>%
        mutate(bool = adjuvant == "Y") %>%
            select(bool) %>%
                filter(!is.na(bool))
    if(sum(adj$bool) == nrow(adj)){
        return("Y")
    } else if (sum(adj$bool) == 0){
        return("N")
    } else {
        return("??")
    }
}, .progress = "text")

test <- unlist(test)

## loop through each produt with the label ??
## print out its name and top 5 matches, then
## delicately request user input to determine what
## it is.
i <- which(test == "??")
nums <- rep(NA, length(i))
for (j in 1:length(nums)){
    prod <- hf$removeParens(hf$trim(prods[i[j]]))
    p <- unlist(strsplit(prod, " "))
    k <- grepl(p[1], pur_prod$product_name, ignore.case = TRUE)
    tmp <- pur_prod[k,]
    tmp <- left_join(tmp, dpr_adj, by = "prodno")
    tmp$dist <- stringdist(prod, tmp$product_name)
    adj <- tmp %>%
        arrange(dist) %>%
            head(5) %>%
                select(product_name, prodno, adjuvant)
    cat(prod)
    cat("\n")
    print(adj)
    cat("\n")
    nums[j] <- readline(prompt = "is it a fucking adjuvant or not, for fuck's sake: ")
}

## revise the test vector with the corrected user-entered values
test[i] <- nums

## great a new data frame relating products to their adjuvant status
prod_adj <- data.frame(Pesticide.commercial.name = prods, Adjuvant = test, stringsAsFactors = FALSE)
prod_adj$Adjuvant[which(prod_adj$Adjuvant == "NA")] <- NA

## save the adjuvant data so I don't have to do all that stuff again:
## write.csv(prod_adj, file = "~/Dropbox/ZhangLabData/ExcelData/InsecticideOrAdjuvantList.csv")
prod_adj <- read.csv("~/Dropbox/ZhangLabData/ExcelData/InsecticideOrAdjuvantList.csv", stringsAsFactors = FALSE) %>%
    select(-X)

## join the adjuvant data into d
d <- left_join(d, prod_adj, by = "Pesticide.commercial.name")

## save the updated d dataframe.
write.csv(d, file = "~/Dropbox/ZhangLabData/ExcelData/Neonic4JanCombined.csv")
