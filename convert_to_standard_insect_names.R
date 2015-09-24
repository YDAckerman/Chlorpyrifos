## function used to convert pest column into column with standardized names

## build table of standardized and unique pest names:
source("/Users/Yoni/Documents/ZhangLab/R/create_pest_name_dict.R")

## EXAMPLE:
## use (use plyr on dat if you want to populate the full db):
## name_comparison_table <- ldply(pests, function(x){
##     data.frame('original' = x, 'standard' = toStandardName(x))
## })

## takes an insect string and returns its standardized form.
toStandardName <- function(s){

    ## search keys for value s
    pos <- sapply(ls(pestDict), function(key){
        s %in% pestDict[[key]]
    })

    ## if not found, say so
    if(sum(pos) == 0){return("not in dict")}

    ## otherwise return the appropriate key
    ls(pestDict)[pos]
}
