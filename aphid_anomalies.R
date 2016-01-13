library(plyr)
library(dplyr)

source("~/Documents/ZhangLab/R/Chlorpyrifos/assemble_final_dataset.R")

findAnom <- function(tab){
    if(!is.data.frame(tab)){return(NA)}
    rel_cols <- attr(attr(tab, "info")$allRanges, "relevantCols")
    cols <- unlist(llply(rel_cols, function(x){
        attr(tab, "info")$allRanges[[x]]
    }))
    stgr_cols <- intersect(cols, grep("stgr", colnames(tab), ignore.case = TRUE))
    date_cols <- try(attr(tab, "info")$dateCols$pos)
    if(identical(class(date_cols), "try-error")){
        date_cols <- intersect(cols, hf$mgrep(c("dat", "wat"), colnames(tab), ignore.case = TRUE))
        if(all(is.na(date_cols))){return(NA)}
    }
    dens_data <- tab[, intersect(cols, date_cols)]
    stgr_data <- tab[, stgr_cols]
    bool_vals <- as.data.frame(llply(dens_data, function(x){
        if(all(is.na(x))){return(x)}
            x > x[length(x)]
    }))
    bool_stgr <- as.data.frame(llply(stgr_data, function(x){
        if(all(is.na(x))){return(x)}
        stgr_letters <- strsplit(x, "")
        unlist(llply(stgr_letters, function(y){
            length(intersect(y, stgr_letters[[length(stgr_letters)]])) == 0
        }))
    }))
    bool <- bool_vals & bool_stgr
    any(na.omit(bool))
}

pests <- c("aphid", "weevil", "mite", "bug", "worm")

ldply(pests, function(x){
    ##x <- "bug"
    pest <- ins_table %>% filter(grepl(x, Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.)) %>% select(Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.) %>% distinct(Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.)

    pest_exps <- semi_join(response_tables_lookup, pest,
                           by = "Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.")
    
    pest_tables <- llply(unique(pest_exps$V1), function(x){
        response_tables[[x]]
    })

    res <- unlist(llply(pest_tables, findAnom, .inform = TRUE))
    data.frame(Pest = x, experiments = length(res), anomalies = sum(na.omit(res)))
}, .inform = TRUE)



