############################################################
## Helper functions for implementing (our) table operations:
###########################################################

## oy vey, should have made this a class...

to <- new.env()
setwd("/Users/Yoni/Documents/ZhangLab")
source("R/helper_functions.R")

## wrapper function to perform operations on
## one of the insect-data tables. So far,
## only insectDays are available
to$tableOperation <- function(df, operation, method = "Fisher"){

    ## unpack the date information from the
    ## df's attributes
    info <- attr(df, "info")
    dates <- info$dateCols
    pt_cols <- info$pretreatCols
    nonNAcols <- to$findNonEmptyCols(df)

    ## operating on each 'relevant unit' individually:
    results <- lapply(info$relevantRange, function(unit){
        ## find the col range associated with that unit
        range <- intersect(info$allRanges[[unit]], nonNAcols)
        range <- setdiff(range, pt_cols)
        ## diverge based on class of dates:
        if (is.list(dates)) {
            ## if it's a list, the work's been done already:
            dts <- as.data.frame(dates)
            dts <- filter(dts, pos %in% range) 
            cols <- dts$pos
            days <- dts$julian
        } else {
            ## if it isn't we've got to calculate days 'ourselves':
            ret <- to$deduceDayCounts(df, range)
            cols <- ret$cols
            days <- ret$days
        }
        switch(operation,
               "insectDays" = to$calcInsectDays(df, cols, days),
               "plot" = to$plot(df, cols, days),
               "studyDuration" = to$Duration(days),
               "ctrlVar" = to$getCtrlVar(df, cols),
               "ctrlSlope" = to$getCtrlSlope(df, cols, days),
               "EstimateSEM" = to$deduceStdError(df, cols,
                   info$allRanges[[unit]], days, method = method),
               "insectDaysSEM" = to$calcSEM(df, cols, info$allRanges[[unit]],
                   days))
    })
    
    if(operation == "plot"){
        require(gridExtra)
        p <- do.call(grid.arrange, results)
        p
        return(p)
    }
    if(operation == "insectDays"){
        results <- rowSums(data.frame(results))
        df[operation] <- results
        return(df)
    }
    if(operation == "insectDaysSEM"){
        results <- sqrt(rowSums(data.frame(results)))
        df[operation] <- results
        return(df)
    }
    if(operation == "studyDuration"){
        df[operation] <- max(unlist(results))
        return(df)
    }
    if(operation == "ctrlVar"){
        df[operation] <- mean(unlist(results))
        return(df)
    }
    if(operation == "ctrlSlope"){
        df[operation] <- mean(unlist(results))
        return(df)
    }
    if(operation == "EstimateSEM"){

        results <- matrix(unlist(results),
                          nrow = length(results),
                          ncol = 2, byrow = TRUE)
        results <- colSums(results)

        df["stdEr_LB"] <- sqrt(results[1])
        df["stdEr_UB"] <- sqrt(results[2])
        return(df)
    }
}

to$getCtrlSlope <- function(df, cols, days){

    i <- hf$mgrep(c("untreated", "check", "utc", "control", "cehck"),
                  df[,1], ignore.case = TRUE)
    vals <- unlist(df[i, cols])
    mdat <- data.frame(y = vals, x = days)
    m <- lm(y ~ x, data = mdat)
    coef(m)[2]
    
}

to$getCtrlVar <- function(df, cols){

    i <- hf$mgrep(c("untreated", "check", "utc", "control", "cehck"),
                  df[,1], ignore.case = TRUE)

    ctrl <- unlist(df[i, cols])

    var(ctrl / mean(ctrl, na.rm = TRUE), na.rm = TRUE)
    
}

to$Duration <- function(days){
    if(length(days) == 1){ return(1) }
    max(dist(days))
}

to$plot <- function(df, cols, days){
    require(reshape)
    require(ggplot2)
    tmp <- df[, c(1,cols)]
    colnames(tmp) <- c("Treatment", as.character(days))
    tmp <- melt(tmp, id = c("Treatment"))
    tmp$variable <- as.numeric(as.character(tmp$variable))
    ggplot(tmp, aes(x = variable, y = value,
                    color = Treatment, group = Treatment)) +
                        geom_line()
}

## calculate the insect days of each treatment
to$calcInsectDays <- function(df, cols, days){

    ## spot check to see if something's gone wrong:
    stopifnot(length(cols) == length(days))
    ## if there's only one column, return it
    if (length(cols) == 1){
        return(df[, cols])
    }
    ## build the indices we'll need to traverse the table
    indices <- data.frame(i = 1:(length(cols) - 1), j = 2:length(cols))
    ## run insect-days calculation
    ins_days <- mdply(indices, function(i,j){
        .5 * (days[j] - days[i]) * (df[, cols[i]] + df[, cols[j]])
    }, .expand = FALSE)

    ## column sum is returned  
    colSums(ins_days[, -1])
}

to$calcStndEr <- function(stndEr, days){

    ## if there's only one day, we can just return
    ## the standard errors as is
    if(nrow(stndEr) == 1){
        return(unlist(stndEr))
    }

    ## create indices to loop over, just like with insect days
    indices <- data.frame(i = 1:(nrow(stndEr) - 1), j = 2:nrow(stndEr))
    
    ## loop first over the type of bound - upper or lower:
    ret <- llply(colnames(stndEr), function(bnd) {

        
        ## now loop over the indices calculating:
        ## sdEr_x^2 = (dx/da)^2 * sdEr_a^2 + (dx/db)^2 * sdEr_b^2
        ## where x = f(a,b) = .5(dDays)(a + b)
        pieces <- unlist(mlply(indices, function(i,j){
            
            ## for each pair of indices
            ## (dx/da)^2 = (dx/db)^2  = .25((dDays) ^ 2)
            constant <- .25 * (as.numeric(days[j]) - as.numeric(days[i])) ^ 2

            ## and each index contributes the square of its standard
            ## error at the bound
            variable <- stndEr[i, bnd] ^ 2 + stndEr[j, bnd] ^ 2
            
            ## return the product
            constant * variable
        }, .expand = FALSE))
        
        ## and sum all the pieces to get total error
        sum(pieces)
    })
    unlist(ret)
}

## find empty columns in the data set
## taking into account the presence of multi-prod rows
to$findNonEmptyCols <- function(df){

    i <- hf$mgrep(c("mult", "prod"), colnames(df), ignore.case = TRUE, strict = 1)

    ## split on multi prod numbers to eventually remove rows that
    ## are mostly NA (because they are a mp sample)
    df <- ddply(df, i, function(x){
        mp <- unique(x[, i])
        if(is.na(mp)){
            x
        } else {
            bool_df <- is.na(x)
            m <- min(rowSums(bool_df))
            if(sum(rowSums(bool_df) == m) > 1){
                x %>% filter_(mp == "fudge")
            } else {
                x[which.min(rowSums(bool_df)), ]
            }
        }
    })

    ## see which entries in the df are NA
    bool_df <- as.data.frame(is.na(df))

    ## return columns that contain no NA's
    which(unlist(llply(bool_df, function(x) !any(x))))
}

## if no dates are given, use the DAT or WAT acronyms to
## deduce the number of days that passed between samples
to$deduceDayCounts <- function(df, range){

    measures <- c("DAT", "WAT")
    col_names <- colnames(df[, range])
    unit <- measures[sapply(measures, function(x){
        any(grepl(x, col_names, ignore.case = FALSE))
    })]
    if (length(unit) != 1){ stop("problem with time units!") }
    cols <- grep(unit, col_names, ignore.case = FALSE) 
    ## now convert the col names to the  number of units they
    ## specify:
    col_names <- sapply(col_names[cols], function(x){
        strsplit(x, "\\.")[[1]][1]
    })
    days <- as.numeric(gsub("[^[:digit:]]", "", col_names))
    days <- switch(unit, WAT = 7 * days, DAT = days)
    list(days = days, cols = range[cols])
}

## if SEM is already in the cols of df, propagate it through
## the insect days calculation
to$calcSEM <- function(df, cols, range, days){

    require(plyr)

    if(!any(hf$mgrepl(c("SEM", "VAR"), colnames(df), ignore.case = TRUE))){
        return(rep(NA, nrow(df)))
    }

    sem_cols <- intersect(range[range >= min(cols)],
                          hf$mgrep(c("SEM", "VAR"),
                                   colnames(df), ignore.case = TRUE))

    if (length(cols) != length(sem_cols)){
        stop("cols and sem cols have different lengths")
    }

    if(any(unlist(llply(sem_cols, function(i){
        all(is.na(df[, i]))
    })))) {
        return(rep(NA, nrows(df)))
    }

    ## if there's only one column, return its SEM
    if (length(cols) == 1){
        return(df[, sem_cols])
    }
    
    ## build the indices we'll need to traverse the table
    indices <- data.frame(i = 1:(length(cols) - 1), j = 2:length(cols))

    ## now loop over the indices calculating:
    ## sem_x^2 = (dx/da)^2 * sem_a^2 + (dx/db)^2 * sem_b^2
    ## where x = f(a,b) = .5(Days between meas_a and meas_b)(a + b)
    ins_days_sem <- mdply(indices, function(i,j){

        ## for each pair of indices
        ## (dx/da)^2 = (dx/db)^2  = .25((change in days) ^ 2)
        constant <- .25 * (as.numeric(days[j]) - as.numeric(days[i])) ^ 2

        ## and each index contributes the square of its sem
        variable <- df[, sem_cols[i]] ^ 2 + df[, sem_cols[j]] ^ 2

        constant * variable
    }, .expand = FALSE)

    ## remove the index, columns are returned
    colSums(ins_days_sem[, -1])
}

to$deduceStdError <- function(df, cols, range, days, method = "Fisher"){

    require(plyr)

    sg_cols <- intersect(range[range >= min(cols)],
                         grep("StGr", colnames(df), ignore.case = TRUE))

    if(any(unlist(llply(sg_cols, function(i){
        all(is.na(df[, i]))
    })))) {
        return(c(NA, NA))
    }
    
    ## operate on each column
    stndEr <- ldply(seq_along(cols), function(i){
        vals <- df[, cols[i]]
        grps <- df[, sg_cols[i]]
        ## call multi function to get bounds
        ret <- to$multi(y.mean = vals, y.mrt = grps, method = method)
        data.frame(L = ret[1], U = ret[2])
    })

    stndErAlt <- ldply(seq_along(cols), function(i){
            vals <- df[, cols[i]]
            grps <- df[, sg_cols[i]]
            ## call multi function to get bounds
            ret <- to$multi(y.mean = vals, y.mrt = grps, method = setdiff(c("Fisher", "Duncan"), method))
            data.frame(L = ret[1], U = ret[2])
        })

    if(sum(is.infinite(unlist(stndEr))) > sum(is.infinite(unlist(stndErAlt)))){
        warning("switching methods")
        stndEr <- stndErAlt
    }
        
    ## calculate standard error for the whole data frame
    ## and return
    to$calcStndEr(stndEr, days)
}

## helper functions for extracting upper and lower bounds on standard error
## from (Knapp et. el 2009 - journal of applied hort)
to$charcomp <- function(x,y){
    i <- strsplit(c(x,y), "")
    ifelse(length(intersect(i[[1]], i[[2]])) > 0, "L", "U")
}
to$multi <- function(y.mean, y.mrt,
                  method = c("Duncan", "Fisher"),
                  alpha = .05){

    y.mean <- na.omit(y.mean)
    y.mrt <- na.omit(y.mrt)
    if (method == "Duncan"){
        y.rank <- 13 - rank(y.mean, ties.method = "min")
    }
    fish <- function(X1, X2){
        mean.diff <- abs(y.mean[X1] - y.mean[X2])
        b <- mean.diff / (qnorm(1 - alpha / 2) * sqrt(2))
        d <- to$charcomp(y.mrt[X1], y.mrt[X2])
        data.frame(bound = b, decision = d)
    }
    dunc <- function(X1, X2){
        mean.diff <- abs(y.mean[X1] - y.mean[X2])
        rank.diff <- abs(y.rank[X1] - y.rank[X2])
        alpha.p <- 1 - ((1 - alpha) ^ rank.diff)
        b <- mean.diff / qtukey( 1 - alpha.p, rank.diff + 1, 10^5)
        d <- to$charcomp(y.mrt[X1], y.mrt[X2])
        data.frame(bound = b, decision = d)
    }
    c.pair <- data.frame(t(combn(length(y.mean), 2)))
    f <- switch(method,
                Duncan = dunc,
                Fisher = fish)
    res <- mdply(c.pair, f)
    c(max(filter(res, decision == "L")$bound, na.rm = TRUE),
      min(filter(res, decision == "U")$bound, na.rm = TRUE))
}


## Testing:
## j <- 1
## tmp <- lapply(entered$V1, function(x) {
##     ins_table <- response_tables[[x]]
##     operations <- c("EstimateSEM")
##     a <- try(to$tableOperation(ins_table, operations[1], method = "Fisher"))
##     if(identical(a, "try-error")){
##         warning(x)
##         return(NA)
##     } else {
##         return(a)
##     }
##     ##Reduce(function(...) merge(..., all = TRUE), ins_tables)
##     j <<- j + 1
## })


