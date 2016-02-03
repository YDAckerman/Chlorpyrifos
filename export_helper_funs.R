############################################################
## Helper functions for importing data from our excel files.
## author: Yoni Ackerman
## contact: jdackerma@ucdavis.edu
############################################################

## create an environment to store all of
## the functions we create. This will help to keep
## work spaces organized.
ehf <- new.env()

## source the helper function (hf) environment
source("~/Documents/Coding/R/R_convenience/helper_functions.R")

## given the correct file, find the indices of the raw block of data
## corresponding to the pdfname
ehf$getBlockRange <- function(file, pdfname){

    ## first occurrence of the pdf's name:
    block_b <- try(pdf_locs_in_files[[names(file)]][[pdfname]])
    if(identical(class(block_b), "try-error")){
        mess <- paste(pdfname, " not found in file")
        stop(mess)
    }

    ## find end of block by looking at the next highest pdf name
    ## occurrence:
    i_s <- which(pdf_locs_in_files[[names(file)]] > block_b)
    if (length(i_s) == 0){
        block_b : length(file[[1]])
    } else {
        block_e <- min(unlist(pdf_locs_in_files[[names(file)]][i_s])) - 1 
        block_b:block_e
    }
}

## given the correct block, find the indices of the table
## corresponding to the tbl_indicator
ehf$getTableRange <- function(file, tbl_ind, block_range){

    ## find occurrence of tbl_ind within the block range
    tbl_b <- grep(
        paste0("^", gsub("[^[:alnum:]]", "", hf$removeParens(tbl_ind)), ";"),
        gsub("[^a-zA-Z0-9;]", "", sapply(file[[1]][block_range], hf$removeParens)),
        ignore.case = TRUE)

    if (length(tbl_b) != 1){
        return(NA)
    }
    
    ## look for all occurrences of 'table' in block_range 
    tbl_locs <- grep("^table", file[[1]][block_range], ignore.case = TRUE)

    ## use these to find the nearest occurrence after the table start
    ## occurrence and set that value as the table's end.
    tbl_es <- tbl_locs[which(tbl_locs > tbl_b)]
    if (length(tbl_es) == 0){
        tbl_e <- length(block_range)
    } else {
            tbl_e <- min(tbl_es) - 1
    }
    block_range[(tbl_b-1):tbl_e]
}

## given the correct table, find the ranges
## of the individual content blocks within it.
ehf$getTableContentRanges <- function(file, tbl_range){

    ## must have:
    ## setwd("/Users/Yoni/Documents/ZhangLab")
    ## source("R/insect_dict_helper_funs.R")

    ## find occurences of the words rate and treatment together
    data_starts <- hf$mgrep(c("rate", ";treatment"),
                             file[[1]][tbl_range],
                             ignore.case = TRUE,
                             strict = 1)

    ## and all occurence of the 'utc' word
    data_ends <- hf$mgrep(c("untreated", "check", "utc", ";control", "cehck"),
                           file[[1]][tbl_range],
                          ignore.case = TRUE)
    
    data.frame(start = tbl_range[data_starts],
               end = tbl_range[data_ends])
}

## given the table range, find the row in that range
## that most likely corresponds to the unit_indicator
ehf$getUnitIndex <- function(file, tbl_range, unit_id, loc, insect_id){

    ## use string intersect to find the most likely match
    str_comparisons <- sapply(file[[1]][tbl_range], function(x){
        ## TODO: change in the data sheet.
        x <- gsub("Note: Column headers in article have \"\"26 Sep\"\" to the left of \"\"28 Aug\"\" for \"\"No. live CRS/fruit\"\"", "", x)
        x <- gsub("sum of samplings", "", x)
        fix <- gsub("Ten sweep", "Ten sweeps", c(x, unit_id))
        length(hf$stringIntersect(fix[1], paste(fix[2], loc, insect_id)))
    })

    if (all(str_comparisons == 0)) {
        ##write(paste(get("pdf", envir = parent.frame()), "unit type not found in block \n", sep = " "), file = "error.txt", append = TRUE)
        stop(paste("unit type not found in block: ", get("pdf", envir = parent.frame()), ": ", get("tbl_ind", envir = parent.frame())))
    }
    
    ## if multiple are returned, just take one of them (built into which.max).
    ## later operations will account for there potentially
    ## being more than one line that matches the insect
    ## indicator well. This doesn't account for situations like:

    ## aphids per ten sweep
    ## aphids per sweep

    ## see above temporary *fix*
    tbl_range[which.max(str_comparisons)]
}

## returns the range of columns relevant to the specified
## insect units. If there aren't multiple units combined
## in the same table, returns NA.
ehf$getUnitColumnRange <- function(file, unit_index, t_index,
                                   loc, unit_id, insect_id){

    match_strings <- paste(unit_id, insect_id, loc, sep = " ")

    ## find number of columns
    col_count <- ehf$getColCount(file, t_index)

    ## some of the raw tables are spaced differently in their
    ## respective csv files, so figure out what column they actually
    ## start in:
    offset <- sum(unlist(strsplit(file[[1]][t_index$start], ";"))[1:4] == "")
    
    ## get the line that best groups the columns
    unit_line <- ehf$getUnitLine(file, unit_index, t_index, match_strings)

    ## find the best matches and keep the words matched
    ## for grouping purposes
    col_ranges <- ehf$getGrpRanges(unit_line, match_strings, col_count, offset)

    dates <- ehf$getColDates(file, unit_index, t_index)
    
    list(columns = 1 + offset : (1 + col_count),
         relevantRange = attr(col_ranges, "relevantCols"),
         allRanges = col_ranges, ## cols refer to post-extracted position
         pretreatCols = ehf$getPretreatCols(file, unit_index, t_index),
         dateCols = dates)

}

## retrieve a list of the dates found as column headers in the
## extracted datatable, as well as their column numbers.
ehf$getColDates <- function(file, unit_index, t_index){
    ## this only gets things that are in date form,
    ## no columns with names in the form of X.DAT are
    ## returned.
    potential_unit_lines <- file[[1]][unit_index:(t_index$start)]
    
    ## fashion a regex to extract date strings
    date_regex <- "[0-9]{1,2}-[a-zA-Z]{3}-[0-9]{2,4}"

    ## determine which rows in the table contain values
    ## in date form
    i <- grep(date_regex, potential_unit_lines)

    if (length(i) == 0 ) { return(NA) }

    ## if there are multiple such rows, consider only the lowest
    if (length(i) > 1 ) { i <- max(i) }

    ## within the row of interest, extract the cells that
    ## have date-headers
    date_col <- unlist(strsplit(potential_unit_lines[i], ";"))

    ## convert to dates, julian dates
    dates <- strptime(date_col, "%d-%b-%y")
    julian <- julian(dates)

    ## determine the column numbers of the stripped date-cells
    date_col_pos <- which(!is.na(dates))
    
    ## give the julian dates, the raw dates, and their column
    ## positions in the table
    list(julian = na.omit(julian),
         rawDates = na.omit(as.character(dates)),
         pos = date_col_pos - 1)
}

## The excel tables frequently consist of multiple data tables
## whose columns have been bound together. In the excel sheets,
## this is indicated by a "Pest unit" header extending over all
## the columns of data it describes. This function matches
## the pest units (sometimes multiple given in a single line of
## the main dataset) to the column ranges they describe.
ehf$getGrpRanges <- function(unit_line, match_strings, col_count, offset){

    ## the line comprised of pest units has already been found.
    ## the task now is to match, as best we can, the pest unit
    ## string from the main datasheet to individual column groups.

    ## see which column headers match with which parts of the
    ## pest unit string
    str_comparisons <- ldply(unit_line, function(x){
        word <- NA
        bool <- FALSE
        len <- NA
        if (x != "" & !grepl("note", x, ignore.case = TRUE)){
            word <- x <- hf$removeParens(x)
            bool <- hf$stringContains(x, match_strings)
            len <- length(hf$stringIntersect(x, match_strings))
        }
        ## return the results as a data frame holding the
        ## number of word-matches, whether or not the word is in
        ## both the unit line and the pest unit string (it's relevance,
        ## represented as TRUE or FALSE), and the word itself
        data.frame(lens = len,
                   bools = bool,
                   words = word, 
                   stringsAsFactors = FALSE)
    })

    ## select which of the grouped set of columns is actually
    ## relevant
    if (!any(str_comparisons$bools)){
        ## if col names dont match match_strings exactly, then
        ## just take the best of them

        ## this didn't work for some cases (?)
        ## relevant_cols_names <- unlist(unique(
        ##     str_comparisons %>%
        ##     filter(lens == max(lens, na.rm = TRUE) & !grepl("%", words)) %>%
        ##     select(words)))

        ## use the number of matched words to select which set of
        ## columns is the most relevant. We depend here on the integrity of
        ## the data relating the two datasheets.
        relevant_cols_names <-                                
            (str_comparisons %>%                                             
                filter(lens == max(lens, na.rm = TRUE) & !grepl("%", words)) %>%
                    distinct(words) %>%
                        select(words))$words

        ## sometimes there are not matches. This should trigger an error.
        if(length(relevant_cols_names) == 0){
            mess <- paste("there are no relevant columns in")
            ## should you wish further identification of the problem
            ## table, add these into the paste:
            ##, get("pdf", envir = parent.frame(n = 2)), ": ",
            ## get("tbl_ind", envir = parent.frame(n = 2)), "?")
            stop(mess)
        }

        ## get a list of all the column names that matched:
        all_cols_names <- unlist(unique(
            str_comparisons %>%
            filter(!is.na(lens)) %>%
            select(words)))
        
    } else {
        ## if there's an exact match, then no % can be present
        all_cols_names <- unique(str_comparisons$words[str_comparisons$bools])
        relevant_cols_names <- all_cols_names
    }

    ## get the ranges of each group (accounting for the right-shift
    ## in the raw data)
    current_grp <- NA
    col_grps <- c()
    col_range <- 1 + offset : (1 + col_count)

    ## for loop for clarity's sake:
    for (word in str_comparisons$words) {
        if (is.na(word)){
            col_grps <- c(col_grps, current_grp)
        } else {
            current_grp <- word
            col_grps <- c(col_grps, current_grp)
        }
    }

    col_ranges <- llply(all_cols_names, function(x){
        intersect(col_range - 1, which(col_grps == x) - 1)
    })
    
    names(col_ranges) <- all_cols_names

    attr(col_ranges, "relevantCols") <- relevant_cols_names
    col_ranges

}

ehf$getColCount <- function(file, t_index){
    ## count number of necessary columns:
    header <- unlist(strsplit(file[[1]][t_index$start], ";"))
    sum(sapply(header, function(x) x != "" ))
}

ehf$getUnitLine <- function(file, unit_index, t_index, match_strings){

    ## quick fix to remove disruptive notes left in the data
    ## (hopefully they will be removed permanently at some point)

    potential_unit_lines <- file[[1]][unit_index:(t_index$start - 1)]
    edit_lines <- gsub("Do not sum cumulative data", "", potential_unit_lines)
    edit_lines <- gsub("Note: Should data for \"ACP adults\" in Table 1 be combined with data for \"ACP nymphs\" in table 2?", "", edit_lines)
    
    date_regex <- "[0-9]{1,2}-[a-zA-Z]{3}-[0-9]{2,4}"
    include_lines <- sapply(edit_lines, function(x){
        x <- gsub(date_regex, "", x)
        as.logical(length(hf$stringIntersect(x, match_strings)))
        ##(length(hf$stringIntersect(x, match_strings)))
    })

    ## select the included lines
    unit_lines <- potential_unit_lines[include_lines]

    ## select the 'lowest down' included line (it includes the highest
    ## granularity of table division)
    unit_line <- unlist(strsplit(unit_lines[length(unit_lines)], ";"))

    ## just in case "table XYZ" or some other crap is in there
    unit_line[1:2] <- ""
    unit_line
}

## find any columns marked as pre-treatment
ehf$getPretreatCols <- function(file, unit_index, t_index){

    range <- unit_index:t_index$start
    strings <- c("pretreat", "PT", "DPT",
                 "pre-treat", "Pretreat", "Pre-treat", ";0 DAT")

    ## search for strings similar to pretreat
    pt_rows <- hf$mgrep(strings,
                     file[[1]][range])

    ## if there are no rows found, return na
    if (length(pt_rows) == 0){ return(NA) }
    
    ## if multiple are returned, we really only need one:
    ## (why is that?)
    pt_row <- file[[1]][range[pt_rows[1]]]

    ## convert ";0 DAT" to ";pretreat" so we can split (then split)
    pretreat_row <- unlist(strsplit(gsub(";0 DAT", ";pretreat", pt_row), ";"))

    ## don't forget the offset:
    hf$mgrep(strings, pretreat_row) - 1
}

### MAIN WRAPPER FUNCTION:

## @Function:     getRawResponseTable
## @Param:        row - a row from the cpyr df
## @Description:  uses xlsx, readLines to find the raw table in excel that
##                contains the relevant data, then excises it into an R df.
##                (I might keep these in an additional column in the cpyr df.)
ehf$getResponseTables <- function(row){

    ## assemble all the row info that will allow us to identify
    ## the correct table
    pdf <- row$PDF.file.name
    tbl_ind <- row$Source..Fig.or.Table.number.
    unit_id <- row$Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf..
    insect_id <- row$Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.
    loc <- row$Locality

    ## some exceptional cases
    if (is.na(unit_id)){
        ## if the unit_id is missing, we'll have to use the next best
        ## indicator
        unit_id <- row$Life.stage.tested..if.stated..egg..larva..pupa..adult.
    }
    if (grepl("%", unit_id)){
        ## the units are in percentage (bad)
        mess <- paste(pdf, ":", tbl_ind, ":", unit_id,
                      "isn't appropriate", sep = " ")
        ##write(mess, file = "error.txt", append = TRUE)
        stop(mess)
    }
    
    ## select the proper file based on the pdf's name
    file <- try(files[unlist(llply(pdf_locs_in_files, function(x){
        length(x[[pdf]]) != 0
    }))])
    
    
    ## use hierarchical information to find the dataset:
    block_range <- ehf$getBlockRange(file, pdf)

    ## do the same to find the table's range:
    tbl_range <- ehf$getTableRange(file, tbl_ind, block_range)

    ## a table range of 1 value would indicate a problem:
    if (length(tbl_range) == 1){
        mess <- paste(pdf, ":", tbl_ind, "not found \n", sep = " ")
        ##write(mess, file = "error.txt", append = TRUE)
        stop(mess)
    }

    ## get the tables indices
    t_indices <- ehf$getTableContentRanges(file, tbl_range) ## df(start, end)
    ## get the index of the pest unit
    unit_index <- ehf$getUnitIndex(file, tbl_range, unit_id, loc, insect_id)

    ## account for some erroneous cases
    if (unit_index %in% t_indices$start || unit_index %in% t_indices$end){
        mess <- paste("Cannot find unit index in \n", pdf, tbl_ind, sep = " ")
        ##write(mess, file = "error.txt", append = TRUE)
        stop(mess)
    }

    ## get the top of the data 
    top_of_data <- min(t_indices$start[t_indices$start > unit_index])

    if(is.infinite(top_of_data)){
        stop(paste(pdf, tbl_ind, "is inappropriate", sep = " "))
    }
    
    t_index <- filter(t_indices, start == top_of_data)
    ## check if we want to look at data:

    ## I marked any 'appropriate' tables with my name,
    ## so if "Yoni" doesn't appear, but the table has been selected
    ## let the user know it won't be used
    if (!any(grepl("Yoni", file[[1]][t_index$start:t_index$end], ignore.case = TRUE))){
        mess <- paste("pdf: ", pdf, "table:", tbl_ind,
                      "is not appropriate \n", sep = " ")
        ## write(mess, file = "error.txt", append = TRUE)
        stop(mess)
    }
    
    ## figure out which columns are relevant to the row-query
    colRanges <- ehf$getUnitColumnRange(file, unit_index, t_index,
                                        loc, unit_id, insect_id)

    ## extract the table
    tble <- openxlsx::read.xlsx(gsub(".csv", ".xlsx", names(file)),
                      sheet = 1,
                      ##rowIndex = t_index$start:t_index$end,
                      rows = t_index$start:t_index$end,
                      ##colIndex = colRanges$columns,
                      cols = colRanges$columns)

    ## give it some useful attributes
    attr(tble, "info") <- colRanges[2:5]
    names(tble) <- make.names(names = names(tble), unique = TRUE, allow_ = TRUE)
    tble
}

############################################################
############################################################
