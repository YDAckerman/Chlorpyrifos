library(plyr)
library(dplyr)
library(reshape2)

source("~/Documents/Coding/R/R_convenience/helper_functions.R")

df <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Data-Jess-Nic-09222014prf.csv",
               stringsAsFactors = FALSE)

df1 <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Data-neonic-Jess10132014prf.csv",
               stringsAsFactors = FALSE)

df2 <- read.csv("~/Dropbox/ZhangLabData/ExcelData/Data-Katy-Nic-08062013prf.csv",
               stringsAsFactors = FALSE)

df <- df %>% filter(PDF.file.name != "AMT30-D06")

## make var name more convenient
df <- df %>% rename(dates_checked = Date.s..effect.is.measured..if.DAT.for..days.after.treatment..include.actual.date.if.possible..if.the.measurement.date.is.prior.to.application.date..precount.)

## CAREFUL, DF2/1 have a utc column...
df2 <- df2 %>% rename(dates_checked = Date.effect.is.measured..or.WAT..etc..,
                      Pesticide.usage = Application.rate,
                      Pesticide.unit..if.not.specified..per.acre. = Appl.rate.units,
                      Pest..common.name..acronym...scientific.name. = Pest..as..common.name..scientific.name...if.both.given..if.not.just.enter.which.is.stated.in.article.,
                      Pest.units = Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf..,
                      StGr = T.StGr,
                      Measurement.results = Treat
                      ) %>%
    mutate(Adjuvant.Surfactant = NA,
           A.S.appl.rate = NA)
df1 <- df1 %>% rename(PDF.file.name = Pdf, Source..Fig.or.Table.number. = Source,
                      Pest..common.name..acronym...scientific.name. = PestName,
                      Life.stage.tested..if.stated..egg..larva..pupa..adult. = LifeStage,
                      Pest.units = DamageUnit,
                      Pesticide.commercial.name = PesticideName,
                      Pesticide.usage = Appl.rate,
                      Treatment.dates = TreatDate,
                      Pesticide.unit..if.not.specified..per.acre. = Appl.rate.units,
                      Measurement.results = MeasResult,
                      Adjuvant.Surfactant = AdjvName,
                      A.S.appl.rate = Adjuv.rate,
                      Multiple.product.numbers = Multiple.prod.no,
                      dates_checked = MeasDate,
                      U.StGr = UTC.StGr
    )


## date fixes
i <- which(df2$Treatment.dates == "4 Jul (253 DD from 2nd biofix) ")
df2$Treatment.dates[i] <- "4 Jul 2003 (253 DD from 2nd biofix) "

## for testing
## tmp_df1 <- df %>% filter(PDF.file.name == "AMT30-A34" & Source..Fig.or.Table.number. == "Table 3")
## tmp_df2 <- df %>% filter(PDF.file.name == "AMT31-D16")
## .data <- df2 %>% filter(PDF.file.name == "AMT29-E16" &
##                         Source..Fig.or.Table.number. == "Table 1 ",
##                         Pest..common.name..acronym...scientific.name. == "Whitefly: Bemisia argentifolii Bellows & Perring",
##                         Life.stage.tested..if.stated..egg..larva..pupa..adult. == "Adult",
##                         Pest.units == "Adult whitefly/melon leaf")


f_tmp <- function(.data){
    if(any(hf$mgrepl(c("%", "plants damaged",
                       "proportion", "mortality",
                       "season", "insects/20 leaves", "percent"),
                     .data$Pest.units, ignore.case = TRUE)) | 
                     unique(.data$PDF.file.name) %in% c("AMT34-D17", "AMT34-D18", "AMT34-A14", "AMT34-A24")){
        .data <- .data %>%
            filter(Pesticide.commercial.name != "UTC")
        if(is.null(.data$UTC)){
            .data$UTC <- NA
        }
        .data
    } else {
        writeTabl(.data)
        dates <- unique(.data$dates_checked)
        .data %>% filter(dates_checked == dates[1]) %>%
            filter(Pesticide.commercial.name != "UTC") %>%
                select(-dates_checked, -Measurement.results, -StGr) %>%
                    mutate(UTC = NA, Measurement.results = NA, StGr = NA)
    }
}

writeTabl <- function(.data){

    ## first lets get dates in order:
    .data$dates_checked  <- hf$fixDates(.data$dates_checked)
    t_dates <- hf$fixDates(unlist(strsplit(unique(.data$Treatment.dates), ",")))

    .data$dates_checked <- sapply(.data$dates_checked, function(x){
        if(all(x < t_dates)){
            paste0("pt.", x)
        } else {
            as.character(x)
        }
    })
    
    dates <- unique(.data$dates_checked)

    
    ## now get the mpns we want (there was an error in the data set that necessitates this)
    mpns <- .data %>%
        filter(dates_checked == dates[1]) %>% select(Multiple.product.numbers)

    ## make the filename
    filename <- with(.data, paste(hf$trim(c(unique(PDF.file.name), unique(Source..Fig.or.Table.number.),
                                  unique(Pest.units), ".csv")), collapse = "_"))
    filename <- hf$mgsub(c(":", "/"),c("", " per "), filename)

    ## pull out the section of the data that we want to work with
    tmp <- .data %>%
        select(Pesticide.commercial.name, Pesticide.usage,
               Pesticide.unit..if.not.specified..per.acre.,
               Multiple.product.numbers, Adjuvant.Surfactant,
               A.S.appl.rate, dates_checked, Treatment.dates,
               Measurement.results, StGr) %>%
                   arrange(dates_checked)

    ## create column to maintain the original order:
    tmp$Order <- rep(1:(nrow(tmp) / length(unique(dates))), length(unique(dates)))
    
    ## if there are mpns, replace them with the corrected versions
    if(any(!is.na(mpns$Multiple.product.numbers))){
        tmp$Multiple.product.numbers <- rep(mpns$Multiple.product.numbers, length(unique(dates)))
    }

    ## create a new column for casting
    tmp <- tmp %>%
        mutate(value = paste(Measurement.results, StGr, sep = " ")) %>%
            select(-Measurement.results, -StGr) 

    ## stupid fixes
    if(unique(.data$PDF.file.name) == "AMT34-E30"){
        tmp <- tmp %>% filter(!(Pesticide.commercial.name == "Coragen 1.67SC" & Pesticide.usage == 7.0))
    }

    ## cast the data
    tmp <- dcast(tmp, Order + Pesticide.commercial.name + Pesticide.usage + Pesticide.unit..if.not.specified..per.acre. + 
                 Multiple.product.numbers + Adjuvant.Surfactant + A.S.appl.rate ~ dates_checked)

    ## stupid debugging for stupid problems
    
    ## print(unique(.data$PDF.file.name))
    ## print(unique(.data$Source..Fig.or.Table.number.))
    ## print(unique(.data$Pest..common.name..acronym...scientific.name.))
    ## print(unique(.data$Life.stage.tested..if.stated..egg..larva..pupa..adult.))
    ## print(unique(.data$Pest.units))

    ## create an empty df for the utc data
    if(!is.null(.data$UTC)){
        utc <- .data %>% select(UTC, U.StGr, dates_checked)
        utc <- utc %>% filter(!is.na(UTC)) %>% distinct(UTC, U.StGr, dates_checked)
        cols_utc <- unlist(lapply(utc$dates_checked, function(x){
            c(x, paste("StGr", x, sep = "."))
        }))
        d_utc <- as.data.frame(matrix(rep(NA, length(cols_utc)), ncol = length(cols_utc), nrow = 1))
        colnames(d_utc) <- cols_utc
    } else {
        utc <- 0
    }

    ## loop through each date and alter tmp accordingly
    for(date in dates){
        ## pull out the values of that date column
        i <- which(colnames(tmp) == date)
        vals <- tmp[, date]

        ## remove the column
        tmp <- tmp[,-i]
        tmp[, date] <- as.numeric(gsub("[a-zA-Z]","", vals))
        tmp[, paste0("StGr.", date)] <- gsub("[^a-zA-Z]","", vals)
        if(is.data.frame(utc)){
            dat_utc <- utc %>% filter(dates_checked == date)
            d_utc[,date] <- dat_utc$UTC
            d_utc[,paste("StGr", date, sep = ".")] <- dat_utc$U.StGr
            d_utc$Order <- max(tmp$Order) + 1
        }
    }
    if(is.data.frame(utc)){
        d_utc$Pesticide.commercial.name <- "UTC"
        tmp <- rbind.fill(tmp, d_utc)
    }
    
    tmp <- tmp %>% arrange(Order) %>% select(-Order)
    write.csv(tmp, file = paste0("~/Desktop/oldNeonicData_YA_converts_CSV/", filename), row.names = FALSE)
    return(1)
}

df_new <- df %>%
    group_by(PDF.file.name, Source..Fig.or.Table.number.,
             Pest..common.name..acronym...scientific.name.,
             Life.stage.tested..if.stated..egg..larva..pupa..adult.,
             Pest.units)  %>%
    do(f_tmp(.))

df1_new <- df1 %>%
    group_by(PDF.file.name, Source..Fig.or.Table.number.,
             Pest..common.name..acronym...scientific.name.,
             Life.stage.tested..if.stated..egg..larva..pupa..adult.,
             Pest.units)  %>%
    do(f_tmp(.))

df2_new <- df2 %>%
    group_by(PDF.file.name, Source..Fig.or.Table.number.,
             Pest..common.name..acronym...scientific.name.,
             Life.stage.tested..if.stated..egg..larva..pupa..adult.,
             Pest.units)  %>%
    do(f_tmp(.))

write.csv(df_new, file = "~/Desktop/oldNeonicData_YA_converts_CSV/Data-Jess-Nic-09222014prf_new.csv", row.names = FALSE)
write.csv(df1_new, file = "~/Desktop/oldNeonicData_YA_converts_CSV/Data-neonic-Jess10132014prf_new.csv", row.names = FALSE)
write.csv(df2_new, file = "~/Desktop/oldNeonicData_YA_converts_CSV/Data-Katy-Nic-08062013prf_new.csv", row.names = FALSE)

