############################################################
##              various looks at the data                 ##
############################################################

##____________________Libraries___________________________##

library(ggplot2)
library(dplyr)
library(plyr)
library(gridExtra)
library(lme4)

##_______________set working directory____________________##

setwd("/Users/Yoni/Documents/ZhangLab")

##__________________set options___________________________##

options(dplyr.width = Inf)

##_____________source: data dicts functions_______________##

source("R/assemble_&_massage_data.R")
## Gives: dat, ins_table, response_tables,
## & response_tables_lookup
source("R/helper_functions.R")
## Gives: hf
source("R/table_operations.R")
## Gives: to

##_________________Efficacy by Pest_______________________##

pest_plots <- new.env()
plots <- dlply(ins_table,.(Pest), function(d){
    pest <- unique(d$Pest)
    q <- ggplot(d, aes(x = AI1, y = PercControl)) +
        geom_boxplot() +
            ggtitle(pest)
    assign(eval(pest), q, envir = pest_plots)
}, .progress = "text")

##_______________________Counts___________________________##

ai1_count <-
    ins_table %>%
    group_by(AI1) %>%
    dplyr::summarise(count = n())

ai2_count <-
    ins_table %>%
    group_by(AI2) %>%
    dplyr::summarise(count = n())

pest_count <- 
    ins_table %>%
    group_by(Pest) %>%
    dplyr::summarise(
        count = n(),
        active_ingr1= length(unique(AI1)))

##______________________Variance by PDF___________________##

pdf_var <-
    ins_table %>%
    group_by(V1) %>%
    filter(Pesticide != "UTC") %>%
    do(tmpfn(.))##  %>%
    ## dplyr::summarise(
    ##     Mean = mean(PercControl, na.rm = TRUE),
    ##     Var = var(PercControl, na.rm = TRUE),
    ##     size = n(),
    ##     CpyrEff2 = unique(CpyrEff)
    ##     ) %>% arrange(desc(Mean))

##limits <- aes(ymax = Mean + Var, ymin = Mean - Var)
ggplot(pdf_var, aes(V1, PercControl, color = CpyrEff)) + geom_boxplot()

tmpfn <- function(.data){
    i <- grep("chlorpyrifos", .data$AI1)
    if(length(i) == 0){
        .data['CpyrEff'] <- "Absent"
    } else {
        j <- which(.data$PercControl == min(.data$PercControl))
        .data['CpyrEff'] <- ifelse(any(i %in% j), "True", "False")
    }
    .data
}

tmp <- ins_table %>%
    group_by(V1) %>%
    dplyr::summarise(VarPC = var(PercControl, na.rm = TRUE),
              VarID = var(insectDays, na.rm = TRUE))

ggplot(tmp, aes(x = V1, y = VarPC)) + geom_point(size = 2)

##______________________PercControl > 1____________________##

tmp <- ins_table %>% filter(PercControl > 1)

compToTable <- function(i){
    id <- unique(tmp$V1)[i]
    cides <- tmp %>% filter(V1 == id)
    tbl <- response_tables[[id]]
    print(cides)
    print(tbl)
}


##___________________Distr of PercControl____________________##

tmp <- ins_table %>% filter(Pesticide != "UTC" )
ggplot(tmp, aes(x = PercControl)) + geom_density()

##____________________PercControl vs. AI1____________________##

tmp <- ins_table %>%
    filter(is.na(MPNkey) & is.na(AI2)) %>%
    mutate(AIabbrev =  substr(AI1, 1, 6))

ggplot(tmp, aes( x = AIabbrev, y = PercControl)) +
    geom_point(size = 2) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0))

##__________________insectDays vs PercControl_________________##

tmp <- ins_table %>% filter(Pesticide != "UTC" )
ggplot(tmp, aes(x = insectDays, y = PercControl, color = V1)) +
    geom_point(size = 2)

i <- which.max(ins_table$insectDays)
response_tables[[ins_table[i,]$V1]]

to$tableOperation(response_tables[[ins_table[i,]$V1]],"plot")

##_____________________Crop Effects?___________________________##
tmp <- ins_table %>%
    filter(is.na(MPNkey) & !is.na(AI1))##  %>%
    ## group_by(Crop) %>%
    ## dplyr::summarise(count = n())

ggplot(tmp, aes(x = Crop, y = LnR)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0))

## Given the count's and the overlap of the box plots, I'd
## say it's worth ignoring crop effects for now.

##_____________________Locality EFF?___________________________##


tmp <- ins_table %>%
    filter(is.na(MPNkey) & !is.na(AI1))

ggplot(tmp, aes(x = standardLocs, y = LnR, color = Pest)) +
    geom_point(size = 2) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())  +
    facet_wrap(~ Pest)

##_____________________Study Duration__________________________##

tmp <- ins_table %>%
    group_by(V1) %>%
    dplyr::summarise(stdyDr = unique(studyDuration),
                     avPC = mean(PercControl, na.rm = TRUE),
                     pdf = unique(PDF.file.name))

ggplot(tmp, aes(x = stdyDr)) + geom_density()

ggplot(tmp, aes(x = stdyDr, y = avPC)) + geom_point(size = 2)

ggplot(ins_table, aes(x = studyDuration, y = LnR, color = V1)) + geom_point(size = 2)

ggplot(tmp, aes(x = studyDuration, y = PercControl)) + geom_point(size = 2)

##_________________________Combs_______________________________##
cmbs <- ins_table %>%
    group_by(Pest, Crop) %>%
    dplyr::summarise(combs =
                     choose(length(unique(Pesticide.commercial.name)), 2))

##_________________________Rates?______________________________##

## can I do a regression with rates to  "cancel" them out?

tmp <- ins_table %>%
    group_by(Pesticide.commercial.name) %>%
    dplyr::summarise(
        count = length(unique(Application.rate))
        )

tmp <- ins_table %>%
    group_by(AI1) %>%
    dplyr::summarise(
        count = length(unique(AIperc1))
        )

tmp <- ins_table %>% filter(AI1 == "chlorpyrifos" &
                            is.na(MPNkey) & is.na(AI2))

a <- tmp %>%
    group_by(Pest) %>%
    dplyr::summarise(count = n()) %>%
    filter(count >= 17)

tmp <- tmp %>% filter( Pest %in% i$Pest)

ggplot(tmp, aes(x = AILbsAcre1, y = LnR1, color = Pest)) + geom_point(size = 2) + facet_wrap(~Locality)

## check out how the lbs per acre affect the response:
tmp <- ins_table %>%
    filter(is.na(MPNkey) &
           Pesticide != "UTC" &
           is.na(AI2) &
           !is.na(AI1) &
           studyDuration < 100
           )

a <- tmp %>%
    group_by(AI1) %>%
    dplyr::summarise(count = n()) %>%
    filter(count > 15)

tmp <- tmp %>% filter(AI1 %in% a$AI1)

ggplot(tmp, aes(x = AILbsAcre1, y = LnR1)) + geom_point()

## It looks like, at low lbdage (regardless of AI) there is enormous variation
## in the AI's effect,  while as the lbdage increases, the variation begins to
## level of (applications perform more consistently). Let's look at the same
## plot but faceted for each AI:

ggplot(tmp, aes(x = AILbsAcre1, y = LnR1)) + geom_point() + facet_wrap(~AI1, scales = "free")

##_________________________V1 vs LnR____________________________##

tmp <- ins_table %>%
    filter(is.na(MPNkey) & Pesticide != "UTC")

ggplot(tmp, aes(x = V1, y = LnR1)) +
    geom_boxplot() +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())

## is this heterogeneity due to pesticides used or experimental
## procedure?

tmp <- ins_table %>% filter(AI1 == "chlorpyrifos" &
                          is.na(MPNkey) &
                            is.na(AI2) &
                            Pest %in% c("BAA", "AW", "PA"))

ggplot(tmp, aes(x = V1, y = LnR1, color = as.numeric(Application.rate))) +
    geom_point() +
    facet_wrap( ~Pest) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())

##__________________Variance of Control_________________________##

tmp <- ldply(unique(ins_table$V1), function(v1){
    tab <- response_tables[[v1]]
    tab <- to$tableOperation(tab, "ctrlVar")
    data.frame(V1 = v1, ctrlVar = unique(tab$ctrlVar),
               stringsAsFactors = FALSE)
})

ggplot(tmp, aes(x = V1, y = ctrlVar)) +
    geom_point(size = 2) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())

##__________________Slope of Control___________________________##


## use normal distribution to assign a weight to each point
## based on slope of the ctrl.
ggplot(tmp, aes(x = ctrlSlope, y = dnorm(ctrlSlope, 0, sqrt(1)))) +
    geom_point(size = 2)

##_____________________Compare LnR and LnR1_____________________##
tmp <- ins_table %>% filter(Pesticide != "UTC")
grid.arrange(ggplot(tmp, aes(x = LnR)) + geom_density(),
             ggplot(tmp, aes(x = LnR1)) + geom_density())

##______________Sample Size??____________________________________##
i <- grep("[0-9]", ins_table$Pest.units..e.g....percent.eggs.hatched..or..larvae.per.leaf.., value = TRUE)

##________________________Pre-Treatment__________________________##

tmp <- response_tables_lookup %>% filter(!is.na(V1))

bools <- unlist(llply(tmp$V1, function(hash){
    !all(is.na(attr(response_tables[[hash]], "info")$pretreatCols))
}))

ptHet <- ldply(tmp$V1[bools], function(hash){
    tb <- response_tables[[hash]]
    ptcols <- attr(tb, "info")$pretreatCols
    ptcols <- data.frame(tb[, ptcols])
    variances <- unlist(alply(ptcols, 2, var, na.rm = TRUE))
    data.frame(variance = variances, id = hash, stringsAsFactors = FALSE)
})

##_____________________MulTrt___________________________________## 
cpy <- ins_table %>% filter(AI1 == "chlorpyrifos")

numAI <- unlist(alply(cpy, 1, function(x){
	sum(!is.na(c(x$AI1, x$AI2, x$MPNkey)))
}))

##_____________________MISC_____________________________________##

tmp <- ins_table %>% filter(!is.na(AI1) &
                            is.na(AI2) &
                            is.na(MPNkey) &
                            studyDuration < 100 &
                            AILbsAcre1 < 10)

ggplot(tmp, aes(x = AILbsAcre1, y = studyDuration, color = LnR1)) +
    geom_point()

ggplot(tmp, aes(x = AILbsAcre1, y = LnR1)) +
    geom_point()

i <- tmp %>%
    group_by(AI1) %>%
    dplyr::summarise(count = n()) %>%
    arrange(desc(count)) %>%
    filter(count > 19)

i <- tmp %>% filter(AI1 %in% i$AI1)

do.call(grid.arrange, llply(unique(i$AI1), function(ai){
    tmp <- i %>% filter(AI1 == ai)
    ggplot(tmp, aes(x = as.factor(AILbsAcre1), y = LnR1)) +
        geom_boxplot() +
            ggtitle(ai)
}))

ggplot(i, aes(x = AILbsAcre1, y = LnR1)) +
    geom_point() +
    facet_wrap(~AI1)

i <- tmp %>% filter(AILbsAcre1 > 10)


