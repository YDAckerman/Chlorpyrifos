############################################################
##          documentation of Meta-analysis                ##
############################################################

##____________________Libraries___________________________##

library(ggplot2)
library(dplyr)
library(plyr)
library(gridExtra)
library(lme4)
library(MCMCglmm)
library(orthopolynom)
library(stringr)

##_______________set working directory____________________##

##setwd("/Users/Yoni/Documents/ZhangLab")

##__________________set options___________________________##

options(dplyr.width = Inf)

##_____________source: data dicts functions_______________##

#source("R/assemble_&_massage_data.R")
load("~/Dropbox/ZhangLabData/cpyrMAdata.rda")
## Gives: dat, ins_table, response_tables,
## & response_tables_lookup
source("~/Documents/Coding/R/R_convenience/helper_functions.R")
## Gives: hf (helper functions)
source("~/Documents/ZhangLab/R/Chlorpyrifos/table_operations.R")
## Gives: to (table operations)

##______________________Intro_____________________________##
## We have amassed data from 114 pesticide efficacy studies in
## order to quantify and compare efficacy of individual
## active ingredients. The studies took
## place across 12 states (CA, AR, OK, TX, OH, KS, WA, AZ, MD,
## LA, FL, and VA) and together
## tested approximately 354 different products, which themselves
## used a combination of 1, 2, or 3 different active
## ingredients out of 141 total tested.

## The variation in effect between active ingredients
## is now our main interest, however measuring it is not straightforward.
## In order to compare trial's effects across trials and across studies, we
## needed a way to quantify in a single number the effectiveness of that trial.
## Towards this goal, we first calculated insect days, or the area
## under the study-long curve of insect density. We could then divide each
## trial's insect-day count by that of the experimental control, giving us a
## ratio describing the percent decrease in insect days in the trial versus
## the control. Finally, in order to achieve some degree of distributional
## normality, we took the natural log of this ratio. In our final effect,
## negative values suggest a trail that reduced the overall insect-day count,
## a value of 0 means there was no difference between that trial and the control,
## and a positive value means that trial experience more insect-days than the
## control.

## Of the 114 studies, 73 contained the necessary information to calculate
## 'insect days', and hence the overall effect, for each pesticide trial. 

## Furthermore, variation in this sample effect
## is complicated by variations in: the pest targeted (of 53 species);
## the rate at which the pesticide was applied; the adjuvant/surfactant
## combined with the pesticide; the location of the trial to name a few.
## In addition, study quality is a potentially confounding factor.
## Of the individual experiments reported (176),
## 44 did not report a measure of standard
## error, 8 reported an SEM, and 124 reported statistical groups from which we
## could deduce a lower and/or upper bound of the SEM. These we will use to
## form weights in our anova (or, should we include rate as a variable,
## regression).

## We are also exploring other measures of study quality based on our knowledge
## of the experiments structure. These include measuring the slope of the insect
## density curve in the experiment's control, the variation in the control, and the
## sample size (when given) of each experiment.
## We hope these statistics will paint a picture (??)

##_____________________Begin Workflow_________________________##
#################################################################


#################################################################
## last minute adjustments

## Let's pare down ins_table into only those trials that used a
## single AI (thus control is not included) and lasted fewer than
## 50 days.

## with the introduction of the new data comes a few new things to
## keep in mind. For instance: multiple active ingredients now have,
## in some cases, multiple rows:
a <- ins_table %>%
    group_by(Pesticide) %>%
    dplyr::summarise(num_ai = length(unique(AI))) %>%
    filter(num_ai <= 1)

ai_thresh <- ins_table %>%
    group_by(V1) %>%
    dplyr::summarise(num_ai = length(na.omit(unique(AI)))) %>%
    filter(num_ai > 2)
    

utc_thresh <- ins_table %>%
    filter(Pesticide == "UTC" & insectDays > 100) %>%
    select(V1) %>%
    distinct(V1)
    
tmp <- ins_table %>%
    filter(is.na(MPNkey) &
           Pesticide != "UTC" &
           !is.na(AI) &
           !is.na(AILbsAcre) &
           !is.infinite(LnR1) &
           studyDuration < 50 &
           Pesticide %in% a$Pesticide &
           V1 %in% utc_thresh$V1 &
           V1 %in% ai_thresh$V1
           )

a <- ins_table %>%
    filter(is.na(MPNkey)) %>%
    group_by(Pest, AI) %>%
    dplyr::summarise(num = n()) %>%
    filter(num > 10 & !is.na(AI))

tmp <- semi_join(tmp, a, by = c("Pest", "AI"))
tmp <- as.data.frame(tmp)

##____________________relabel AI__________________________##
## as per the Belova et al. paper, to address the application
## rate problem, we're going to bin the rates based on standard
## use rates for each AI, then relabel each treatment (l,s,h) depending
## on which bin its ai/rate combination falls into

b <- mlply(tmp %>% select(AI, AILbsAcre), function(AI, AILbsAcre){
    ai <- AI
    (ranges %>%
        filter(AI == ai) %>%
            transmute(
                l = AILbsAcre < minLbs_ai_a,
                h = AILbsAcre > maxLbs_ai_a
                ) %>% transmute(
                    label = paste0(ai, "-", c("l", "s", "h")[c(l, !(l | h), h)])
                    ))$label
})

tmp$AI_s <- unname(unlist(b))

## let's quickly look at which location, ai, pest triples will
## be appropriate for meta-analysis:

b <- tmp %>%
    dplyr::group_by(Pest, AI_s) %>%
    dplyr::summarise(count1 = n()) %>%
    ##ungroup() %>%
    dplyr::arrange(desc(count1))

## print.data.frame(b)

## the above table indicates that AC, AW, BAA, BAW, and
## PA are all appropriate for MA (So long as we filter
## by count1 >= 10). This gives:
b <- b %>% filter(count1 >= 10 & Pest %in% c("AC", "AW", "BAA", "BAW", "PA"))

tmp <- semi_join(tmp, b, by = c("Pest", "AI_s"))

##############################################################################
## Analysis

#### Without Errors

### MCMC

## 
## dsetsMCMC <- llply(unique(tmp$Pest), failwith(NA, function(pest){
##     tmp_pest <- tmp %>% filter(Pest == pest) ## teehee
##     mcmcMod <- MCMCglmm(LnR1 ~ AI_s - 1,
##                         random = ~ V1,
##                         family = "gaussian",
##                         data = tmp_pest,
##                         prior = prior3,
##                         nitt = 20000,
##                         burnin = 10000, verbose = FALSE)
##     means <- colMeans(mcmcMod$Sol)
##     testd <- data.frame(AI = gsub("AI_s", "", names(means)),
##                         post.mean = means,
##                         HPDinterval(mcmcMod$Sol),
##                         Pest = pest, stringsAsFactors = FALSE
##                         )
##     colnames(testd)[3:4] <- c("lower", "upper")
##     testd
## }), .progress = "text")

## ### LMER

## dsetsLMER <- llply(unique(tmp$Pest), failwith(NA , function(pest){
##     tmp_pest <- tmp %>% filter(Pest == pest) ## teehee
##     ## use a random coefficient for rate, random intercepts for V1
##     ## and PDF, and a fixed effect for AI:
##     ## print(pest)
##     ## print(tmp_pest %>% group_by(AI_s) %>% dplyr::summarise(cnt = n()))
##     ##stop()
##     lmerMod <- glmer(LnR1 ~
##                     ##(Rate - 1| AI) +
##                     (1 | V1) +
##                     ##(1 | PDF.file.name) +
##                     AI_s - 1,
##                     tmp_pest)
##     means <- fixef(lmerMod)
##     intervals <- (confint(lmerMod, method = "boot"))[names(means),]
##     testd <- data.frame(AI= gsub("AI_s", "", names(means)),
##                post.mean = means,
##                intervals,
##                Pest = pest)
##     colnames(testd)[3:4] <- c("lower", "upper")
##     testd
## }), .progress = "text")

## ## vis

## dsets_combined <- llply(1:5, function(x){
##     rbind(
##     dsetsMCMC[[x]] %>% mutate(set = "mcmc"),
##     dsetsLMER[[x]] %>% mutate(set = "lmer")
##     )
## })

## grobs_unweighted <- llply(dsets_combined, failwith(NA, function(c_d){
##     if(class(c_d) != "data.frame"){ return(NULL) }
##     ##stop()
##     pest <-  unique(c_d$Pest)
##     c_d$AI_s <- unlist(
##         llply(c_d$AI,
##               function(x) paste(str_sub(x, c(1,-2), c(6,-1)), collapse = "")))
##     #### print(c_d$AI_s)
##     c_d <- c_d %>% arrange(post.mean)
##     c_d$AI_s <- factor(c_d$AI_s, levels = c("chlorp-s", setdiff(c_d$AI_s, "chlorp-s")))
##     ggplot(c_d, aes(x = AI_s, y = post.mean, color = set)) +
##         geom_point() +
##             geom_pointrange(aes(ymax = upper, ymin = lower)) +
##                 theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
##                     xlab(label = "Active Ingredient") +
##                     ggtitle(pest)
## }))


## g <- do.call(arrangeGrob, grobs_unweighted)
## do.call(grid.arrange, grobs)

#### With error estimates

filter_set <- tmp %>%
    filter(!is.na(SEMestMin) & !is.na(SEMestMax)) %>%
    group_by(Pest, AI_s) %>%
    dplyr::summarise(num = n(), numV1 = length(unique(V1))) %>%
    filter(num > 10 & Pest != "BAA" & Pest != "ACP")

tmp <- semi_join(tmp, filter_set, by = c("Pest", "AI_s"))


## prior (see mcmcMods.r)
prior3 <- list(R = list(V = 1, nu = .002),
              G = list(G1 = list(V = 1, nu = .002)))

dsetsMCMC <- llply(unique(tmp$Pest), failwith(NA, function(pest){
    tmp_pest <- tmp %>% filter(Pest == pest) ## teehee
    mcmcMod <- MCMCglmm(LnR1 ~ AI_s - 1,
                        random = ~ V1,
                        family = "gaussian",
                        data = tmp_pest,
                        prior = prior3,
                        nitt = 20000,
                        burnin = 10000, verbose = FALSE)
    means <- colMeans(mcmcMod$Sol)
    testd <- data.frame(AI = gsub("AI_s", "", names(means)),
                        post.mean = means,
                        HPDinterval(mcmcMod$Sol),
                        Pest = pest, stringsAsFactors = FALSE
                        )
    colnames(testd)[3:4] <- c("lower", "upper")
    testd
}))

### LMER

dsetsLMER <- llply(unique(tmp$Pest), failwith(NA , function(pest){
    tmp_pest <- tmp %>% filter(Pest == pest) ## teehee
    ## use a random coefficient for rate, random intercepts for V1
    ## and PDF, and a fixed effect for AI:
    ## print(pest)
    ## print(tmp_pest %>% group_by(AI_s) %>% dplyr::summarise(cnt = n()))
    ##stop()
    lmerMod <- glmer(LnR1 ~
                    ##(Rate - 1| AI) +
                    (1 | V1) +
                    ##(1 | PDF.file.name) +
                    AI_s - 1,
                    tmp_pest)
    means <- fixef(lmerMod)
    intervals <- (confint(lmerMod, method = "boot"))[names(means),]
    testd <- data.frame(AI= gsub("AI_s", "", names(means)),
               post.mean = means,
               intervals,
               Pest = pest)
    colnames(testd)[3:4] <- c("lower", "upper")
    testd
}))

dsets_combined_w <- llply(1:2, function(x){
    rbind(
    dsetsMCMC[[x]] %>% mutate(set = "mcmc"),
    dsetsLMER[[x]] %>% mutate(set = "lmer")
    )
})

grobs_weighted <- llply(dsets_combined_w, failwith(NA, function(c_d){
    if(class(c_d) != "data.frame"){ return(NULL) }
    ##stop()
    pest <-  unique(c_d$Pest)
    c_d$AI_s <- unlist(
        llply(c_d$AI,
              function(x) paste(str_sub(x, c(1,-2), c(6,-1)), collapse = "")))
    #### print(c_d$AI_s)
    c_d <- c_d %>% arrange(post.mean)
    c_d$AI_s <- factor(c_d$AI_s, levels = c("chlorp-s", setdiff(c_d$AI_s, "chlorp-s")))
    ggplot(c_d, aes(x = AI_s, y = post.mean, color = set)) +
        geom_point() +
            geom_pointrange(aes(ymax = upper, ymin = lower)) +
                theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
                    xlab(label = "Active Ingredient") +
                    ggtitle(pest)
}))

##do.call(grid.arrange, grobs_weighted)

#### ALL data sets:

