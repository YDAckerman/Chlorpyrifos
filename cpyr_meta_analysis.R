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

## Let's pare down ins_table into only those trials that used a
## single AI (thus control is not included) and lasted fewer than
## 100 days.

## with the introduction of the new data comes a few new things to
## keep in mind. For instance: multiple active ingredients now have,
## in some cases, multiple rows:
a <- ins_table %>%
    group_by(Pesticide) %>%
    dplyr::summarise(num_ai = length(unique(AI))) %>%
    filter(num_ai <= 1)

tmp <- ins_table %>%
    filter(is.na(MPNkey) &
           Pesticide != "UTC" &
           !is.na(AI) &
           !is.na(AILbsAcre) &
           !is.infinite(LnR1) &
##           Pest %in% c("PA", "AW") & ## might want to remove in future
           studyDuration < 50 &
           Pesticide %in% a$Pesticide
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

print.data.frame(b)

## the above table indicates that AC, AW, BAA, BAW, and
## PA are all appropriate for MA (So long as we filter
## by count1 >= 10). This gives:
b <- b %>% filter(count1 >= 10 & Pest %in% c("AC", "AW", "BAA", "BAW", "PA"))

tmp <- semi_join(tmp, b, by = c("Pest", "AI_s"))

################################################################################
################################################################################


## tmp <- merge(tmp, a, by = "Pest")

##____________________Weights__________________________##
##                                                     ##

## The estimated and given SEMs needed to be converted into
## log ratio SEMs ... that is why things  looked so strange.

## weights1 <- dnorm(tmp$ctrlSlope, 0, sqrt(1))
## weights2 <- unlist(alply(tmp %>% select(trSEM_LB, trSEM_UB, trSEM), 1,
##                   function(x){
##                       x <- unlist(x)
##                       bool <- !is.na(x) & !is.infinite(x)
##                       if(!any(bool)){
##                           return(NA)
##                       } else {
##                           return(max(x[bool]))
##                       }
##                   }))
## weights3 <- unlist(alply(tmp %>% select(trSEM_LB, trSEM_UB, trSEM), 1,
##                   function(x){
##                       x <- unlist(x)
##                       bool <- !is.na(x) & !is.infinite(x)
##                       if(!any(bool)){
##                           return(NA)
##                       } else {
##                           return(min(x[bool]))
##                       }
##                   }))
## tmp['slopeWeights'] <- weights1
## tmp['SEMestMax'] <- weights2
## tmp['SEMestMin'] <- weights3

## mod1 <- lmer(LnR1 ~ -1 + (1 | PDF.file.name/V1) + AI1, 
##              weights = weights2, tmp)

## mod2 <- lmer(LnR1 ~ -1 + (1 | PDF.file.name/V1) + AI1, tmp)

## grobs <- llply(list(mod1, mod2), function(mod){
##     y <- fixef(mod)
##     x <-  gsub("AI1", "", names(y))
##     err <- confint(mod, method = "boot")
##     err <- err[names(y), ]
##     tst <- data.frame(EffectSize = y, ActiveIngredient = x)
##     tst <- merge(tst, a, by.x = "ActiveIngredient", by.y = "AI1" )
##     title <- any(grepl("AILbsAcre1", formula(mod)))
##     title <- ifelse(title, "WithRate", "WithoutRate")
##     limits <- aes(ymax  = err[, 2], ymin = err[, 1])
##     g2 <- ggplot(tst, aes(x = ActiveIngredient, y = EffectSize)) +
##         geom_point(aes(size = countOfAI1)) +
##             geom_pointrange(limits) +
##                 theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
##                     ggtitle(title)
## })

## do.call(arrangeGrob, grobs)
##________________________##

## use qq plots to determine if normal distribution is appropriate

do.call(grid.arrange, llply(unique(b$AI_s), function(ingredient){
    g <- tmp %>% filter(AI_s == ingredient)
    ## ggplot(g, aes(x = LnR)) + geom_density() + ggtitle(ingredient)
    ggplot(g, aes(sample = LnR)) + stat_qq() + ggtitle(ingredient)
}))

##____________________Naive MCMC_______________________##
prior <- list(R = list(V = 1, nu = .002),
              G = list(G1 = list(V = 1, nu = .002),
                  G2 = list(V = 1, nu = .002)))

mcmcMod <- MCMCglmm(LnR1 ~ AI - 1,
                    mev = weights2, ## make sure to pare to i
                    random = ~ PDF.file.name + V1,
                    family = "gaussian",
                    data = tmp, prior = prior)

intervals <- HPDinterval(mcmcMod$Sol)
means <- colMeans(mcmcMod$Sol)

d <- data.frame(AI = names(means), post.mean = means, intervals)
limits <- aes(ymax = intervals[,1], ymin = intervals[, 2])
ggplot(d, aes(x = AI, y = post.mean)) +
    geom_point() +
    geom_pointrange(limits) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) 

##________________________________________________________
## I suppose I should look at heterogeneity
## sources mentioned above: rate, location, year, pest, author.
## PDF.file.name and V1 account for loc, year, pest, and author.
## Rate, however, I haven't figured out yet. 


##____________________Variation________________________##
##                 in response due                     ##
##                  to study/table                     ##

## let's just look a cpyr and a few of the most commonly occuring pests:
tmp_cpyr <- tmp %>% filter(AI == "chlorpyrifos")

a <- tmp_cpyr %>%
    group_by(Pest) %>%
    dplyr::summarise(count  = n()) %>%
    arrange(desc(count)) %>%
    filter(count >= 5)
tmp_cpyr <- tmp_cpyr %>%
    filter(Pest %in% a$Pest)

## Plot cpyr lbs per acre against the effect. Facet the plot by Insect type
## and color by hash (V1) to see how the relationship (cpyr vs quantity) varies
## between insect species and experiment. 
ggplot(tmp_cpyr, aes(x = AILbsAcre, y = LnR1, color = V1, group = V1)) +
    geom_point() +
    facet_wrap(~Pest) +
    stat_smooth(method = "lm", se = FALSE)

## check out slope lines - they don't appear to vary much (definitely
## some anomalies) within a species! This is what I would expect to see;
## increasing the amount of cpyr reduces each insect density at a
## different rate, but within each insect type, that rate stays pretty
## consistent. Can we assume this to be true across all AI's? 
## The variation from experiments then, is in the y intercept - we can
## consider it a random effect within Pest.

ggplot(tmp_cpyr, aes(x = LnR1)) + geom_density() + facet_wrap(~Pest)

ggplot(tmp_cpyr, aes(x = LnR1)) + geom_density()
## no weights, nor variances included:

pairs <- unique(tmp_cpyr %>% select(Pest, Study = V1, pdf = PDF.file.name))


##___________What's up with so much variation?_________##

tmp_cpyr <- tmp %>% filter(AI == "chlorpyrifos" & Pest == "PA" & AILbsAcre < 4)

ggplot(tmp_cpyr, aes(x = AILbsAcre, y = LnR1, color = V1)) + geom_point()

##_______________GLM______________##
mod <- glm(LnR1 ~ AILbsAcre + V1, tmp_cpyr, family = "gaussian")

coefs <- coefficients(mod)
means <- coefs[3:length(coefs)]
intervals <- confint(mod, method = "boot")[3:length(coefs),]
colnames(intervals) <- c("lower", "upper")

d <- data.frame(Study = gsub("V1", "", names(means)),
                post.mean = means,
                intervals, stringsAsFactors = FALSE)

d <- left_join(d, pairs, by = "Study") %>% arrange(Pest, desc(post.mean))
limits <- aes(ymax = d$upper, ymin = d$lower)

## Effect and 95% confint for each experiment, faceted by Pest
## and colored by study. 
ggplot(d, aes(x = Study, y = post.mean, color = pdf)) +
    geom_point() +
    geom_pointrange(limits) +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
    facet_wrap(~Pest, scale = "free") +
    theme(legend.position = "none")

## density of effects with each pest type:
ggplot(d, aes(x = post.mean)) +
    geom_density() +
    facet_wrap(~Pest, scale = "free")

##_______________MCMC_____________##
prior <- list(R = list(V = 1, nu = .002))
prior3 <- list(R = list(V = 1, nu = .002),
              G = list(G1 = list(V = 1, nu = .002)))

mcmcMod <- MCMCglmm(LnR1 ~ AILbsAcre,
                    random = ~ V1,
                    family = "gaussian",
                    data = tmp_cpyr, prior = prior3, burnin = 10000)

means <- colMeans(mcmcMod$Sol)
intervals <- HPDinterval(mcmcMod$Sol)
d <- data.frame(Variable = names(means),
                post.mean = means,
                intervals, stringsAsFactors = FALSE)

d <- left_join(d, pairs, by = "Study") %>%
    arrange(Pest, desc(post.mean))

limits <- aes(ymax = d$upper, ymin = d$lower)

ggplot(d, aes(x = Variable, y = post.mean)) +
    geom_point() +
    geom_pointrange(limits) ## +
    ##theme(axis.ticks = element_blank(), axis.text.x = element_blank()) ## +
    ##facet_wrap(~Pest, scale = "free")

##____________________Variation________________________##
##               Due to the interaction                ##
##                  of AI and Pest                     ##

b <- tmp %>% group_by(Pest, AI) %>%
    dplyr::summarise(num_rec = n(),
                     num_V1 = length(unique(V1)),
                     num_pdf = length(unique(PDF.file.name))) %>%
    filter(num_rec > 10 & num_V1 > 1 & num_pdf > 1)

tmp1 <- left_join(b, tmp, by = c("AI", "Pest"))

## Plot of Lbs AI vs LnR1 faceted by Pest and AI.
ggplot(tmp1, aes(x = AILbsAcre, y = LnR1, color = V1, group = V1)) +
    geom_point() +
    facet_grid(Pest~AI, scale = "free") +
    stat_smooth(se = FALSE, method = "lm") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(legend.position = "none") +
    ggtitle("Quantity AI vs. Treatment statistic")

## This plot summarizes the problem I have been struggling to name: AI effects
## vary widely from pest to pest and depend, within a single pest, on the
## application rate. I guess the question now is, can I ignore Rate effects?

## Here's the plan:
## "loop" over pest type in tmp perform MCMCglmm analysis using
## AI as fixed effect and PDF.file.name and V1 as random effects
## plot the outputs for each pest. Let's ignore weights for now. ok go!

## 'uninformative' prior (?)
prior1 <- list(R = list(V = 1, nu = .002),
              G = list(G1 = list(V = 1, nu = .002),
                  G2 = list(V = 1, nu = .002)
                  ))
prior2 <- list(R = list(V = 1, nu = .002),
              G = list(G1 = list(V = 1, nu = .002),
                  G2 = list(V = 1, nu = .002),
                  G3 = list(V = 1, nu = .002)))
prior3 <- list(R = list(V = 1, nu = .002),
              G = list(G1 = list(V = 1, nu = .002)))


## see mcmcMods.R for details. Note, none of the following
## include SEM estimates.
dsetsMCMC <- llply(unique(tmp$Pest), failwith(NA, function(pest){
    tmp_pest <- tmp %>% filter(Pest == pest) ## teehee
    mcmcMod <- MCMCglmm(LnR1 ~ AI - 1,
                        random = ~ PDF.file.name + V1,
                        family = "gaussian",
                        data = tmp_pest,
                        prior = prior1,
                        nitt = 20000,
                        burnin = 10000, verbose = FALSE)
    means <- colMeans(mcmcMod$Sol)
    testd <- data.frame(AI = gsub("AI", "", names(means)),
                        post.mean = means,
                        HPDinterval(mcmcMod$Sol),
                        Pest = pest, stringsAsFactors = FALSE
                        )
    colnames(testd)[3:4] <- c("lower", "upper")
    testd
}), .progress = "text")

dsetsLMER <- llply(unique(tmp$Pest), failwith(NA , function(pest){
    tmp_pest <- tmp %>% filter(Pest == pest) ## teehee
    ## use a random coefficient for rate, random intercepts for V1
    ## and PDF, and a fixed effect for AI:
    print(pest)
    print(tmp_pest %>% group_by(AI_s) %>% dplyr::summarise(cnt = n()))
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
}), .progress = "text")

grobs <- llply(dsetsLMER, failwith(NA, function(c_d){
    if(class(c_d) != "data.frame"){ return(NULL) }
    ##stop()
    pest <-  unique(c_d$Pest)
    c_d$AI_s <- unlist(
        llply(c_d$AI,
              function(x) paste(str_sub(x, c(1,-2), c(6,-1)), collapse = "")))
    ##print(c_d$AI_s)
    c_d <- c_d %>% arrange(post.mean)
    c_d$AI_s <- factor(c_d$AI_s, levels = c("chlorp-s", setdiff(c_d$AI_s, "chlorp-s")))
    ggplot(c_d, aes(x = AI_s, y = post.mean)) +
        geom_point() +
            geom_pointrange(aes(ymax = upper, ymin = lower)) +
                theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
                    xlab(label = "Active Ingredient") +
                    ggtitle(pest)
}))

g <- do.call(arrangeGrob, grobs) ##[c(1:7, 9:10)]

##_____________________Changes_________________________##
##               Due to the choice of                  ##
##                     weights                         ##

library(lazyeval)

source("R/mcmcMods.R")

prior <- list(R = list(V = 1, nu = .002),
              G = list(G1 = list(V = 1, nu = .002),
                  G2 = list(V = 1, nu = .002)))

priors <- list(prior1, prior2, prior1, prior3, prior3, prior1)

i <-tmp$SEMestMax
i <- !is.na(i) & !is.infinite(i)

dsetsMCMC <- ldply(c('SEMestMax',
                     'SEMestMin', "None"), failwith(NA, function(w_i){
                         ## choose pest
                         pest <- "AW"
                         ## filter table by pest and i (appropriate sem ests)
                         tmp_pest <- tmp %>%
                             filter(Pest == pest & i)
                         ## run ldply on 1:6
                         ldply(c(1,2,5,6), function(v){
                             ## create model name and use it to get
                             ## the model
                             mod <- paste0("mcmcMod", v)
                             model <- get(mod)
                             ## choose the correct prior (See above)
                             prior <- priors[[v]]
                             ## switch basted on w_i
                             mcmcMod <- switch(w_i, None =
                                               ## no variances
                                               MCMCglmm(
                                                   model$Fixed$formula,
                                                   random = model$Random$formula,
                                                   family = "gaussian",
                                                   data = tmp_pest,
                                                   prior = prior,
                                                   verbose = FALSE,
                                                   nitt = 20000,
                                                   burnin = 10000),
                                               ## yes, please, variances
                                               MCMCglmm(
                                                   model$Fixed$formula,
                                                   mev = unlist(tmp_pest[, w_i]),
                                                   random = model$Random$formula,
                                                   family = "gaussian",
                                                   data = tmp_pest,
                                                   prior = prior,
                                                   verbose = FALSE,
                                                   nitt = 20000,
                                                   burnin = 10000))
                             ## calculate posterior means
                             means <- colMeans(mcmcMod$Sol)
                             ## create data frame containing info
                             testd <- data.frame(AI = gsub("AI1", "",
                                                     names(means)),
                                                 post.mean = means,
                                                 ## confidence inverval
                                                 HPDinterval(mcmcMod$Sol),
                                                 SEMestimate = w_i,
                                                 Model = mod,
                                                 ## DIC score
                                                 DICscore = mcmcMod$DIC,
                                                 stringsAsFactors = FALSE
                                                 )
                             ## remove interaction effects
                             ## if applicable
                             testd <- testd %>% filter(!grepl(":", AI))
                             colnames(testd)[3:4] <- c("lower", "upper")
                             testd
                         }, .inform = TRUE, .progress = "text")
                     }))


## prepare labels - can these be prettier? maybe change carbofuran...
ad_text <- ddply(dsetsMCMC, .(SEMestimate, Model), summarize,
                 dic = paste0(unique(SEMestimate), " DIC: ", round(unique(DICscore), digits = 2)))
ad_text$AI <- "carbofuran"
ad_text$post.mean <- sort(rep(c(.8, 1, 1.2), length(unique(dsetsMCMC$Model))))

## for presentation
modelDesc <- data.frame(Model = paste0("mcmcMod", c(1,2,5,6)), desc = c("Study & Table \n random effects", "Rate Coefficient, Study, & \n Table random effects", "Table random effect", "Table & Rate Coefficient \n random effects"), stringsAsFactors = FALSE)
dsetsMCMC <- merge(dsetsMCMC, modelDesc, by = "Model")

dsetsMCMC$AI <- gsub("chlorpyrifos"," chlorpyrifos", dsetsMCMC$AI)

ad_text <- merge(ad_text, modelDesc, by = "Model")

## plot the results
ggplot(dsetsMCMC, aes(x = AIranked,
                      y = post.mean, color = SEMestimate)) +
    geom_point(alpha = .3) +
    geom_pointrange(aes(ymax = upper, ymin = lower), alpha = .5) +
    facet_grid(~desc, scale =  "free") +
    geom_text(data = ad_text, aes(label = dic), size = 2, color = "black") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    ##theme(axis.title.x = element_blank())
    xlab("Active Ingredient") +
    ylab("Effect Size") +
    ggtitle("Effect of Models and SEM estimates on AI Effects. \n Pest = PA")

## g-to-the-save
ggsave("ModelsVaryingSEMest_PA_6_8_15.pdf")

##____________________Rate_____________________________##
##              Response Curve                         ##


## for presentation
tmp_ <- tmp %>% filter(AI1 != "spinosad" &
                      AI1 != "lambda-cyhalothrin" &
                       Pest != "EAW" & Pest != "AWW" & Pest != "CPA" &
                       !is.na(SEMestMax))

## linReg
eqs <- ddply(tmp_, .(AI1, Pest),  function(d){
    w <- unlist(llply(d$SEMestMax, function(x){
        if(x == 0){ x <- .01}
        x
    }))
    if(length(unique(d$AILbsAcre1)) < 2) {
        return(data.frame(int = NA, slp = NA, type = NA))
    }
    if(length(unique(d$V1)) < 2){
        m <- lm(LnR1 ~ AILbsAcre1,
                data = d, weights = 1/w)
        data.frame(int = coef(m)[1],
                   slp = coef(m)[2],
                   type = "Single Table Sample", stringsAsFactors = FALSE)
    } else {
        m <- lm(LnR1 ~ AILbsAcre1 + V1,
                data = d, weights = 1/w)
        data.frame(int = coef(m)[1],
                   slp = coef(m)[2],
                   type = "Multi-Table Sample", stringsAsFactors = FALSE)
        ## m <- lmer(LnR1 ~ AILbsAcre1 + (1 | V1),
        ##           data = d, weights = 1/w)
        ## data.frame(int = fixef(m)[1],
        ##            slp = fixef(m)[2],
        ##            type = "R", stringsAsFactors = FALSE)
    }
}, .inform = TRUE)

eqs$AI1 <- gsub("-", "\n", eqs$AI1)

tmp__ <- tmp %>%
    filter(AI1 != "spinosad" &
           AI1 != "lambda-cyhalothrin" &
           Pest != "EAW" & Pest != "AWW" & Pest != "CPA" &
           !is.na(SEMestMax))
tmp__$AI1 <- gsub("-", "\n", tmp__$AI1)

## Let's look at this plot again for starters
ggplot(tmp__,
       aes(x = AILbsAcre, y = LnR1)) + ##, color = V1, group = V1)) +
    geom_point() +
    geom_abline(data = eqs,
                aes(intercept = int, slope = slp),
                show_guide = TRUE) +
    facet_grid(Pest~AI1, scale = "free") +
    ##stat_smooth(se = FALSE, method = "lm") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(legend.position = "none") +
    xlab("Lbs of AI") +
    ylab("Natural Log of Effect") +
    ggtitle("Quantity AI vs. Treatment statistic")

##ggsave("RateVsEffect.pdf")
ggsave("GlobalRateResponseCurves_6_8_15.pdf")

## gam
## let's first take the residuals from a prediction using V1:
m <- lm(PercControl1 ~ V1, data = tmp)

tmp_gam <- tmp
tmp_gam$res <- residuals(m)
a <- tmp_gam %>%
    group_by(AI1, Pest) %>%
    dplyr::summarise(num = n()) %>%
    filter(num > 3)
tmp_gam <- tmp_gam %>% filter(AI1 %in% a$AI1 & Pest %in% a$Pest)

ggplot(tmp_gam, aes(x = AILbsAcre1, y = PercControl1)) +
    geom_point() +
    stat_smooth(se = FALSE) +
    facet_grid(AI1 ~ Pest, scale = "free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
    theme(legend.position = "none")
## not super helpful

## grobs <- llply(unique(ins_table$AI),
##                function(ai){
##                    ggplot(ins_table %>% filter(AI == ai),
##                           aes(x = LnR1)) + geom_density()})

##____________________rJags____________________________##
##                  learning                           ##
library(rjags)
library(coda)

a.dat <- list( fatal = rnorm(27), I = 27)

cat( "model
{
    for( i in 1:I )
        {
            fatal[i] ~ dnorm(mu, tau)
        }
    mu ~ dnorm(0, .0001)
    sigma ~ dlnorm(0, 0.0625)
    tau <- 1 / pow(sigma, 2)
}",
file = "m1.jag")

a.ini <- list(
list(mu = 0),
list(mu = 1),
list(mu = -1)
)

m <- jags.model(
file = "m1.jag",
data = a.dat,
n.chains = 3,
inits = a.ini,
n.adapt = 2000
)

res <- coda.samples(m, var = "mu", n.iter = 10000, thin = 10)

summary(res)


##__________Simple LinReg_________##
y <- c(1,3,3,3,5,7)
x <- c(1,2,3,4,5,6)

a.dat <- list( x = x, y = y, I = 6)

cat( "model
{
    for( i in 1:I )
        {
         y[i] ~ dnorm(alpha + beta * x[i], tau)   
        }
    alpha ~ dnorm(0, .0001)
    beta ~ dnorm(0, .0001)
    sigma ~ dlnorm(0, 0.0625)
    tau <- 1 / pow(sigma, 2)
}",
file = "m2.jag")

a.ini <- list( list(alpha = 0, beta = 0),
list(alpha = 1, beta = 1),
list(alpha = -1, beta = -1))

m <- jags.model(
file = "m2.jag",
data = a.dat,
n.chains = 3,
inits = a.ini,
n.adapt = 2000
)

res <- coda.samples(m, var = c("alpha", "beta"), n.iter = 10000, thin = 10)

summary(res)

## try with a different set of data!

t <- 1:500                               
x_t <- 2 * cos(2 * pi * t / 50 + .6 * pi)
w_t <- rnorm(500, 0 , 1)                 
x <- x_t + w_t                           
qplot(seq_along(x), x, geom = "line")    
a.dat <- list(x = seq_along(x), y = x, I = 500)

cat( "model
{
    for( i in 1:I )
        {
         y[i] ~ dnorm(beta * cos(alpha * x[i] + gamma), tau)   
        }
    alpha ~ dnorm(0, .0001)
    beta ~ dnorm(0, .0001)
    gamma ~ dnorm(0, .0001)
    sigma ~ dlnorm(0, 0.0625)
    tau <- 1 / pow(sigma, 2)
}",
file = "m3.jag")

a.ini <- list( list(alpha = 1, beta = 1, gamma = 0),
list(alpha = 0, beta = 1, gamma  = .5),
list(alpha = -1, beta = -1, gamma = 0))

m <- jags.model(
file = "m3.jag",
data = a.dat,
n.chains = 3,
inits = a.ini,
n.adapt = 2000
)

res <- coda.samples(m, var = c("alpha", "beta", "gamma"), n.iter = 10000, thin = 10)

summary(res)

