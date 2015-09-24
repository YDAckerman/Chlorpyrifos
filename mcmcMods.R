
#### Priors

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

#### Representative Data subset

pest <- "AW"
tmp_pest <- tmp %>% filter(Pest == pest) ## teehee

#### Models

## This is the first model with a fixed effect for
## AI and random effects for PDF and table (V1). Rate
## is notably absent from this model.
## DIC: 507
mcmcMod1 <- MCMCglmm(LnR1 ~ AI - 1,
                     ##mev = weights2, ## make sure to pare to i
                     random = ~ PDF.file.name + V1,
                     family = "gaussian",
                     data = tmp_pest,
                     prior = prior1,
                     nitt = 20000,
                     burnin = 10000, verbose = FALSE)

## I think this is the proper specification to add a random
## coefficient in MCMCglmm, but I'm not sure. I'll probably
## have to email hadfield. Either way, this is model attempt
## number two. I now try to account for the changes in rate
## accross the samples - here with a random effect
## DIC: 508
mcmcMod2 <- MCMCglmm(LnR1 ~ AI - 1 ,
                     ##mev = weights2, ## make sure to pare to i
                     random = ~ idv(AILbsAcre):AI + PDF.file.name + V1,
                     family = "gaussian",
                     data = tmp_pest,
                     prior = prior2,
                     nitt = 20000,
                     burnin = 10000, verbose = FALSE)

## Rate could be a fixed effect with a different coefficient
## for each AI (interaction effect, essentially)
## DIC: 522
mcmcMod3 <- MCMCglmm(LnR1 ~ AI - 1 + AILbsAcre:AI,
                     ##mev = weights2, ## make sure to pare to i
                     random = ~ PDF.file.name + V1,
                     family = "gaussian",
                     data = tmp_pest,
                     prior = prior1,
                     nitt = 20000,
                     burnin = 10000, verbose = FALSE)

## There doesn't appear to be much correlated variation
## within PDFS:
## ggplot(tmp, aes(x = V1, y = LnR1, color = Pest)) +
## geom_point() +
## facet_wrap(~PDF.file.name) +
## theme(legend.position = "none") +
## theme(axis.ticks = element_blank(), axis.text.x = element_blank())

## leaving out PDF as a random effect, keeping interaction
## effect with rate and AI:
## DIC: 524
mcmcMod4 <- MCMCglmm(LnR1 ~ AI - 1 + AILbsAcre:AI,
                     ##mev = weights2, ## make sure to pare to i
                     random = ~ V1,
                     family = "gaussian",
                     data = tmp_pest,
                     prior = prior3,
                     nitt = 20000,
                     burnin = 10000, verbose = FALSE)

## Just to see what happened, I left out both rate and PDF
## factors.
## DIC: 509
mcmcMod5 <- MCMCglmm(LnR1 ~ AI - 1,
                     ##mev = weights2, ## make sure to pare to i
                     random = ~ V1,
                     family = "gaussian",
                     data = tmp_pest,
                     prior = prior3,
                     nitt = 20000,
                     burnin = 10000, verbose = FALSE)

## Also for curiosity's sake, I'll throw rate back in,
## but as a random effect.
## DIC: 510
mcmcMod6 <- MCMCglmm(LnR1 ~ AI - 1,
                     ##mev = weights2, ## make sure to pare to i
                     random = ~ V1 + idv(AILbsAcre):AI,
                     family = "gaussian",
                     data = tmp_pest,
                     prior = prior1,
                     nitt = 20000,
                     burnin = 10000, verbose = FALSE)
