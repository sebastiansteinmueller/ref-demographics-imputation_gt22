############################################ START ###########################################################

############################################ model_age_only.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, UNICEF
#### Project: Demographic models end-2022
#### Description: Multinomial models for sex/age counts

##### I. Read data, packages etc #####

### packages
library(tidyverse)
library(stringi)
library(brms)
library(tidybayes)
library(bayesplot)
library(writexl)

### read dataset
load("data/dem_refoip_end2022.RData")

dem_longMissing$age  <- with(dem_longMissing, cbind(female_0_4, female_5_11, female_12_17, female_18_59, female_60,
                                                    male_0_4, male_5_11, male_12_17, male_18_59, male_60))


##### II. Define model formulas #####

f.m.ageonly <- bf(formula =  age | trials(total) ~ (1|origin_iso3/asylum_sdgregion/asylum_iso3)  + # varying intercept
                      neighbor,
               family = multinomial(link = "logit", refcat = "female_18_59"),
             center = T # intercept centered at mean of population level covariates
)

##### III. Define prior distributions #####

priors.m.age.empty <- get_prior(f.m.ageonly,
          data = dem_longMissing %>%
          filter(missing %in% c("none")))




priors.m.ageonly <- c(
  prior(normal(0,1), class = Intercept),
  prior(student_t(7,0,2.5), class = b), # population-level parameters have a multiplicative effect on odds of female. Absolute values above ~3 (ten-fold reduction/increase in odds for doubling in distance/gni ratio) are implausible, thus choice of narrower prior with df=7
  prior(normal(log(0.0404298480924641/0.275983783554867),1), class = Intercept, dpar = "mufemale04"),
  prior(normal(log(0.0579618443970155/0.275983783554867),1), class = Intercept, dpar = "mufemale511"),
  prior(normal(log(0.0471698762941686/0.275983783554867),1), class = Intercept, dpar = "mufemale1217"),
  prior(normal(log(0.0758180652645299/0.275983783554867),1), class = Intercept, dpar = "mufemale60"),
  prior(normal(log(0.0427171808020409/0.275983783554867),1), class = Intercept, dpar = "mumale04"),
  prior(normal(log(0.0617149971425719/0.275983783554867),1), class = Intercept, dpar = "mumale511"),
  prior(normal(log(0.0503653654246226/0.275983783554867),1), class = Intercept, dpar = "mumale1217"),
  prior(normal(log(0.284651538551754/0.275983783554867),1), class = Intercept, dpar = "mumale1859"),
  prior(normal(log(0.0631875004759655/0.275983783554867),1), class = Intercept, dpar = "mumale60"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mufemale04"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mufemale04"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mufemale04"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mufemale511"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mufemale511"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mufemale511"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mufemale1217"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mufemale1217"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mufemale1217"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mufemale60"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mufemale60"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mufemale60"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mumale04"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mumale04"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mumale04"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mumale511"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mumale511"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mumale511"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mumale1217"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mumale1217"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mumale1217"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mumale1859"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mumale1859"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mumale1859"),
  prior(normal(0,0.01), class = sd, group = "origin_iso3:asylum_sdgregion:asylum_iso3", lb =0, dpar = "mumale60"),
  prior(student_t(7,0,2.5), class = sd, group = "origin_iso3:asylum_sdgregion",lb =0, dpar = "mumale60"),
  prior(student_t(5,0,2.5), class = sd, group = "origin_iso3", lb =0, dpar = "mumale60")
)


##### IV. Fit and save model #####

m.ageonly <- brm(formula = f.m.ageonly,
            data = dem_longMissing %>% filter(missing %in% c("none")),
            prior = priors.m.ageonly,
            init = 0,
            sample_prior = "yes",
            iter = 3000,
            chains = 4,
            cores = 4,
            control=list(adapt_delta=0.9,
                          max_treedepth=10),
            seed = 1146
            )
saveRDS(m.ageonly, file =  paste0("models/m.ageonly_", str_remove_all(as.character(Sys.Date()), "-"),".rds"))


##### V. Model diagnostics #####

# plot(m.age)
# p.m.age.mcmcacf <- mcmc_acf(m.age,pars = variables(m.age)[c(1:8)]) # acf of intercept mcmcs
# m.age.loo <- loo(m.age)
# plot(m.age.loo)


############################################ END ###########################################################
