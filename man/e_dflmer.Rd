\name{e_dflmer}
\alias{e_dflmer}
\title{HLM dataset}
\usage{
e_dflmer()
}
\description{
HLM dataset
}
\examples{

## HLM dataset -----------  
  HLM 완벽 가이드 dataset  
  1) salary_data  
  2) therapie  

## lmer dataset -----------  
  # HLM 완벽 가이드 dataset ---- 
  1) salary_data  
  2) therapie  
    
## Carolyn dataset ---- 
  depression_data_long 
  hsb1data 
  hsb2data 
  lsat6_long_data 
  nels_data 
  nels10 
  respitory 
  RIESBY_data_time_varying_for_r 

## Bodo Winter ---- 
   politeness_data

## lmer dataset -----------  
    
  ﻿icc <- function(modl){   
       vars <- as.data.frame(VarCorr(modl))[4] 
       total <- sum(vars)
       tau00 <- vars[1,1] 
       icc <- tau00/total 
       return(icc)  }  # example of how to use this  

****************************************************************   
    
  library(dplyr); library(dflmer); ﻿library(lme4); library(lmerTest); library(aj412s) 
   
  ## Intercept Only Model(IOM) M1 ----- 
     intercept.only.model <- lmer(salary ~ 1  + (1  | firma), data = df, REML = TRUE) 
     ranova(intercept.only.model) 
   
  ## Radnom Intercept Model(RIM) M2 ----- 
     random.intercept.model <-  lmer(salary ~ experience + (1  | firma), data = df, REML = TRUE) 
     df$random.intercept.preds <-  predict(random.intercept.model) 
     ranef(random.intercept.model)  # obtain the individual random effects υ0i 
   
  ## Random Coefficients Modell (RCM) M3 ------ 
     random.coefficients.model <-  lmer(salary ~ experience + (experience | firma), data = df, REML = TRUE) 
     df$random.coefficients.preds <-  predict(random.coefficients.model) 
     ranef(random.coefficients.model) 
   
  # Comparing Models 
     ranova(random.coefficients.model) 
     anova(random.coefficients.model, random.intercept.model, refit = FALSE) 
     0.5*pchisq(10.011, 1, lower.tail = FALSE) + 0.5*pchisq(10.011, 2, lower.tail = FALSE) 
   
****************************************************************   
  ## Context effect model : M4 ----- 
     df <- df %>% group_by(firma) %>%  
     mutate(experience_gm = mean(experience), experience_gc = experience - experience_gm) %>% 
     ungroup() %>% dplyr::select(firma, sector, experience, experience_gm, experience_gc, salary) 
   
     df %>% group_by(firma) %>% summarise(experience_gm = mean(experience_gm)) %>% ungroup() 
   
     context.model <- lmer(salary ~ experience_gc + experience_gm + (1|firma), data = df, REML = TRUE) 
     df$context.model.preds <- predict(context.model) 
   
  ## General intercept-as-outcome model M4 ----- 
     intercept.as.outcome.model <- lmer(salary ~ experience + sector + (experience | firma), data = df, REML = TRUE)  
     df$intercept.as.outcome.preds <- predict(intercept.as.outcome.model) 
   
  ## Slope-as-outcome model ----- 
     slope.as.outcome.model <- lmer(salary ~ experience*sector + (experience | firma), data = df, REML = TRUE) 
   
  # Comparing Models 
     ranova(slope.as.outcome.model) 
     0.5*pchisq(5.0152, 1, lower.tail = FALSE) + 0.5*pchisq(5.0152, 2, lower.tail = FALSE) 
   
****************************************************************   
  library(dplyr); library(dflmer); ﻿library(lme4); library(lmerTest); library(aj412s) 
  ﻿hsb1 <- hsb1 
  ﻿hsb2 <- hsb2 
  ﻿hsb <- merge(hsb1,hsb2, by=c('id')) 
   
  ## Making mean variables ----- 
  ﻿meanminority  <- aggregate(minority ~ id , data=hsb, mean) 
  names(meanminority) <- c('id', 'meanminority') 
  hsb <- merge(hsb,meanminority, by =c('id')) 
   
  meanfemale  <- aggregate(female ~ id , data=hsb, mean) 
  names(meanfemale) <- c('id', 'meanfemale') 
  hsb <- merge(hsb,meanfemale, by =c('id')) 
   
  ## Random intercept ----- 
     ﻿model0 <- lmer(mathach ~ 1 + (1 | id), data=hsb, REML=FALSE) 
     icc(model0) 
  # Calc by Hand: ﻿icc <- 8.553/(8.553+39.148) 
   
  ## ﻿Random intercept and slope for centered ses ----- 
  # By centering here, we will be looking at the impact of where a student is relavtiave to their peers in terms of SES. 
     model1 <- lmer(mathach ~ 1 + cses + (1 | id), data=hsb, REML=FALSE) 
   
  ## ﻿Random intercept and slope & meanses for ses ----- 
     model2 <- lmer(mathach ~ 1 + cses  + (1 + cses| id), data=hsb, REML=FALSE) 
   
****************************************************************   
  ## ﻿Discrete Level 1 predictors (fixed effects) ----- 
  # Note female and minority are dummy (i.e., I have not declared them to be factors in this model) 
     model.d1 <- lmer(mathach ~ 1 + cses  + female + minority+ (1 + cses| id), data=hsb, REML=FALSE) 
     model.3 <- lmer(mathach ~ 1 + cses  + meanses + cses*meanses + (1 + cses| id), data=hsb, REML=FALSE 
   
   
  # Logit Model ----------- 
  ﻿library(lme4); library(lmerTest); library(gee); library(dflmer) 
     depress <- opencsv(depression_data_long.csv) 
  ﻿   model1a <- glm(y ~ severe +  Rx + time + Rx*time, data=depress, family=binomial) 
   
  # ﻿GEE using exchangable -----------  
     model.gee <- gee(y ~ severe  + Rx + time + Rx*time, id, data=depress, family=binomial, corstr='exchangeable')  
  ﻿   summary(model.gee) 

}
