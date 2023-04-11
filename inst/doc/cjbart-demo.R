## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 5
)

## ----setup--------------------------------------------------------------------
set.seed(89)
library(ggplot2)
library(cjbart)

## ----data, include=TRUE-------------------------------------------------------
subjects <- 250
rounds <- 5
profiles <- 2
obs <- subjects*rounds*profiles

fake_data <- data.frame(A = sample(c("a1","a2","a3"), obs, replace = TRUE),
                        B = sample(c("b1","b2","b3"), obs, replace = TRUE),
                        C = sample(c("c1","c2","c3"), obs, replace = TRUE),
                        D = sample(c("d1","d2","d3"), obs, replace = TRUE),
                        E = sample(c("e1","e2","e3"), obs, replace = TRUE),

                        covar1 = rep(runif(subjects, 0 ,1),
                                     each = rounds),
                        covar2 = rep(sample(c(1,0),
                                            subjects,
                                            replace = TRUE),
                                     each = rounds),

                        id1 = rep(1:subjects, each=rounds),
                        stringsAsFactors = TRUE)

fake_data$Y <- ifelse(fake_data$E == "e2",
                      rbinom(obs, 1, fake_data$covar1),
                      sample(c(0,1), obs, replace = TRUE))

## ----train, include=TRUE------------------------------------------------------
cj_model <- cjbart(data = fake_data,
                   Y = "Y", 
                   id = "id1")


## ----OMCE, include=TRUE-------------------------------------------------------
het_effects <- IMCE(data = fake_data, 
                    model = cj_model, 
                    attribs = c("A","B","C","D","E"),
                    ref_levels = c("a1","b1","c1","d1","e1"),
                    cores = 1)

## ----summary------------------------------------------------------------------
summary(het_effects)

## ----plot_imces---------------------------------------------------------------
plot(het_effects, covar = "covar1")

## ----plot_imces2--------------------------------------------------------------
plot(het_effects, covar = "covar1", plot_levels = c("a2","a3","e2","e3"))

## ----plot_vimp1---------------------------------------------------------------
vimp_estimates <- het_vimp(imces = het_effects, cores = 1)
plot(vimp_estimates)

## ----plot_vimp2---------------------------------------------------------------
plot(vimp_estimates, att_levels = "d3")

## ----pimces-------------------------------------------------------------------
fake_marginals <- list()
fake_marginals[["A"]] <- c("a1" = 0.33,"a2" = 0.33,"a3"=0.34)
fake_marginals[["B"]] <- c("b1" = 0.33,"b2" = 0.33,"b3" = 0.34)
fake_marginals[["C"]] <- c("c1" = 0.33,"c2" = 0.33, "c3" = 0.34)
fake_marginals[["D"]] <- c("d1" = 0.75,"d2" = 0.2,"d3" = 0.05)
fake_marginals[["E"]] <- c("e1" = 0.33,"e2" = 0.33,"e3" = 0.34)

# Reduced number of covariate data for sake of speed
fake_pimces <- pIMCE(covar_data = fake_data[fake_data$id1 %in% 1:3,
                                            c("id1","covar1","covar2")],
                     model = cj_model,
                     attribs = c("A","B","C","D","E"),
                     l = "E",
                     l_1 = "e2",
                     l_0 = "e1",
                     marginals = fake_marginals,
                     method = "bayes",
                     cores = 1)

head(fake_pimces)

