## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7, fig.height = 5
)

set.seed(89)
library(ggplot2)

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
cj_model <- cjbart::cjbart(data = fake_data,
                   Y = "Y", 
                   id = "id1")


## ----OMCE, include=TRUE-------------------------------------------------------
het_effects <- cjbart::IMCE(data = fake_data, 
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

## ----cregg--------------------------------------------------------------------
cregg_amce <- cregg::cj(fake_data, Y ~ A + B + C + D + E, id = ~ id1)
cjbart_amce <- summary(het_effects)

colnames(cjbart_amce)[1] <- "level"

merge(cregg_amce[,c("level","estimate")], cjbart_amce[,c("level","AMCE")])


