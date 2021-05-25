subjects <- 5
rounds <- 2
profiles <- 2
obs <- subjects*rounds*profiles

fake_data <- data.frame(A = sample(c("a1","a2"), obs, replace = TRUE),
                        B = sample(c("b1","b2"), obs, replace = TRUE),
                        id1 = rep(1:subjects, each=rounds),
                        stringsAsFactors = TRUE)

fake_data$Y <- sample(c(0,1), obs, replace = TRUE)

cj_model <- cjbart(data = fake_data,
                   Y = "Y",
                   id = "id1")

## Skip if not Unix due to longer CPU time
if (.Platform$OS.type=='unix') {

  het_effects <- IMCE(data = fake_data,
                      model = cj_model,
                      attribs = c("A","B"),
                      ref_levels = c("a1","b1"),
                      cores = 1)

  summary(het_effects)
}


