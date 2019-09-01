source('Slides/Rscripts/simdata_PartIII.R')

library(JointAI)
set.seed(1234)
JointAI_nonlin <- lm_imp(y ~ x*z + I(x^2), data = DF_nonlin, n.iter = 2500)
save(JointAI_nonlin, file =  "Slides/workspaces/JointAI_nonlin.RData")

  
JointAI_long <- lme_imp(y ~ x1 + x3 + x2 + x4 + time + I(time^2),
                        random = ~time|id, data = DFexlong2,
                        n.iter = 5000)

save(JointAI_long, file = "Slides/workspaces/JointAI_long.RData")


JointAI_surv <- coxph_imp(Surv(Time, event) ~ x1 + x2 + x3, data = survdat,
                          n.iter = 1500, quiet = FALSE, parallel = TRUE, ncores = 3)
JointAI_survreg <- survreg_imp(Surv(Time, event) ~ x1 + x2 + x3, data = survdat,
                               n.iter = 1500, quiet = FALSE, parallel = TRUE, ncores = 3)
save(JointAI_surv, JointAI_survreg, file = 'Slides/workspaces/JointAI_surv.RData')

  
