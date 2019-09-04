load(file.path('Slides', 'data', "NHANES_for_lectures.RData"))
NHANES <- subset(NHANES, select = -DBP)


# imp -------------------------------------------------------------------------
library(mice)
imp0 <- mice(NHANES, maxit = 0,
             defaultMethod = c("norm", "logreg", "polyreg", "polr"))
meth <- imp0$method
meth["educ"] <- "polr"
meth["HyperMed"] <- ""

pred <- imp0$predictorMatrix
pred[, "HyperMed"] <- 0
imp <- mice(NHANES, method = meth, predictorMatrix = pred,
            seed = 2019)
save(imp, file = file.path("Slides/workspaces/imp.RData"))

  
# imp2 -------------------------------------------------------------------------
post <- imp$post
post["creat"] <- "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(0, 100))"
imp2 <- update(imp, post = post, maxit = 20, seed = 123, printFlag = TRUE)

save(imp2, file = file.path("Slides/workspaces/imp2.RData"))

# imp2b -----------------------------------------------------------------------
imp2b <- update(imp2, post = post, maxit = 20, seed = 456)
save(imp2b, file = file.path("Slides", "workspaces/imp2b.RData"))

  
