## ----imp0v2-hint-2, solution = TRUE, purl = TRUE--------------------------------------------------
imp0 <- mice(NHANES2, maxit = 0, 
             defaultMethod = c("norm", 'logreg', 'polyreg', 'polr'))

## ----changeimpmod-solution, solution = TRUE, purl = TRUE------------------------------------------
meth <- imp0$meth
meth["creat"] <- "pmm"
meth

## ----predmat-solution, solution = TRUE, purl = TRUE-----------------------------------------------
pred <- imp0$predictorMatrix
pred

## ----changepred-solution, solution = TRUE, purl = TRUE--------------------------------------------
# BMI will not be used as predictor of height and weight
pred[c("hgt", "wgt"), "BMI"] <- 0
# height and weight will not be used as predictor in any model
pred[, c("hgt", "wgt")] <- 0
# height and weight will be used as predictors for each other
pred["hgt", "wgt"] <- 1
pred["wgt", "hgt"] <- 1

# WC is not used as predictor for weight
pred["wgt", "WC"] <- 0

# HyperMed will not be used as predictor in any model
pred[, "HyperMed"] <- 0

# hypchol will not be used as predictor in the imputation model for chol
pred["chol", "hypchol"] <- 0

# BMI will be imputed passively
meth["BMI"] <- "~I(wgt/hgt^2)"
# HyperMed will not be imputed
meth["HyperMed"] <- ""

## ----changevisseq-solution, solution = TRUE, purl = TRUE------------------------------------------
visSeq <- imp0$visitSequence
visSeq

# find "BMI" in the visit sequence
which_BMI <- match("BMI", visSeq)
# move "BMI" to the end of the sequence
visSeq <- c(visSeq[-which_BMI], visSeq[which_BMI])

