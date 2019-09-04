# packages ---------------------------------------------------------------------
library(splines)
library(RColorBrewer)
library(survival)
library(mice)

# quadrtic example -------------------------------------------------------------
set.seed(2)
N <- 200
x <- runif(N, -1, 1)
y <- -1 - 0.6 * x + 0.5 * x^2 + rnorm(N, 0, 0.2)

DFexqdr <- data.frame(y = y, x = x)
DFexqdr$xmis <- DFexqdr$x
DFexqdr$xmis[sample(1:length(x), size = N/2, prob = plogis(5 * DFexqdr$y))] <- NA

impmod <- lm(xmis ~ y, DFexqdr)
imps <- predict(impmod, newdata = DFexqdr[is.na(DFexqdr$xmis), ]) +
  rnorm(sum(is.na(DFexqdr$xmis)), 0, summary(impmod)$sigma)

DFexqdr$ximp <- DFexqdr$xmis
DFexqdr$ximp[is.na(DFexqdr$xmis)] <- imps

lm0_qdr <- lm(y ~ x + I(x^2), DFexqdr)
lm_imp_qdr <- lm(y ~ ximp + I(ximp^2), DFexqdr)


# interaction example ----------------------------------------------------------
set.seed(2)
N <- 200
x <- runif(N, -1, 1)
z <- rbinom(N, size = 1, prob = 0.5)
y <- -1 - 0.6 * x + 0.5 * z + x*z + rnorm(N, 0, 0.2)

DFexint <- data.frame(y = y, x = x, z = z)
DFexint$xmis <- DFexint$x
DFexint$xmis[sample(1:length(x), size = N/2, prob = plogis(2 * DFexint$y))] <- NA

impmod <- lm(xmis ~ y + z, DFexint)
impmod2 <- lm(xmis ~ y*z, DFexint)

impresid <- rnorm(sum(is.na(DFexint$xmis)), 0, summary(impmod)$sigma)

imps <- predict(impmod, newdata = DFexint[is.na(DFexint$xmis), ]) + impresid
imps2 <- predict(impmod2, newdata = DFexint[is.na(DFexint$xmis), ]) + impresid

DFexint$ximp <- DFexint$ximp2 <- DFexint$xmis
DFexint$ximp[is.na(DFexint$xmis)] <- imps
DFexint$ximp2[is.na(DFexint$xmis)] <- imps2

lm0_int <- lm(y ~ x*z, DFexint)
lm_imp_int <- lm(y ~ ximp*z, DFexint)
lm_imp2_int <- lm(y ~ ximp2*z, DFexint)

plotexint <- reshape2::melt(DFexint, id.vars = c("y", "z", "ximp2", "xmis"))
plotexint$combi <- paste0(plotexint$variable, "_",
                          ifelse(is.na(plotexint$xmis), "mis", "obs"), plotexint$z)



# nonlinear comparison ---------------------------------------------------------
# * simulate data -------------------------------------------------------------
set.seed(2018)
N <- 200
x <- rnorm(N)
z <- rbinom(N, size = 1, prob = plogis(x))
y <- x + x^2 + z + x*z + rnorm(N, 0, 0.5)

DF_nonlin <- data.frame(y = y, x = x, z = z)

# model on complete data
mod_nonlin <- lm(y ~ x + I(x^2) + z + x:z, data = DF_nonlin)

# create missing values
DF_nonlin$x[sample(1:length(x), size = N/2)] <- NA


# longitudinal example ---------------------------------------------------------
set.seed(2018)
N <- 50
id <- 1:N
nj <- sample(3:5, N, replace = T)
ti <- sapply(nj, sample, x = seq(1, 9, by = 2), prob = c(0.1, rep(0.2, 4)))
time <- lapply(ti, function(x) sort(runif(length(x), min = x - 1, max = x)))

x1 <- rnorm(N, 28, 2.5) # maternal age
x2 <- factor(sample(c("boy", "girl"), N, replace = T)) # gender
x3 <- factor(sample(c("low", "mid", "high"), N, replace = T)) # income/educ
x4 <- runif(N, min = 37, max = 42)

DFexlong_orig <- data.frame(id = rep(1:N, sapply(time, length)),
                            time = unlist(time),
                            tp = unlist(sapply(time, seq_along)),
                            ti = unlist(sapply(ti, sort)),
                            x1 = rep(x1, sapply(time, length)),
                            x2 = rep(x2, sapply(time, length)),
                            x3 = rep(x3, sapply(time, length)),
                            x4 = rep(x4, sapply(time, length))
)

D <- matrix(nrow = 2, ncol = 2, data = c(0.2, -0.02, -0.02, 0.005))
b <- MASS::mvrnorm(N, c(0,0), D)
Z <- model.matrix(~1 + time, DFexlong_orig)

betas <- c(Intercept = 15,
           time1 = -0.4, time2 = 0.3, time3 = 0.6,
           matage = 0.3,
           gender = -0.9,
           educ2 = -0.4, educ3 = -0.9,
           gestbir = 0.3)

fmla <- ~ ns(time, df = 3) + x1 + x2 + x3 + x4
X <- model.matrix(fmla, model.frame(fmla, DFexlong_orig, na.action = "na.pass"))

DFexlong_orig$y <- as.numeric(X %*% betas +
                                rowSums(Z * b[match(DFexlong_orig$id,
                                                    unique(DFexlong_orig$id)), , drop = F]) +
                                rnorm(nrow(DFexlong_orig), 0, 0.1))

DFexlong <- DFexlong_orig[, c("id", paste0("x", 1:4), "y", "time", "tp", "ti")]


# create missing values
DFexlong$x2[DFexlong$id %in% c(6, 18, sample.int(N, size = 0.3 * N))] <- NA
DFexlong$x3[DFexlong$id %in% c(6, sample.int(N, size = 0.3 * N))] <- NA
DFexlong$x4[DFexlong$id %in% sample.int(N, size = 0.3 * N)] <- NA

DFexlongwide <- reshape(DFexlong, direction = 'wide',
                        v.names = c("y", "time"), idvar = "id",
                        timevar = 'ti', drop = "tp")



IDs <- c(5, 6, 7, 8, 18)
subIDs <- IDs[IDs != 7]
subDFexlong <- DFexlong[DFexlong$id %in% IDs, ]


subDFexlongwide <- DFexlongwide[DFexlongwide$id %in% subIDs, ]



IDs <- c(5, 6, 7, 8, 18)
subIDs <- IDs[IDs != 7]
subDFexlong <- DFexlong[DFexlong$id %in% IDs, ]

DFexlongwide <- reshape(DFexlong, direction = 'wide', v.names = c("y", "time"),
                        idvar = "id",
                        timevar = 'ti', drop = "tp")
subDFexlongwide <- DFexlongwide[DFexlongwide$id %in% subIDs, ]


coefDFexlong <- as.data.frame(t(sapply(lapply(split(subDFexlong, subDFexlong$id),
                                              lm, formula = y ~ time), coef)))
coefDFexlong$ID <- IDs


ltDFexlong <- subDFexlong[subDFexlong$id != 7,
                          c("id", "y", paste0("x", 1:4), "time")]
ltDFexlong$time <- sprintf(ltDFexlong$time, fmt = "%.2f")
ltDFexlong$y <- "\\checkmark"
ltDFexlong$x1 <- "\\checkmark"
ltDFexlong$x2 <- as.character(ltDFexlong$x2)
ltDFexlong$x2[!is.na(ltDFexlong$x2)] <- "\\checkmark"
ltDFexlong$x3 <- as.character(ltDFexlong$x3)
ltDFexlong$x3[!is.na(ltDFexlong$x3)] <- "\\checkmark"
ltDFexlong$x4[!is.na(ltDFexlong$x4)] <- "\\checkmark"

ltDFexlong <- rbind(ltDFexlong, rep("\\vdots", 7))


colvec <- brewer.pal(length(IDs), "Dark2")


# long imputation ------------------------------------------------------------------\
impexlong <- mice(DFexlong, seed = 123)
impDFexlong <- complete(impexlong, 3)
impwide <- mice(DFexlongwide, maxit = 10)



# * DFexlong2 ------------------------------------------------------------------
# sim data2 longitudinal --------------------------------------------------------
set.seed(1234)
N <- 150
id <- 1:N
t1 <- runif(N, 0, 10)
nj <- sample(5:10, N, replace = T)
time <- lapply(1:N, function(x) sort(runif(nj[x], min = t1[x], max = t1[x] + nj[x])))

x1 <- rnorm(N, 28, 2.5) # maternal time
x2 <- factor(sample(c(0, 1), N, replace = T)) # gender
x3 <- factor(sample(c(1:3), N, replace = T)) # income/educ
x4 <- rnorm(N, 35, 4)

DFexlong2_orig <- data.frame(id = rep(1:N, sapply(time, length)),
                             time = unlist(time),
                             ti = unlist(lapply(nj, seq, from = 1, by = 1)),
                             x1 = rep(x1, sapply(time, length)),
                             x2 = rep(x2, sapply(time, length)),
                             x3 = rep(x3, sapply(time, length)),
                             x4 = rep(x4, sapply(time, length))
)
DFexlong2_orig$time <- DFexlong2_orig$time - mean(DFexlong2_orig$time)

D <- matrix(nrow = 2, ncol = 2,
            data = c(0.2, -0.005, -0.005, 0.001))

b <- MASS::mvrnorm(N, c(0,0), D)
Z <- model.matrix(~1 + time, DFexlong2_orig)

betas <- c(Intercept = 15,
           time1 = -0.7, time2 = 0.3, time3 = 0.8,
           matage = 0.3,
           gender = -0.3,
           educ2 = -0.4, educ3 = -0.9,
           gestbir = 0.01)


fmla <- ~ ns(time, df = 3) + x1 + x2 + x3 + x4
X <- model.matrix(fmla, model.frame(fmla, DFexlong2_orig, na.action = "na.pass"))


DFexlong2_orig$y <- as.numeric(X %*% betas +
                                 rowSums(Z * b[match(DFexlong2_orig$id,
                                                     unique(DFexlong2_orig$id)), , drop = F]) +
                                 rnorm(nrow(DFexlong2_orig), 0, 0.03))

DFexlong2 <- DFexlong2_orig

# create missing values
DFexlong2$x2[DFexlong2$id %in% c(sample.int(N, size = 0.3 * N))] <- NA
DFexlong2$x3[DFexlong2$id %in% c(sample.int(N, size = 0.3 * N))] <- NA
DFexlong2$x4[DFexlong2$id %in% c(sample.int(N, size = 0.3 * N))] <- NA

DFexlong2_sub <- DFexlong2[DFexlong2$id %in% c(1, 2, 6, 15, 16), ]

coefDFexlong2 <- as.data.frame(t(sapply(lapply(split(DFexlong2, DFexlong2$id),
                                               lm, formula = y ~ time), coef)))

coefDFexlong22 <- as.data.frame(t(sapply(lapply(split(DFexlong2, DFexlong2$id),
                                                lm, formula = y ~ time + I(time^2)), coef)))

  

# survial example -------------------------------------------------------------
n <- 300 # number of subjects

# parameters for the survival model
phi <- 1.6458 # shape for the Weibull baseline hazard
mean.Cens <- 12 # mean of the exponential distribution for the censoring mechanism

################################################
gammas <- c("(Intercept)" = -5.7296, "x2" = 2.4092, "x1" = -2, x3 = 0.2) # coefficients for baseline covariates

set.seed(2019)
x2 <- rep(0:1, each = n/2) # group indicator, i.e., '0' placebo, '1' active treatment
x1 <- rnorm(n)
x3 <- rnorm(n)

# design matrix for the survival model
W <- cbind("(Intercept)" = 1,
           "x2" = x2,
           "x1" = x1,
           "x3" = x3)

################################################

# simulate event times
eta.t <- as.vector(W %*% gammas)
invS <- function(t, u, i) {
  h <- function(s) {
    x20 <- 1 - x2[i]
    x21 <- x2[i]
    exp(log(phi) + (phi - 1) * log(s) + eta.t[i])
  }
  integrate(h, lower = 0, upper = t)$value + log(u)
}
u <- runif(n)
trueTimes <- numeric(n)
for (i in 1:n) {
  Up <- 50
  tries <- 5
  Root <- try(uniroot(invS, interval = c(1e-05, Up), u = u[i], i = i)$root, TRUE)
  while (inherits(Root, "try-error") && tries > 0) {
    tries <- tries - 1
    Up <- Up + 200
    Root <- try(uniroot(invS, interval = c(1e-05, Up), u = u[i], i = i)$root, TRUE)
  }
  trueTimes[i] <- if (!inherits(Root, "try-error")) Root else NA
}
na.ind <- !is.na(trueTimes)
trueTimes <- trueTimes[na.ind]
W <- W[na.ind, , drop = FALSE]

n <- length(trueTimes)

# simulate censoring times from an exponential distribution,
# and calculate the observed event times, i.e., min(true event times, censoring times)
Ctimes <- runif(n, 0, 2 * mean.Cens)
Time <- pmin(trueTimes, Ctimes)
event <- as.numeric(trueTimes <= Ctimes) # event indicator

survdat_orig <- data.frame(Time = Time,
                           event = event,
                           x2 = x2[na.ind],
                           x1 = x1[na.ind],
                           x3 = x3[na.ind])



survdat <- survdat_orig
N = n
survdat$x1[sample(1:N, N*0.3)] <- NA
survdat$x3[sample(1:N, N*0.3)] <- NA
survdat$x2[sample(1:N, N*0.3)] <- NA
survdat$x2 <- factor(survdat$x2)
survdat_orig$x2 <- factor(survdat_orig$x2)

