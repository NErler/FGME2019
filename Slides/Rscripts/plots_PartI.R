library(MASS)
library(RColorBrewer)
library(ggplot2)
library(gganimate)


# simulate data ----------------------------------------------------------------
set.seed(2019)

N = 50
x <- runif(N, -2, 2)
y <- rnorm(N, 0, 0.9) + x

DF0 <- data.frame(x = x, y = y)
DF0$yorig <- DF0$y
DF0$y[order(DF0$x)[c(0.25, 0.45, 0.7, 0.95) * N]] <- NA

m <- 5
lm1 <- lm(y ~ x, DF0)
xmis <- DF0$x[is.na(DF0$y)]
imps <- predict(lm1, newdata = DF0[is.na(DF0$y), ])

DF0$y2 <- DF0$y
DF0$y2[is.na(DF0$y)] <- -3.5
DF0$ismis <- is.na(DF0$y)


# baseplot ---------------------------------------------------------------------
baseplot_min <- ggplot(DF0) +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 16),
        panel.border = element_blank(),
        legend.position = c(0.12, 0.92),
        legend.background = element_blank(),
        legend.text = element_text(size = 14)) +
  xlab(expression(X[1])) +
  ylab(expression(X[2]))


baseplot <- baseplot_min +
  scale_color_manual(name = "",
                     limits = c(FALSE, TRUE),
                     values = c(grey(0.3), 2),
                     labels = c("observed", "missing")) +
  scale_shape_manual(name = "",
                     limits = c(FALSE, TRUE),
                     values = c(19, 1),
                     labels = c("observed", "missing"))


# plot + points ----------------------------------------------------------------
dataplot <- baseplot +
  geom_rect(xmin = -2.15, xmax = 1.97, ymin = -3.5, ymax = 3,
            fill = 'transparent', color = grey(0.7)) +
  geom_point(aes(x = x, y = y2, shape = ismis, color = ismis),
             na.rm = T, size = 2, stroke = 1.2)

# add regression line ----------------------------------------------------------
reglineplot <- dataplot +
  geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], lwd = 1)
# ggsave(reglineplot, file = 'Slides/figure/reglineplot.pdf', width = 5.25, height = 3.5)

ggsave(ggplot() + theme_void(), file = 'Slides/figure/emptyplot.pdf', width = 5.25, height = 3.5)

# regimp_movie -----------------------------------------------------------------
nstep <- 50

yvals <- sapply(imps, function(k) {
  s <- seq(-3.5, to = k, by = 5/(0.75*nstep))
  c(s, rep(k, nstep - length(s)))
})

linedat <- do.call(rbind, lapply(2:nrow(yvals), function(k) {
  data.frame(yline = c(yvals[c(1, k), ]),
             xline = rep(xmis, each = 2),
             group = rep(1:ncol(yvals), each = 2),
             time = k-1)
}))

dotdat <- data.frame(xline = rep(xmis, colSums(apply(yvals, 2, duplicated)) + 1),
                     yline = rep(imps,
                                 colSums(apply(yvals, 2, duplicated)) + 1),
                     time = as.numeric(unlist(sapply(colSums(apply(yvals, 2,
                                                                   duplicated)) + 1,
                                          function(k) (nstep-k):(nstep-1))))
)



regimp_movie <- reglineplot +
  geom_point(data = dotdat,
             aes(x = xline, y = yline),
             color = 2, shape = 19, size = 2) +
  geom_line(data = linedat,
            aes(x = xline, y = yline, group = group), color = 2, lty = 2) +
  transition_manual(time)

animate(regimp_movie, nframes = 49, fps = 10, width = 5.25, height = 3.5,
        units = 'in', res = 300,
        renderer = file_renderer("Slides/figure", prefix = "regimp_movie", overwrite = TRUE))


# multiple regression lines ----------------------------------------------------
set.seed(2019)
reglines_DF <- as.data.frame(MASS::mvrnorm(10, coef(lm1), vcov(lm1)))
reglines_DF$time <- as.numeric(1:nrow(reglines_DF))

regimps_DF <- do.call(rbind, lapply(xmis, function(k) {
  data.frame(x = k,
             y = reglines_DF$`(Intercept)` + k * reglines_DF$x,
             time = reglines_DF$time)
})
)
regimps_DF$id <- match(regimps_DF$x, unique(regimps_DF$x))

reglines_movie <- dataplot +
  geom_abline(data = reglines_DF, aes(intercept = `(Intercept)`,
                                      slope = x, group = time)) +
  transition_components(time, enter_length = 0.7,
                        exit_length = 1) +
  enter_fade() + exit_fade()

animate(reglines_movie, nframes = 50, fps = 10, width = 5.25, height = 3.5,
        units = 'in', res = 300,
        renderer = file_renderer("Slides/figure", prefix = "reglines_movie", overwrite = TRUE))



# regimps_movie ----------------------------------------------------------------
regimps_movie <- dataplot +
  geom_abline(data = reglines_DF, aes(intercept = `(Intercept)`,
                                      slope = x, group = time)) +
  geom_point(data = regimps_DF, aes(x = x, y = y, group = interaction(time, id)),
             size = 2, col = 2) +
  transition_components(time, enter_length = 0.7,
                        exit_length = 1) +
  enter_fade() + exit_fade()

animate(regimps_movie, nframes = 50, fps = 10, width = 5.25, height = 3.5,
        units = 'in', res = 300,
        renderer = file_renderer("Slides/figure", prefix = "regimps_movie", overwrite = TRUE))

# regimps_movie <- dataplot +
#   geom_abline(data = reglines_DF, aes(intercept = `(Intercept)`,
#                                       slope = x, group = time)) +
#   geom_point(data = regimps_DF, aes(x = x, y = y, group = time), size = 2, col = 2) +
#   transition_time(time = time)# + enter_fade() + exit_fade()


# add random error -------------------------------------------------------------

test <- regimps_DF[regimps_DF$time %in% c(2, 4, 6, 8, 10), ]
test$group <- match(test$x, unique(test$x))
s <- seq(-2, 2, length = 20)
dn <- dnorm(s, 0, summary(lm1)$sigma)

densdat <- do.call(rbind, lapply(seq_along(test$x), function(k) {
  data.frame(xval = rep(test$x[k] - dn + 0.05, 2),
             yval = rep(test$y[k] + s, 2),
             group = test$group[k],
             time = test$time[k] - rep(c(1, 0), each = length(s)),
             col = test$time[k])
}))

impdat <- do.call(rbind, lapply(seq_along(test$x), function(k) {
  data.frame(xval = test$x[k],
             yval = c(NA, rep(rnorm(1, test$y[k], summary(lm1)$sigma),
                      max(test$time) - test$time[k] + 1)),
             group = test$group[k],
             time = c(test$time[k] - 1, test$time[k]:max(test$time)),
             col = test$time[k])
}))

reglines_DFnew <- reglines_DF[rep(c(2, 4, 6, 8, 10), each = 2), ]
reglines_DFnew$col <- as.character(reglines_DFnew$time)
reglines_DFnew$time <- 1:10


ranerrplot <- baseplot_min +
  geom_rect(xmin = -2.15, xmax = 1.97, ymin = -3.5, ymax = 3.9,
            fill = 'transparent', color = grey(0.7)) +
  geom_point(aes(x = x, y = y2, shape = as.character(ismis),
                 color = as.character(ismis)),
             na.rm = T, size = 1.5, stroke = 1.2) +
  geom_abline(data = reglines_DFnew[reglines_DFnew$time == 1, ],
              aes(intercept = `(Intercept)`,
                  slope = x, group = time, color = col)) +
  geom_polygon(data = densdat[densdat$time == 1, ],
               aes(xval, yval, group = interaction(group, time),
                   fill = as.character(col)),
               alpha = 0.1) +
  geom_path(data = densdat[densdat$time == 1, ], na.rm = TRUE,
            aes(xval, yval, group = interaction(group, time),
                color = as.character(col))) +
  scale_y_continuous(limits = c(-3.6, 3.9), expand = c(0, 0)) +
  scale_color_manual(name = "", guide = FALSE,
                     limits = c("FALSE", "TRUE", 2, 4, 6, 8, 10),
                     values = c(grey(0.7), 'red', brewer.pal(5, 'Dark2'))) +
  scale_fill_manual(guide = FALSE, limits = c(2, 4, 6, 8, 10),
                    values = brewer.pal(5, 'Dark2')) +
  scale_shape_manual(name = "",
                     limits = c('FALSE', "TRUE"),
                     labels = c('observed', 'missing'),
                     values = c(19, 1))

ranerr_movie <- baseplot_min +
  geom_rect(xmin = -2.15, xmax = 1.97, ymin = -3.5, ymax = 3.9,
            fill = 'transparent', color = grey(0.7)) +
  geom_point(aes(x = x, y = y2, shape = as.character(ismis),
                 color = as.character(ismis)),
             na.rm = T, size = 1.5, stroke = 1.2) +
  geom_abline(data = reglines_DFnew,
              aes(intercept = `(Intercept)`,
                  slope = x, group = time, color = col)) +
  geom_polygon(data = densdat,
               aes(xval, yval, group = interaction(group, time),
                   fill = as.character(col)),
                  alpha = 0.1) +
  geom_path(data = densdat, na.rm = TRUE,
            aes(xval, yval, group = interaction(group, time),
                color = as.character(col))) +
  geom_point(data = impdat, size = 2, na.rm = TRUE, stroke = 1.2,
             aes(xval, yval, group = interaction(group, time),
                 color = as.character(col), shape = "imp")) +
  scale_y_continuous(limits = c(-3.6, 3.9), expand = c(0, 0)) +
  transition_manual(time) +
  scale_color_manual(name = "", guide = FALSE,
                     limits = c("FALSE", "TRUE", 2, 4, 6, 8, 10),
                     values = c(grey(0.7), 'red', brewer.pal(5, 'Dark2'))) +
  scale_fill_manual(guide = FALSE, limits = c(2, 4, 6, 8, 10),
                    values = brewer.pal(5, 'Dark2')) +
  scale_shape_manual(name = "",
                     limits = c('FALSE', "TRUE", "imp"),
                     values = c(19, 1, 8),
                     labels = c('observed', 'missing', 'imputed'))


animate(ranerr_movie, nframes = 10, fps = 10, width = 5.25, height = 3.75,
        units = 'in', res = 300,
        renderer = file_renderer("Slides/figure", prefix = "ranerr_movie", overwrite = TRUE))



ranerrimpsplot <- baseplot_min +
  geom_rect(xmin = -2.15, xmax = 1.97, ymin = -3.5, ymax = 3.9,
            fill = 'transparent', color = grey(0.7)) +
  geom_point(aes(x = x, y = y2, shape = as.character(ismis),
                 color = as.character(ismis)),
             na.rm = T, size = 1.5, stroke = 1.2) +
  geom_point(data = impdat, size = 2, na.rm = TRUE, stroke = 1.2,
             aes(xval, yval, group = interaction(group, time),
                 color = as.character(col), shape = "imp")) +
  scale_y_continuous(limits = c(-3.6, 3.9), expand = c(0, 0)) +
  scale_color_manual(name = "", guide = FALSE,
                     limits = c("FALSE", "TRUE", 2, 4, 6, 8, 10),
                     values = c(grey(0.7), 'red', brewer.pal(5, 'Dark2'))) +
  scale_shape_manual(name = "",
                     limits = c('FALSE', "TRUE", "imp"),
                     values = c(19, 1, 8),
                     labels = c('observed', 'missing', 'imputed'))

ggsave(ranerrimpsplot, file = 'Slides/figure/ranerrimpsplot.pdf', width = 5.25, height = 3.75)





# convergence plot -------------------------------------------------------------
library(ggplot2)
library(reshape2)

d <- 100
m <- 3
init <- c(-50, 50, 20)
x <- sapply(init, rnorm, n = d, sd = 0.1)
colnames(x) <- 1:ncol(x)
xorig <- rnorm(d/5, 3, 1)
mus <- sds <- matrix(nrow = 0, ncol = m)
for (i in 1:2000) {
  set <- x[nrow(x):(nrow(x) - d + 1), ]
  samp <- rbind(cbind(xorig, xorig, xorig), set)
  mu <- colMeans(samp) + rgamma(3, 0.1, 0.1)
  sd <- pmin(3, apply(samp, 2, sd))
  mus <- rbind(mus, mu)
  sds <- rbind(sds, sd)
  x <- rbind(x, rnorm(m, mu, sd))
}



ggplot(melt(x[d:nrow(x), ]), aes(x = Var1, y = value,
                                 color = factor(Var2))) +
  geom_line() +
  scale_color_brewer(palette = "Dark2", name = "",
                     labels = paste("chain", 1:3)) +
  theme_light() +
  theme(legend.position = c(0.9, 0.15),
        legend.background = element_rect(fill = 'transparent')) +
  xlab("iteration")
ggsave(file = 'Slides/figure/sim_convergence.pdf', width = 6, height = 4)


# simconv summary -------------------------------------------------------------
N = 5
p = 4
m = 3
maxit <- 30

dat <- expand.grid(list(id = paste("ID", 1:N),
                        x = paste0("x", 1:p),
                        imp = paste0("imp", 1:m),
                        it = 1:maxit))

coef <- c(0, 0.5, 0.2, 0.4, -0.1, -1, 0.5, 1)
X <- model.matrix(~ id + x, data = dat)
mu <- X %*% coef
dat$y <- rnorm(nrow(dat), mean = mu, sd = 0.5)


meandat <- plyr::ddply(dat, c("x", "imp", "it"), summarize, mean = mean(y))


ggplot(dat, aes(x = it, y = y, color = imp)) +
  geom_line() +
  facet_grid(id ~ x, scales = 'free') +
  theme(legend.position = 'bottom') +
  xlab("iteration") +
  ylab("imputed value") +
  scale_color_brewer(palette = "Dark2",
                     name = "imputation number:",
                     labels = 1:3)

ggsave(file = 'Slides/figure/convplot1a.pdf', width = 7, height = 4.5)


g2 <- ggplot(dat, aes(x = it, y = y, color = imp, group = id)) +
  facet_grid(imp ~ x, scales = 'free',
             labeller = labeller(imp = c("imp1" = "imputation 1",
                                         "imp2" = "imputation 2",
                                         "imp3" = "imputation 3"))) +
  theme(legend.position = 'bottom') +
  xlab("iteration") +
  ylab("imputed value") +
  scale_color_brewer(palette = "Dark2",
                     name = "imputation number:",
                     labels = c(1:3))

g2 + geom_line()
ggsave(file = 'Slides/figure/convplot1b.pdf', width = 7, height = 4.5)


g2 + geom_line(alpha = 0.5) +
  geom_line(data = meandat, aes(x = it, y = mean, group = 1), lwd = 1)
ggsave(file = 'Slides/figure/convplot1c.pdf', width = 7, height = 4.5)


ggplot(meandat, aes(x = it, y = mean, color = imp)) +
  geom_line(lwd = 0.7) +
  facet_wrap("x", scales = 'free', ncol = 4) +
  theme(legend.position = 'bottom') +
  xlab("iteration") +
  ylab("imputed value") +
  scale_color_brewer(palette = "Dark2",
                     name = "imputation number:",
                     labels = c(1:3))

ggsave(file = 'Slides/figure/convplot1d.pdf', width = 7.5, height = 2.5)


# imputaton --------------------------------------------------------------------
library(mice)
set.seed(123)
N <- 100
p <- 4
m <- 3

S <- diag(c(0.5, 5, 1, 1))
s <- c(0.7, -0.5, 0.15, 0.15, 0.1, 0.2)
S[upper.tri(S)] <- s
S[lower.tri(S)] <- s

X <- MASS::mvrnorm(n = N,
                   mu = c(0.2, 12, 3, 0.2),
                   Sigma = S)
colnames(X) <- paste0("x", 1:4)

DF_orig <- as.data.frame(cbind(id = 1:N, X))
DF_orig$id <- as.integer(DF_orig$id)

p1 <- rbinom(n = nrow(DF_orig), size = 1, prob = plogis(-5 + 2*DF_orig$x3))
p2 <- rbinom(n = nrow(DF_orig), size = 1, prob = plogis(4 - DF_orig$x3))
p3 <- rbinom(n = nrow(DF_orig), size = 1, prob = plogis(4 - DF_orig$x3))

p1[1:3] <- c(0, 1, 0)
p2[1:3] <- c(1, 0, 1)
p3[1:3] <- c(1, 0, 0)

DF_orig[p1 == 0, "x2"] <- NA
DF_orig[p2 == 0, "x3"] <- NA
DF_orig[p3 == 0, "x4"] <- NA

imp <- mice(DF_orig, m = m)
# round(complete(imp, 1)[1:3, ], 1)
res <- with(imp, lm(x1 ~ x2 + x3 + x4))

for (i in 1:3) {
  subres <- round(summary(res$analyses[[i]])$coef[,1:2], 2)
  pr <- paste0(paste0("$\\beta_", 0:3, "$"), " & ",
               apply(subres, 1, paste0, collapse = " & "), collapse = "\\\\\n")
  
  assign(paste0("print", i), pr)
}
save(print1, print2, print3, file = 'Slides/workspaces/PartI.RData')
  
# pooling ----------------------------------------------------------------------
# <<poolplot1a, echo = F, fig.width = 6, fig.height = 1.5>>=
reslist <- lapply(seq_along(res$analyses),
                  function(i) {
                    x <- res$analyses[[i]]
                    a <- as.data.frame(cbind(coef(x), confint(x)))
                    names(a) <- c("coef", "lo", "hi")
                    a$imp <- i
                    a$var <- rownames(a)
                    a
                  })

poolDF <- do.call(rbind, reslist)

ppool0 <- ggplot(poolDF, aes(x = coef, y = imp)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25) +
  facet_grid(~var, scales = 'free') +
  theme_light() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  scale_y_discrete(limits = c(1, 2, 3),
                   labels = c(paste("imp", 1:3))) +
  # scale_y_discrete(limits = c("pooled", 1,2,3),
  #                  labels = c("pooled", paste("imp", 1:3))) +
  ylab("") +
  scale_x_continuous(breaks = NULL) +
  xlab("parameter estimate & 95% confidence interval")
ggsave(ppool0, file = 'Slides/figure/poolplot1a.pdf', width = 6, height = 1.5)




ppool1b <- ppool0 +
  geom_vline(data = ddply(poolDF, "var", summarize, coefmean = mean(coef)),
             aes(xintercept = coefmean), lty = 2)
ggsave(ppool1b, file = 'Slides/figure/poolplot1b.pdf', width = 6, height = 1.5)


library(plyr)
seDF <- do.call(rbind,
                lapply(seq_along(res$analyses), function(i){
                  ses <- as.data.frame(summary(res$analyses[[i]])$coef[, 2, drop = F])
                  ses$var <- rownames(ses)
                  ses$imp <- i
                  ses
                })
)
means <- ddply(poolDF, "var", summarize, coefmean = mean(coef))
ses <- ddply(seDF, "var", summarize, semean = mean(`Std. Error`))

CIs <- ddply(poolDF, "var", summarize, himean = mean(hi), lomean = mean(lo))



ppool1c <- ppool0 +
  geom_vline(data = ddply(poolDF, "var", summarize, coefmean = mean(coef)),
             aes(xintercept = coefmean), lty = 2) +
  geom_vline(data = CIs, aes(xintercept = himean), col = 2, lty = 2) +
  geom_vline(data = CIs, aes(xintercept = lomean), col = 2, lty = 2)
ggsave(ppool1c, file = 'Slides/figure/poolplot1c.pdf', width = 6, height = 1.5)



# calculation by hand ---------------------------
# m <- 3
# alpha = 0.05
# Q <- means[,2]
# B <- 1/(m - 1) * colSums(matrix(nrow = m, data = (poolDF[, 1] - rep(Q, m))^2, byrow = T))
# U <- ses[, 2]^2
# T <- U + B + B/m
# 
# r <- (B + B/m)/U
# 
# nu = (m - 1) * (1 + mic$r^{-1})^2
# 
# gamma = (r + 2/(nu+3))/(r+1)
# 
# Q + qt(alpha/2, df = nu) * T^{1/2}
# Q - qt(alpha/2, df = nu) * T^{1/2}
# 
# 1 - pf(Q^2/T, df1 = 1, df2 = nu)

# pooled results ----------------------------------------------------------------
pooled <- as.data.frame(summary(pool(res), conf.int = TRUE)[, c("estimate", "2.5 %", "97.5 %")])
pooled$imp <- "pooled"
pooled$var <- rownames(reslist[[1]])


CIDF <- data.frame(
  x = rep(unlist(t(pooled[, 2:3])), each = 2),
  y = rep(c(0.5, 3.5)[c(1,2,2,1)], nrow(pooled)),
  var = rep(rownames(pooled), each = 4)
)

ggplot(poolDF, aes(x = coef, y = as.numeric(imp))) +
  geom_polygon(data = CIDF, aes(x = x, y = y), fill = "blue4", alpha = 0.2) +
  geom_point() +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25) +
  facet_grid(~var, scales = 'free') +
  theme_light() +
  theme(strip.text = element_text(size = 12, face = "bold")) +
  scale_y_discrete(limits = c(1, 2, 3),
                   labels = c(paste("imp", 1:3))) +
  ylab("") +
  scale_x_continuous(breaks = NULL) +
  xlab("parameter estimate & 95% confidence interval") +
  geom_vline(data = ddply(poolDF, "var", summarize, coefmean = mean(coef)),
             aes(xintercept = coefmean), lty = 2) +
  geom_vline(data = CIs, aes(xintercept = himean), col = 2, lty = 2) +
  geom_vline(data = CIs, aes(xintercept = lomean), col = 2, lty = 2)


ggsave(file = "Slides/figure/cis_final.pdf", width = 6, height = 1.5)
# geom_vline(data = CIs, aes(xintercept = himean), col = 2, lty = 2, alpha = 0.5) +
# geom_vline(data = CIs, aes(xintercept = lomean), col = 2, lty = 2, alpha = 0.5)
# geom_vline(data = pooled, aes(xintercept = `lo 95`), lty = 2) +
# geom_vline(data = pooled, aes(xintercept = `hi 95`), lty = 2)

