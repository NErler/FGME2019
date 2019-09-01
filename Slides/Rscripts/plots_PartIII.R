library(ggplot2)
library(lme4)

source('Slides/Rscripts/simdata_PartIII.R')

# quadratic exampe -------------------------------------------------------------
# * plot 0: base ---------------------------------------------------------------
p_qdr0 <- ggplot(DFexqdr, aes(x = x, y = y,
                              color = is.na(xmis),
                              shape = is.na(xmis))) +
  geom_point(data = DFexqdr[is.na(DFexqdr$xmis), ], na.rm = TRUE,
             aes(x = ximp, y = y, shape = 'shapeimp', color = 'colimp')) +
  geom_point(size = 2) +
  theme_light() +
  theme(legend.position = c(0.1, 0.15),
        legend.background = element_rect(fill = 'transparent'),
        legend.text = element_text(size = 13),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 15),
        axis.ticks = element_blank())


# * plot 1: obs + mis ------------------------------------------------------------
p_qdr1 <- p_qdr0 +
  scale_color_manual(name = "",
                     limits = c(F, T),
                     values = c("black", "darkgrey"),
                     labels = c("observed", "missing")) +
  scale_shape_manual(name = "",
                     limits = c(F, T),
                     values = c(19, 1),
                     labels = c("observed", "missing"))
ggsave(p_qdr1, file = 'Slides/figure/p_qdr1.pdf', width = 6, height = 4)

# * plot 2: obs, mis + imp -------------------------------------------------------
p_qdr2 <- p_qdr0 +
  scale_color_manual(name = "",
                     limits = c(F, T, 'colimp'),
                     values = c("black", "darkgrey", "black"),
                     labels = c("observed", "missing", "imputed")) +
  scale_shape_manual(name = "",
                     limits = c(F, T, 'shapeimp'),
                     values = c(19, 1, 8),
                     labels = c("observed", "missing", "imputed"))
ggsave(p_qdr2, file = 'Slides/figure/p_qdr2.pdf', width = 6, height = 4)


# * plot 3: reglines -------------------------------------------------------------
p_qdr3 <- p_qdr0 +
  geom_smooth(aes(group = "1", linetype = "ltycompl"), color = 'darkred',
              method = "lm", formula = y~x + I(x^2), se = F, lwd = 1.5) +
  geom_smooth(data = DFexqdr, aes(x = ximp, y = y, group = '1',
                                  linetype = "ltyimp"),
              color = 'darkred', se = F, lwd = 1.5,
              method = "lm", formula = y~x + I(x^2)) +
  scale_color_manual(name = "",
                     limits = c(F, T, 'colimp'),
                     values = c("black", "darkgrey", "black"),
                     labels = c("observed", "missing", "imputed")) +
  scale_shape_manual(name = "",
                     limits = c(F, T, 'shapeimp'),
                     values = c(19, 1, 8),
                     labels = c("observed", "missing", "imputed")) +
  scale_linetype_manual(name = "",
                        limits = c('ltycompl', 'ltyimp'),
                        values = c(1, 2),
                        labels = c("fit on complete", "fit on imputed")) +
  theme(legend.key.width = unit(1.2,"cm"),
        legend.title = element_blank(),
        legend.position = c(0.15, 0.19),
        legend.spacing.y = unit(-0.3, "lines")
  )

ggsave(p_qdr3, file = 'Slides/figure/p_qdr3.pdf', width = 6, height = 4)


# Interaction example ----------------------------------------------------------
# * plot 0: base ---------------------------------------------------------------
p_int0 <- ggplot(plotexint,
                 aes(x = value, y = y, color = combi, shape = combi, alpha = combi)) +
  theme_light() +
  theme(legend.position = c(0.15, 0.16),
        legend.background = element_rect(fill = 'transparent'),
        legend.key = element_rect(fill = 'transparent'),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        legend.spacing.x = unit(-0.1,  "lines"),
        legend.spacing.y = unit(-1.5, "lines"),
        legend.key.width = unit(1,"cm"),
        legend.box = "horizontal") +
  xlab("x")


# * plot 1: obs + mis ------------------------------------------------------------
p_int1 <- p_int0 +
  geom_point(size = 2, na.rm = TRUE) +
  scale_shape_manual(name = "",
                     limits = c("x_mis0", "x_mis1", "x_obs0", "x_obs1"),
                     values = c(1, 1, 19, 19),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)")) +
  scale_alpha_manual(name = "",
                     limits = c("x_mis0", "x_mis1", "x_obs0", "x_obs1"),
                     values = c(0.5, 0.5, 1, 1),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)")) +
  scale_color_manual(name = "",
                     limits = c("x_mis0", "x_mis1", "x_obs0", "x_obs1"),
                     values = c("blue", "chartreuse4", "blue", "chartreuse4"),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)"))

ggsave(p_int1, file = 'Slides/figure/p_int1.pdf', width = 6, height = 4.5)


# * plot 2: obs, mis + imp -------------------------------------------------------
p_int2 <- p_int0 +
  geom_point(na.rm = TRUE, size = 2) +
  scale_shape_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = c(1, 1, 19, 19, 8, 8),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_color_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = rep(c("blue", "chartreuse4"), 3),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_alpha_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = c(0.5, 0.5, 1, 1, 1, 1),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)"))

ggsave(p_int2, file = 'Slides/figure/p_int2.pdf', width = 6, height = 4.5)


# * plot 3: reglines -----------------------------------------------------------
p_int3 <- p_int0 +
  geom_point(size = 2, na.rm = TRUE, alpha = 0.3) +
  geom_smooth(data = plotexint[plotexint$z == "0", ], method = "lm", se = F,
              aes(x = value, y = y, linetype = variable,
                  group = variable), color = 'blue') +
  geom_smooth(data = plotexint[plotexint$z == "1", ], method = "lm", se = F,
              aes(x = value, y = y, linetype = variable,
                  group = variable), color = 'chartreuse4') +
  theme(legend.position = c(0.26, 0.19)) +
  expand_limits(y = -2.5) +
  scale_shape_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = c(1, 1, 19, 19, 8, 8),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_color_manual(name = "",
                     limits = c("x_mis0", "x_mis1",
                                "x_obs0", "x_obs1",
                                "ximp_mis0", "ximp_mis1"),
                     values = rep(c("blue", "chartreuse4"), 3),
                     labels = c("missing (z = 0)", "missing (z = 1)",
                                "observed (z = 0)", "observed (z = 1)",
                                "imputed (z = 0)", "imputed (z = 1)")) +
  scale_linetype_manual(name = "",
                        limits = c("x", "ximp"),
                        values = c(1, 2),
                        labels = c(" true",  " imputed"))

ggsave(p_int3, file = 'Slides/figure/p_int3.pdf', width = 6, height = 4.5)




# Longitudinal example --------------------------------------------------------
# * plong1_1 -------------------------------------------------------------------
plong1_0 <- ggplot(subDFexlong, aes(x = time, y = y, color = factor(id))) +
  theme(legend.position = "none",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = NULL,
                     minor_breaks = seq(from = 4, to = 6, by = 0.02))

plong1_1 <- plong1_0 +
  geom_line(lwd = 1) +
  geom_point(lwd = 2)

ggsave(plong1_1, file = 'Slides/figure/plong1_1.pdf', width = 6, height = 4.5)


plong1_1b <- plong1_1 +
  geom_vline(xintercept = seq(1, 9, 2), lty = 2) +
  scale_x_continuous(breaks = seq(1, 9, 2))

ggsave(plong1_1b, file = 'Slides/figure/plong1_1b.pdf', width = 6, height = 4.5)
  
  
# * reslong comparison ---------------------------------------------------------

  lme0 <- lmer(y ~ x1 + x2 + x3 + x4 + ns(time, df = 3) +
                 (time|id), data = DFexlong_orig)

lme_imp <- with(impexlong, lmer(y ~ x1 + x2 + x3 + x4 + ns(time, df = 3) +
                                  (time|id),
                                control = lmerControl(optimizer = 'Nelder_Mead')
))

res0 <- as.data.frame(cbind(fixef(lme0),
                            confint(lme0, parm = names(fixef(lme0)),
                                    method = "Wald")))
resimp <- as.data.frame(summary(pool(lme_imp), conf.int = TRUE)[, c("estimate", "2.5 %", "97.5 %")])

res0$mod <- "orig"
resimp$mod <- "imp"
names(res0) <- names(resimp) <- c("est", "lo", "hi", "mod")

res <- rbind(res0, resimp)
res$var <- rep(rownames(res0), 2)

p_impcomplong <- ggplot(res[!res$var %in% grep("time", res$var, value = T), ],
       aes(x = mod, y = est)) +
  geom_point() +
  geom_errorbar(aes(min = lo, max = hi), width = 0.2) +
  facet_wrap("var", scales = 'free',
             labeller = as_labeller(c("x2girl" = "x2",
                                      "(Intercept)" = "Intercept",
                                      "x1" = "x1",
                                      "x3low" = "x3 (low)",
                                      "x3mid" = "x3 (mid)",
                                      "x4" = "x4"))) +
  scale_x_discrete(limits = c("orig", "imp"),
                   labels = c("original", "imputed")) +
  xlab("") +
  ylab("estimate & 95% CI")

ggsave(p_impcomplong, file = 'Slides/figure/p_impcomplong.pdf', width = 5, height = 4)


# * wide format ----------------------------------------------------------------
implist <- mice::complete(impwide, 'long') %>% split(., .$.imp)

longList <- lapply(implist, reshape, direction = 'long',
                   varying = list(y = paste0("y.", seq(1, 9, 2)),
                                  time = paste0("time.", seq(1, 9, 2))),
                   v.names = c("y", "time"),
                   timevar = 'tp', drop = ".imp")

midsobj <- miceadds::datalist2mids(longList)

lme_impwide <- with(midsobj, lmer(y ~ x1 + x2 + x3 + x4 + ns(time, df = 3) + (time|id),
                                  control = lmerControl(optimizer = "Nelder_Mead")))
reswide <- as.data.frame(summary(pool(lme_impwide), conf.int = TRUE))[, c("estimate", "2.5 %", "97.5 %")]
reswide$mod <- "impwide"
names(reswide) <- names(resimp)
reswide$var <- rownames(res0)

res2 <- rbind(res, reswide)#, coefJointAI)

p_compwide <- ggplot(res2[!res2$var %in% grep("time", res2$var, value = T), ],
       aes(x = mod, y = est)) +
  geom_point() +
  geom_errorbar(aes(min = lo, max = hi), width = 0.2) +
  facet_wrap("var", scales = 'free',
             labeller = as_labeller(c("x2girl" = "x2",
                                      "(Intercept)" = "Intercept",
                                      "x1" = "x1",
                                      "x3low" = "x3 (low)",
                                      "x3mid" = "x3 (mid)",
                                      "x4" = "x4"))) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_discrete(limits = c("orig", "imp", "impwide"),#, "JointAI"),
                   labels = c("orig.", "imp.\nlong", "imp.\nwide")) +#, "JointAI")) +
  xlab("") +
  ylab("estimate & 95% CI")

ggsave(p_compwide, file = 'Slides/figure/p_compwide.pdf', width = 5, height = 4)


# long2 -----------------------------------------------------------------------
# * plong2_0 ------------------------------------------------------------------
plong2_0 <- ggplot(DFexlong2_sub, aes(x = time, y = y, color = factor(id))) +
  theme_light() +
  theme(legend.position = "none",
        axis.title = element_text(size = 14)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = NULL) +
  # minor_breaks = seq(from = 4, to = 6, by = 0.02)) +
  scale_x_continuous(breaks = NULL) +
  xlab("time")

# * plong2_1 -------------------------------------------------------------------
plong2_1 <- plong2_0 +
  geom_line(lwd = 1) +
  geom_point(lwd = 2)

ggsave(plong2_1, file = "Slides/figure/plong2_1.pdf", width = 6, height = 4)


# * plong2_2 -------------------------------------------------------------------
plong2_2 <- plong2_1 +
  geom_line(data = data.frame(x = c(min(DFexlong2_sub$time), max(DFexlong2_sub$time),
                                    min(DFexlong2_sub$time), max(DFexlong2_sub$time)),
                              y = c(min(DFexlong2_sub$y), max(DFexlong2_sub$y),
                                    max(DFexlong2_sub$y), min(DFexlong2_sub$y)),
                              id = c(1, 1, 2, 2)),
            aes(x = x, y = y, group = factor(id)), color = "red", lwd = 2) +
  theme(axis.title = element_text(size = 16))

ggsave(plong2_2, file = "Slides/figure/plong2_2.pdf", width = 6, height = 3.25)

# * plong2_3 -------------------------------------------------------------------
plong2_3 <- plong2_0 +
  geom_point(data = DFexlong2_sub[DFexlong2_sub$ti == 1, ],
             aes(x = time, y = y, fill = factor(id)),
             size = 8, shape = 21, alpha = 0.5) +
  geom_line(lwd = 1) +
  geom_point(lwd = 2) +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.title = element_text(size = 16))
ggsave(plong2_3, file = "Slides/figure/plong2_3.pdf", width = 6, height = 3.25)


# survival example -------------------------------------------------------------

# * imputation ----------------------------------------------------------------
imp00 <- mice(survdat, maxit = 0)

meth01 <- imp00$method
meth01[c("x1")] <- "norm"
meth01[c("x3")] <- "norm"

impsurv_naive <- mice(survdat, maxit = 10)


cox <- with(survdat_orig, coxph(Surv(Time, as.numeric(event)) ~ x1 + x2 + x3))
cox01 <- with(impsurv_naive, coxph(Surv(Time, as.numeric(event)) ~ x1 + x2 + x3))

rescox01 <- as.data.frame(summary(pool(cox01), conf.int = TRUE)[, c("estimate", "2.5 %", "97.5 %")])
rescox <- as.data.frame(cbind(cox$coef, confint(cox)))

rescox01$meth <- "norm"
rescox$meth <- "orig"

rescox$var = rownames(rescox)
rescox01$var = rownames(rescox)

names(rescox) <- names(rescox01)
plotcox <- rbind(rescox, rescox01)


p_compsurv <- ggplot(plotcox, aes(x = meth, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(min = `2.5 %`, max = `97.5 %`), width = 0.2) +
  theme(axis.text = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  facet_wrap("var", scales = 'free',
             labeller = labeller(var = c("x21" = "x2 (binary)",
                                         "x1" = "x1 (continuous)",
                                         "x3" = "x3 (continuous)"))) +
  scale_x_discrete(limits = c("orig", "norm"),
                   labels = c("original", "naive\nimputation")) +
  xlab("") +
  ylab("estimate & 95% CI")

ggsave(p_compsurv, file = 'Slides/figure/p_compsurv.pdf', width = 7, height = 3)  


# latent normal distribution ---------------------------------------------------
pdf("Slides/figure/latnormdistr.pdf", width = 6, height = 3)
par(mar = c(3.5, 4.4, 0.5, 0.5), mgp = c(2.5, 0.6, 0))
plot(seq(-4, 4, 0.1), dnorm(seq(-4, 4, 0.1)), type = "l", cex.lab = 1.5,
     xlab = expression(hat(x)[k]), ylab = expression(density~of~hat(x)[k]))
polygon(x = c(seq(-4, 0, 0.1), 0),
        y = c(dnorm(seq(-4, 0, 0.1)), 0), col = grey(0.9), border = 'transparent')
polygon(x = c(seq(4, 0, -0.1), 0),
        y = c(dnorm(seq(4, 0, -0.1)), 0), col = grey(0.7), border = 'transparent')
abline(v = 0, lty = 2)
lines(seq(-4, 4, 0.1), dnorm(seq(-4, 4, 0.1)))
text(x = -1, y = 0.05, label = bquote(x[k] == 0), cex = 1.5)
text(x = 1, y = 0.05, label = bquote(x[k] == 1), cex = 1.5)
dev.off()







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



# * imputation -----------------------------------------------------------------
# naive imputation, using only y, x, z
impnaive <- mice(DF_nonlin)


# add quadratic term and interaction to data
DF2 <- DF_nonlin
DF2$xx <- DF2$x^2
DF2$xz <- DF2$x * DF2$z

# JAV imputation
impJAV <- mice(DF2, maxit = 20)

# add interaction between y and z to data
DF3 <- DF2
DF3$yz <- DF3$y * DF3$z

# JAV imputation with additional interaction
impJAV2 <- mice(DF3,  maxit = 20)


# adapt the imputation method for quadratic imputation
methqdr <- impJAV$meth
methqdr[c("x", "xx", "xz")] <- c("quadratic", "~I(x^2)", "~I(x*z)")

# adapt the predictor matrix
predqdr <- impJAV$pred
predqdr[, "xx"] <- 0

impqdr <- mice(DF2, meth = methqdr, pred = predqdr,
               printFlag = F, maxit = 10)

# * plot results ---------------------------------------------------------------
res_impnaive <- with(impnaive, lm(y ~ x + I(x^2) + z + x:z)) %>% pool %>% summary(conf.int = TRUE)
res_JAV <- with(impJAV, lm(y ~ x + xx + z + xz)) %>% pool %>% summary(conf.int = TRUE)
res_JAV2 <- with(impJAV2, lm(y ~ x + xx + z + xz)) %>% pool %>% summary(conf.int = TRUE)
res_qdr <- with(impqdr, lm(y ~ x + xx + z + x:z)) %>% pool %>% summary(conf.int = TRUE)

resqdr <- rbind(
  with(impnaive, lm(y ~ x + I(x^2) + z + x:z)) %>% pool %>% summary(conf.int = TRUE),
  with(impJAV, lm(y ~ x + xx + z + xz)) %>% pool %>% summary(conf.int = TRUE),
  with(impJAV2, lm(y ~ x + xx + z + xz)) %>% pool %>% summary(conf.int = TRUE),
  with(impqdr, lm(y ~ x + xx + z + x:z)) %>% pool %>% summary(conf.int = TRUE)
  
)[, c("estimate", '2.5 %', '97.5 %')] %>%
  `row.names<-`(1:(4*5)) %>%
  as.data.frame() %>%
  plyr::mutate(meth = rep(c("naive", "JAV", "JAV2", "qdr"), each = 5),
               var = rep(c("(Intercept)", "x", "I(x^2)", "z", "x:z"), 4),
               beta = rep(coef(mod_nonlin), 4)
  )

polyDF_nonlin <- data.frame(x = rep(c(0, 5, 5, 0), 5),
                            y = c(apply(confint(mod_nonlin), 1, rep, each = 2)),
                            var = rep(c("(Intercept)", "x", "I(x^2)", "z", "x:z"),
                                      each = 4)
)

p_comp_nonlin <- ggplot(resqdr, aes(x = meth, y = estimate)) +
  geom_polygon(data = polyDF_nonlin,
               aes(x = x, y = y), fill = 'blue', alpha = 0.2) +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = 0.3) +
  facet_wrap("var", scales = 'free') +
  geom_hline(aes(yintercept = beta), lty = 2) +
  scale_x_discrete(limits = c("naive", "JAV", "JAV2", "qdr")) +
  ylab("estimate & 95% CI") +
  xlab("imputation method")

ggsave(p_comp_nonlin, file = "Slides/figure/p_comp_nonlin.pdf", width = 6, height = 3.5)


# JointAI nonlin ---------------------------------------------------------------
load('Slides/workspaces/JointAI_nonlin.RData')
pdf('Slides/figure/traceplotnonlin.pdf', width = 6, height = 4)
traceplot(JointAI_nonlin, ncol = 3)
dev.off()

res_JointAI_nonlin <- summary(JointAI_nonlin)

  
res_nonlin <- list(naive = as.data.frame(res_impnaive[, c("estimate", "2.5 %", "97.5 %")]),
                   JAV = as.data.frame(res_JAV[, c("estimate", "2.5 %", "97.5 %")]),
                   JAV2 = as.data.frame(res_JAV2[, c("estimate", "2.5 %", "97.5 %")]),
                   qdr = as.data.frame(res_qdr[, c("estimate", "2.5 %", "97.5 %")]),
                   JointAI = as.data.frame(res_JointAI_nonlin$stat[names(coef(JointAI_nonlin)),
                                                                   c(1,3,4)])
                   # smcfcs = as.data.frame(res_smcfcs_nonlin[, c("estimate", "2.5 %", "97.5 %")]),
                   # jomo = as.data.frame(res_jomo_nonlin[, c("estimate", "2.5 %", "97.5 %")])
)

res_nonlin <- lapply(res_nonlin, function(x) {
  colnames(x) <- c("coef", "lo", "up")
  x$var <- gsub("xx", "I(x^2)", rownames(x)) %>%
    gsub("z1", "z", .) %>%
    gsub("xz", "x:z", .)
  x
})

plot_nonlin <- reshape2::melt(res_nonlin, id.vars = c("coef", "lo", "up", "var"))

polyDF <- data.frame(x = rep(c(0.5, length(res_nonlin) + .5)[c(1,2,2,1)], 5),
                     y = c(apply(confint(mod_nonlin), 1, rep, each = 2)),
                     var = rep(c("(Intercept)", "x", "I(x^2)", "z", "x:z"), each = 4)
)

p_comp_nonlin2 <- ggplot(plot_nonlin, aes(x = L1, y = coef)) +
  geom_point() +
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.3) +
  facet_wrap("var", scales = 'free') +
  geom_polygon(data = polyDF, aes(x = x, y = y), fill = 'blue', alpha = 0.2) +
  geom_hline(data = data.frame(betas = coef(mod_nonlin),
                               var = names(coef(mod_nonlin))),
             aes(yintercept = betas), lty = 2) +
  ylab("estimate & 95% CI") +
  xlab("imputation method") +
  scale_x_discrete(limits = c("naive", "JAV", "JAV2", "qdr",
                              "JointAI")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p_comp_nonlin2, file = 'Slides/figure/p_comp_nonlin2.pdf', width = 6, height = 4)


# longitudinal imputaiton -------------------------------------------------------
# * imputations ----------------------------------------------------------------
imp0 <- mice(DFexlong2, maxit = 0)
meth <- imp0$method
pred <- imp0$predictorMatrix

meth[c("x2", "x3")] <- "2lonly.pmm"
meth[c("x4")] <- "2lonly.norm"

pred[, "id"] <- -2  # identify id variable
pred[, "ti"] <- 0 # don't use time-point indicator


imp <- mice(DFexlong2, maxit = 10, method = meth,
            predictorMatrix = pred)


pred[, c("id", "ti")] <- 0
impnaive <- mice(DFexlong2, predictorMatrix = pred, maxit = 10)
models2 <- with(impnaive,
                lmer(y ~ x1 + x2 + x3 + x4 + time + I(time^2) + (time|id)))

mice_longimp_naive <- summary(pool(models2), conf.int = TRUE)


# * plots ----------------------------------------------------------------------
load("Slides/workspaces/JointAI_long.RData")
res_JointAI_long <- summary(JointAI_long)

models <- with(imp, lmer(y ~ x1 + x2 + x3 + x4 + time + I(time^2) +
                           (time|id),
                         control = lmerControl(optimizer = "Nelder_Mead")
))
mice_longimp <- summary(pool(models), conf.int = TRUE)

  
mod_long2 <- with(DFexlong2_orig,
                  lmer(y ~ x1 + x2 + x3 + x4 + time + I(time^2) + (time|id)))

res_long2 <- list(JointAI = as.data.frame(res_JointAI_long$stat[names(coef(JointAI_long)),
                                                                c(1,3,4)]),
                  mice_long = as.data.frame(mice_longimp)[, c("estimate", "2.5 %", "97.5 %")],
                  mice_naive = as.data.frame(mice_longimp_naive)[, c("estimate", "2.5 %", "97.5 %")]
                  # mice_wide = as.data.frame(mice_wideimp)[, c("est", "lo 95", "hi 95")]
)

res_long2 <- lapply(res_long2, function(x) {
  colnames(x) <- c("coef", "lo", "up")
  x$var <- rownames(x)
  x$var <- gsub("x22", "x21", rownames(x))
  x
})

plot_long <- reshape2::melt(res_long2, id.vars = c("coef", "lo", "up", "var"))

polyDF <- data.frame(x = rep(c(0.5, length(res_long2) + .5)[c(1,2,2,1)], 8),
                     y = c(apply(confint(mod_long2,
                                         method = "Wald",
                                         parm = names(fixef(mod_long2))), 1, rep,
                                 each = 2)),
                     var = rep(names(fixef(mod_long2)), each = 4)
)

p_comp_long <- ggplot(plot_long[plot_long$L1 != "mice_naive", ], aes(x = L1, y = coef)) +
  geom_point(na.rm = T) +
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.3, na.rm = T) +
  facet_wrap("var", scales = 'free', ncol = 4 ) +
  geom_polygon(data = polyDF, aes(x = pmin(x, 2.5), y = y), fill = 'blue', alpha = 0.2) +
  geom_hline(data = data.frame(betas = fixef(mod_long2),
                               var = names(fixef(mod_long2))),
             aes(yintercept = betas), lty = 2) +
  ylab("coefficient") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = c("mice_long", "JointAI"),
                   labels = c("mice", "JointAI"))

ggsave(p_comp_long, file= 'Slides/figure/p_comp_long.pdf', width = 6, height = 4)

p_comp_long2 <- ggplot(plot_long, aes(x = L1, y = coef)) +
  geom_point(na.rm = T) +
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.3, na.rm = T) +
  facet_wrap("var", scales = 'free', ncol = 4) +
  geom_polygon(data = polyDF, aes(x = x, y = y), fill = 'blue', alpha = 0.2) +
  geom_hline(data = data.frame(betas = fixef(mod_long2),
                               var = names(fixef(mod_long2))),
             aes(yintercept = betas), lty = 2) +
  xlab("") +
  ylab("coefficient") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = c("mice_long", "JointAI", "mice_naive"),
                   labels = c("mice", "JointAI", "mice\nnaive"))
ggsave(p_comp_long2, file= 'Slides/figure/p_comp_long2.pdf', width = 6, height = 4)


# survival --------------------------------------------
# * imputation -------------------------------------
survdat$H0 <- nelsonaalen(survdat, timevar = Time, statusvar = event)

# setup run
imp0 <- mice(survdat, maxit = 0)
meth <- imp0$method
pred <- imp0$predictorMatrix

# specify normal imputation for continuous covariates
meth[c("x1", "x3")] <- "norm"

# remove event time from predictor (high correlation with H0)
pred[, "Time"] <- 0


survimp <- mice(survdat, maxit = 10, method = meth,
                predictorMatrix = pred)

cox_mice <- with(survimp, coxph(Surv(Time, event) ~ x1 + x2 + x3))
res_mice_surv <- summary(pool(cox_mice, dfcom = 99999), conf.int = TRUE)


load('Slides/workspaces/JointAI_surv.RData')

# * plot ----------------------------------------------------------------
mod_cox <- with(survdat_orig, coxph(Surv(Time, event) ~ x1 + x2 + x3))
mod_survreg <- with(survdat_orig, survreg(Surv(Time, event) ~ x1 + x2 + x3))

mice_wb <- with(survimp, survreg(Surv(Time, event) ~  x1 + x2 + x3))
mice_naive_wb <- with(impsurv_naive, survreg(Surv(Time, event) ~  x1 + x2 + x3))
mice_naive_cox <- with(impsurv_naive, coxph(Surv(Time, event) ~  x1 + x2 + x3))


res_survreg <- list(mice_naive = summary(pool(mice_naive_wb, dfcom = 99999),
                                         conf.int = TRUE)[names(coef(JointAI_survreg)),
                                                          c('estimate', '2.5 %', '97.5 %')],
                    mice = summary(pool(mice_wb, dfcom = 99999), conf.int = TRUE)[names( coef(JointAI_survreg)),
                                                                                  c("estimate", "2.5 %", "97.5 %")],
                    JointAI = as.data.frame(summary(JointAI_survreg)$stat[
                      names(coef(JointAI_survreg)),
                      c(1,3,4)])
)

res_cox <- list(mice_naive = summary(pool(mice_naive_cox, dfcom = 999999),
                                     conf.int = TRUE)[, c('estimate', '2.5 %', '97.5 %')],
                mice = as.data.frame(res_mice_surv)[, c("estimate", "2.5 %", "97.5 %")],
                JointAI = as.data.frame(summary(JointAI_surv)$stat[names(coef(JointAI_surv)),
                                                                   c(1,3,4)])
)

res_survreg <- lapply(res_survreg, function(x) {
  colnames(x) <- c("coef", "lo", "up")
  x$var <- rownames(x)
  x
})


res_cox <- lapply(res_cox, function(x) {
  colnames(x) <- c("coef", "lo", "up")
  x$var <- rownames(x)
  x
})

plot_survreg <- reshape2::melt(res_survreg, id.vars = c("coef", "lo", "up", "var"))
plot_cox <- reshape2::melt(res_cox, id.vars = c("coef", "lo", "up", "var"))

polyDF_cox <- data.frame(x = rep(c(0.5, length(res_cox) + .5)[c(1,2,2,1)],
                                 length(mod_cox$coef)),
                         y = c(apply(log(summary(mod_cox)$conf.int[, 3:4]), 1, rep,
                                     each = 2)),
                         var = rep(names(coef(JointAI_surv)), each = 4)
)


polyDF_survreg <- data.frame(x = rep(c(0.5, length(res_survreg) + .5)[c(1,2,2,1)],
                                     length(mod_survreg$coef)),
                             y = c(apply(confint(mod_survreg), 1, rep,
                                         each = 2)),
                             var = rep(names(coef(JointAI_survreg)), each = 4)
)


p_comp_cox <- ggplot(plot_cox, aes(x = L1, y = coef)) +
  geom_point(na.rm = T) +
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.3, na.rm = T) +
  facet_wrap("var", scales = 'free', nrow = 1) +
  geom_polygon(data = polyDF_cox, aes(x = x, y = y), fill = 'blue', alpha = 0.2) +
  geom_hline(data = data.frame(betas = coef(mod_cox),
                               var = names(coef(JointAI_surv))),
             aes(yintercept = betas), lty = 2) +
  ylab("coefficient") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = c("mice_naive", "mice", "JointAI"),
                   labels = c("mice\nnaive", "mice", "JointAI"))

ggsave(p_comp_cox, file = 'Slides/figure/p_comp_cox.pdf', width = 6, height = 2.5)


p_comp_survreg <- ggplot(plot_survreg, aes(x = L1, y = coef)) +
  geom_point(na.rm = T) +
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.3, na.rm = T) +
  facet_wrap("var", scales = 'free', nrow = 1) +
  geom_polygon(data = polyDF_survreg, aes(x = x, y = y), fill = 'blue', alpha = 0.2) +
  geom_hline(data = data.frame(betas = coef(mod_survreg),
                               var = names(coef(JointAI_survreg))),
             aes(yintercept = betas), lty = 2) +
  ylab("coefficient") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(limits = c("mice_naive", "mice", "JointAI"),
                   labels = c("mice\nnaive", "mice", "JointAI"))
  

ggsave(p_comp_survreg, file = 'Slides/figure/p_comp_survreg.pdf', width = 6, height = 2.5)
