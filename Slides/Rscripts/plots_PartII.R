load(file.path('Slides', 'data', "NHANES_for_lectures.RData"))
NHANES <- subset(NHANES, select = -DBP)


pdf('Slides/figure/JointAI_mdpat.pdf', width = 7, height = 5)
par(mar = c(5, 0.5, 1, 3), mgp = c(2, 0.6, 0))
JointAI::md_pattern(NHANES)
dev.off()


pdf('Slides/figure/VIM_mdpat.pdf', width = 10, height = 5)
par(mar = c(6, 3, 2, 1))
VIM::aggr(NHANES, prop = TRUE, numbers = FALSE)
dev.off()

pdf('Slides/figure/naniar_vis_miss.pdf', width = 10, height = 5)
naniar::vis_miss(NHANES)
dev.off()

pdf('Slides/figure/naniar_miss_upset.pdf', width = 10, height = 5, onefile = FALSE)
naniar::gg_miss_upset(NHANES[, 1:10], nsets = 10)
dev.off()


pdf('Slides/figure/plot_all.pdf', width = 8.5, height = 4.78)
par(mgp = c(2, 0.6, 0), mar = c(2, 3, 2.5, 0.5))
JointAI::plot_all(NHANES, nclass = 30)
dev.off()


Corr <- cor(sapply(NHANES, as.numeric),
            use = "pairwise.complete.obs", method = "spearman")

pdf('Slides/figure/corrplot.pdf', width = 7, height = 6)
corrplot::corrplot(Corr, method = "square", type = "upper",
                   tl.col = "black")

dev.off()




load(file.path("Slides", "workspaces/imp2.RData"))
# load(file.path("Slides", "workspaces/imp2b.RData"))

pdf('Slides/figure/convergence_imp2.pdf', width = 10, height = 5, onefile = FALSE)
plot(imp2, layout = c(6, 3))
dev.off()


pdf("Slides/figure/densityplot_imp2.pdf", width = 9, height = 5)
densityplot(imp2)
dev.off()

pdf('Slides/figure/densityplot_imp2_hgt.pdf', width = 9, height = 5)
densityplot(imp2, ~hgt|gender, plot.points = TRUE)
dev.off()

pdf('Slides/figure/stripplot_imp2.pdf', width = 10, height = 5)
stripplot(imp2, hgt ~ race|gender, pch = c(1, 20),
          scales = list(x = list(rot = 45)))
dev.off()



source(file.path("Slides", "Rscripts", "propplot.R"))

pdf('Slides/figure/propplot_imp2.pdf', width = 10, height = 6)
propplot(imp2, strip.text = element_text(size = 14))
dev.off()
  
  
