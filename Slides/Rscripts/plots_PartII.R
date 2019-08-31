datadir <- file.path(projdir, "data")
load(file.path(datadir, "NHANES_for_lectures.RData"))
NHANES <- subset(NHANES, select = -DBP)


pdf('Slides/figure/JointAI_mdpat.pdf', width = 7, height = 5)
par(mar = c(5, 0.5, 1, 3), mgp = c(2, 0.6, 0))
JointAI::md_pattern(NHANES)
dev.off()


pdf('Slides/figure/VIM_mdpat.pdf', width = 10, height = 5)
par(mar = c(6, 3, 2, 1))
VIM::aggr(NHANES, prop = TRUE, numbers = FALSE)
dev.off()
