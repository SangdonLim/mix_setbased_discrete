library(TestDesign)

fp <- "~/Box Sync/Behaviormetrika_Special_Issue/Article_2_PassageCAT/Results"
# needs to be replaced with the path of your own simulation results folder
# run the simulation first using: simulation/study2.R

# requires TestDesign 1.2.6 for plotting exposure rate panels

setEPS()
postscript("figures/study2_exposure_a.eps", width = 8, height = 5)
par(mfrow = c(2, 3), oma = c(3, 2, 0, 2), mar = c(5, 5, 2, 1))
load(file.path(fp, "Study2", "solution_conditional_ds_bigm_maxinfo_setlevel_1.Rdata"))
panel_a <- solution
plot(panel_a, type = "exposure", segment = 0:5, rmse = TRUE)
mtext("(a) With EC, MFI", side = 1, line = 0.5, outer = TRUE)
dev.off()

setEPS()
postscript("figures/study2_exposure_b.eps", width = 8, height = 5)
par(mfrow = c(2, 3), oma = c(3, 2, 0, 2), mar = c(5, 5, 2, 1))
load(file.path(fp, "Study2", "solution_conditional_ds_bigm_goalinfo6_setlevel_1.Rdata"))
panel_b <- solution
plot(panel_a, type = "exposure", segment = 0:5, rmse = TRUE)
mtext(paste0("(b) With EC, GFI", "\uad", "6"), side = 1, line = 0.5, outer = TRUE)
dev.off()

sum(panel_a@exposure_rate[, 3] > 0.25)
sum(panel_b@exposure_rate[, 3] > 0.25)
mean(panel_a@exposure_rate[, 3] > 0.25)
mean(panel_b@exposure_rate[, 3] > 0.25)
sd(panel_a@exposure_rate[, 3])
sd(panel_b@exposure_rate[, 3])
