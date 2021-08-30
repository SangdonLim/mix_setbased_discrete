library(TestDesign)

fp <- "~/Box Sync/Behaviormetrika_Special_Issue/Article_2_PassageCAT/Results"
# needs to be replaced with the path of your own simulation results folder
# run the simulation first using: simulation/study1.R

# requires TestDesign 1.2.6 for plotting exposure rate panels

setEPS()
postscript("figures/study1_exposure.eps", width = 8, height = 8)
par(mfrow = c(2, 2), oma = c(3, 2, 0, 2))

load(file.path(fp, "Study1", "solution_ds_none_maxinfo_1.Rdata"))
panel_a <- solution
plot(panel_a, type = "exposure", segment = 1, rmse = TRUE, use_segment_label = FALSE)
mtext("(a) No EC, MFI", side = 1, line = 5)

load(file.path(fp, "Study1", "solution_ds_none_goalinfo6_1.Rdata"))
panel_b <- solution
plot(panel_b, type = "exposure", segment = 1, rmse = TRUE, use_segment_label = FALSE)
mtext(paste0("(b) No EC, GFI", "\uad", "6"), side = 1, line = 5)

sum(panel_a@exposure_rate[, 3] > 0.25)
sum(panel_b@exposure_rate[, 3] > 0.25)
mean(panel_a@exposure_rate[, 3] > 0.25)
mean(panel_b@exposure_rate[, 3] > 0.25)

load(file.path(fp, "Study1", "solution_ds_bigm_maxinfo_1.Rdata"))
panel_c <- solution
plot(panel_c, type = "exposure", segment = 1, rmse = TRUE, use_segment_label = FALSE)
mtext("(c) With EC, MFI", side = 1, line = 5)

load(file.path(fp, "Study1", "solution_ds_bigm_goalinfo6_1.Rdata"))
panel_d <- solution
plot(panel_d, type = "exposure", segment = 1, rmse = TRUE, use_segment_label = FALSE)
mtext(paste0("(d) With EC, GFI", "\uad", "6"), side = 1, line = 5)

sd(panel_c@exposure_rate[, 3])
sd(panel_d@exposure_rate[, 3])

dev.off()
