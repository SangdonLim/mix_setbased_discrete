library(tools)
library(progress)
library(TestDesign)

fp <- "~/Box Sync/Behaviormetrika_Special_Issue/Article_2_PassageCAT/Results"
# needs to be replaced with the path of your own simulation results folder
# run the simulation first using: simulation/study1.R

fs <- list.files(file.path(fp, "Study1"))
o <- as.data.frame(matrix(NA, length(fs), 4))
colnames(o) <- c("weave", "exposure", "target", "replication")

oo <- o
oo$rmse                <- NA
oo$overexposed         <- NA
oo$sd_exposure         <- NA

pb <- progress_bar$new(format = "[:bar] :current / :total | :eta", total = length(fs))

for (i in 1:length(fs)) {

  f <- fs[i]
  solution <- NULL
  load(file.path(fp, "Study1", f))
  IVs <- strsplit(file_path_sans_ext(f), "_")[[1]][-1]
  oo[i, 1:4] <- IVs
  oo$rmse[i] <- sqrt(mean((solution@final_theta_est - solution@true_theta)**2))

  er <- solution@exposure_rate[, "Item ER"]
  oo$overexposed[i] <- mean(er > 0.25)
  oo$sd_exposure[i] <- sd(er)

  pb$tick(1)

}

oo$weave    <- factor(oo$weave   , c("interspersed", "ds", "sd", "setbased"))
oo$exposure <- factor(oo$exposure, c("none", "bigm"))
oo$target   <- factor(oo$target  , c("maxinfo", "goalinfo8", "goalinfo7", "goalinfo6"))

write.csv(oo, "analysis/study1.csv", row.names = FALSE)
oo <- read.csv("analysis/study1.csv")

# ANOVA

DVs <- names(oo)[-(1:4)]
for (DV in DVs) {
  m <- formula(sprintf("%s ~ (weave + exposure + target)^2", DV))
  fit <- aov(eval(m), oo)
  x <- summary(fit)[[1]]
  f <- sprintf("analysis/study1_aov_%s.csv", DV)
  write.csv(x, f)
}

# main effects

DVs <- names(oo)[-(1:4)]
for (DV in DVs) {
  m <- formula(sprintf("%s ~ (weave + exposure + target)", DV))
  fit <- lm(eval(m), oo)
  x <- coef(summary(fit))
  f <- sprintf("analysis/study1_main_%s.csv", DV)
  write.csv(x, f)
}
