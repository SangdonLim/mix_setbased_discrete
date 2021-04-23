library(tools)
library(progress)
library(TestDesign)

fp <- "~/Box Sync/Behaviormetrika_Special_Issue/Article_2_PassageCAT/Results"

fs <- list.files(file.path(fp, "Study2"))
o <- as.data.frame(matrix(NA, length(fs), 5))
colnames(o) <- c("weave", "exposure", "target", "refresh", "replication")

oo <- o
oo$rmse                <- NA
for (theta in seq(-2, 2)) {
  v <- sprintf("rmse_%s", theta)
  oo[[v]] <- NA
}
oo$overexposed         <- NA
oo$sd_exposure         <- NA

pb <- progress_bar$new(format = "[:bar] :current / :total | :eta", total = length(fs))

for (i in 1:length(fs)) {

  f <- fs[i]
  solution <- NULL
  load(file.path(fp, "Study2", f))
  IVs <- strsplit(file_path_sans_ext(f), "_")[[1]][-c(1:2)]
  oo[i, 1:5] <- IVs

  oo$rmse[i] <- sqrt(mean((solution@final_theta_est - solution@true_theta)**2))
  for (theta in seq(-2, 2)) {
    v <- sprintf("rmse_%s", theta)
    idx <- solution@true_theta == theta
    true_theta_subset <- solution@true_theta[idx]
    final_theta_subset <- solution@final_theta_est[idx]
    oo[[v]][i] <- sqrt(mean((final_theta_subset - true_theta_subset)**2))
  }

  er <- solution@exposure_rate[, "Item ER"]
  oo$overexposed[i] <- mean(er > 0.25)
  oo$sd_exposure[i] <- sd(er)

  pb$tick(1)

}

oo$weave    <- factor(oo$weave   , c("interspersed", "ds", "sd", "setbased"))
oo$exposure <- factor(oo$exposure, c("none", "bigm"))
oo$target   <- factor(oo$target  , c("maxinfo", "goalinfo8", "goalinfo7", "goalinfo6"))
oo$refresh  <- factor(oo$refresh , c("always", "setlevel"))

write.csv(oo, "analysis/study2.csv", row.names = FALSE)
oo <- read.csv("analysis/study2.csv", check.names = FALSE)

# ANOVA

oo <- subset(oo, refresh == "setlevel")

DVs <- c("rmse", "overexposed", "sd_exposure")
for (DV in DVs) {
  m <- formula(sprintf("`%s` ~ (weave + exposure + target)^2", DV))
  fit <- aov(m, oo)
  x <- summary(fit)[[1]]
  f <- sprintf("analysis/study2_aov_%s.csv", DV)
  write.csv(x, f)
}

# main effects

DVs <- c("rmse", "overexposed", "sd_exposure")
for (DV in DVs) {
  m <- formula(sprintf("`%s` ~ (weave + exposure + target)", DV))
  fit <- lm(eval(m), oo)
  x <- coef(summary(fit))
  f <- sprintf("analysis/study2_main_%s.csv", DV)
  write.csv(x, f)
}
