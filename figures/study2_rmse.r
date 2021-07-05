library(TestDesign)

plotRMSE <- function(oo) {

  plot(
    0, 0, type = "n",
    xlim = c(-2, 2), ylim = c(0, 0.8),
    xlab = "True theta", ylab = "RMSE"
  )

  lines(-2:2, subset(oo, target == "maxinfo"   & refresh == "setlevel")[5:9], col = "black", lwd = 3)
  lines(-2:2, subset(oo, target == "goalinfo8" & refresh == "setlevel")[5:9], col = "blue", lwd = 3)
  lines(-2:2, subset(oo, target == "goalinfo7" & refresh == "setlevel")[5:9], col = "blue", lwd = 2)
  lines(-2:2, subset(oo, target == "goalinfo6" & refresh == "setlevel")[5:9], col = "blue", lwd = 1)
  lines(-2:2, subset(oo, target == "maxinfo"   & refresh == "always")[5:9]  , col = "red", lwd = 3, lty = 3)

  legend(
    "topleft",
    c("MFI (always)",
      "MFI",
      paste0("GFI", "\uad", "8"),
      paste0("GFI", "\uad", "7"),
      paste0("GFI", "\uad", "6")
    ),
    lty = c(3, 1, 1, 1, 1),
    lwd = c(3, 3, 3, 2, 1),
    col = c("red", "black", "blue", "blue", "blue")
  )

}

o <- read.csv("analysis/study2.csv", check.names = FALSE)
o$weave    <- factor(o$weave   , c("interspersed", "ds", "sd", "setbased"))
o$exposure <- factor(o$exposure, c("none", "bigm"))
o$target   <- factor(o$target  , c("maxinfo", "goalinfo8", "goalinfo7", "goalinfo6"))
o$refresh  <- factor(o$refresh , c("always", "setlevel"))

o <- aggregate(
  o[, c("rmse_-2", "rmse_-1", "rmse_0", "rmse_1", "rmse_2")],
  by = list(o$weave, o$exposure, o$target, o$refresh),
  function(x) {
    sqrt(mean(x ** 2))
  }
)
names(o)[1:4] <- c("weave", "exposure", "target", "refresh")

setEPS()
postscript("figures/study2_rmse_ds.eps", width = 8, height = 5)

par(mfrow = c(1, 2), oma = c(2, 1, 0, 1), mar = c(5, 4, 1, 1))
oo <- subset(o, weave == "ds" & exposure == "none")
plotRMSE(oo)
mtext(paste0("(a) No EC"), side = 1, line = 5)
oo <- subset(o, weave == "ds" & exposure == "bigm")
plotRMSE(oo)
mtext(paste0("(b) With EC"), side = 1, line = 5)

dev.off()

setEPS()
postscript("figures/study2_rmse_setonly.eps", width = 8, height = 5)

par(mfrow = c(1, 2), oma = c(2, 1, 0, 1), mar = c(5, 4, 1, 1))
oo <- subset(o, weave == "setbased" & exposure == "none")
plotRMSE(oo)
mtext(paste0("(a) No EC"), side = 1, line = 5)
oo <- subset(o, weave == "setbased" & exposure == "bigm")
plotRMSE(oo)
mtext(paste0("(b) With EC"), side = 1, line = 5)

dev.off()
