rstudioapi::restartSession()

library(TestDesign)

setEPS()
postscript("figures/pool_info.eps", width = 7, height = 5)

itempool    <- loadItemPool("data/itempool_mixed.csv")
itemattrib  <- loadItemAttrib("data/itemattrib_mixed.csv", itempool)
stimattrib  <- loadStAttrib("data/stimattrib_mixed.csv", itemattrib)

itempool_setbased <- itempool[which(itemattrib@data$IS_SET == 1)]
itempool_discrete <- itempool[which(itemattrib@data$IS_DISC == 1)]

theta_grid <- seq(-4, 4, .1)
info_all      <- calcFisher(itempool, theta_grid)
info_setbased <- calcFisher(itempool_setbased, theta_grid)
info_discrete <- calcFisher(itempool_discrete, theta_grid)

info_all      <- apply(info_all     , 1, sum)
info_setbased <- apply(info_setbased, 1, sum)
info_discrete <- apply(info_discrete, 1, sum)

plot(
  theta_grid, theta_grid, type = "n",
  xlab = "Theta",
  ylab = "Information", ylim = c(0, 120),
  main = sprintf("Pool%slevel Information", "\uad"))
lines(theta_grid, info_all     , col = "blue")
lines(theta_grid, info_setbased, col = "red")
lines(theta_grid, info_discrete, col = "blue", lty = 2)

legend(
  "topleft",
  c(
    sprintf("All items (n = %s)", itempool@ni),
    sprintf("Set%sbased items (n = %s)", "\uad", itempool_setbased@ni),
    sprintf("Discrete items (n = %s)", itempool_discrete@ni)
  ),
  col = c("blue", "red", "blue"),
  lty = c(1, 1, 2)
)

dev.off()
