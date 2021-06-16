rstudioapi::restartSession()

library(TestDesign)

itempool   <- loadItemPool("data/itempool_mixed.csv")
itemattrib <- loadItemAttrib("data/itemattrib_mixed.csv", itempool)
stimattrib <- loadStAttrib("data/stimattrib_mixed.csv", itemattrib)

o <- list()
for (s in unique(na.omit(itemattrib@data$STID))) {
  idx_items_in_set <- which(itemattrib@data$STID == s)
  b_params <- itempool[idx_items_in_set]@ipar[, -1]
  for (i in 1:length(idx_items_in_set)) {
    b_params
    idx_to_omit <- setdiff(
      1:dim(b_params)[2],
      1:(itempool[idx_items_in_set]@NCAT[i] - 1)
    )
    b_params[i, idx_to_omit] <- NA
  }

  b_itemwise_average <- apply(
    b_params,
    1,
    function(x) {
      mean(x, na.rm = TRUE)
    }
  )

  o[[s]]$ipar_b <- b_itemwise_average
  o[[s]]$ipar_b_mean <- mean(o[[s]]$ipar_b)

}

o <- o[order(sapply(o, function(x) x$ipar_b_mean))]

setEPS()
postscript("figures/ipar_setwise.eps", width = 7, height = 5)

plot(
  0, 0, type = "n", axes = FALSE, xlim = c(-4, 4), ylim = c(0, length(o)),
  xlab = "Difficulty", ylab = "Item Sets")
axis(1)

for (i in 1:length(o)) {
  lines(
    range(o[[i]]$ipar_b),
    rep(i, 2)
  )
  points(
    o[[i]]$ipar_b,
    rep(i, length(o[[i]]$ipar_b)),
    col = "blue"
  )
  points(
    o[[i]]$ipar_b_mean,
    i,
    col = "red", bg = "red",
    pch = 23
  )
}

box(lwd = 1)

dev.off()
