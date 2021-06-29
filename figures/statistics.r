library(TestDesign)
library(reshape)

itempool    <- loadItemPool("data/itempool_mixed.csv")
itemattrib  <- loadItemAttrib("data/itemattrib_mixed.csv", itempool)
stimattrib  <- loadStAttrib("data/stimattrib_mixed.csv", itemattrib)

# Average number of items in sets
mean(table(itemattrib@data$STID))

# Item models
table(itempool@model)

# Explained variation by passages
o <- itempool@ipar
o <- o[, -1]
for (i in 1:dim(o)[1]) {
  idx_remove <- setdiff(1:dim(o)[2], 1:(itempool@NCAT[i] - 1))
  o[i, idx_remove] <- NA
}
o <- apply(o, 1, function(x) mean(x, na.rm = TRUE))
o <- data.frame(b = o, STID = itemattrib@data$STID)

o <- o[!is.na(o$STID), ]
o$STID <- as.factor(o$STID)

fit <- aov(b ~ STID, o)
SS <- summary(fit)[[1]][["Sum Sq"]]
SS / sum(SS)
