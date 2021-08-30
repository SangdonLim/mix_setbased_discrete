library(tools)
library(progress)
library(TestDesign)

fp <- "~/Box Sync/Behaviormetrika_Special_Issue/Article_2_PassageCAT/Results"
# needs to be replaced with the path of your own simulation results folder
# run the simulation first using: simulation/study2.R

fs <- list.files(file.path(fp, "Study2"))
o <- as.data.frame(matrix(NA, length(fs), 4))
colnames(o) <- c("weave", "exposure", "target", "replication")

oo <- o
oo$info <- NA

pb <- progress_bar$new(format = "[:bar] :current / :total | :eta", total = length(fs))

for (i in 1:length(fs)) {

  f <- fs[i]
  solution <- NULL
  load(file.path(fp, "Study2", f))
  IVs <- strsplit(file_path_sans_ext(f), "_")[[1]][-c(1:2)]
  if (IVs[4] == "always") next
  oo[i, 1:4] <- IVs[c(1:3, 5)]

  item_pool <- solution@constraints@pool
  info_all_items <- calcFisher(item_pool, solution@true_theta)
  info_list <-
    sapply(
      1:length(solution@output),
      function(i) {
        info_administered_items <- info_all_items[i, solution@output[[i]]@administered_item_index]
        testinfo <- sum(info_administered_items)
        return(testinfo)
      }
    )
  info_list <- unlist(info_list)
  oo$info[i] <- mean(info_list)

  pb$tick(1)

}

oo$weave    <- factor(oo$weave   , c("interspersed", "ds", "sd", "setbased"))
oo$exposure <- factor(oo$exposure, c("none", "bigm"))
oo$target   <- factor(oo$target  , c("maxinfo", "goalinfo8", "goalinfo7", "goalinfo6"))

write.csv(oo, "analysis/study2_testinfo.csv")

o <- aggregate(oo$info, by = list(oo$target), mean)
names(o)[2] <- "mean"
x <- aggregate(oo$info, by = list(oo$target), sd)$x
o[["sd"]] <- x
x <- aggregate(oo$info, by = list(oo$target), min)$x
o[["min"]] <- x
x <- aggregate(oo$info, by = list(oo$target), max)$x
o[["max"]] <- x
for (q in seq(.1, .9, .1)) {
  x <- aggregate(oo$info, by = list(oo$target), function(x) quantile(x, q))$x
  o[[sprintf("%s", q)]] <- x
}

write.csv(o, "analysis/study2_testinfo_average.csv")
