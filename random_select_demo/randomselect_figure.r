library(TestDesign)

load("random_select_demo/randomselect_demo_first.RData")
load("random_select_demo/randomselect_demo_random.RData")

# Aggregate information by position
info_by_position_first <- lapply(
  solution_first@output,
  function(x) {
    info_at_true_theta <-
      calcFisher(
        solution_first@constraints@pool[x@administered_item_index],
        x@true_theta
      )
    return(info_at_true_theta)
  }
)
info_by_position_first <- do.call("rbind", info_by_position_first)
info_by_position_first <- apply(info_by_position_first, 2, mean)

info_by_position_random <- lapply(
  solution_random@output,
  function(x) {
    info_at_true_theta <-
      calcFisher(
        solution_random@constraints@pool[x@administered_item_index],
        x@true_theta
      )
    return(info_at_true_theta)
  }
)
info_by_position_random <- do.call("rbind", info_by_position_random)
info_by_position_random <- apply(info_by_position_random, 2, mean)

summary_first  <- summary(solution_first)
summary_random <- summary(solution_random)

# Plot information by position

setEPS()
postscript("random_select_demo/randomselect_demo.eps", height = 5, width = 7)

plot(
  0, 0,
  type = "n",
  xlim = c(1, 30),
  ylim = c(0, 1),
  xlab = "Item Position",
  ylab = "Average information of administered item at true theta"
)
lines(
  1:30,
  info_by_position_first,
  col = "blue"
)
lines(
  1:30,
  info_by_position_random,
  col = "red", lty = 2
)
legend(
  "topleft",
  c(
    sprintf("Highest information (r = %1.3f, RMSE = %1.3f)", summary_first@corr, sqrt(summary_first@mse)),
    sprintf("Random selection (r = %1.3f, RMSE = %1.3f)", summary_random@corr, sqrt(summary_random@mse))
  ),
  col = c("blue", "red"),
  lty = c(1, 2)
)

dev.off()

sum(info_by_position_first)
sum(info_by_position_random)


setEPS()
postscript("random_select_demo/exposure_first.eps", height = 5, width = 7)
par(mfrow = c(2, 3))
plot(solution_first, type = "exposure")
dev.off()

setEPS()
postscript("random_select_demo/exposure_random.eps", height = 5, width = 7)
par(mfrow = c(2, 3))
plot(solution_random, type = "exposure")
dev.off()
