library(TestDesign)

plotExposureRate <- function(x, segment, color = "blue", color_final = "blue") {

  theta_value <- x@final_theta_est
  nj          <- length(theta_value)

  ni <- x@pool@ni
  nv <- ncol(x@usage_matrix)
  max_rate      <- x@config@exposure_control$max_exposure_rate
  segment_cut   <- x@config@exposure_control$segment_cut
  n_segment     <- x@config@exposure_control$n_segment
  cut_lower     <- segment_cut[1:n_segment]
  cut_upper     <- segment_cut[2:(n_segment + 1)]
  segment_label <- character(n_segment)

  for (k in 1:n_segment) {
    if (k < n_segment) {
      segment_label[k] <- sprintf("(%s,%s]", cut_lower[k], cut_upper[k])
    } else {
      segment_label[k] <- sprintf("(%s,%s)", cut_lower[k], cut_upper[k])
    }
  }

  theta_segment_index <- numeric(nj)
  theta_segment_index <- find_segment(theta_value, segment_cut)

  segment_n    <- numeric(n_segment)
  segment_dist <- table(theta_segment_index)
  segment_n[as.numeric(names(segment_dist))] <- segment_dist
  segment_index_table <- matrix(NA, nj, x@constraints@test_length)

  usage_matrix       <- x@usage_matrix
  usage_matrix_final <- x@usage_matrix

  for (j in 1:nj) {
    administered_items <- x@output[[j]]@administered_item_index
    pos_item_outside_of_segment <- x@output[[j]]@theta_segment_index != theta_segment_index[j]
    idx_item_outside_of_segment <- administered_items[pos_item_outside_of_segment]
    usage_matrix_final[j, idx_item_outside_of_segment] <- FALSE
    segment_index_table[j, ] <- x@output[[j]]@theta_segment_index
  }

  ## visited segments across item positions and each examinee
  segment_freq <- matrix(0, n_segment, n_segment)
  for (i in 1:x@constraints@test_length) {
    interim_segment_dist <- factor(segment_index_table[, i], levels = 1:n_segment)
    segment_table <- tapply(interim_segment_dist, theta_segment_index, table)
    for (s in 1:length(segment_table)) {
      idx_r <- as.numeric(names(segment_table)[s])
      idx_c <- as.numeric(names(segment_table[[s]]))
      segment_freq[idx_r, idx_c] <- segment_freq[idx_r, idx_c] + segment_table[[s]]
    }
  }
  segment_rate                <- segment_freq / segment_n
  segment_rate_table          <- data.frame(
    segment_class = factor(rep(segment_label, rep(n_segment, n_segment)), levels = segment_label),
    segment = rep(1:n_segment, n_segment),
    avg_visit = matrix(
      t(segment_rate),
      nrow = n_segment^2, ncol = 1)
  )
  exposure_rate               <- colSums(usage_matrix) / nj
  exposure_rate_final         <- colSums(usage_matrix_final) / nj
  item_exposure_rate          <- exposure_rate[1:ni]
  item_exposure_rate_final    <- exposure_rate_final[1:ni]

  if (x@constraints@set_based) {
    stim_exposure_rate        <- exposure_rate[(ni + 1):nv][x@constraints@stimulus_index_by_item]
    stim_exposure_rate_final  <- exposure_rate_final[(ni + 1):nv][x@constraints@stimulus_index_by_item]
  } else {
    stim_exposure_rate        <- NULL
    stim_exposure_rate_final  <- NULL
  }
  exposure_rate_segment       <- vector("list", n_segment)
  exposure_rate_segment_final <- vector("list", n_segment)
  names(exposure_rate_segment)       <- segment_label
  names(exposure_rate_segment_final) <- segment_label
  for (k in 1:n_segment) {
    if (segment_n[k] > 2) {
      exposure_rate_segment[[k]]       <- colMeans(usage_matrix[theta_segment_index == k, ])
      exposure_rate_segment_final[[k]] <- colMeans(usage_matrix_final[theta_segment_index == k, ])
    }
    if (is.null(exposure_rate_segment[[k]])) {
      exposure_rate_segment[[k]] <- numeric(nv)
    } else if (any(is.nan(exposure_rate_segment[[k]]))) {
      exposure_rate_segment[[k]][is.nan(exposure_rate_segment[[k]])] <- 0
    }
    if (is.null(exposure_rate_segment_final[[k]])) {
      exposure_rate_segment_final[[k]] <- numeric(nv)
    } else if (any(is.nan(exposure_rate_segment_final[[k]]))) {
      exposure_rate_segment_final[[k]][is.nan(exposure_rate_segment_final[[k]])] <- 0
    }
  }
  item_exposure_rate_segment       <- exposure_rate_segment
  item_exposure_rate_segment_final <- exposure_rate_segment_final
  for (k in 1:n_segment) {
    item_exposure_rate_segment[[k]]       <- item_exposure_rate_segment[[k]][1:ni]
    item_exposure_rate_segment_final[[k]] <- item_exposure_rate_segment_final[[k]][1:ni]
  }
  if (x@constraints@set_based) {
    stim_exposure_rate_segment       <- exposure_rate_segment
    stim_exposure_rate_segment_final <- exposure_rate_segment_final
    for (k in 1:n_segment) {
      stim_exposure_rate_segment[[k]]       <- stim_exposure_rate_segment[[k]][(ni + 1):nv][x@constraints@stimulus_index_by_item]
      stim_exposure_rate_segment_final[[k]] <- stim_exposure_rate_segment_final[[k]][(ni + 1):nv][x@constraints@stimulus_index_by_item]
    }
  } else {
    stim_exposure_rate_segment       <- NULL
    stim_exposure_rate_segment_final <- NULL
  }

  if (is.null(segment)) {
    segment <- c(0, 1:n_segment)
  }
  for (k in segment) {
    if (k == 0) {
      TestDesign:::plotER(
        item_exposure_rate, item_exposure_rate_final, stim_exposure_rate, x@constraints@stimulus_index_by_item,
        max_rate = max_rate, title = "Overall", color = color, color_final = color_final, simple = TRUE
      )
    } else {
      TestDesign:::plotER(
        item_exposure_rate_segment[[k]], item_exposure_rate_segment_final[[k]], stim_exposure_rate_segment[[k]], x@constraints@stimulus_index_by_item,
        max_rate = max_rate, title = NULL, color = color, color_final = color_final, simple = TRUE
      )
    }
  }

}


fp <- "~/Box Sync/Behaviormetrika_Special_Issue/Article_2_PassageCAT/Results"

setEPS()
postscript("figures/study1_exposure.eps", width = 8, height = 8)
par(mfrow = c(2, 2), oma = c(3, 2, 0, 2))

load(file.path(fp, "Study1", "solution_ds_none_maxinfo_1.Rdata"))
panel_a <- solution
plotExposureRate(panel_a, segment = 1)
mtext("(a) No EC, MFI", side = 1, line = 5)
rmse <- sqrt(mean((panel_a@final_theta_est - panel_a@true_theta) ** 2))
text(0, 1, sprintf("RMSE = %1.3f", rmse), adj = c(0, 1))

load(file.path(fp, "Study1", "solution_ds_none_goalinfo6_1.Rdata"))
panel_b <- solution
plotExposureRate(panel_b, segment = 1)
mtext(paste0("(b) No EC, GFI", "\uad", "6"), side = 1, line = 5)
rmse <- sqrt(mean((panel_b@final_theta_est - panel_b@true_theta) ** 2))
text(0, 1, sprintf("RMSE = %1.3f", rmse), adj = c(0, 1))

sum(panel_a@exposure_rate[, 3] > 0.25)
sum(panel_b@exposure_rate[, 3] > 0.25)
mean(panel_a@exposure_rate[, 3] > 0.25)
mean(panel_b@exposure_rate[, 3] > 0.25)

load(file.path(fp, "Study1", "solution_ds_bigm_maxinfo_1.Rdata"))
panel_c <- solution
plotExposureRate(panel_c, segment = 1)
mtext("(c) With EC, MFI", side = 1, line = 5)
rmse <- sqrt(mean((panel_c@final_theta_est - panel_c@true_theta) ** 2))
text(0, 1, sprintf("RMSE = %1.3f", rmse), adj = c(0, 1))

load(file.path(fp, "Study1", "solution_ds_bigm_goalinfo6_1.Rdata"))
panel_d <- solution
plotExposureRate(panel_d, segment = 1)
mtext(paste0("(d) With EC, GFI", "\uad", "6"), side = 1, line = 5)
rmse <- sqrt(mean((panel_d@final_theta_est - panel_d@true_theta) ** 2))
text(0, 1, sprintf("RMSE = %1.3f", rmse), adj = c(0, 1))

sd(panel_c@exposure_rate[, 3])
sd(panel_d@exposure_rate[, 3])

dev.off()
