library(TestDesign)
library(foreach)
library(doParallel)
library(doSNOW)

fp <- "~/Box Sync/Behaviormetrika_Special_Issue/Article_2_PassageCAT/Results/Study1"

# check package versions
if (packageVersion("TestDesign") != "1.2.2") {
  .rs.restartR()
}
if (packageVersion("gurobi") != "9.1.1") {
  .rs.restartR()
}

itempool   <- loadItemPool("data/itempool_mixed.csv")
itemattrib <- loadItemAttrib("data/itemattrib_mixed.csv", itempool)
stimattrib <- loadStAttrib("data/stimattrib_mixed.csv", itemattrib)

constraints_ds <- loadConstraints(
  "data/constraints_ds.csv", itempool, itemattrib, stimattrib
)
constraints_sd <- loadConstraints(
  "data/constraints_sd.csv", itempool, itemattrib, stimattrib
)
constraints_inter <- loadConstraints(
  "data/constraints_inter.csv", itempool, itemattrib, stimattrib
)
constraints_setbased <- loadConstraints(
  "data/constraints_setbased.csv", itempool, itemattrib, stimattrib
)

cfg_base <- createShadowTestConfig(
  MIP = list(solver = "gurobi"),
  refresh_policy = list(
    method = "SET"
  ),
  exposure_control = list(
    method = "BIGM",
    M = 100,
    segment_cut = c(-Inf, Inf),
    n_segment = 1
  )
)

# Create conditions matrix
conditions <- expand.grid(
  weave = c("ds", "sd", "interspersed", "setbased"),
  exposure_control = c("none", "bigm"),
  info_type = c("maxinfo", "goalinfo8", "goalinfo6"),
  stringsAsFactors = FALSE
)
n_conditions <- dim(conditions)[1]

# ! Change replication range here
#  1:50  Sangdon
# 51:100 Choi
idx_replications <- 1:50

tasks <-
  expand.grid(
    idx_condition   = 1:n_conditions,
    idx_replication = idx_replications
  )

n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Run simulation
o <- foreach(
  idx_task = 1:dim(tasks)[1],
  .packages = c("TestDesign")
) %dopar% {

  idx_condition   <- tasks[idx_task, ]$idx_condition
  idx_replication <- tasks[idx_task, ]$idx_replication

  condition <- conditions[idx_condition, ]
  constraints <- NULL
  if (condition$weave == "ds") {
    constraints <- constraints_ds
  }
  if (condition$weave == "sd") {
    constraints <- constraints_sd
  }
  if (condition$weave == "interspersed") {
    constraints <- constraints_inter
  }
  if (condition$weave == "setbased") {
    constraints <- constraints_setbased
  }
  cfg <- NULL
  cfg <- cfg_base
  if (condition$exposure_control == "none") {
    cfg@exposure_control$method <- "NONE"
  }
  if (condition$exposure_control == "bigm") {
    cfg@exposure_control$method <- "BIGM"
  }
  if (condition$info_type == "maxinfo") {
    cfg@item_selection$method <- "MFI"
  }
  if (condition$info_type == "goalinfo8") {
    cfg@item_selection$method       <- "GFI"
    cfg@item_selection$target_value <- 8
  }
  if (condition$info_type == "goalinfo6") {
    cfg@item_selection$method       <- "GFI"
    cfg@item_selection$target_value <- 6
  }

  fn <- sprintf(
    "solution_%s_%s_%s_%s.Rdata",
    condition$weave,
    condition$exposure_control,
    condition$info_type,
    idx_replication
  )

  if (file.exists(file.path(fp, fn))) {
    return(NULL)
  }

  # Generate thetas
  set.seed(idx_replication)
  true_theta <- rnorm(5000, 0, 1)
  set.seed(idx_replication)
  true_theta <- sample(true_theta)
  resp_data  <- simResp(itempool, true_theta)

  # Run CAT
  set.seed(1)
  solution <- NULL
  solution <- Shadow(cfg, constraints, true_theta = true_theta, data = resp_data)

  save(
    solution,
    file = file.path(fp, fn)
  )

}
