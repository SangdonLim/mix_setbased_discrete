library(TestDesign)
library(foreach)
library(doParallel)
library(doSNOW)

itempool   <- loadItemPool("data/itempool_mixed.csv")
itemattrib <- loadItemAttrib("data/itemattrib_mixed.csv", itempool)
stimattrib <- loadStAttrib("data/stimattrib_mixed.csv", itemattrib)
constraints <- loadConstraints(
  "data/constraints_inter.csv", itempool, itemattrib, stimattrib
)

cfg <- createShadowTestConfig(
  MIP = list(solver = "gurobi"),
  refresh_policy = list(
    method = "ALWAYS"
  ),
  exposure_control = list(
    method = "BIGM",
    M = 100,
    segment_cut = c(-Inf, seq(-1.5, 1.5, 1), Inf),
    n_segment = 5
  )
)

cfg_first  <- cfg
cfg_first@item_selection$sort_type <- 1
cfg_random <- cfg
cfg_random@item_selection$sort_type <- 0

idx_replication <- 1

# Generate thetas

set.seed(idx_replication)
true_theta <- rep(seq(-2, 2, 1), each = 1000)
set.seed(idx_replication)
true_theta <- sample(true_theta)
resp_data  <- simResp(itempool, true_theta)

# Run CAT with selecting the first item
set.seed(1)
solution_first <- Shadow(cfg_first, constraints, true_theta = true_theta, data = resp_data)
save(
  solution_first,
  file = "random_select_demo/randomselect_demo_first.RData"
)

# Run CAT with random select
set.seed(1)
solution_random <- Shadow(cfg_random, constraints, true_theta = true_theta, data = resp_data)
save(
  solution_random,
  file = "random_select_demo/randomselect_demo_random.RData"
)
