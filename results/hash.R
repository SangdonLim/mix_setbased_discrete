# result data is very large in size (~ 90 GB total)
# because of this only hashes of result files are stored in the repo

fp <- "~/Box Sync/Behaviormetrika_Special_Issue/Article_2_PassageCAT/Results"

# Study 1 data (3200 files, ~ 40 GB)

new_snapshot <- fileSnapshot(file.path(fp, "Study1"), md5sum = TRUE, file.info = FALSE)
fn <- "results/study1_hash.rds"
if (!file.exists(fn)) {
  saveRDS(new_snapshot, fn)
} else {
  stored_snapshot <- readRDS(fn)
}

changedFiles(before = stored_snapshot, after = new_snapshot, check.file.info = NULL)

# Study 2 data (3600 files, ~ 50 GB)

new_snapshot <- fileSnapshot(file.path(fp, "Study2"), md5sum = TRUE, file.info = FALSE)
fn <- "results/study2_hash.rds"
if (!file.exists(fn)) {
  saveRDS(new_snapshot, fn)
} else {
  stored_snapshot <- readRDS(fn)
}

changedFiles(before = stored_snapshot, after = new_snapshot, check.file.info = NULL)
