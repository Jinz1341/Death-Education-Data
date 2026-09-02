repo_root <- normalizePath(getwd(), mustWork = TRUE)
all_paths <- list.files(repo_root, recursive = TRUE, all.files = TRUE, full.names = TRUE)
all_paths <- all_paths[basename(all_paths) != "." & basename(all_paths) != ".."]

forbidden_files <- c(".DS_Store", ".Rapp.history", ".Rhistory", "Rplots.pdf")
stopifnot(!any(basename(all_paths) %in% forbidden_files))
stopifnot(!any(grepl("[.]bak$|~$|[.]tmp$", all_paths, ignore.case = TRUE)))
stopifnot(!any(basename(all_paths) == ".git"))

study1 <- read.csv(file.path(repo_root, "data", "study1_public.csv"))
study2 <- read.csv(file.path(repo_root, "data", "study2_public.csv"))
study2_items <- read.csv(file.path(repo_root, "data", "study2_item_choice_purpose_long.csv"))
stopifnot(nrow(study1) == 230L, length(unique(study1$participant_id)) == 230L)
stopifnot(sum(study1$school_level == "grade_4") == 104L)
stopifnot(sum(study1$school_level == "grade_8") == 126L)
stopifnot(!anyNA(study1$age_years[study1$school_level == "grade_4"]))
stopifnot(all(study1$age_years[study1$school_level == "grade_4"] %in% c(9, 10)))
stopifnot(nrow(study2) == 184L, nrow(study2_items) == 2576L)

data_md5 <- unname(tools::md5sum(c(
  file.path(repo_root, "data", "study2_public.csv"),
  file.path(repo_root, "data", "study2_item_choice_purpose_long.csv")
)))
stopifnot(identical(data_md5, c("56e5515fd6782c51f9922ef9785d2d75", "498ee6a74c93c2fc36d5f936f1fae75c")))

cat("Release-hygiene contract passed.\n")
