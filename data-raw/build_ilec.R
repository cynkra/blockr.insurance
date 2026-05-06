# Build ilec_mortality package data from the SOA Research Institute's
# RILEC repository (publicly published, Apache-2.0 licensed):
#
#   https://github.com/Society-of-actuaries-research-institute/RILEC
#
# Source dataset: ilec13_17_framework_light.parquet — a 6 MB summarised
# version of the full ILEC 2013-2017 individual life mortality experience
# (39M rows in the original SOA release; this is the "lean" cube prepared
# by the SOA team for tutorial / GLM examples). See
# `datafiles/data prep framework - lean.R` in RILEC for the exact recipe.
#
# Output:
#   data/ilec_mortality.rda  (~3.7 MB, xz-compressed)

src_url <- paste0(
  "https://raw.githubusercontent.com/",
  "Society-of-actuaries-research-institute/RILEC/main/",
  "datafiles/ilec13_17_framework_light.parquet"
)

tmp <- tempfile(fileext = ".parquet")
on.exit(unlink(tmp), add = TRUE)
download.file(src_url, tmp, mode = "wb")

ilec_mortality <- arrow::read_parquet(tmp)
ilec_mortality <- as.data.frame(ilec_mortality)

# tidy types: keep factors that came in as factors, coerce char cols
ilec_mortality$gender <- factor(ilec_mortality$gender, levels = c("F", "M"))
ilec_mortality$uw     <- as.factor(ilec_mortality$uw)
ilec_mortality        <- tibble::as_tibble(ilec_mortality)

dir.create("data", showWarnings = FALSE)
save(ilec_mortality,
     file = "data/ilec_mortality.rda",
     compress = "xz")

cat("Wrote ", nrow(ilec_mortality), " rows to data/ilec_mortality.rda\n",
    sep = "")
