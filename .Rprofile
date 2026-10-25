# A project .Rprofile replaces ~/.Rprofile, so source the user-level one first
if (file.exists("~/.Rprofile")) {
    source("~/.Rprofile")
}

# Number of parallel testthat workers (see Config/testthat/parallel in
# DESCRIPTION). Used by devtools::test(), including Cmd/Ctrl+Shift+T in
# Positron/RStudio. An existing TESTTHAT_CPUS (e.g. from ~/.Renviron) wins.
if (!nzchar(Sys.getenv("TESTTHAT_CPUS"))) {
    Sys.setenv(TESTTHAT_CPUS = 8)
}
