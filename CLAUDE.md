# CLAUDE.md

R package template. Source in `R/`, tests in `tests/testthat/`.

## Writing functions

- Document every function with roxygen2 (`@param`, `@return`, `@examples`, and `@export` if user-facing). See `R/example_function.R`.
- Validate all arguments at the top of the function with `stopifnot()` checks (type, length, allowed values).
- Call functions from other packages as `pkg::fun()`; never use `library()` or `require()` inside the package. Add new dependencies to `DESCRIPTION` with `usethis::use_package("pkg")`.

## Testing

- Every exported function needs testthat tests in `tests/testthat/test-<name>.R` to test whether the code works as expected.
- Tests run in parallel (one R process per test file, `Config/testthat/parallel: true`), so each test file must be self-contained: never rely on objects, options, or files created by another test file, and write any output files to `tempfile()`.

## Workflow

- After changing roxygen comments, run `devtools::document()`. Never hand-edit `NAMESPACE` or `man/`.
- Edit `README.Rmd`, not `README.md`; re-render with `devtools::build_readme()`.
- Add user-facing changes to `NEWS.md` under the development version.
- Before finishing, run `devtools::test()` to make sure new changes don't break tests.
