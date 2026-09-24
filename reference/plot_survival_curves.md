# Kaplan–Meier plot from a Surv object

Plot a Kaplan–Meier curve (with confidence intervals and optional risk
table) using a supplied `Surv` object. If the grouping variable has
exactly two levels the function fits a Cox model and annotates hazard
ratio (95% CI) and p-value; if more than two groups it displays only the
log-rank p-value. Optional weights (e.g. IPTW) can be supplied to
produce weighted survival curves and a weighted Cox model annotation;
see `weights` below.

## Usage

``` r
plot_survival_curves(
  surv_obj,
  data,
  group_var = "met_exercise_guidelines",
  weights = NULL,
  id = NULL,
  confidence_bands = TRUE,
  line_size = 1,
  time_limits = NULL,
  x_breaks = NULL,
  annotate_y = 0.99,
  annotate_x = NULL,
  x_label = "Time (units??)",
  y_label = NULL,
  title = NULL,
  custom_hr_pvalue_text = NULL,
  type = c("survival", "risk"),
  show_risktable = TRUE,
  risktable_stats = c("n.risk", "cum.event"),
  risktable_counts = c("both", "weighted", "unweighted"),
  ristable_text_size = 3.5,
  annotate_hjust = 1
)
```

## Arguments

- surv_obj:

  A `Surv` object (can be right-censored or left-truncated).

- data:

  A data frame containing the variables referenced by `surv_obj` and
  `group_var`.

- group_var:

  Character, name of the grouping column in `data`.

- weights:

  Optional weights for producing weighted survival curves (e.g. inverse
  probability of treatment weights, IPTW). Either the name of a numeric
  column in `data`, or a numeric vector with length equal to
  `nrow(data)`. Weights must be strictly positive (an error is thrown
  otherwise);
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html),
  which this function always fits when weights are supplied, requires
  weights `> 0`, and
  [`survival::survfit.formula()`](https://rdrr.io/pkg/survival/man/survfit.formula.html)
  treats zero weights as ambiguous, so observations that should be
  excluded should be filtered out of `data` beforehand rather than given
  a zero weight. There is no required scale/normalization (e.g. weights
  do not need to sum to 1 or to `nrow(data)`); IPTW weights are commonly
  left unstabilized or stabilized to a mean of 1, and either is fine
  here. If `NULL` (default), curves are unweighted. When supplied, the
  Cox model used for the HR/p-value annotation is fit with
  `robust = TRUE` (sandwich variance), as is standard practice for
  IPTW-type weights. If `group_var` has more than two levels, the
  omnibus p-value is a robust Wald test from a weighted Cox model rather
  than a log-rank test, since
  [`survival::survdiff()`](https://rdrr.io/pkg/survival/man/survdiff.html)
  does not support weights. When `show_risktable = TRUE`, the risk table
  statistics are rounded to 1 decimal place (they are non-integer
  "effective" counts when weighted); see `risktable_counts` to show
  unweighted counts instead of or alongside the weighted ones. When
  `surv_obj` is a counting-process (left-truncated)
  `Surv(time1, time2, event)` object, the robust sandwich variance needs
  to know which rows belong to the same subject, so `id` (below) is
  **required** in that case; it is optional for a plain right-censored
  `Surv(time, event)` object, where each row is always its own
  independent subject.

- id:

  Character name of a subject identifier column in `data`. Only relevant
  when `weights` is supplied, to correctly cluster rows belonging to the
  same subject for the robust sandwich variance used by the weighted Cox
  model. **Required** when `weights` is supplied and `surv_obj` is a
  counting-process `Surv(time1, time2, event)` object (an error is
  thrown otherwise), since such data may have a single subject
  contributing multiple `(time1, time2]` intervals (e.g. time-varying
  covariates) — silently guessing that every row is an independent
  subject risks an anti-conservative HR/p-value (standard errors/CIs too
  narrow). If every row of `data` is already its own independent subject
  (e.g. simple left truncation with one row per subject, such as age at
  entry/age at exit), add a row-number column and pass its name here.
  Optional (default `NULL`) when `surv_obj` is a plain right-censored
  `Surv(time, event)` object, where each row is always its own
  independent subject and `NULL` is equivalent to a row-number id.

- confidence_bands:

  Logical, if `TRUE` (default) display confidence bands

- line_size:

  Numeric, line size for the survival curves (default 1).

- time_limits:

  Numeric(2), x-axis limits for the plot. If `NULL`, sensible limits are
  estimated from the observed event times.

- x_breaks:

  Numeric vector of x-axis breaks. If `NULL`, reasonable breaks are
  selected automatically.

- annotate_y:

  Numeric, y position for annotation text (default 0.99).

- annotate_x:

  Numeric or `NULL`, x position for annotation; if `NULL` uses the
  rightmost value of `time_limits`.

- x_label:

  Character, label for the x axis.

- y_label:

  Character, label for the y axis. If `NULL`, a default label is chosen
  based on `type`.

- title:

  Character or `NULL`, plot title.

- custom_hr_pvalue_text:

  Character or `NULL`; if supplied, overrides the automatically
  generated HR/log-rank annotation text.

- type:

  Character, one of `"survival"` or `"risk"`; passed to `ggsurvfit()`.
  If `"survival"`, the default y-axis label is
  `"Probability Event-Free"`. If `"risk"`, the default y-axis label is
  `"Probability of Event"`.

- show_risktable:

  Logical; if `TRUE` (default), display a risk table beneath the
  survival curve.

- risktable_stats:

  Character vector specifying statistics shown in the risk table. Must
  contain one or more of:
  `c("n.risk", "cum.event", "cum.censor", "n.event", "n.censor")`. The
  default is `c("n.risk", "cum.event")`.

- risktable_counts:

  Character, one of `"both"` (default), `"weighted"`, or `"unweighted"`.
  Only relevant when `weights` is supplied and `show_risktable = TRUE`.
  `"weighted"` shows the (rounded) weighted counts, with row labels
  suffixed `": Weighted"` (e.g. `"At Risk: Weighted"`) to flag that they
  are non-integer "effective" counts rather than raw subject counts.
  `"unweighted"` shows the raw unweighted subject counts in the risk
  table instead (curves, CI, and the HR/p-value annotation remain
  weighted), with unsuffixed row labels (e.g. `"At Risk"`). `"both"`
  shows each cell as `"weighted (unweighted)"` with row labels suffixed
  `": Weighted (Raw)"`; this is exact for `"n.risk"`, `"cum.event"`, and
  `"cum.censor"`, but the raw (non-cumulative) `"n.event"`/`"n.censor"`
  counts are totals over each displayed risk table interval, and the
  unweighted side of `"both"` can only be computed exactly at actual
  event/censoring times. Combining `risktable_counts = "both"` with
  `risktable_stats` containing `"n.event"` or `"n.censor"` therefore
  throws an error; use `"cum.event"`/`"cum.censor"` instead (exact in
  `"both"` mode), or set `risktable_counts` to `"weighted"` or
  `"unweighted"`.

- ristable_text_size:

  Numeric, text size for the risk table (default 3.5).

- annotate_hjust:

  Numeric, horizontal justification of the annotation text relative to
  `annotate_x` (default 1, i.e. `annotate_x` is the text's right edge,
  matching the `NULL`/rightmost default of `annotate_x`). Set to 0 to
  left-justify the text against `annotate_x` instead (its left edge),
  e.g. when placing the annotation at the left side of the plot – with
  the default `1`, text placed near the left edge of `time_limits`
  extends further left and is clipped out of the plot entirely.

  Available statistics:

  - `"n.risk"` Number of patients at risk

  - `"cum.event"` Cumulative number of observed events

  - `"cum.censor"` Cumulative number of censored observations

  - `"n.event"` Number of events in each time interval

  - `"n.censor"` Number of censored observations in each time interval

## Value

A ggsurvfit ggplot object.

## Examples

``` r
data(cancer, package = "survival")

# Example with two groups
lung$sex <- factor(lung$sex, labels = c("Male", "Female"))
surv_obj <- with(lung, survival::Surv(time, status == 2))

plot_survival_curves(
    surv_obj,
    lung,
    group_var = "sex"
) + theme_bw2()
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
#> ℹ The deprecated feature was likely used in the ggsurvfit package.
#>   Please report the issue at <https://github.com/pharmaverse/ggsurvfit/issues>.


# Hide the risk table
plot_survival_curves(
    surv_obj,
    lung,
    group_var = "sex",
    show_risktable = FALSE
) + theme_bw2()
#> Warning: Removed 7 rows containing missing values or values outside the scale range
#> (`geom_step()`).


# Show only number at risk
plot_survival_curves(
    surv_obj,
    lung,
    group_var = "sex",
    risktable_stats = "n.risk"
) + theme_bw2()


# Example with more than two groups
lung$ph.ecog[lung$ph.ecog == 3] <- NA
lung$ph.ecog <- factor(lung$ph.ecog)

plot_survival_curves(
    surv_obj,
    lung,
    group_var = "ph.ecog"
) + theme_bw2()


# Cumulative incidence plot
plot_survival_curves(
    surv_obj,
    lung,
    group_var = "ph.ecog",
    type = "risk"
) + theme_bw2()


# Weighted survival curves (e.g. IPTW)
lung$iptw <- runif(nrow(lung), 0.5, 2)

# risktable_counts = "both" (the default) shows each risk table cell as
# "weighted (unweighted)", so the actual number of observed events/at-risk
# subjects stays visible alongside the weighted ("effective") Ns used for
# the curves/CI/HR
plot_survival_curves(
    surv_obj,
    lung,
    group_var = "sex",
    weights = "iptw",
    risktable_counts = "both"
) + theme_bw2()


# Weighted curves with only the (rounded) weighted Ns in the risk table
plot_survival_curves(
    surv_obj,
    lung,
    group_var = "sex",
    weights = "iptw",
    risktable_counts = "weighted"
) + theme_bw2()


# Weighted curves with only the raw unweighted Ns in the risk table
plot_survival_curves(
    surv_obj,
    lung,
    group_var = "sex",
    weights = "iptw",
    risktable_counts = "unweighted"
) + theme_bw2()

```
