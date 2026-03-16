devtools::load_all()

data(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)
p <- plot_2_categorical_vars(
    d = mtcars,
    xvar = "cyl",
    yvar = "gear",
    xvar_label = 'Cylinders',
    yvar_label = 'Gears',
    title = 'test'
)
p$ggplot
