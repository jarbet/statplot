#' Build an upset plot from a named list of sets
#'
#' @param list_input A named list where each element is a vector of items in that set
#' @param xlab X axis label for the intersection bar plot
#' @param ylab Y axis label for the bar counts
#' @param title Plot title
#' @param n_intersections Number of intersections to show in the upset x-axis (passed to ggupset::scale_x_upset). Default: 20
#' @param ymax_offset Numeric offset added to the computed ymax to provide headroom above the tallest bar. Default: 0.5
#' @param order_by How to order intersections: "freq" or "degree". Default: "freq"
#' @param backend Which backend to use for plotting: 'UpSetR' or 'ggupset'
#' @param text_scale Numeric scaling factor for text size (only used if backend is 'UpSetR')
#' @return A ggplot object
#' @export
#' @examples
#' ggplot2::theme_set(theme_bw2())
#' list_input <- list(
#'   A = c(1, 2, 3, 5, 7, 8, 11, 12, 13),
#'   B = c(1, 2, 4, 5, 10),
#'   C = c(1, 5, 6, 7, 8, 9, 10, 12, 13)
#' )
#' plot_upset(list_input)
plot_upset <- function(
    list_input,
    xlab = "Combination",
    ylab = "Count",
    title = "",
    n_intersections = 10,
    ymax_offset = 0.5,
    order_by = c("freq", "degree"),
    backend = c('UpSetR', 'ggupset'),
    text_scale = 1.5
) {
    order_by <- match.arg(order_by)
    backend <- match.arg(backend)

    if (backend == 'ggupset') {
        all_elements <- sort(unique(unlist(list_input)))
        membership <- lapply(all_elements, function(el) {
            names(list_input)[vapply(
                list_input,
                function(x) el %in% x,
                logical(1)
            )]
        })

        df <- tibble::tibble(
            element = all_elements,
            membership = membership
        )

        # drop elements that are in no set (shouldn't normally happen, but safe)
        df <- df |> dplyr::filter(lengths(membership) > 0)

        # prepare a string key for each intersection (keeps combos stable)
        df <- df |>
            dplyr::mutate(
                membership_key = vapply(
                    membership,
                    function(x) paste(sort(x), collapse = " & "),
                    character(1)
                )
            )

        # compute counts per intersection and derive adaptive ymax
        counts <- df |> dplyr::count(membership_key, name = "n")
        if (nrow(counts) == 0) {
            stop("No intersections to plot (no elements belong to any set).")
        }
        ymax <- max(counts$n, na.rm = TRUE) + ymax_offset

        # use the list-column 'membership' as the x aesthetic (ggupset requires a list)
        p <- ggplot2::ggplot(df, ggplot2::aes(x = membership)) +
            ggplot2::geom_bar() +
            ggupset::scale_x_upset(
                order_by = order_by,
                n_intersections = n_intersections
            ) +
            ggplot2::geom_text(
                stat = 'count',
                ggplot2::aes(label = ggplot2::after_stat(count)),
                vjust = -1
            ) +
            ggplot2::scale_y_continuous(limits = c(0, ymax)) +
            ggplot2::labs(title = title, x = xlab, y = ylab) +
            ggplot2::theme(
                axis.title.x = ggtext::element_markdown(face = "bold"),
                axis.title.y = ggtext::element_markdown(face = "bold"),
                plot.title = ggtext::element_markdown(face = "bold")
            )
    } else if (backend == 'UpSetR') {
        input_list2 <- UpSetR::fromList(list_input)

        # UpSetR::upset() builds its plots against the current global
        # ggplot2 theme (via ggplot2::theme_get()) and overrides some theme
        # elements (e.g. axis.title.y) with plain ggplot2::element_text().
        # If the user has set a global theme via
        # ggplot2::theme_set(theme_bw2()) (the default markdown = TRUE),
        # that override conflicts with the element_markdown() used for
        # those elements and errors. Temporarily reset to a plain theme for
        # this call so UpSetR is unaffected by the global theme.
        old_theme <- ggplot2::theme_get()
        ggplot2::theme_set(ggplot2::theme_grey())
        on.exit(ggplot2::theme_set(old_theme), add = TRUE)

        p <- UpSetR::upset(
            input_list2,
            order.by = order_by,
            nintersects = n_intersections,
            nsets = length(list_input),
            text.scale = text_scale
        )
    }
    return(p)
}
