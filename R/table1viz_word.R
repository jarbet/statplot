# Word (flextable) output for table1viz() -------------------------------------
#
# Graphics are placed in flextable cells as placeholder text tokens and stored
# (as SVG) in x$statplot_graphics. When the table is knitted, each token's run
# is replaced by:
#   * Word/.docx: an inline DrawingML group of vector shapes (svg_to_wml()),
#     so pandoc/Quarto need no image files and no post-processing;
#   * HTML (and the RStudio viewer): the SVG itself.

table1viz_flextable <- function(
    tbl,
    cell_svg,
    cell_bar,
    dist_svg,
    span_groups,
    combined,
    grouped,
    col_rows,
    col_colors,
    col_names,
    plot_width,
    bar_width,
    header_align,
    overall_divider,
    var_start,
    group_header,
    distribution_label,
    note
) {
    tb <- tbl$table_body
    stat_cols <- names(col_rows)
    if (combined) {
        tbl <- gtsummary::modify_table_body(
            tbl,
            ~ dplyr::mutate(.x, distribution = '')
        )
        tbl <- gtsummary::modify_table_styling(
            tbl,
            columns = 'distribution',
            hide = FALSE,
            label = 'Distribution'
        )
    }
    ft <- gtsummary::as_flex_table(tbl)

    graphics <- character(0)
    add_graphic <- function(svg) {
        token <- sprintf('STATPLOTGFX%05d', length(graphics) + 1)
        graphics[[token]] <<- svg
        token
    }

    # ---- body graphics ----
    need_width <- stats::setNames(rep(0, length(stat_cols)), stat_cols)
    for (cc in stat_cols) {
        for (i in which(!is.na(cell_svg[[cc]]))) {
            token <- add_graphic(cell_svg[[cc]][i])
            if (cell_bar[[cc]][i]) {
                txt <- tb[[cc]][i]
                txt <- if (is.na(txt)) '' else txt
                value <- flextable::as_paragraph(token, ' ', txt)
                w <- bar_width / 96 + nchar(txt) * 0.075 + 0.25
            } else {
                value <- flextable::as_paragraph(token)
                w <- plot_width / 96 + 0.2
            }
            ft <- flextable::compose(ft, i = i, j = cc, part = 'body', value = value)
            need_width[[cc]] <- max(need_width[[cc]], w)
        }
    }
    if (combined) {
        for (i in which(!is.na(dist_svg))) {
            ft <- flextable::compose(
                ft,
                i = i,
                j = 'distribution',
                part = 'body',
                value = flextable::as_paragraph(add_graphic(dist_svg[[i]]))
            )
        }
        for (rows in span_groups) {
            ft <- flextable::merge_at(ft, i = rows, j = 'distribution', part = 'body')
        }
    }

    # ---- headers (plain text + colored swatch; no markdown/HTML) ----
    n_hdr <- flextable::nrow_part(ft, 'header')
    ft <- flextable::compose(
        ft,
        i = n_hdr,
        j = 'label',
        part = 'header',
        value = flextable::as_paragraph(flextable::as_b('Variable'))
    )
    for (cc in stat_cols) {
        swatch <- if (grouped) {
            flextable::as_chunk(
                '■ ',
                props = flextable::fp_text_default(color = col_colors[[cc]])
            )
        } else {
            ''
        }
        ft <- flextable::compose(
            ft,
            i = n_hdr,
            j = cc,
            part = 'header',
            value = flextable::as_paragraph(
                swatch,
                flextable::as_b(col_names[[cc]]),
                '\n',
                sprintf(
                    'N = %s',
                    format(length(col_rows[[cc]]), big.mark = ',', scientific = FALSE)
                )
            )
        )
    }
    if (combined) {
        ft <- flextable::compose(
            ft,
            i = n_hdr,
            j = 'distribution',
            part = 'header',
            value = flextable::as_paragraph(flextable::as_b(strip_md(distribution_label)))
        )
    }
    if (grouped && n_hdr > 1) {
        group_cols <- setdiff(stat_cols, 'stat_0')
        ft <- flextable::compose(
            ft,
            i = 1,
            j = group_cols,
            part = 'header',
            value = flextable::as_paragraph(flextable::as_b(strip_md(group_header)))
        )
        ft <- flextable::align(ft, i = 1, align = 'center', part = 'header')
    }

    if (length(note) > 0) {
        ft <- flextable::add_footer_lines(ft, paste(note, collapse = ' '))
    }

    graphic_cols <- c(stat_cols, if (combined) 'distribution')
    ft <- ft |>
        flextable::align(j = stat_cols, align = 'left', part = 'body') |>
        flextable::align(i = n_hdr, j = graphic_cols, align = header_align, part = 'header') |>
        flextable::valign(valign = 'center', part = 'body') |>
        flextable::autofit()
    if (overall_divider) {
        line <- flextable::fp_border_default(color = divider_color, width = 1.5)
        for (part in c('header', 'body')) {
            ft <- flextable::vline(ft, j = 'stat_0', border = line, part = part)
        }
    }
    if (length(var_start) > 0) {
        ft <- flextable::hline(
            ft,
            i = var_start - 1,
            border = flextable::fp_border_default(color = divider_color, width = 1.5),
            part = 'body'
        )
    }
    # autofit() measures the placeholder text, not the graphic: widen as needed
    widths <- stats::setNames(ft$body$colwidths, ft$col_keys)
    for (cc in stat_cols) {
        if (need_width[[cc]] > widths[[cc]]) {
            ft <- flextable::width(ft, j = cc, width = need_width[[cc]])
        }
    }
    if (combined) {
        ft <- flextable::width(ft, j = 'distribution', width = plot_width / 96 + 0.2)
    }

    ft$statplot_graphics <- graphics
    class(ft) <- c('table1viz_flextable', class(ft))
    ft
}

strip_md <- function(x) gsub('\\*\\*|__|(?<![*])\\*(?![*])', '', x, perl = TRUE)

plain_flextable <- function(x) {
    x$statplot_graphics <- NULL
    class(x) <- setdiff(class(x), 'table1viz_flextable')
    x
}

# Swap placeholder tokens in rendered flextable output for graphics.
fill_graphics <- function(out, graphics, to = c('docx', 'html', 'none')) {
    to <- match.arg(to)
    for (token in names(graphics)) {
        out <- switch(
            to,
            docx = sub(
                sprintf('<w:r[ >](?:(?!</w:r>).)*?%s</w:t></w:r>', token),
                svg_to_wml(graphics[[token]]),
                out,
                perl = TRUE
            ),
            html = sub(
                sprintf('<span[^>]*>%s</span>', token),
                graphics[[token]],
                out,
                perl = TRUE
            ),
            none = sub(token, '', out, fixed = TRUE)
        )
    }
    out
}

#' Knit a table1viz() flextable
#'
#' Draws the graphics of a [table1viz()] flextable (`output = "flextable"`):
#' as native Word vector shapes when knitting to `.docx`, as SVG when knitting
#' to HTML. Other formats show the table without graphics.
#'
#' @param x A `table1viz_flextable` from [table1viz()].
#' @param ... Passed to flextable's `knit_print()` method.
#' @return A `knit_asis` object.
#' @keywords internal
#' @exportS3Method knitr::knit_print
knit_print.table1viz_flextable <- function(x, ...) {
    graphics <- x$statplot_graphics
    out <- knitr::knit_print(plain_flextable(x), ...)
    to <- if (isTRUE(knitr::pandoc_to('docx'))) {
        'docx'
    } else if (knitr::is_html_output()) {
        'html'
    } else {
        if (length(graphics) > 0) {
            warning(
                'table1viz(): graphics are only drawn in Word (.docx) and HTML ',
                'output; use output = "gt" for HTML.',
                call. = FALSE
            )
        }
        'none'
    }
    attrs <- attributes(out)
    out <- fill_graphics(as.character(out), graphics, to)
    attributes(out) <- attrs
    out
}

#' Print a table1viz() flextable
#'
#' In an interactive session, shows the table with its graphics (as SVG) in
#' the viewer; otherwise falls back to flextable's print method.
#'
#' @param x A `table1viz_flextable` from [table1viz()].
#' @param ... Passed to flextable's `print()` method.
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.table1viz_flextable <- function(x, ...) {
    if (!interactive()) {
        print(plain_flextable(x), ...)
        return(invisible(x))
    }
    h <- flextable::htmltools_value(plain_flextable(x))
    html <- fill_graphics(as.character(h), x$statplot_graphics, 'html')
    print(htmltools::browsable(htmltools::attachDependencies(
        htmltools::HTML(html),
        htmltools::findDependencies(h)
    )))
    invisible(x)
}

# ---- SVG -> DrawingML ------------------------------------------------------
# Converts the small SVG subset drawn by table1viz() (rect, line, circle, path of
# M/L/Z commands, text, hatch <pattern>s) into an inline Word drawing group.
# 1 SVG px = 1/96 in = 9525 EMU.

wml_env <- new.env(parent = emptyenv())
wml_env$id <- 0L

wml_next_id <- function() {
    wml_env$id <- wml_env$id + 1L
    50000L + wml_env$id # well above ids pandoc gives its own images
}

emu <- function(px) round(px * 9525)

wml_hex <- function(col) {
    m <- grDevices::col2rgb(col)
    sprintf('%02X%02X%02X', m[1, ], m[2, ], m[3, ])
}

wml_solid <- function(col, alpha = 1) {
    sprintf(
        '<a:solidFill><a:srgbClr val="%s">%s</a:srgbClr></a:solidFill>',
        wml_hex(col),
        if (alpha < 1) sprintf('<a:alpha val="%d"/>', round(alpha * 1e5)) else ''
    )
}

wml_escape <- function(x) {
    x <- gsub('&', '&amp;', x, fixed = TRUE)
    x <- gsub('<', '&lt;', x, fixed = TRUE)
    gsub('>', '&gt;', x, fixed = TRUE)
}

svg_num <- function(node, attr, default = 0) {
    v <- xml2::xml_attr(node, attr)
    if (is.na(v)) default else as.numeric(v)
}

svg_to_wml <- function(svg) {
    doc <- xml2::read_xml(svg)
    width <- as.numeric(xml2::xml_attr(doc, 'width'))
    height <- as.numeric(xml2::xml_attr(doc, 'height'))
    patterns <- xml2::xml_find_all(doc, '//pattern')
    hatch <- stats::setNames(
        xml2::xml_attr(xml2::xml_find_first(patterns, './rect'), 'fill'),
        xml2::xml_attr(patterns, 'id')
    )
    fill_xml <- function(node) {
        f <- xml2::xml_attr(node, 'fill')
        if (is.na(f)) f <- '#000000' # SVG default
        if (f == 'none') {
            return('<a:noFill/>')
        }
        if (grepl('^url\\(#', f)) {
            col <- hatch[[sub('^url\\(#(.*)\\)$', '\\1', f)]]
            return(sprintf(
                '<a:pattFill prst="wdUpDiag"><a:fgClr><a:srgbClr val="%s"/></a:fgClr><a:bgClr><a:srgbClr val="FFFFFF"/></a:bgClr></a:pattFill>',
                wml_hex(col)
            ))
        }
        wml_solid(f, svg_num(node, 'fill-opacity', 1))
    }
    line_xml <- function(node) {
        s <- xml2::xml_attr(node, 'stroke')
        if (is.na(s) || s == 'none') {
            return('<a:ln><a:noFill/></a:ln>')
        }
        sprintf(
            '<a:ln w="%d">%s</a:ln>',
            emu(svg_num(node, 'stroke-width', 1)),
            wml_solid(s, svg_num(node, 'stroke-opacity', 1))
        )
    }
    shape <- function(x, y, w, h, geom, fill, line) {
        id <- wml_next_id()
        sprintf(
            paste0(
                '<wps:wsp><wps:cNvPr id="%d" name="Shape %d"/><wps:cNvSpPr/>',
                '<wps:spPr><a:xfrm><a:off x="%d" y="%d"/><a:ext cx="%d" cy="%d"/></a:xfrm>',
                '%s%s%s</wps:spPr><wps:bodyPr/></wps:wsp>'
            ),
            id, id, emu(x), emu(y), max(1, emu(w)), max(1, emu(h)), geom, fill, line
        )
    }
    preset <- function(prst) {
        sprintf('<a:prstGeom prst="%s"><a:avLst/></a:prstGeom>', prst)
    }
    poly <- function(xs, ys, closed) {
        x0 <- min(xs)
        y0 <- min(ys)
        w <- max(1, emu(max(xs) - x0))
        h <- max(1, emu(max(ys) - y0))
        pts <- sprintf('<a:pt x="%d" y="%d"/>', emu(xs - x0), emu(ys - y0))
        list(
            x = x0,
            y = y0,
            w = max(xs) - x0,
            h = max(ys) - y0,
            geom = sprintf(
                paste0(
                    '<a:custGeom><a:avLst/><a:gdLst/><a:ahLst/><a:cxnLst/>',
                    '<a:rect l="0" t="0" r="r" b="b"/><a:pathLst>',
                    '<a:path w="%d" h="%d">%s%s%s</a:path></a:pathLst></a:custGeom>'
                ),
                w, h,
                sprintf('<a:moveTo>%s</a:moveTo>', pts[1]),
                paste(sprintf('<a:lnTo>%s</a:lnTo>', pts[-1]), collapse = ''),
                if (closed) '<a:close/>' else ''
            )
        )
    }

    nodes <- xml2::xml_find_all(
        doc,
        '//*[self::rect or self::line or self::path or self::circle or self::text][not(ancestor::defs)]'
    )
    shapes <- vapply(nodes, function(n) {
        switch(
            xml2::xml_name(n),
            rect = shape(
                svg_num(n, 'x'), svg_num(n, 'y'),
                svg_num(n, 'width'), svg_num(n, 'height'),
                preset('rect'), fill_xml(n), line_xml(n)
            ),
            circle = {
                r <- svg_num(n, 'r')
                shape(
                    svg_num(n, 'cx') - r, svg_num(n, 'cy') - r, 2 * r, 2 * r,
                    preset('ellipse'), fill_xml(n), line_xml(n)
                )
            },
            line = {
                p <- poly(
                    c(svg_num(n, 'x1'), svg_num(n, 'x2')),
                    c(svg_num(n, 'y1'), svg_num(n, 'y2')),
                    closed = FALSE
                )
                shape(p$x, p$y, p$w, p$h, p$geom, '<a:noFill/>', line_xml(n))
            },
            path = {
                d <- xml2::xml_attr(n, 'd')
                v <- as.numeric(regmatches(d, gregexpr('-?[0-9.]+', d))[[1]])
                p <- poly(v[c(TRUE, FALSE)], v[c(FALSE, TRUE)], closed = grepl('Z', d))
                shape(p$x, p$y, p$w, p$h, p$geom, fill_xml(n), line_xml(n))
            },
            text = {
                label <- xml2::xml_text(n)
                fs <- svg_num(n, 'font-size', 9)
                w <- nchar(label) * fs * 0.6 + 4
                h <- fs * 1.4
                x <- svg_num(n, 'x')
                anchor <- xml2::xml_attr(n, 'text-anchor')
                anchor <- if (is.na(anchor)) 'start' else anchor
                left <- switch(anchor, start = x, end = x - w, x - w / 2)
                jc <- switch(anchor, start = 'left', end = 'right', 'center')
                col <- xml2::xml_attr(n, 'fill')
                col <- if (is.na(col)) '#000000' else col
                id <- wml_next_id()
                sprintf(
                    paste0(
                        '<wps:wsp><wps:cNvPr id="%d" name="Text %d"/><wps:cNvSpPr txBox="1"/>',
                        '<wps:spPr><a:xfrm><a:off x="%d" y="%d"/><a:ext cx="%d" cy="%d"/></a:xfrm>',
                        '%s<a:noFill/><a:ln><a:noFill/></a:ln></wps:spPr>',
                        '<wps:txbx><w:txbxContent><w:p><w:pPr>',
                        '<w:spacing w:before="0" w:after="0" w:line="240" w:lineRule="auto"/>',
                        '<w:ind w:left="0" w:right="0"/><w:jc w:val="%s"/></w:pPr>',
                        '<w:r><w:rPr><w:color w:val="%s"/><w:sz w:val="%d"/><w:szCs w:val="%d"/></w:rPr>',
                        '<w:t xml:space="preserve">%s</w:t></w:r></w:p></w:txbxContent></wps:txbx>',
                        '<wps:bodyPr lIns="0" tIns="0" rIns="0" bIns="0" wrap="none" anchor="t">',
                        '<a:noAutofit/></wps:bodyPr></wps:wsp>'
                    ),
                    id, id,
                    emu(left), emu(svg_num(n, 'y') - fs), emu(w), emu(h),
                    preset('rect'), jc, wml_hex(col),
                    round(fs * 1.5), round(fs * 1.5), # px -> half-points
                    wml_escape(label)
                )
            }
        )
    }, character(1))

    id <- wml_next_id()
    sprintf(
        paste0(
            '<w:r><w:drawing><wp:inline distT="0" distB="0" distL="0" distR="0">',
            '<wp:extent cx="%d" cy="%d"/><wp:docPr id="%d" name="table1viz graphic %d"/>',
            '<a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">',
            '<a:graphicData uri="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup">',
            '<wpg:wgp xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup" ',
            'xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape">',
            '<wpg:cNvGrpSpPr/><wpg:grpSpPr><a:xfrm><a:off x="0" y="0"/>',
            '<a:ext cx="%d" cy="%d"/><a:chOff x="0" y="0"/><a:chExt cx="%d" cy="%d"/>',
            '</a:xfrm></wpg:grpSpPr>%s</wpg:wgp></a:graphicData></a:graphic>',
            '</wp:inline></w:drawing></w:r>'
        ),
        emu(width), emu(height), id, id,
        emu(width), emu(height), emu(width), emu(height),
        paste(shapes, collapse = '')
    )
}
