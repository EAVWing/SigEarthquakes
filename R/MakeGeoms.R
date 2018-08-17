#' Earthquake Time Line
#'
#' @description This geom plots a time line of earthquakes which occured over a give span of years with a point for each earthquake
#'
#'@importFrom ggplot2 layer
#'
#'@inheritParams ggplot2::geom_point
#'
#'@examples
#'\dontrun{
#'getData() %>% eq_clean_data() %>%
#' filter(COUNTRY %in% c("USA"), YEAR > 2010) %>%
#'  ggplot(aes(x = DATE, y = COUNTRY, color = as.numeric(TOTAL_DEATHS),
#'   size = as.numeric(EQ_PRIMARY))) +
#'  geom_timeline() + labs(size = "Richter scale value", color = "# deaths")
#'}
#'
#'@export
geom_timeline <-
    function(mapping = NULL,
             data = NULL,
             stat = "identity",
             position = "identity",
             na.rm = FALSE,
             show.legend = NA,
             inherit.aes = TRUE,
             ...) {
        ggplot2::layer(
            geom = GeomTimeline,
            mapping = mapping,
            data = data,
            stat = stat,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)
        )
    }

#' GeomTimeline - this is the code to create the geom we'll use in our plot
#'
#' @importFrom ggplot2 aes draw_key_point
#' @importFrom grid pointsGrob linesGrob gList gpar
#' @importFrom scales alpha
GeomTimeline <- ggplot2::ggproto(
    "GeomTimeline",
    ggplot2::Geom,
    required_aes = c("x"),
    default_aes = ggplot2::aes(
        color = "grey",
        size = 1.5,
        alpha = 0.5,
        shape = 21,
        fill = "grey",
        stroke = 0.0001
    ),
    draw_key = ggplot2::draw_key_point,
    draw_panel = function(data, panel_scales, coord) {
        if (!("y" %in% colnames(data))) {
            data$y <- 0.2
        }

        coords <-
            coord$transform(data, panel_scales)

        points <- grid::pointsGrob(
            coords$x,
            coords$y,
            pch = coords$shape,
            size = unit(coords$size / 2, "char"),
            gp = grid::gpar(
                col = scales::alpha(coords$colour, coords$alpha),
                fill = scales::alpha(coords$colour, coords$alpha)
            )
        )

        y_lines <- unique(coords$y)

        lines <- grid::polylineGrob(
            x = unit(rep(c(0, 1), each = length(y_lines)), "npc"),
            y = unit(c(y_lines, y_lines), "npc"),
            id = rep(seq_along(y_lines), 2),
            gp = grid::gpar(col = "grey",
                            lwd = .pt)
        )

        grid::gList(points, lines)
    }
)

#' Theme for better timeline visualization in ggplot2
#'
#' @description  This is a simple theme that makes \code{\link{geom_timeline}}
#' look better.
#'
#' @examples
#' \dontrun{
#'getData() %>% eq_clean_data() %>%
#' filter(COUNTRY %in% c("USA"), YEAR > 2010) %>%
#'  ggplot(aes(x = DATE, y = COUNTRY, color = as.numeric(TOTAL_DEATHS),
#'   size = as.numeric(EQ_PRIMARY))) +
#'  geom_timeline() + theme_timeline() +
#'  labs(size = "Richter scale value", color = "# deaths")
#'}
#'
#' @importFrom ggplot2 theme element_blank element_line
#'
#' @export
theme_timeline <- function() {
    ggplot2::theme(
        plot.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        legend.key = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(size = 1),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = "bottom"
    )
}

#' Timeline labels of earthquakes
#'
#' @description This will add labels to the earthquake timeline.
#'
#' @inheritParams ggplot2::geom_text
#'
#' @param n_max An integer. If used, it only plots the labels for the
#' largest earthquakes in the selected group in the timeline
#'
#' @details The function adds timeline labels for the earthquakes  plotted with geom_timeline
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' getData() %>% eq_clean_data() %>%
#' filter(COUNTRY %in% c("USA"), YEAR > 2010) %>%
#' ggplot(aes(x = DATE, y = COUNTRY, color = as.numeric(TOTAL_DEATHS),
#'  size = as.numeric(EQ_PRIMARY))) +
#' geom_timeline() + geom_timeline_label(aes(label = LOCATION_NAME)
#'   , n_max = 5) +
#' theme_timeline() + labs(size = "Richter scale value", color = "# deaths")
#' }
#'
#' @export
geom_timeline_label <-
    function(mapping = NULL,
             data = NULL,
             stat = "identity",
             position = "identity",
             ...,
             na.rm = FALSE,
             n_max = NULL,
             show.legend = NA,
             inherit.aes = TRUE) {
        ggplot2::layer(
            geom = GeomTimelineLabel,
            mapping = mapping,
            data = data,
            stat = stat,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, n_max = n_max, ...)
        )
    }

#' @importFrom ggplot2 draw_key_blank
#' @importFrom dplyr %>% group_by top_n ungroup
#' @importFrom grid gpar linesGrob textGrob gList
GeomTimelineLabel <-
    ggplot2::ggproto(
        "GeomTimelineLabel",
        ggplot2::Geom,
        required_aes = c("x", "label"),
        draw_key = ggplot2::draw_key_blank,
        setup_data = function(data, params) {
            if (!is.null(params$n_max)) {
                if (!("size" %in% colnames(data))) {
                    stop(paste(
                        "'size' aesthetics needs to be",
                        "provided when 'n_max' is defined."
                    ))
                }
                data <- data %>%
                    dplyr::group_by_("group") %>%
                    dplyr::top_n(params$n_max, size) %>%
                    dplyr::ungroup()
            }
            data
        },
        draw_panel = function(data, panel_scales, coord, n_max) {
            if (!("y" %in% colnames(data))) {
                data$y <- 0.15
            }

            coords <- coord$transform(data, panel_scales)
            n_grp <- length(unique(data$group))
            offset <- 0.1 / n_grp

            lines <- grid::polylineGrob(
                x = unit(c(coords$x, coords$x), "npc"),
                y = unit(c(coords$y, coords$y + offset), "npc"),
                id = rep(1:dim(coords)[1], 2),
                gp = grid::gpar(col = "grey")
            )

            names <- grid::textGrob(
                label = coords$label,
                x = unit(coords$x, "npc"),
                y = unit(coords$y + offset, "npc"),
                just = c("left", "bottom"),
                rot = 25
            )

            grid::gList(lines, names)
        }
    )
