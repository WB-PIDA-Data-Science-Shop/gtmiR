#' Scatter plot of any GTMI index vs an outcome, with flexible grouping and lines
#'
#' Creates a scatter plot with points coloured by a grouping variable and an
#' optional set of linear regression lines. Country codes can be shown as
#' repelled text labels. The plot can be saved to disk by providing a filename.
#'
#' @param data A data frame containing all columns referenced by the other
#'   arguments. Typically \code{corr_base} or \code{corr_label} from the
#'   correlation analysis pipeline.
#' @param x A string naming the column to plot on the x-axis. Intended for any
#'   GTMI index (\code{"gtmi"}, \code{"cgsi"}, \code{"psdi"}, \code{"dcei"},
#'   \code{"gtei"}) or any numeric indicator column with the same structure.
#' @param y A string naming the column to plot on the y-axis.
#'   Defaults to \code{"outcome_value"}.
#' @param color_by A string naming the column used for point colour.
#'   Supported values: \code{"region"}, \code{"income_group"}, \code{"grp"}.
#'   When \code{"grp"} is used, a fixed four-colour palette matching GTMI
#'   groups A–D is applied. All other values use
#'   \code{scale_color_brewer(palette = "Paired")}.
#'   Defaults to \code{"region"}.
#' @param group_lines A string naming the column used to draw one regression
#'   line per level (mapped to \code{linetype}), or \code{NULL} (default) to
#'   draw a single overall dashed regression line.
#' @param facet_by A string naming the column to use for \code{facet_wrap},
#'   or \code{NULL} (default) for no faceting.
#' @param show_labels Logical. If \code{TRUE} (default), country codes are
#'   shown as repelled text labels positioned at the mean x/y per country
#'   (and per facet panel when \code{facet_by} is set).
#' @param x_label A string for the x-axis label. Defaults to
#'   \code{toupper(x)}.
#' @param y_label A string for the y-axis label. Defaults to
#'   \code{toupper(y)}.
#' @param title A string for the plot title. Auto-generated from \code{x} and
#'   \code{y} if \code{NULL} (default).
#' @param filename A string file path to save the plot, or \code{NULL}
#'   (default) to return the plot without saving. The directory must exist or
#'   be created before calling the function.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Single plot: gtmi vs judicial accountability, coloured by region
#' make_corr_scatter(
#'   data     = corr_base,
#'   x        = "gtmi",
#'   y        = "outcome_value",
#'   color_by = "region"
#' )
#'
#' # Per-group lines using adoption_label
#' make_corr_scatter(
#'   data        = corr_label,
#'   x           = "cgsi",
#'   color_by    = "income_group",
#'   group_lines = "adoption_label",
#'   facet_by    = "adoption_label",
#'   show_labels = FALSE
#' )
#'
#' # Save to disk
#' make_corr_scatter(
#'   data     = corr_base,
#'   x        = "psdi",
#'   color_by = "grp",
#'   filename = "analysis/figs/corr/grp/psdi_judicial_accountability.png"
#' )
#' }
#'
#' @importFrom dplyr filter group_by across all_of summarise
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth facet_wrap
#'   scale_color_manual scale_color_brewer scale_linetype_discrete
#'   scale_x_continuous labs theme_minimal theme guides guide_legend
#'   ggsave
#' @importFrom ggrepel geom_text_repel
#' @importFrom glue glue
#' @importFrom stats as.formula
#' @importFrom tools toTitleCase
#' @export
make_corr_scatter <- function(
    data,
    x,
    y           = "outcome_value",
    color_by    = "region",
    group_lines = NULL,
    facet_by    = NULL,
    show_labels = TRUE,
    x_label     = toupper(x),
    y_label     = toupper(y),
    title       = NULL,
    filename    = NULL
) {

  # Fixed colour palette for GTMI groups A-D
  grp_colors <- c(
    "A" = "#4DAF4A",
    "B" = "#377EB8",
    "C" = "#FF7F00",
    "D" = "#E41A1C"
  )

  # ── Filter to rows with valid x, y and color_by ───────────────────────────
  plot_data <- data |>
    dplyr::filter(
      !is.na(.data[[x]]),
      !is.na(.data[[y]]),
      !is.na(.data[[color_by]])
    )

  if (!is.null(group_lines)) {
    plot_data <- plot_data |> dplyr::filter(!is.na(.data[[group_lines]]))
  }

  auto_title <- title %||% glue::glue("{toupper(x)} vs {toupper(y)}")

  # ── Deduplicate labels: one per country (× facet panel if faceting) ───────
  label_group_vars <- c("country_code", color_by, if (!is.null(facet_by)) facet_by)
  label_data <- plot_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(label_group_vars))) |>
    dplyr::summarise(
      x_pos = mean(.data[[x]], na.rm = TRUE),
      y_pos = mean(.data[[y]], na.rm = TRUE),
      .groups = "drop"
    )

  # ── Base plot ─────────────────────────────────────────────────────────────
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[x]], y = .data[[y]])
  ) +
    ggplot2::geom_point(
      ggplot2::aes(color = .data[[color_by]]),
      alpha = 0.7,
      size  = 2
    )

  # ── Optional country code labels ──────────────────────────────────────────
  if (show_labels) {
    p <- p + ggrepel::geom_text_repel(
      data = label_data,
      ggplot2::aes(
        x     = x_pos,
        y     = y_pos,
        label = country_code,
        color = .data[[color_by]]
      ),
      size         = 2.5,
      max.overlaps = 20,
      show.legend  = FALSE
    )
  }

  # ── Regression line(s) ────────────────────────────────────────────────────
  if (is.null(group_lines)) {
    p <- p + ggplot2::geom_smooth(
      ggplot2::aes(group = 1),
      method    = "lm",
      formula   = y ~ x,
      se        = FALSE,
      color     = "grey30",
      linetype  = "dashed",
      linewidth = 0.9,
      na.rm     = TRUE
    )
  } else {
    p <- p +
      ggplot2::geom_smooth(
        ggplot2::aes(
          linetype = .data[[group_lines]],
          group    = .data[[group_lines]]
        ),
        method    = "lm",
        formula   = y ~ x,
        se        = FALSE,
        color     = "grey30",
        linewidth = 0.8,
        na.rm     = TRUE
      ) +
      ggplot2::scale_linetype_discrete(
        name = tools::toTitleCase(gsub("_", " ", group_lines))
      )
  }

  # ── Colour scale ──────────────────────────────────────────────────────────
  if (color_by == "grp") {
    p <- p + ggplot2::scale_color_manual(
      values = grp_colors,
      name   = "GTMI Group"
    )
  } else {
    p <- p + ggplot2::scale_color_brewer(
      palette = "Paired",
      name    = tools::toTitleCase(gsub("_", " ", color_by))
    )
  }

  # ── Optional facet ────────────────────────────────────────────────────────
  if (!is.null(facet_by)) {
    p <- p + ggplot2::facet_wrap(
      stats::as.formula(paste("~", facet_by)),
      scales = "free_y"
    )
  }

  # ── Scales, labels, theme ─────────────────────────────────────────────────
  line_caption <- if (is.null(group_lines)) {
    "Dashed line: overall linear fit."
  } else {
    paste0("One regression line per ", gsub("_", " ", group_lines), " (linetype).")
  }

  subtitle_text <- glue::glue(
    "Color = {gsub('_', ' ', color_by)}",
    "{if (!is.null(group_lines)) paste0(' | Lines = ', gsub('_', ' ', group_lines)) else ''}"
  )

  p <- p +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::labs(
      title    = auto_title,
      subtitle = subtitle_text,
      x        = x_label,
      y        = y_label,
      caption  = line_caption
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        override.aes = list(linetype = 0, size = 3)
      )
    )

  # ── Optional save ─────────────────────────────────────────────────────────
  if (!is.null(filename)) {
    ggplot2::ggsave(
      filename = filename,
      plot     = p,
      width    = 9,
      height   = 6,
      dpi      = 300,
      bg       = "white"
    )
  }

  p
}
