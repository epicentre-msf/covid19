div_inline <- function(..., margin_right = TRUE) {
  display <- "display: inline-block;"
  margin <- ifelse(margin_right, "margin-right: 10px;", "")
  tags$div(style = paste(display, margin), ...)
}

box_w_inputs <- function(..., title, inputs = NULL, width = 6, height = NULL, footer = NULL) {
  box_custom(
    width = width, height = height, solidHeader = TRUE, footer = footer,
    title = title, inputs = purrr::map(inputs, div_inline),
    ...
  )
}

box_custom <- function (..., title = NULL, inputs = NULL, footer = NULL, status = NULL, solidHeader = FALSE,
                        background = NULL, width = 6, height = NULL, collapsible = FALSE,
                        collapsed = FALSE)
{
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    shinydashboard:::validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    shinydashboard:::validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", style = "display: inline-block; margin-right: 15px; font-weight: bold;", title)
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed)
      "plus"
    else "minus"
    collapseTag <- div(
      class = "box-tools pull-right",
      tags$button(class = paste0("btn btn-box-tool"), `data-widget` = "collapse", shiny::icon(collapseIcon))
    )
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(class = "box-header", titleTag, inputs, collapseTag)
  }
  div(class = if (!is.null(width))
    paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style))
      style, headerTag, div(class = "box-body", ...), if (!is.null(footer))
        div(class = "box-footer", footer)))
}
