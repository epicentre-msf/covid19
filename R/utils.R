
valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle = NULL, icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      p(title),
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      if (!is.null(subtitle)) p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

# highcharts stuff

hc_download_btns <- c("downloadPNG", "downloadJPEG", "downloadSVG")

my_hc_export <- function(hc, title = "", subtitle = "", source = "", width = 800, height = 300, 
                         dl_buttons = hc_download_btns, dl_text = "", filename = "Epicentre-COVID19-Figure-") {
  title <- ifelse(is.null(hc$x$hc_opts$title$text), title, hc$x$hc_opts$title$text)
  subtitle <- ifelse(is.null(hc$x$hc_opts$subtitle$text), subtitle, hc$x$hc_opts$subtitle$text)
  legend_title <- stringr::str_remove(hc$x$hc_opts$legend$title$text, "\\(Click to hide\\)")
  credits <- paste0("Data source: ", source, " | Graphic: Epicentre MSF")
  
  hc_exporting(
    hc,
    enabled = TRUE, sourceWidth = width, sourceHeight = height,
    buttons = list(contextButton = list(menuItems = dl_buttons, text = dl_text)),
    filename = paste0(filename, Sys.Date()), 
    #url = "https://export.highcharts.com/", fallbackToExportServer = TRUE,
    tableCaption = "", useMultiLevelHeaders = FALSE, csv = list(dateFormat = "%d/%m/%Y"),
    chartOptions = list(
      title = list(text = title, style = list(fontFamily = "Arial")),
      subtitle = list(text = " ", style = list(fontFamily = "Arial")),
      credits = list(enabled = TRUE, text = credits),
      legend = list(title = list(text = legend_title)),
      colors = epi_pal2,
      # plotOptions = list(series = list(dataLabels = list(enabled = TRUE, format="{point.y:,.0f}"))),
      chart = list(
        style = list(fontFamily = "Arial"),
        backgroundColor = "#fff",
        events = list(
          load = JS(paste0("function () {  

            this.renderer.image(
            'https://epicentre.msf.org/sites/default/files/logo_Epicentre_1.png',", width - 176, ", 0, 176, 45)
            .add();
            
          }        
          "))
        )
      )
    )
  )
}

hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = FALSE, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = FALSE,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.25,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}
