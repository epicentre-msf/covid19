

# highcharts export function 

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
    #tableCaption = "", useMultiLevelHeaders = FALSE, csv = list(dateFormat = "%d/%m/%Y"),
    chartOptions = list(
      title = list(text = title, style = list(fontFamily = "Arial")),
      subtitle = list(text = " ", style = list(fontFamily = "Arial")),
      credits = list(enabled = TRUE, text = credits),
      legend = list(title = list(text = legend_title)),
      #colors = pal,
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
