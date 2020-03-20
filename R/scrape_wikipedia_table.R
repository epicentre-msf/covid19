## following option 2 in this: https://stackoverflow.com/a/31177077
url_iran <- "http://en.wikipedia.org/wiki/Template:2019%E2%80%9320_coronavirus_pandemic_data/Iran_medical_cases"
xpath_iran <- "/html/body/div[3]/div[3]/div[4]/div/table[2]"

scrape_iran_wiki <- function(url = url_iran, xpath = xpath_iran) {
    raw <-
        url %>%
        xml2::read_html() %>% 
        rvest::html_nodes(xpath = xpath) %>% 
        rvest::html_table(fill = TRUE, header = FALSE)

    raw <- raw[[1]]

    colrange <- 1:32

    headers1 <- raw[1,colrange] %>% as.character()
    headers2 <- raw[2,colrange] %>% as.character()
    # totals <- raw[nrow(raw) - 1,]
    # notes <- raw[nrow(raw),]
    data <- raw[3:(nrow(raw)-2), colrange] %>%
        set_names(headers2) %>%
        mutate(
            Date = as.Date(Date)
        ) %>%
        mutate_at(
            vars(-Date),
            as.numeric
        ) %>%
        mutate_at(
            vars(-Date),
            replace_na,
            replace = 0
        )
    
    return(data)
}
