#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readxl)
library(tidyr)
library(dplyr)
library(maps)
library(maptools)
library(sp)
library(ggplot2)
library(plotly)
library(lubridate)
####harita bilgileri######
tur<- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsp/gadm36_TUR_1_sp.rds"))
tur_for <- fortify(tur)
head(tur@data$NAME_1)
turkceden_ingilizceye <- function(dataset){
    turkce_harfler<- c("Ç","Ş","Ğ","İ","Ü","Ö","ç","ş","ğ","ı","ü","ö")
    ingilizce_harfler<- c("C","S","G","I","U","O","c","s","g","i","u","o")
    dataset=mgsub(turkce_harfler,ingilizce_harfler,dataset)
    return(dataset)
}
# Multiple gsub function
mgsub <- function(pattern, replacement, x, ...) {
    n = length(pattern)
    if (n != length(replacement)) {
        stop("pattern and replacement do not have the same length.")
    }
    result = x
    for (i in 1:n) {
        result <- gsub(pattern[i],replacement[i],result)
    }
    return(result)
}
tur@data$NAME_1 <- turkceden_ingilizceye(tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("K. Maras", "Kahramanmaras",tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Kinkkale","Kirikkale",tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Zinguldak", "Zonguldak", tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Afyon","Afyonkarahisar", tur@data$NAME_1 )
tur@data$NAME_1 <- gsub("Mersin","Icel", tur@data$NAME_1 )
#####harita bilgileri son####

#####https://verisistemi.tbb.org.tr/index.php?/tbb/report_bolgeler######

bankacilar <- read_excel("PivotGrid.xlsx")
bankacilar <- bankacilar %>%
    fill(`İl/bölge`,.direction = "down") %>%
    mutate(Yıl = as.Date(paste0(Yıl,"/01/01")))
il <- unique(bankacilar$`İl/bölge`)[-1:-15]
dates <- c(as.Date("2019/01/01"),
           as.Date("2018/01/01"),
           as.Date("2017/01/01"),
           as.Date("2016/01/01"),
           as.Date("2015/01/01"),
           as.Date("2014/01/01"),
           as.Date("2013/01/01"),
           as.Date("2012/01/01"),
           as.Date("2011/01/01"),
           as.Date("2010/01/01")
)
il_data <- bankacilar %>% filter(`İl/bölge` %in% il & Yıl %in% dates)
cs_banka <- il_data %>% select( "sehir" = `İl/bölge`, everything())

cs_banka$sehir=turkceden_ingilizceye(cs_banka$sehir)
cs_banka <- cs_banka %>% rowwise() %>%
    mutate(`Toplam Mevduat` = sum(`Altın Depo Hesabı`,
                                  `Mevduat Sertifikası`,
                                  `Bankalar Mevduatı`,
                                  `Diğer Mevduat`,
                                  `Döviz Tevdiat Hesapları`,
                                  `Resmi Kuruluşlar Mevduatı`,
                                  `Tasarruf Mevduatı`,
                                  `Ticari Kuruluşlar Mevduatı`,
                                  na.rm = T))
cs_banka <- cs_banka %>% rowwise() %>%
    mutate(
        Döviz_Orani = `Döviz Tevdiat Hesapları` / `Toplam Mevduat`*100,
        Altin_Orani = `Altın Depo Hesabı` / `Toplam Mevduat`*100,
        DövizTl_Orani = `Döviz Tevdiat Hesapları` / (`Toplam Mevduat`-`Döviz Tevdiat Hesapları`-`Altın Depo Hesabı`-`Tasarruf Mevduatı`)*100,
        Tasarruf_Orani = `Tasarruf Mevduatı` / `Toplam Mevduat`*100)
##########harita icin##################
id_and_cities<- tibble(id = rownames(tur@data), sehir = tur@data$NAME_1) %>% left_join(cs_banka, by = "sehir")
final_map <- left_join(tur_for, id_and_cities, by = "id") %>% mutate(`Yıl` = year(`Yıl`))
###########degiskene gore harita ve kayıt################





library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage( 

    # Application title
    titlePanel("Bankacılıkla ilgili göstergelerin tematik haritalarla sunumu"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2,
            selectInput(inputId = "bins", label = "Değişken:",choices = colnames(final_map)[c(-1:-9)],selected = "DövizTl_Orani",selectize = T)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot",  width = "100%",height = "100%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
        p <- ggplot(final_map,aes(frame = `Yıl`)) +
            geom_polygon( aes(x = long, y = lat, group = group, fill = final_map[,input$bins]), color = "grey") +
            coord_map() +theme_void() + labs(title = paste0(
                sub("\\_.*", "", input$bins)," illere göre dağılımı ",
                " - 2019"),
                caption = "Kaynak: Bankalar Birliği") +
            scale_fill_distiller(name = input$bins,palette = "Spectral",
                                 limits = c(min(unique(final_map[,input$bins])),
                                            max(unique(final_map[,input$bins]))),
                                 na.value = "white") +
            theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.line=element_blank())
        p <- plotly::ggplotly(p) %>%
            animation_opts(
                1, transition = 1, easing = "elastic", redraw = FALSE
            ) %>%
            animation_button(
                x = 1, xanchor = "right", y = 0, yanchor = "bottom"
            ) %>%
            animation_slider(
                currentvalue = list(prefix = "YIL ", font = list(color="blue"))
            )
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
