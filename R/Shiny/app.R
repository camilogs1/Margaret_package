library(shiny)
library(shinythemes)
library(datasets)
library(tidyverse)
library(here)
library(DT)
library(plotly)
library(readxl)
library(openxlsx)
library(stringi)
library(stringr)
library(shinydashboard)
library(writexl)
library(margaret)
#-----------------------------------------------------------------------------------------------------#

get_data <- function(data){
  new_data <- getting_data(data)
  return(new_data)
}

#Prueba cargando
shiny_busy <- function() {
  HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
    '<span data-display-if="',
    '$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;',
    '">',
    '<i class="fa fa-spinner fa-pulse fa-fw" style="color:orange"></i>',
    '</span>'
  ))
}
#-----------------------------------------------------------------------------------------------------#
# grupos = read.csv("C:\\Users\\bryan\\Desktop\\Cienciometria\\prueba\\UCLA.csv", header=T, sep=",")
# #inves_UCLA = read.csv("C:\\Users\\bryan\\Desktop\\Cienciometria\\prueba\\inves.csv", header=T, sep=",")
# #UCLA = read.csv("C:\\Users\\User\\Downloads\\UCLA.csv", header=T, sep=",")
#
# margaret = getting_data(grupos)
#-----------------------------------------------------------------------------------------------------#
#dataframe filtros
#filtro grupo

# grupos <- grupos_general |>
#   select(grupo) |>
#   unique()
#
# general_grupos <- list(grupos$grupo)

#-----------------------------------------------------------------------------------------------------#
#Inicio
# filterside <- selectInput("grupos_input","Grupos:",
#                           c('General'= FALSE, margaret()$grupo),
#                           selectize = FALSE)

#butonside <- actionButton("aplicar_input", "Aplicar")

sliderside <- sliderInput("fechas_input", "Años:", min = 2014, max = 2022, value = c(2016,2022), sep = "")

sidebar <- dashboardSidebar(
  #filterside,
  sliderside,
  #butonside,
  sidebarMenu(
    menuItem("Importar datos", tabName = "importar_datos", icon = icon("upload")),

    menuItem("Datos", tabName = "general_datos", icon = icon("atlas")),

    menuItem("Producción cientifica", icon = icon("book"), tabName = ("produccion")),


    menuItem("Grupos en cifras", icon = icon("bar-chart-o"),
             menuSubItem("Clasificación grupos", tabName = "clasi_grupos"),
             menuSubItem("Clasificación investigadores", tabName = "clasi_inves"),
             menuSubItem("Categoría revistas", tabName = "cate_revista"),
             menuSubItem("Evolución temporal", tabName = "evolu_articulos"),
             menuSubItem("Formación investigadores", tabName = "forma_inves")
    ),
    #download
    menuItem("Descargar",icon = icon("fas fa-download"), downloadButton("download", "Download full results"))
  ),
  mainPanel(
    textOutput("grupos_input")
  )
)

setup <- dashboardBody(
  tabItems(
    tabItem(tabName = "importar_datos",
            fluidPage(br(), h2("Importar grupos y direcciones URL para ejecutar Margaret"), fileInput("upload", "Choose csv or excel file", accept = c(".xlsx", ".csv"), width = '500px')),
            actionButton("go", "Subir"),br(),h2("Estructura del archivo"),br(),
            column(1, align="left", offset = 1,
                   a(img(src="ejemplo.png", height=200, width=900),
                     target="_blank")
            )),
    tabItem(tabName = "general_datos",
            tabsetPanel(type = "tabs",
                        tabPanel("Grupos", fluidPage(br(),h3(textOutput("carga")),(DT::dataTableOutput('ex1'))
                        ),),

                        tabPanel("Investigadores", fluidPage(br(),(DT::dataTableOutput('ex2'))
                        ),),

                        # tabPanel("Paises", fluidPage(br(),(DT::dataTableOutput('ex3'))
                        # ),),

                        # tabPanel("Revistas", fluidPage(br(),(DT::dataTableOutput('ex4'))
                        # ))
                        )),
    tabItem(tabName = "produccion",
            tabsetPanel(type = "tabs",
                        tabPanel("Articulos", fluidPage(br(),(DT::dataTableOutput('articulo'))
                        )),
                        tabPanel("Capitulos", fluidPage(br(),(DT::dataTableOutput('capitulo'))
                        )),
                        tabPanel("Libros", fluidPage(br(),(DT::dataTableOutput('libro'))
                        )),
                        tabPanel("Software", fluidPage(br(),(DT::dataTableOutput('software'))
                        )),
                        tabPanel("Innovaciones", fluidPage(br(),(DT::dataTableOutput('innovaciones'))
                        )),
                        tabPanel("Trabajos dirigidos/Tutorías",
                                 fluidPage(br(),(DT::dataTableOutput('trabajosd'))
                                 )))),
    tabItem(tabName = "clasi_grupos",
            fluidPage(plotlyOutput("graf1"))),

    tabItem(tabName = "clasi_inves",
            fluidPage(plotlyOutput("graf2"))),

    tabItem(tabName = "cate_revista",
            fluidPage(
              fluidRow(box(
              title = "PUBLINDEX",width = 8, status = "warning", solidHeader = TRUE,
              collapsible = TRUE, collapsed = T,
              plotlyOutput("graf3", height = 300)
            )),
            fluidRow(box(
              title = "SCIMAGO", width = 8, status = "warning", solidHeader = TRUE,
              collapsible = TRUE,collapsed = T,
              plotlyOutput("graf3_1", height = 300)
            ))
              )),

    tabItem(tabName = "evolu_articulos",
            fluidPage(plotlyOutput("graf4"))),

    tabItem(tabName = "forma_inves",
            fluidPage(plotlyOutput("graf5")))
  )
)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Margaret",
                  dropdownMenu(type = "notifications", icon = shiny::icon("code"),
                               badgeStatus = "info", headerText = "Desarrolladores",
                               tags$li(a(href = "https://github.com/srobledog",
                                         target = "_blank",
                                         tagAppendAttributes(icon("github")),
                                         "Sebastian Robledo")),
                               tags$li(a(href = "https://github.com/bryanariasq02",
                                         target = "_blank",
                                         tagAppendAttributes(icon("github")),
                                         "Bryan Arias")),
                               tags$li(a(href = "https://github.com/camilogs1",
                                         target = "_blank",
                                         tagAppendAttributes(icon("github")),
                                         "Camilo García"))
                  )),
  sidebar,
  setup
)

server <- function(input, output) {

  filtro <- reactive(input$grupos_input)

  filtro_fecha_min <- reactive({input$fechas_input[1]})

  filtro_fecha_max <- reactive({input$fechas_input[2]})

  margaret <- reactive({
    req(input$upload)

    ext <- input$upload$datapath
    ext <- str_remove(ext, ".*/0.")

    if(ext == "xlsx")
    {
      data <- read.xlsx(input$upload$datapath)
    }else{
      data <- read.csv(input$upload$datapath, header = TRUE)
    }

    showModal(modalDialog("Exportanto información a Margaret espere un momento",
                            shiny_busy(), easyClose = FALSE))

    new_data <- get_data(data)

    if(is.null(input$upload)){
      showModal(modalDialog("Hubo un problema, intentelo de nuevo"))
    }else{
      showModal(modalDialog("Margaret se ejecutó correctamente"))
    }
    return(new_data)
  })

  observeEvent(input$go, {
    margaret()
  })

  output$download <- downloadHandler(
    filename = "Margaret.xlsx",
    content = function(file) {
      write_xlsx(margaret(), file)
    }
  )

  getstatus <- reactive(
    if(is.null(input$upload)){
      "Por favor Importar archivo para visualizar la información"
    }
  )

  output$carga <- renderText({
    getstatus()
  })

  # output$imagen <- renderImage({
  #
  #   outfile <- tempfile(fileext = 'img/prueba.png')
  #
  #   png(outfile, width = 400, height = 300)
  #   hist(rnorm(input$obs), main = "Generated in renderImage()")
  #   dev.off()
  #
  #   list(src = outfile,
  #        width = 400,
  #        height = 300,
  #        alt = "Imagen ejemplo")
  # }, deleteFile = TRUE)

  output$ex1 <- DT::renderDataTable(server = FALSE,{

    grupos_general <- margaret()
    grupos_general2 <- as.data.frame(grupos_general[1]) |>
      select(grupo_main_cleaned.grupo, grupo_main_cleaned.clasificacion,
             grupo_main_cleaned.web, grupo_main_cleaned.sum_papers,
             grupo_main_cleaned.departamento, grupo_main_cleaned.url.x,
             grupo_main_cleaned.fecha_creacion, grupo_main_cleaned.lider,
             grupo_main_cleaned.email, grupo_main_cleaned.area_conocimiento_1) |>
      mutate(grupo_main_cleaned.url.x= str_c('<a href="',grupo_main_cleaned.url.x,
                               '" target="_blank">Link</a>'),
             grupo_main_cleaned.web = str_c('<a href="',grupo_main_cleaned.web,
                                            '" target="_blank">Link</a>')) |>
      datatable(filter = 'top',extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons =
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Clasificación", "Pagina web", "Cantidad artículos",
                             "Departamento","GrupLAC",
                             "Fecha Creación", "Líder", "Email",
                             "Área de Conocimiento"),
                class = 'cell-border stripe')
  })

  output$ex2 <- DT::renderDataTable(server = FALSE,{

      investigadores_general <- as.data.frame(margaret()[["grupo_researcher_cleaned"]])

      investigadores_general <- investigadores_general |>
        mutate(url = str_c('<a href="',
                           url,
                           '" target="_blank">Link</a>')) |>
        select(-vinculacion,
               -fin_vinculacion) |>
        rename(Investigador = integrantes,
               Horas = horas_dedicacion,
               CvLAC = url,
               Grupo = grupo,
               Inicio = inicio_vinculacion,
               Formacion = posgrade,
        ) |>
        select(Investigador,
               Grupo,
               articulos,
               capitulos,
               libros,
               softwares,
               trabajos_dirigidos,
               innovaciones,
               clasification,
               Formacion,
               Inicio,
               CvLAC)

        datatable(investigadores_general,filter = 'top',extensions = c('Scroller','Buttons'),
                  options = list(dom = 'Bfrtip',
                                 buttons =
                                   list('copy', list(
                                     extend = 'collection',
                                     buttons = c('csv', 'excel', 'pdf'),
                                     text = 'Download'
                                   )),
                                 deferRender = TRUE,
                                 scrollY = 420,
                                 scroller = TRUE,
                                 scrollX = TRUE),
                  escape = FALSE,
                  rownames = FALSE,
                  colnames = c("Investigador", "Grupo", "Artículos", "Capítulos",
                               "Libros", "Softwares", "Trabajos Dirigidos",
                               "Innovaciones", "Categoría",
                               "Formación","Inicio", "CvLAC"),
                  class = 'cell-border stripe')
  })

  # output$ex3 <- DT::renderDataTable(server = FALSE,{
  #
  #   paises_general <- paises_general |>
  #     mutate(porcentaje = str_c(porcentaje," %"),
  #            pais_revista = if_else(is.na(pais_revista), "No registra", pais_revista)) |>
  #     datatable(extensions = c('Scroller','Buttons'),
  #               options = list(dom = 'Bfrtip',
  #                              buttons =
  #                                list('copy', list(
  #                                  extend = 'collection',
  #                                  buttons = c('csv', 'excel', 'pdf'),
  #                                  text = 'Download'
  #                                )),
  #                              deferRender = TRUE,
  #                              scrollY = 420,
  #                              scroller = TRUE,
  #                              scrollX = TRUE),
  #               escape = FALSE,
  #               rownames = FALSE,
  #               colnames = c("País", "Cantidad", "Porcentaje"),
  #               class = 'cell-border stripe')
  #
  # })

  # output$ex4 <- DT::renderDataTable(server = FALSE,{
  #
  #   revistas_actuales <- revistas_actuales |>
  #     mutate(porcentaje = str_c(porcentaje," %")) |>
  #     datatable(filter = 'top',extensions = c('Scroller','Buttons'),
  #               options = list(dom = 'Bfrtip',
  #                              buttons =
  #                                list('copy', list(
  #                                  extend = 'collection',
  #                                  buttons = c('csv', 'excel', 'pdf'),
  #                                  text = 'Download'
  #                                )),
  #                              deferRender = TRUE,
  #                              scrollY = 420,
  #                              scroller = TRUE,
  #                              scrollX = TRUE),
  #               rownames = FALSE,
  #               colnames = c('Revista', 'ISSN', 'Categoría Publindex',
  #                            'Categoría Scimago','Cantidad', 'Porcentaje'),
  #               class = 'cell-border stripe')
  # })

  output$articulo <- DT::renderDataTable(server = FALSE,{

    articulos_2016_2020 <- as.data.frame(margaret()[["articulos"]])
    articulos_2016_2020 <- articulos_2016_2020 |>
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max()) |>
      select(-id) |>
      mutate(DOI = str_extract(DOI, "\\d.*")) |>
      mutate(DOI =  str_c("<a href=","\"",
                          "https://doi.org/",
                          DOI,
                          "\"",
                          '" target="_blank">Link</a>'))

      datatable(articulos_2016_2020 ,filter = 'top',extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons =
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                class = ('cell-border stripe'),
                colnames = c("Grupo", "Categoría", "Tipo producto",
                             "Título", "País revista", "Revista",
                             "ISSN","Categoría Publindex", "Categoría Scimago", "Año", "Volumen",
                             "Fasc","Paginas", "Enlace artículo", "Autores"))

  })

  output$capitulo <- DT::renderDataTable(server = FALSE,{

    capitulos_2016_2020 <- as.data.frame(margaret()[["capitulos"]])
    capitulos_2016_2020 <- capitulos_2016_2020 |>
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max()) |>
      select(-vol, -tipo_producto)

      datatable(capitulos_2016_2020 ,filter = 'top',extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons =
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Categoría",
                             "Título capitulo", "País", "Año",
                             "Titulo libro","ISBN", "Paginas", "Editorial",
                             "Autores"),
                class = 'cell-border stripe')
  })

  output$libro <- DT::renderDataTable(server = FALSE,{

    libros_2016_2020 <- as.data.frame(margaret()[["libros"]])
    libros_2016_2020 <- libros_2016_2020 |>
      filter(Ano >= filtro_fecha_min(),
             Ano <=filtro_fecha_max()) |>
      select(-Tipo_producto)

      datatable(libros_2016_2020 ,filter = 'top', extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons =
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Categoría",
                             "Título libro", "País", "Año",
                             "ISBN","Editorial", "Autores"),
                class = 'cell-border stripe')
  })

  output$software <- DT::renderDataTable(server = FALSE,{

    software_2016_2020 <- as.data.frame(margaret()[["softwares"]])
    software_2016_2020 <- software_2016_2020 |>
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max()) |>
      select(-nombre_proyecto, -tipo_producto) |>
      mutate(sitio_web= str_c("<a href=",
                              sitio_web,
                              '" target="_blank">Link</a>'))

      datatable(software_2016_2020 ,filter = 'top', extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons =
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Categoría",
                             "Título", "País", "Año",
                             "Disponibilidad","Sitio web", "Nombre comercial", "Institución financiadora",
                             "Autores"),
                class = 'cell-border stripe')
  })

  output$innovaciones <- DT::renderDataTable(server = FALSE,{

    innovacion_2016_2020 <- as.data.frame(margaret()[["innovaciones_procesos"]])
    innovacion_2016_2020 <- innovacion_2016_2020 |>
      filter(ano >= filtro_fecha_min(),
             ano <=filtro_fecha_max())

      datatable(innovacion_2016_2020 ,filter = 'top',extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons =
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Categoría", "Tipo Producto",
                             "Título", "país", "Año",
                             "Disponibilidad","Institución financiadora", "Autores"),
                class = 'cell-border stripe')
  })

  output$trabajosd <- DT::renderDataTable(server = FALSE,{

    trabajo_2016_2020 <- as.data.frame(margaret()[["trabajos_dirigidos"]])
    trabajo_2016_2020 <- trabajo_2016_2020 |>
      mutate(hasta = str_remove(hasta, ".* "),
             hasta = str_trim(hasta),
             desde = str_remove(desde, "\\d.* "),
             desde = str_trim(desde)) |>
      filter(desde >= filtro_fecha_min(),
             hasta <= filtro_fecha_max())

      datatable(trabajo_2016_2020 ,filter = 'top', extensions = c('Scroller','Buttons'),
                options = list(dom = 'Bfrtip',
                               buttons =
                                 list('copy', list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )),
                               deferRender = TRUE,
                               scrollY = 420,
                               scroller = TRUE,
                               scrollX = TRUE),
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Grupo", "Categoría", "Tipo Producto",
                             "Título", "Desde", "Hasta",
                             "Tipo de Orientación","Estudiante", "Programa académico",
                             "Páginas", "Valoración", "Institución", "Tutor Coautor"),
                class = 'cell-border stripe')
  })


  output$graf1 <- renderPlotly({

    grupos_general2 <- as.data.frame(margaret()[["grupo_main_cleaned"]])

      datos_clasi1 <- grupos_general2 |>
        count(clasificacion) |>
        arrange(desc(clasificacion)) |>
        plot_ly(x = ~clasificacion, y = ~n, type = 'bar') |>
          layout(title = 'Clasificación Grupos de investigación',
                 xaxis = list(title = ""),
                 yaxis = list(title = ""))
  })


  output$graf2 <- renderPlotly ({
     datos_clasificacion <- as.data.frame(margaret()[["grupo_researcher_cleaned"]]) |>
       count(grupo ,clasification) |>
       arrange(desc(clasification))

       plot_ly(datos_clasificacion ,labels= ~clasification, values=~n, type = 'pie') |>
         layout(title = 'Categorías investigadores',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


  })

  output$graf3 <- renderPlotly({
     datos_revista <- as.data.frame(margaret()[["articulos"]]) |>
       count(grupo ,categoria_revista) |>
       arrange(desc(categoria_revista)) |>
       mutate(categoria_revista = ifelse(is.na(categoria_revista),"N/A",categoria_revista))

       plot_ly(datos_revista, labels= ~categoria_revista, values=~n, type = 'pie') |>
         layout(title = 'Categorías revistas',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })

  output$graf3_1 <- renderPlotly({
    datos_revista <- as.data.frame(margaret()[["articulos"]]) |>
      count(grupo ,SJR_Q) |>
      arrange(desc(SJR_Q)) |>
      mutate(SJR_Q = ifelse(is.na(SJR_Q),"N/A",SJR_Q))

      plot_ly(datos_revista, labels= ~SJR_Q, values=~n, type = 'pie') |>
        layout(title = 'Categorías revistas',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })

  output$graf4 <- renderPlotly({

       datos_produccion1 <- as.data.frame(margaret()[["articulos"]]) |>
         select(categoria, ano, grupo) |>
         count(ano, sort = FALSE, name = "producciones") |>
       plot_ly(x = ~ano, y = ~producciones, type = 'scatter', mode = 'lines') |>
         layout(title = "Producción articulos",
                xaxis = list(title = "Año"),
                yaxis = list(title = "Producción"))
  })

  output$graf5 <- renderPlotly({
     datos_formacion <- as.data.frame(margaret()[["grupo_researcher_cleaned"]]) |>
       count(grupo, posgrade) |>
       arrange(desc(posgrade))

       plot_ly(datos_formacion, labels= ~posgrade, values=~n, type = 'pie') |>
         layout(title = 'Formación investigadores',
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })

}

shinyApp(ui = ui , server = server)
