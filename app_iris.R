library(dplyr)
library(ggplot2)
library(stringi)
library(stringr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)


# Llamo a iris----
data(iris)

# creo variables para un identificador unico por observacion----
iris_base <- as_tibble(iris) %>% 
  mutate(lote = 'IRIS', 
         doc = stri_rand_strings(nrow(iris), 1, '[A-C]'), 
         tipo = toupper(substr(Species, 1, 3)))


# Asigno numeración random por grupos para poder así la app revisar cuando esten mal 
#mapeados y no duplicar IDs ----
iris_base_2 <- iris_base %>% 
  group_by(doc, tipo) %>% 
  mutate(numeracion = sample(1:40, n(), replace=FALSE))

iris_base_3 <- iris_base_2 %>%
  mutate(numeracion_str = str_pad(string = numeracion, width = 3, side = c('left'), pad = '0'))



# Genero identificador unico para cada obvservacion ----

iris_base_id <- iris_base_3 %>% mutate(id = paste(lote, doc, tipo, numeracion_str, sep = '.'))


# Para los inputs de la interfaz visual, genero relaciones entre los nombres y las abreviaciones ----
iniciales_a <- c('A', 'B', 'C')
cientificos <- c('Augusto', 'Andres', 'Luciana')
names(iniciales_a) <- cientificos

iniciales_b <- c('SET', 'VER', 'VIR')
especies <- c('setosa', 'versicolor', 'virginia')
names(iniciales_b) <- especies

# Gernero interfaz visual 
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = 'Modelo IRIS',
                    titleWidth = 400),
    dashboardSidebar(width = 400,
                     br(),
                     br(),
                     h3('Navega por la aplicación', align = "center"),
                     br(),
                     sidebarMenu(
                       id = "tabs",
                       menuItem("Inicio", 
                                tabName = "id_ini", 
                                icon = icon("user")),
                       menuItem("Analytics", 
                                tabName = "id_ana", 
                                icon = icon("line-chart")),
                       menuItem("Generador de ID ", 
                                tabName = "id_gen", 
                                icon = icon("database"))),
                       conditionalPanel(
                         "input.tabs == 'id_gen'",
                         selectInput('nombre', 
                                     label = 'Responsable', 
                                     choices = unique(cientificos)),
                         selectInput('especie',
                                     label = 'Especie',
                                     choices = unique(especies)),
                         numericInput('numero', 
                                      'Cantidad de observaciones a agregar', 
                                      1, 
                                      min = 1, 
                                      max = 100)),
                        conditionalPanel(
                          "input.tabs == 'id_ana'",
                          selectInput('inputs_grafico_x', 
                                      label = 'Eje X', 
                                      choices = c('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width'),
                                      selected = 'Petal.Width'),
                          selectInput('inputs_grafico_y', 
                                      label = 'Eje Y', 
                                      choices = c('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width'),
                                      selected = 'Petal.Lenth'),
                          awesomeRadio(
                            inputId = "nombre_2",
                            label = "Elegir científico", 
                            choices = unique(cientificos),
                            selected = "Augusto",
                            status = "warning"),
                          awesomeRadio(
                            inputId = "especie_2",
                            label = "Elegir científico", 
                            choices = unique(especies),
                            selected = "setosa",
                            status = "warning"),
                          sliderInput("petal.l", "Petal legth filter",
                                      min = 0, max = 10.0, value = c(0,10.0), step = 0.1, round = 1),
                          sliderInput("petal.w", "Petal width filter",
                                      min = 0, max = 5.0, value = c(0,5.0), step = 0.1, round = 1),
                          sliderInput("sepal.l", "Sepal length filter",
                                      min = 0, max = 10.0, value = c(0,10.0), step = 0.1, round = 1),
                          sliderInput("sepal.w", "Sepal width filter",
                                      min = 0, max = 5.0, value = c(0,5.0), step = 0.1, round = 1))),
    dashboardBody(
      fluidRow(
        tabItems(
          tabItem("id_ini",
                  width = 600,
                  box(title = h1('Bienvenidos a la aplicación IRIS', align = 'center'),
                      width = 600,
                      height = 700,
                      h4('En esta aplicación se puede realizar analisis exploratorio', align = 'center'),
                      h4('de las flores cargadas como también asignar IDs a nuevas observaciones', align = 'center'),
                      br(),
                      actionButton('jumpTogen', 'Jump to ID generator'),
                      actionButton('jumpToana', 'Jump to Analytics'))
          ),
          tabItem("id_gen",
                  width = 600,
                  box(title = 'Nuevos IDs por agregar', 
                      tableOutput('observaciones'),
                      solidHeader = TRUE, 
                      status = "primary")
          ),
          tabItem("id_ana",
                  br(),
                  width = 600,
                  box(h1('Analytics', align = 'center'),
                      br(),
                      width = 600,
                      DTOutput('tabla_1'),
                      br(),
                      h3('Gráfico dinamico de análisis', align = 'center'),
                      plotOutput("grafico_1"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  #Saltar a la tab de generacion de ID ----
  observeEvent(input$jumpTogen, {
    newtab <- switch(input$tabs,
                     "id_ini" = "id_gen"
    )
    updateTabItems(session, "tabs", newtab)
  })
  
  observeEvent(input$jumpToana, {
    newtab <- switch(input$tabs,
                     "id_ini" = "id_ana"
    )
    updateTabItems(session, "tabs", newtab)
  })
  #Inputs----
  #Realaciono iniciales a partir de los inputs de analytics
  cientificos_2 <- reactive({iniciales_a[input$nombre_2]})
  
  especies_2 <- reactive({iniciales_b[input$especie_2]})
  
  min_pet_len <- reactive({as.numeric(input$petal.l[1])})
  
  max_pet_len <- reactive({as.numeric(input$petal.l[2])})
  
  min_pet_wid <- reactive({as.numeric(input$petal.w[1])})
  
  max_pet_wid <- reactive({as.numeric(input$petal.w[2])})
  
  min_sep_len <- reactive({as.numeric(input$sepal.l[1])})
  
  max_sep_len <- reactive({as.numeric(input$sepal.l[2])})
  
  min_sep_wid <- reactive({as.numeric(input$sepal.w[1])})
  
  max_sep_wid <- reactive({as.numeric(input$sepal.w[2])})
  
 # Defino abreviacion del cientifico(inputs para generacion ID)
  cientificos <- reactive({iniciales_a[input$nombre]})
  
  # Defino abreviacion de la especie(inputs para generacion ID)
  especies <- reactive({iniciales_b[input$especie]})
  
  filtros_analytics <- reactive({
    iris_base_id %>% 
      filter(doc %in% cientificos_2(), 
             tipo %in% especies_2(),
             Petal.Length >= min_pet_len() &
             Petal.Length <= max_pet_len(),
             Petal.Width >= min_pet_wid() &
             Petal.Width <= max_pet_wid(),
             Sepal.Length >= min_sep_len() &
             Sepal.Length <= max_sep_len(),
             Sepal.Width >= min_sep_wid() &
             Sepal.Width <= max_sep_wid()) %>%
      select(Sepal.Length,
             Sepal.Width,
             Petal.Length,
             Petal.Width,
             Species,
             id)
  })
 
  
  # Genero lista de observaciones para revisar que el nuevo ID no caia dentro de uno ya existente
  filtros_id_gen <- reactive({iris_base_id %>%
      filter(doc == cientificos(), tipo == especies())})
  
  lista_obser_existentes <- reactive({filtros_id_gen()$id})
  
  # Genero ID para la especie y cientifico, creo un generico inicial 001
  nueva_obser <- reactive({paste('IRIS', cientificos() , 
                            especies(),
                            '001', 
                            sep = '.')})
  
  # Tomo el ID generado y reviso contra la lista de los ya existentes y actualizarlo con el PRIMERO no existente
  id <- reactive({
    #Defino lista de existentes
    lista_obser_existentes <- as.vector(lista_obser_existentes())
    
    #Defino ID generado 
    nueva_obser <- as.vector(nueva_obser())
    
    #Defino vector vacío para las observaciones que se van a agregar
    x <- c()
    
    #Genero funcion not in
    `%notin%` <- Negate(`%in%`)
    
    #Se define la cantidad de observaciones a agregar y se revisa que la cantidad de observaciones en x
    #se itera siempre que haya menos observaciones que las que se definieron para agregar
    while (length(x) < input$numero) {
      
      #Reviso si el ID generico esta dentro de la lista de los existentes
      while (nueva_obser %in% lista_obser_existentes) {
        
        #Parto ID generico en los cuatro arumentos que lo conforman
        nueva_obser <- str_split(nueva_obser, '\\.', n = 4, simplify = TRUE)
        
        #Asigno nombres a cada campo del ID generico
        colnames(nueva_obser) <- c('Lote', 'Cientifico', 'Tipo', 'numero')
        
        #Transformo en tibble la matriz
        nueva_obser <- as_tibble(nueva_obser) 
        
        #Como el ID es repetido le tengo que sumar uno para continuar revisando y ver si ya no existe
        nueva_obser <- nueva_obser %>% mutate(numero = as.numeric(numero) + 1)
        
        #le asigno las tres posiciones que debe tener el campo numero
        nueva_obser <- nueva_obser %>% mutate(numero = str_pad(string = as.character(numero), 
                                                               width = 3, side = c('left'), 
                                                               pad = '0'))
        
        #Vuelvo a recrear el ID con el nuevo numero
        nueva_obser <- nueva_obser %>% 
          mutate(nueva_obser = paste(Lote, 
                                     Cientifico, 
                                     Tipo, 
                                     numero, 
                                     sep = '.')) %>% 
          select(nueva_obser)
        
        #Para volver a testear si existe, lo transformo en vector nuevamente
        nueva_obser <- as.vector(nueva_obser$nueva_obser)
        
        #Reviso si este ID no existe, de no existir, rompemos la iteracion
        if (nueva_obser %notin% lista_obser_existentes) {
          
          break
          
        }}
      #La primera revisiÃ³n va agregar el primer ID a el vector x y a la lista de observaciones
      if (length(x) == 0) {
        
        x <- nueva_obser
        lista_obser_existentes <- c(lista_obser_existentes,nueva_obser)}
      
      #Caso contrario agrego a x igual pero sumado a los ya existentes (tambiÃ©n sumo a lista de observaciones)
      else  
      {x <- c(x, nueva_obser)
      lista_obser_existentes <- c(lista_obser_existentes,nueva_obser)}}
    
    #Paso x con todos los ID generados para las observaciones a agregar
    x})
  
  # Genero Output para poder pasar en el UI 
  # ObservaciÃ³n: value es el nombre asignado por R  para nueva_obse vluego de transformar en tibble
  output$observaciones <- renderTable(as_tibble(id()) %>% mutate(orden=row_number(), `Identificador de observaciones`= value) %>% select(orden,`Identificador de observaciones`))

  
  output$tabla_1 <- renderDT(filtros_analytics())
  
  output$grafico_1 <- renderPlot(ggplot(filtros_analytics(), 
                                        aes_string(x = input$inputs_grafico_x,
                                            y= input$inputs_grafico_y)) + 
                                   geom_point())
  
}


shinyApp(ui, server)





