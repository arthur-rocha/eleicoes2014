##App eleições 2014 - Brasil
options(encoding = "utf-8",OutDec = ",")
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(magrittr)
library(shinythemes)
library(curl)
#UI --------------------------------------------------------------
ui <- navbarPage(title = "Deputados Federais e Estaduais elegidos no Brasil em 2014",theme = shinytheme("flatly"),
                 tabPanel(title = "Gráficos",icon = icon("signal"),
                          
                          
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "estado",
                                          label = "Escolha o estado",
                                          choices = c("AC","AL","AM","AP","BA","BR","CE","DF",
                                                      "ES","GO","MA","MG","MS","MT","PA","PB",
                                                      "PE","PI","PR","RJ","RN","RO","RR","RS",
                                                      "SC","SE","SP","TO")[-c(6,8)]),  #BR e DF com bug
                              selectInput(inputId = "municipio",
                                          label = "Escolha o município",
                                          choices = "Municipio")
                            ),
                            
                            
                            
                            
                            mainPanel(
                              plotOutput("Plot")
                            )
                          ), 
                          hr(),
                          print("Feito por Arthur Rocha e Omar Pereira")
                 ),
                 tabPanel(title = "Sobre",icon = icon("info-circle"),
                          tags$ul(
                            tags$h2("Autores:"),
                            tags$hr(style="border-color:darkblue;")),
                          
                          tags$ul(
                            tags$h3("Arthur C. Moura Rocha"),
                            tags$hr(),
                            tags$li("Graduando em Estatística - UEM"),
                            tags$h4(
                              tags$a("Github",href="https://github.com/arthur-rocha") )
                          ),
                          tags$ul(
                            tags$h3("Omar C. Neves Pereira"),
                            tags$hr(),
                            tags$li("Pós doutor em Bioestatística - UEM"),
                            tags$li("Doutor em Agronomia"),
                            tags$li("Mestre em Física"),
                            tags$li("Mestre em Agronomia"),
                            tags$li("Bacharel em Física"),
                            tags$li("Engenheiro Agrônomo"),
                            tags$h4(
                              tags$a("Github",href="https://github.com/omarcnpereira") )
                          )
                 ),
                 tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: visible; content: 'Buscando dados... Aguarde'; }"
                 )
)
# SERVER ---------------------------------------------------
server <- function(input, output,session) {
  
  dados = reactive({
    dados = case_when(input$estado =="AC" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/AC.txt",
                      input$estado =="AL" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/AL.txt",
                      input$estado =="AM" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/AM.txt",
                      input$estado =="AP" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/AP.txt",
                      input$estado =="BA" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/BA.txt",
                      input$estado =="BR" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/BR.txt",
                      input$estado =="CE" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/CE.txt",
                      input$estado =="DF" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/DF.txt",
                      input$estado =="ES" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/ES.txt",
                      input$estado =="GO" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/GO.txt",
                      input$estado =="MA" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/MA.txt",
                      input$estado =="MG" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/MG.txt",
                      input$estado =="MS" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/MS.txt",
                      input$estado =="MT" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/MT.txt",
                      input$estado =="PA" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/PA.txt",
                      input$estado =="PB" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/PB.txt",
                      input$estado =="PE" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/PE.txt",
                      input$estado =="PI" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/PI.txt",
                      input$estado =="PR" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/PR.txt",
                      input$estado =="RJ" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/RJ.txt",
                      input$estado =="RN" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/RN.txt",
                      input$estado =="RO" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/RO.txt",
                      input$estado =="RR" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/RR.txt",
                      input$estado =="RS" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/RS.txt",
                      input$estado =="SC" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/SC.txt",
                      input$estado =="SE" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/SE.txt",
                      input$estado =="SP" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/SP.txt",
                      input$estado =="TO" ~"https://raw.githubusercontent.com/arthur-rocha/eleicoes2014/master/TO.txt")
    dados = fread(dados,encoding = "UTF-8")
    updateSelectInput(session, "municipio",choices =sort(dados$NOME_MUNICIPIO))
    return(dados)
  })
  output$Plot <- renderPlot({
    
    municipio = input$municipio
    
    dados() %>%
      filter(NOME_MUNICIPIO==municipio) %>%
      mutate(DESC_SIT_CAND_TOT = factor(DESC_SIT_CAND_TOT,
                                        levels = c("ELEITO POR MÉDIA",
                                                   "ELEITO POR QP","SUPLENTE",
                                                   "NÃO ELEITO")),
             NOME_URNA_CANDIDATO = factor(NOME_URNA_CANDIDATO),
             prop.total = gsub(pattern = ":",replacement = " : ", prop.total)) %>%
      group_by(DESCRICAO_CARGO) %>%
      ggplot(aes(reorder(NOME_URNA_CANDIDATO,PROP_VOT,FUN = max),
                 PROP_VOT,fill=DESC_SIT_CAND_TOT,label=paste0(PROP_VOT,"%")))+
      geom_col()+
      theme_minimal()+
      coord_flip()+
      facet_wrap(~DESCRICAO_CARGO,scales = "free_y")+
      theme(axis.title.y = element_blank(),
            axis.title.x = element_text(color = 2),
            title = element_text(hjust = 60,face = 2),
            legend.position = "bottom")+
      ylim(c(0,100))+
      labs(title=paste("Candidatos mais votados de",municipio),
           subtitle= "Proporção de votos da cidade (%)",
           caption="Fonte: TSE ") +
      geom_text(aes(label=prop.total),x = .8,y=50,fontface=2)+
      scale_fill_manual("Situação",
                        values = c("skyblue2","lightblue4","lightgreen","salmon"),
                        drop=F)+
      geom_text(hjust="inward")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

