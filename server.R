library(rCharts)
library(shiny)
library(dplyr)


rh <- readRDS("./data/rh.rds")

rh$COD_UORG_LOTACAO <- format(rh$COD_UORG_LOTACAO,scientific=F)
rh$COD_UORG_EXERCICIO <- format(rh$COD_UORG_EXERCICIO,scientific=F)
rh$UORG_LOTACAO <- gsub("^$", "REITORIA", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("COORD GERAL DE EXEC ORÇ E FINANC - CGEO","REITORIA", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("PRÓ-REITORIA DE ADMINISTRAÇÃO - PRAD","REITORIA", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("PRÓ-REITORIA DE DESENV INSTITUCIONAL","REITORIA", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("DIREÇÃO GERAL DE CAMPUS-DGBR","CAMPUS BRASÍLIA", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("DIREÇÃO GERAL DE CAMPUS-DGCE","CAMPUS CEILÂNDIA", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("DIREÇÃO GERAL DE CAMPUS-DGES","CAMPUS ESTRUTURAL", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("DIREÇÃO GERAL DE CAMPUS-DGGA","CAMPUS GAMA", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("DIREÇÃO GERAL DE CAMPUS-DGPL","CAMPUS PLANALTINA", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("DIREÇÃO GERAL DE CAMPUS-DGSA","CAMPUS SAMAMBAIA", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("DIREÇÃO GERAL DE CAMPUS-DGTG","CAMPUS TAGUATINGA", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("DIREÇÃO GERAL DE CAMPUS - DGSS","CAMPUS SÃO SEBASTIÃO", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("DIREÇÃO GERAL DE CAMPUS - DGTC","CAMPUS TAGUATINGA CENTRO", rh$UORG_LOTACAO)
rh$UORG_LOTACAO <- gsub("DIRETORIA GERAL DE CAMPUS-DGRF","CAMPUS RIACHO FUNDO", rh$UORG_LOTACAO)


rhsf <- filter(rh, NIVEL_FUNCAO == ""&DESCRICAO_CARGO!="")

rhsf_last <- filter(rhsf,periodo==last(rh$periodo))

serv_ug_lot <- rhsf_last %>% group_by(UORG_LOTACAO) %>% filter(grepl("26428",COD_UORG_LOTACAO)) %>% summarise(Quantidade=n())

shinyServer( 
  function(input, output,session) {
    output$chart3_3 <- renderChart2({
      n1 <- Highcharts$new()
      n1$title(text = "Total de Servidores por Setor de Lotação no IFB")
      n1$chart(width = 870, borderRadius=20,borderColor="#b7db8a",borderWidth= 1)
      n1$data(x = serv_ug_lot$UORG_LOTACAO , y = serv_ug_lot$Quantidade, type = "pie",name = "Servidores")
      n1$tooltip(formatter = "#! function() { return '<a>'+ this.point.name +'</a>'+'<br/>'+'<a>'+ this.series.name +'</a>:'+ ' '+
                 '<b>'+ Highcharts.numberFormat(this.y, 0,',','.') + '</b>'; } !#")
      n1$plotOptions(pie= list(dataLabels = list(enabled = T),allowPointSelect = T, showInLegend=T))
      n1$legend(align = 'right',verticalAlign = 'middle',layout = 'vertical')
      n1$colors('#1b250e','#364a1c','#517029','#283815','#5e8230','#79a83e', '#93c157', '#abcf7d', '#c3dda2', '#dbeac7','#f3f8ec')
      n1$exporting(enabled = F)
      return(n1)
      
  })
    
  }) 