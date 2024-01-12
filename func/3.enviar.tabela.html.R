enviar.tabela.html <- function(df) {
  
  config <- jsonlite::fromJSON('conf/configuracao.json')
  EMISSOR <- 'informartizar@gmail.com'
  SMTP.CONFIG <- list(
    host.name = 'smtp.gmail.com',
    port = 465,
    user.name = config$usuario,
    passwd = config$senha,
    ssl = TRUE
  )
  
  
  epoxy::epoxy_transform_set(
    .dia = function(x) {
      format(x, '%d/%m/%Y')
    },
    .numero = function(x) {
      format(x, big.mark = '.', decimal.mark = ',')
    })
  
  
  # Verifique se o data frame é vazio
  if (nrow(df) == 0) {
    cat('Nenhum registro disponível para envio.\n')
    return()
  } else {
    cat(epoxy::epoxy('Há {.numero nrow(df)} registros disponíveis para envio.\n'))
  }
  
  # Nome do arquivo HTML com carimbo de data e hora
  horario <- format(Sys.time(), format = '%Y%m%d-%H%M%S')
  arquivo.html <- epoxy::epoxy('saida/tabela-{horario}.html')
  
  df$Afetação <- as.Date(df$Afetação, format = '%d/%m/%Y')
  df$Publicação <- format(df$Publicação, '%d/%m/%Y')
  
  # Crie a tabela HTML usando DT
  tabela.html <- DT::datatable(df, rownames = F, class = 'cell-border stripe', filter = 'top', options = list(pageLength = 50)) |>
    DT::formatDate(tabela.html, columns = c(4,6), method = 'toLocaleDateString') |>
    DT::formatStyle(columns = c(3,5), textAlign = 'justify')
  
  
  DT::saveWidget(tabela.html, file = arquivo.html, selfcontained = T)
  
  
  
  # Construa o corpo do email
  # Ler o arquivo CSV e armazenar no dataframe
  df.novo <- readr::read_csv2('dados/temas.csv')
  df.antigo <- readr::read_csv2('dados/temas.csv.antigo')
  
  if (identical(df.novo, df.antigo)) {
    cat('Não houve mudança na relação de temas.')
    
    assunto.email <- paste0(epoxy::epoxy('Em {.dia Sys.Date()}, não houve alteração na relação de temas do STJ.'))
    corpo.email <- paste0(epoxy::epoxy('Não houve alteração em nenhum dos {.numero nrow(df)} temas do STJ já incluídos no portal.'))
    anexo.email <- NULL
  } else {
    assunto.email <- paste0(epoxy::epoxy('Relação de temas do STJ atualizada até {.dia Sys.Date()}.'))
    anexo.email <- arquivo.html
    
    
    comparacao <- diffdf::diffdf(df.novo, df.antigo, suppress_warnings = TRUE)
    
    if(is.integer(comparacao$NumDiff$`No of Differences`)) {
      colunas <- epoxy::epoxy('Houve {sum(comparacao$NumDiff$`No of Differences`)} mudança(s) na tabela de temas do STJ')
      mudancas <- tableHTML::tableHTML(comparacao$NumDiff, headers = c('Variável', 'Quat. Diferenças'))
      
    } else {
      colunas <- 'Não houve mudança em nenhuma coluna da tabela de temas do STJ.'
      mudancas <- htmltools::HTML('<br>')
    }
    
    if(is.integer(comparacao$ExtRowsBase$..ROWNUMBER..)) {
      linhas <- epoxy::epoxy('Houve a adição da(s) linha(s) {.and comparacao$ExtRowsBase$..ROWNUMBER..} na tabela de temas do STJ.')
      inclusoes <- tableHTML::tableHTML(df.novo[comparacao$ExtRowsBase$..ROWNUMBER..,])
    } else {
      linhas <- 'Não aumentou a quantidade de temas do SJT.'
      inclusoes <- htmltools::HTML('<br>')
    }
    
    cat('Houve mudança na relação de temas.')
    corpo.email <- paste0(
      epoxy::epoxy(
        'Há {nrow(df.novo)} temas do STJ a incluir na relação do portal.'
      ),
      '<br><br><br>',
      colunas,
      '<br><br><br>',
      rvest::read_html(mudancas),
      '<br><br><br>',
      linhas,
      '<br><br><br>',
      rvest::read_html(inclusoes),
      '<br><br><br><br><br><br>',
      # rvest::read_html(tabela.html)
    )
    
    
  }
  
  
  # Configurações de envio de email
  email <- mailR::send.mail(
    from = EMISSOR,
    to = DESTINATARIO,
    subject = assunto.email,
    body = corpo.email,
    smtp = SMTP.CONFIG,
    authenticate = TRUE,
    send = TRUE,
    attach.files = anexo.email
  )
  
  # Verifique se o email foi enviado com sucesso
  if (exists('email')) {
    cat(epoxy::epoxy('Email enviado com sucesso para {.and DESTINATARIO}.\n'))
  } else {
    cat(epoxy::epoxy('Falha ao enviar o email para {.and DESTINATARIO}.\n'))
  }
}
