enviar.tabela.html <- function(df, teste = F) {

  # Verificando se há arquivo anterior para comparar
  if (!file.exists('dados/temas.csv') | !file.exists('dados/temas.csv.anterior')) {
    cat('\n\nNão há arquivos para comparar.\n\n')
    return()
  }

  # Verifique se o data frame é vazio
  if (nrow(df) == 0) {
    cat('\n\nNenhum registro disponível para envio.\n\n')
    return()
  } else {
    cat(epoxy::epoxy('\n\nHá {.numero nrow(df)} registros disponíveis para envio.\n\n'))
  }
  
  if (teste) {
    DESTINATARIO <- 'armando.nahmias@tjrr.jus.br'
  } else {
    DESTINATARIO <- c('armando.nahmias@tjrr.jus.br', 'debora.silva@tjrr.jus.br', 'rayandria.santiago@tjrr.jus.br')
  }
  
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
  
  # Nome do arquivo HTML com carimbo de data e hora
  horario <- format(Sys.time(), format = '%Y%m%d-%H%M%S')
  arquivo.html <- epoxy::epoxy('saida/tabela-{horario}.html')
  
  # Crie a tabela HTML usando DT
  source(here::here('func', '4.criar.tabela.html.R'))
  tabela.html <- criar.tabela.html(tabela.temas, arquivo.html)
  
  # Construa o corpo do email
  # Ler o arquivo CSV e armazenar no dataframe
  df.novo <- read.csv2('dados/temas.csv')
  df.anterior <- read.csv2('dados/temas.csv.anterior')

  if (identical(df.novo, df.anterior)) {
    cat('\nNão houve mudança na relação de temas.\n\n')
    
    if (file.exists('dados/data.checagem.rds')) {
      data.checagem <- readRDS('dados/data.checagem.rds')
    } else {
      data.checagem <- Sys.Date()
    }
    
    periodo <- dplyr::case_when(
      identical(Sys.Date(), data.checagem) ~ 'Hoje',
      identical(Sys.Date() - 1, data.checagem) ~ 'Ontem',
      format(Sys.Date()-31, "%Y-%m") == format(data.checagem, "%Y-%m") ~ 'Desde mês passado',
      TRUE ~ paste0(epoxy::epoxy('Desde {.dia data.checagem} até {.dia Sys.Date()}'))
    )
    
    assunto.email <- paste0(epoxy::epoxy('{periodo}, não houve alteração na relação de temas do STJ.'))
    corpo.email <- paste0(epoxy::epoxy('Não houve alteração em nenhum dos {.numero nrow(df)} temas do STJ já incluídos no portal.'))
    anexo.email <- NULL
  } else {
    cat('\nHouve mudança na relação de temas.\n\n')
    
    assunto.email <- paste0(epoxy::epoxy('Relação de temas do STJ atualizada até {.dia Sys.Date()}.'))
    anexo.email <- arquivo.html
    
    
    comparacao <- diffdf::diffdf(df.novo, df.anterior, suppress_warnings = TRUE)
    
    if(is.integer(comparacao$NumDiff$`No of Differences`)) {
      colunas <- epoxy::epoxy('Houve {sum(comparacao$NumDiff$`No of Differences`)} mudança(s) na tabela de temas do STJ')
      lista.mudancas <- list()
      lista.variaveis <- comparacao$NumDiff$Variable
      
      for (variavel in lista.variaveis) {
        assign(paste0('variavel.', variavel), epoxy::epoxy('comparacao$VarDiff_{variavel}'))
        lista.mudancas[[variavel]] <- tableHTML::tableHTML(comparacao[[paste0('VarDiff_', variavel)]], headers = c('VARIABLE', '..ROWNUMBER..', 'BASE', 'COMPARE'))
      }
      
      mudancas <- paste0(lista.mudancas)
      
    } else {
      colunas <- 'Não houve mudança em nenhuma coluna da tabela de temas do STJ.'
      mudancas <- htmltools::HTML('<br>')
      lista.variaveis <- ''
      variaveis <- htmltools::HTML('<br>')
    }
    
    if(is.integer(comparacao$ExtRowsBase$..ROWNUMBER..)) {
      linhas <- epoxy::epoxy('Houve a adição da(s) linha(s) {.and comparacao$ExtRowsBase$..ROWNUMBER..} na tabela de temas do STJ.')
      inclusoes <- tableHTML::tableHTML(df.novo[comparacao$ExtRowsBase$..ROWNUMBER..,])
    } else {
      linhas <- 'Não aumentou a quantidade de temas do SJT.'
      inclusoes <- htmltools::HTML('<br>')
    }
    
    corpo.email <- paste0(
      epoxy::epoxy(
        'Há {.numero nrow(df.novo)} temas do STJ a incluir na relação do portal.'
      ),
      '<br><br><br>',
      colunas,
      '<br><br><br>',
      mudancas,
      '<br><br><br>',
      linhas,
      '<br><br><br>',
      inclusoes,
      '<br><br><br><br><br><br>',
      # rvest::read_html(tabela.html),
      '<br>'
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
    html = TRUE,
    attach.files = anexo.email,
    send = TRUE
  )
  
  # Verifique se o email foi enviado com sucesso
  if (exists('email')) {
    cat(epoxy::epoxy('\nEmail enviado com sucesso para {.and DESTINATARIO}.\n\n'))
  } else {
    cat(epoxy::epoxy('\nFalha ao enviar o email para {.and DESTINATARIO}.\n\n'))
  }
}
