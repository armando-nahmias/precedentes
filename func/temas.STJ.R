# Testes
# system(paste('touch -t 202301010000.00', 'dados/processos.csv'))
# arquivos.fonte <- file.path('func', c('baixar_arquivos_STJ.R', 'preparar.tabela.temas.R', 'enviar.tabela.R'))
# lapply(arquivos.fonte, source)



atualizar.arquivos.STJ <- function() {
  base.url <-
    'https://dadosabertos.web.stj.jus.br/dataset/4238da2f-c07b-4c1a-b345-4402accacdcf/resource/'
  
  informacoes <- list(
    'dicionario-temas' = 'd5e50514-6dba-4f1e-8557-94f135eae03b/download/',
    'temas' = 'df29da13-7d6b-41ba-ad96-cd1a5bbd191c/download/',
    'dicionario-processos' = '162e58f0-01c1-4d91-94a4-664b4de81e79/download/',
    'processos' = '7ed21202-0049-4fcb-aa7c-48d810d3c499/download/'
  )
  
  lista.df <- list()
  
  for (i in seq_along(informacoes)) {
    informacao <- informacoes[i]
    url.completa <-
      paste0(base.url, informacao, names(informacao), '.csv')
    arquivo <- paste0('dados/', names(informacao), '.csv')
    
    if (file.exists(arquivo)) {
      consultar <- FALSE
      info.arquivo <- file.info(arquivo)
      modificacao.arquivo <- format(info.arquivo$mtime, '%Y-%m-%d')
      
      if (Sys.Date() > modificacao.arquivo) {
        consultar <- TRUE
        cat(
          epoxy::epoxy(
            '\n\n Precisa baixar novamente o arquivo de {names(informacao)}.\n\n'
          )
        )
      } else {
        cat(
          epoxy::epoxy(
            '\n\n Não precisa baixar novamente o arquivo de {names(informacao)}.\n\n'
          )
        )
      }
    } else {
      consultar <- FALSE
      cat(
        epoxy::epoxy(
          '\n\n Precisa baixar novamente o arquivo de {names(informacao)}.\n\n'
        )
      )
      download.file(url = url.completa,
                    destfile = arquivo,
                    mode = 'wb')
    }
    
    if (!consultar) {
      cat(epoxy::epoxy('\n Lendo o arquivo {names(informacao)}.\n\n'))
      # Ler o arquivo CSV e armazenar no dataframe
      df <- readr::read_csv2(arquivo)
      
    } else {
      arquivo.baixado <- paste0(arquivo, '.baixado')
      
      try({
        download.file(url = url.completa,
                      destfile = arquivo.baixado,
                      mode = 'wb')
        df.baixado <- read.csv2(arquivo.baixado)
        df.anterior <- read.csv2(arquivo)
        
        if (identical(df.baixado, df.anterior)) {
          atualizar <- FALSE
          cat(
            epoxy::epoxy(
              '\n O arquivo de {names(informacao)} baixado é igual ao anterior.\n\n'
            )
          )
          
        } else {
          atualizar <- TRUE
          file.rename(arquivo, paste0(arquivo, '.anterior'))
          file.rename(arquivo.baixado, arquivo)
          cat(
            epoxy::epoxy(
              '\n O arquivo de {names(informacao)} anterior foi substituído pelo baixado.\n\n'
            )
          )
        }
      })
    }
  }
  
}

preparar.tabela.temas <- function(tabela.temas) {
  df <- read.csv2('dados/temas.csv', sep = ';', header = TRUE, colClasses = 'character', fileEncoding = 'Windows-1252')
  
  df.filtrado <- dplyr::filter(df, tipoPrecedente == 'Tema')
  
  colunas.data <- c('dataPrimeiraAfetacao', 'dataJulgamento', 'dataPublicacaoAcordao', 'dataAudienciaPublica')
  colunas.fator <- c('situacao')
  
  df.filtrado[colunas.data] <- lapply(df.filtrado[colunas.data], function(x) {
    data.formatada <- format(as.Date(x, format = '%Y-%m-%d'), '%d/%m/%Y')
    return(data.formatada)
  })
  df.filtrado[colunas.fator] <- lapply(df.filtrado[colunas.fator], as.factor)
  
  tabela.temas <-
    df.filtrado[, c(
      'numeroPrecedente',
      'situacao',
      'questaoSubmetidaAJulgamento',
      'dataPrimeiraAfetacao',
      'teseFirmada',
      'dataPublicacaoAcordao'
    )]
  
  # Mapeamento de nomes de colunas
  mapeamento.colunas <- c(
    'numeroPrecedente' = 'Número',
    'situacao' = 'Situação',
    'questaoSubmetidaAJulgamento' = 'Questão',
    'dataPrimeiraAfetacao' = 'Afetação',
    'teseFirmada' = 'Tese',
    'dataPublicacaoAcordao' = 'Publicação'
  )
  
  colnames(tabela.temas) <- mapeamento.colunas[match(colnames(tabela.temas), names(mapeamento.colunas))]
  
  return(tabela.temas)
}

enviar.temas.html <- function(tabela.temas) {
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
    .dia = function(x) {format(x, '%d/%m/%Y')},
    .numero = function(x) {format(x, big.mark = '.', decimal.mark = ',')}
  )
  
  if (nrow(tabela.temas) == 0) {
    cat('Nenhum registro disponível para envio.\n')
    return()
  } else {
    cat(epoxy::epoxy('Há {.numero nrow(tabela.temas)} registros disponíveis para envio.\n'))
  }
  
  horario <- format(Sys.time(), format = '%Y%m%d-%H%M%S')
  arquivo.html <- epoxy::epoxy('saida/tabela-{horario}.html')
  
  tabela.temas.html <- DT::datatable(tabela.temas, rownames = F, class = 'cell-border stripe', filter = 'top', options = list(pageLength = 50))
  DT::saveWidget(tabela.temas.html, file = arquivo.html, selfcontained = TRUE)
  
  if (file.exists('dados/temas.csv')) {
    df.novo <- read.csv2('dados/temas.csv')
  } else {
    df.novo <- NULL
  }
  
  
  if (file.exists('dados/temas.csv.anterior')) {
    df.anterior <- read.csv2('dados/temas.csv.anterior')
  } else {
    df.anterior <- NULL
  }
  
  if (identical(df.novo, df.anterior)) {
    cat('Não houve mudança na relação de temas.\n')
    
    if (file.exists('dados/data.checagem.rds')) {
      data.checagem <- readRDS('dados/data.checagem.rds')
    } else {
      data.checagem <- Sys.Date()
      saveRDS(data.checagem, 'dados/data.checagem.rds')
    }
    
    
    periodo <- dplyr::case_when(
      identical(Sys.Date(), data.checagem) ~ 'Hoje',
      identical(Sys.Date() - 1, data.checagem) ~ 'Ontem',
      format(Sys.Date()-31, "%Y-%m") == format(data.checagem, "%Y-%m") ~ 'Desde mês passado',
      TRUE ~ paste0(epoxy::epoxy('Desde {.dia data.checagem} até {.dia Sys.Date()}'))
    )
    
    assunto.email <- paste0(epoxy::epoxy('{periodo}, não houve alteração na relação de temas do STJ.'))
    corpo.email <- paste0(epoxy::epoxy('Não houve alteração em nenhum dos {.numero nrow(tabela.temas)} temas do STJ já incluídos no portal.'))
    anexo.email <- NULL
  } else {
    cat('Houve mudança na relação de temas.\n')
    
    
    assunto.email <- paste0(epoxy::epoxy('Relação de temas do STJ atualizada até {.dia Sys.Date()}.'))
    anexo.email <- arquivo.html
    
    # Construir corpo do email
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
        'Há {nrow(df.novo)} temas do STJ a incluir na relação do portal.'
      ),
      '<br><br><br>',
      colunas,
      '<br><br><br>',
      mudancas,
      '<br><br><br>',
      linhas,
      '<br><br><br>',
      rvest::read_html(inclusoes),
      '<br><br><br><br><br><br>',
      # rvest::read_html(arquivo.html),
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
    send = TRUE,
    html = TRUE,
    attach.files = anexo.email
  )
  
  # Verifique se o email foi enviado com sucesso
  if (exists('email')) {
    cat(epoxy::epoxy('Email enviado com sucesso para {.and DESTINATARIO}.\n'))
  } else {
    cat(epoxy::epoxy('Falha ao enviar o email para {.and DESTINATARIO}.\n'))
  }
  
}


# atualizar.arquivos.STJ()

tabela.temas <- preparar.tabela.temas()

# DESTINATARIO <- c('armando.nahmias@tjrr.jus.br', 'debora.silva@tjrr.jus.br', 'rayandria.santiago@tjrr.jus.br')
DESTINATARIO <- 'armando.nahmias@tjrr.jus.br'

enviar.temas.html(tabela.temas)


# Testes
df.anterior <- head(df.anterior, -1)
