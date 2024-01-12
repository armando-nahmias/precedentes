

baixar.info.STJ <- function() {
  
  # Definir os URLs base e os caminhos locais para salvar os arquivos
  base.url <- 'https://dadosabertos.web.stj.jus.br/dataset/4238da2f-c07b-4c1a-b345-4402accacdcf/resource/'
  
  recursos <- c(
    'd5e50514-6dba-4f1e-8557-94f135eae03b/download/',
    'df29da13-7d6b-41ba-ad96-cd1a5bbd191c/download/',
    '162e58f0-01c1-4d91-94a4-664b4de81e79/download/',
    '7ed21202-0049-4fcb-aa7c-48d810d3c499/download/'
  )
  
  informacoes <- c(
    'dicionario-temas.csv',
    'temas.csv',
    'dicionario-processos.csv',
    'processos.csv'
  )
  
  lista.df <- list()
  
  # Loop para baixar as informacoess
  for (i in seq_along(informacoes)) {
    informacao <- informacoes[i]
    url.completa <- paste0(base.url, recursos[i], informacao)
    arquivo <- paste0('dados/', informacao)
    
    if (file.exists(arquivo)) {
      consultar <- FALSE
      info.arquivo <- file.info(arquivo)
      modificacao.arquivo <- format(info.arquivo$mtime, '%Y-%m-%d')
      if (Sys.Date() > modificacao.arquivo) {
        consultar <- TRUE
        cat(epoxy::epoxy('Precisa atualizar o arquivo {informacao}.\n\n\n'))
      } else {
        cat(epoxy::epoxy('Não precisa atualizar o arquivo {informacao}.\n\n\n'))
      }
    } else {
      consultar <- TRUE
      cat(epoxy::epoxy('Precisa atualizar o arquivo {informacao}.\n\n\n'))
    }
    
    if (consultar) {
      
      arquivo.novo <- paste0(arquivo, '.novo')
      
      try({
        download.file(url.completa, destfile = arquivo.novo, mode = 'wb')
        # Ler o arquivo CSV e armazenar no dataframe
        df.novo <- readr::read_csv2(arquivo.novo)
        df.anterior <- readr::read_csv2(arquivo)
        if (identical(df.novo, df.anterior)) {
          atualizar <<- FALSE
          cat(epoxy::epoxy('Arquivo {informacao} baixado é igual ao anterior.\n\n\n'))
          df <- df.anterior
        } else {
          atualizar <<- TRUE
          diffdf::diffdf(df.novo, df.anterior)
          file.rename(arquivo, paste0(arquivo, '.antigo'))
          file.rename(arquivo.novo, arquivo)
          cat(epoxy::epoxy('Arquivo {informacao} anterior substituído pelo baixado.\n\n\n'))
          df <- df.novo
        }
      })
      
    } else {
      cat(epoxy::epoxy('Arquivo {informacao} atualizado.\n\n\n'))
      # Ler o arquivo CSV e armazenar no dataframe
      df <- readr::read_csv2(arquivo)
    }
    
    
    # Adicionar o dataframe à lista
    lista.df[[informacao]] <- df
  }
  
  
  # Gravar em disco o resultado da consulta
  saveRDS(lista.df, file = 'dados/dados-completos.rds')
  
}


preparar.tabela.temas <- function() {
  df <- read.csv2('dados/temas.csv', sep = ';', header = T, colClasses = 'character', fileEncoding = 'Windows-1252')
  
  df <- tibble::tibble(df)
  
  df.filtrado <- dplyr::filter(df, tipoPrecedente == 'Tema')
  
  
  # Lista de colunas que precisam ser convertidas para data
  colunas.data <- c('dataPrimeiraAfetacao', 'dataJulgamento', 'dataPublicacaoAcordao', 'dataAudienciaPublica')
  colunas.fator <- c('situacao')
  
  # Converter as colunas para o tipo de data
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
    'teseFirmada' = 'Tese',
    'dataPrimeiraAfetacao' = 'Afetação',
    'dataPublicacaoAcordao' = 'Publicação'
  )
  
  # Renomeie as colunas no DataFrame usando o mapeamento
  colnames(tabela.temas) <- mapeamento.colunas[match(colnames(tabela.temas), names(mapeamento.colunas))]
  
  
  
  return(tabela.temas)
}


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
  
  # Crie a tabela HTML usando DT
  tabela.html <- DT::datatable(df, rownames = F, class = 'cell-border stripe', filter = 'top', options = list(pageLength = 50))
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


# arquivos.fonte <- file.path('func', c('baixar_arquivos_STJ.R', 'preparar.tabela.temas.R', 'enviar.tabela.R'))
# 
# lapply(arquivos.fonte, source)

# DESTINATARIO <- c('armando.nahmias@tjrr.jus.br', 'debora.silva@tjrr.jus.br', 'rayandria.santiago@tjrr.jus.br')
DESTINATARIO <- 'armando.nahmias@tjrr.jus.br'
atualizar <- FALSE


baixar.info.STJ()

df <- preparar.tabela.temas()

enviar.tabela.html(df)



DT::datatable(
  tabela.temas,
    rownames = F,
    class = 'cell-border stripe',
    filter = 'top',
    options = list(pageLength = 50,
                   order = list(4, 'asc')),
  selection = list(mode = 'multiple', selected = NULL, target = 'row', selectable = NULL)
  )
