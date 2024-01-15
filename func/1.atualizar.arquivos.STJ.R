atualizar.arquivos.STJ <- function() {
  base.url <-
    'https://dadosabertos.web.stj.jus.br/dataset/4238da2f-c07b-4c1a-b345-4402accacdcf/resource/'
  
  informacoes <- list(
    'dicionario-temas' = 'd5e50514-6dba-4f1e-8557-94f135eae03b/download/',
    'temas' = 'df29da13-7d6b-41ba-ad96-cd1a5bbd191c/download/',
    'dicionario-processos' = '162e58f0-01c1-4d91-94a4-664b4de81e79/download/',
    'processos' = '7ed21202-0049-4fcb-aa7c-48d810d3c499/download/'
  )
  
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
    
    if (consultar) {
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
