preparar.tabela.temas <- function() {
  df <- read.csv2('dados/temas.csv', sep = ';', header = T, colClasses = 'character', fileEncoding = 'Windows-1252')
  
  df <- tibble::tibble(df)
  
  df.filtrado <- dplyr::filter(df, tipoPrecedente == 'Tema')
  
  
  # Lista de colunas que precisam ser convertidas para data
  colunas.data <- c('dataPrimeiraAfetacao', 'dataJulgamento', 'dataPublicacaoAcordao', 'dataAudienciaPublica')
  colunas.fator <- c('tipoPrecedente', 'situacao', 'orgaoJulgador')
  colunas.logico <- c('audienciaPublica')
  
  # Converter as colunas para o tipo de data
  df.filtrado[colunas.data] <- lapply(df.filtrado[colunas.data], function(x) {
    data.formatada <- as.Date(x, format = '%Y-%m-%d')
    return(data.formatada)
  })
  
  df.filtrado[colunas.data] <- lapply(df.filtrado[colunas.data], as.Date)
  
  df.filtrado[colunas.fator] <- lapply(df.filtrado[colunas.fator], as.factor)
  
  df.filtrado[colunas.logico] <- lapply(df.filtrado[colunas.logico], as.logical.factor)
  
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
