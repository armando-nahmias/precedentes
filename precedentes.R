
df <- read.csv2('dados/temas.csv', sep = ';', header = T, colClasses = 'character', fileEncoding = 'Windows-1252')

df <- tibble::tibble(df)

# Lista de colunas que precisam ser convertidas para data
colunas.data <- c("dataPrimeiraAfetacao", "dataJulgamento", "dataPublicacaoAcordao", "dataAudienciaPublica")

# Loop para converter as colunas para o tipo de data
for (coluna in colunas.data) {
  df[[coluna]] <- as.Date(df[[coluna]], format = "%Y-%m-%d")
}

tabela.temas <- df[, c('numeroPrecedente', 'questaoSubmetidaAJulgamento', 'situacao', 'dataPublicacaoAcordao', 'teseFirmada')]

