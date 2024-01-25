arquivos.fonte <- file.path('func', '2.preparar.tabela.temas.R')
source(arquivos.fonte)

df <- preparar.tabela.temas()

# Filter rows where 'Situação' is 'Afetado'
selected_rows <- which(df$Situação == 'Afetado')

DT::datatable(
  df,
  rownames = FALSE,
  class = 'cell-border stripe',
  filter = 'top',
  options = list(
    pageLength = 50,
    autoWidth = TRUE,
    columnDefs = list(list(
      width = '500px', targets = c(2, 4)
    )),
    order = list(list(1, 'desc')),
    selection = list(
      mode = 'multiple',
      target = 'row',
      selected = c(1)
    )
  ),
  caption = 'teste selection'
) |>
  DT::formatDate(columns = c(4, 6), method = 'toLocaleDateString') |>
  DT::formatStyle(columns = c(3, 5), textAlign = 'justify')

