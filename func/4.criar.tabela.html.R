criar.tabela.html <- function(df, arquivo.html) {
  
  
  # Crie a tabela HTML usando DT
  tabela.html <-
    DT::datatable(
      tabela.temas,
      rownames = F,
      class = 'cell-border stripe',
      filter = 'top',
      options = list(pageLength = 50,
                     autoWidth = TRUE,
                     columnDefs = list(list(width = '500px', targets = c(2,4))),
                     order = list(list(1, 'desc'), list(4, 'desc')),
                     select = list(style = 'multi', items = 'row', selected = which(df$Situação == 'Afetado'))),
      caption = 'Lista de Temas do STJ'
    ) |>
    DT::formatDate(columns = c(4, 6), method = 'toLocaleDateString') |>
    DT::formatStyle(columns = c(3, 5), textAlign = 'justify')
  
  DT::saveWidget(tabela.html, file = arquivo.html, selfcontained = T)
  
  return(tabela.html)
}
