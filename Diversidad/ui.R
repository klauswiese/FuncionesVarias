shinyUI(fluidPage(
  fileInput('datafile', 'Seleccione Archivo CSV',
            accept=c('csv', 'comma-separated-values','.csv')),
  tableOutput('Estimadores'),
  tableOutput('Diversidad'),
  plotOutput('Acumulacion'),
  plotOutput('Dshannon'),
  plotOutput('Dsimpson'),
  plotOutput('Dinvsimpson'),
  plotOutput('Dberger'),
  plotOutput('Dlogalpha'),
  plotOutput('Djevenness')
))
