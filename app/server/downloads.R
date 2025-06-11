output$download = downloadHandler(
  filename = function() {
    paste("data", "zip", sep=".")
  },
  content = function(fname) {
    zip(zipfile=fname, 
        files=c('Palestinian.Violence.Covariates_new.csv','Israeli.Violence.Covariates_new.csv'))
  },
  contentType = "application/zip"
)

output$download_1 = downloadHandler(
  filename = function() {
    paste("codebook", "zip", sep=".")
  },
  content = function(fname) {
    zip(zipfile=fname, 
        files=c('Codebook.pdf'))
  },
  contentType = "application/zip"
)