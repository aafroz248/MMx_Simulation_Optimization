
require(rmarkdown)

# env = new.env(hash = TRUE, parent = parent.frame())
# names(mmxRDS) %>% map(~{
#   assign(.,mmxRDS[[.]],envir=env)
# })
mmxRDS<-readRDS("./mmxRDS.RDS")
fileName<-"MMx_Simulation_and_Optimization_Report2020_11_18_03_17_41"
file<-'Source/report.Rmd'
rmarkdown::render(file
                  ,params = mmxRDS,
                  switch(mmxRDS$format,
                         PDF = pdf_document(),
                         PrettyHTML = prettydoc::html_pretty(css = "../www/Styles/pretty_styles.css"),
                         HTML = html_document(css = "../www/Styles/html_styles.css" ,toc= T,toc_float= T),
                         
                         MSWord = word_document(toc = T)),
                  output_dir = "Downloads/" , 
                  output_file = paste(fileName,'.html',sep='')
)
