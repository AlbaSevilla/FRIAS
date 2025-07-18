FreshwaterMacrophytes_Download_and_Depurate <- function(){
  ######################################################
  ######### PARA DESCARGAR #############################
  ######################################################
  url <- "https://ars.els-cdn.com/content/image/1-s2.0-S025462992100363X-mmc1.docx"
  destfile <- "InputFiles/Step0_OriginalDatabase_FreshwaterMacrophytes.docx"
  if (!dir.exists("InputFiles")) {
    dir.create("InputFiles")
  }
  download.file(url, destfile, mode = "wb")


  # PASAR A PDF
  library(locatexec)
  library(doconv)

  input <- "InputFiles/Step0_OriginalDatabase_FreshwaterMacrophytes.docx"
  ext <- tools::file_ext(input)
  print(ext)
  file.exists(input)  # Should return TRUE
  doconv::docx2pdf(input)
  doconv::word_available()

}







library(officer)
library(doconv)

# Crear un archivo DOCX de prueba
doc <- read_docx()
doc <- body_add_par(doc, "Este es un documento de prueba.")
print(doc, target = "test_doc.docx")

# Intentar convertirlo a PDF
doconv::docx2pdf("test_doc.docx")








library(officer)
library(dplyr)
# create empty Word file
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_par("This is the first paragraph")
sample_doc <- sample_doc %>% body_add_par("This is the second paragraph")
sample_doc <- sample_doc %>% body_add_par("This is the third paragraph")
# create sample data frame
df <- data.frame(a = 1:10, b = 11:20, c= 21:30)

# add table containing the data frame's contents
sample_doc <- sample_doc %>% body_add_table(df, style = "table_template")
set.seed(0)

# create a temp file
src <- tempfile(fileext = ".png")

# create PNG object
png(filename = src, width = 4, height = 4, units = 'in', res = 400)

# create plot
plot(sample(100, 10))

# save PNG file
dev.off()

# add PNG image to Word document
sample_doc <- sample_doc %>% body_add_img(src = src, width = 4, height = 4, style = "centered")


print(sample_doc, target = "sample_file.docx")



sample_data <- read_docx("Inputfiles/Step0_OriginalDatabase_FreshwaterMacrophytes.docx")

content <- docx_summary(sample_data)
content2 <- content %>% filter(content_type == "table cell")
content3 <- content2$text
head(content3)


name_backbone_checklist("Hygrophila corymbosa")$canonicalName
