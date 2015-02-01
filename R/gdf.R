
writeGDF <- function(fileName, df, freq, last_names) {
  write("nodedef> name VARCHAR", fileName)
  write.table(last_names, fileName, append = TRUE, row.names=FALSE, col.names=FALSE)
  write("edgedef> node1 VARCHAR, node2 VARCHAR, Weight DOUBLE", fileName, append = TRUE)
  write.table(freq, fileName, append = TRUE, 
              row.names=FALSE, col.names=FALSE, sep = ",")
}

createGDF <- function(df, fileName) {
  #df[ ,c("paterno", "materno")] <- sortApellidos(df)
  
  df$paterno <- str_replace_all(df$paterno, "\\s","_")
  df$materno <- str_replace_all(df$materno, "\\s","_")
  last_names <- unique(c(as.character(df$paterno), as.character(df$materno)))
  
  
  
  freq <- df %>%
    group_by(paterno, materno) %>%
    summarise(Weight = n()) %>%
    arrange(desc(Weight))
  
  writeGDF(fileName, df, freq, last_names)
  return(TRUE)
}

createGDF(pan, file.path("gephi", "pan.gdf"))
createGDF(filter(prd, paterno != "" | materno != "."), file.path("gephi", "prd.gdf"))
createGDF(prd, file.path("gephi", "prd.gdf"))
freq <- pri %>%
  group_by(paterno, materno) %>%
  summarise(Weight = n()) %>%
  arrange(desc(Weight)) %>%
  filter(Weight > 2)
createGDF(semi_join(pri, freq), file.path("gephi", "pri10.gdf"))
createGDF(rbind(filter(pan, entidad == "YUCATAN"),
                filter(pri, entidad == "YUCATAN"),
                filter(prd, entidad == "YUCATAN")), 
          file.path("gephi", "pan-pri-prd-yucatan.gdf"))
createGDF(rbind(filter(pan, entidad == "QUINTANA ROO"),
                filter(pri, entidad == "QUINTANA ROO"),
                filter(prd, entidad == "QUINTANA ROO")), 
          file.path("gephi", "pan-pri-prd-qroo.gdf"))
createGDF(rbind(filter(pan, entidad == "CAMPECHE"),
                filter(pri, entidad == "CAMPECHE"),
                filter(prd, entidad == "CAMPECHE")),
          file.path("gephi", "pan-pri-prd-camp.gdf"))

createGDF(rbind(filter(pan, entidad == "CHIAPAS"),
                filter(pri, entidad == "CHIAPAS"),
                filter(prd, entidad == "CHIAPAS")), 
          file.path("gephi", "pan-pri-prd-chis.gdf"))

