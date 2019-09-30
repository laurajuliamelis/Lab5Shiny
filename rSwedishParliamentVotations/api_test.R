library(xml2)

path <- "http://data.riksdagen.se/voteringlista/?"

# rm = year span ex. 2018/19
# //bet = "betäckning" -> designation
# //punkt = "förslagspunkt" -> proposal point
# parti = party e.g. C for Centerpartiet
# //valkrets = constituency
# rost = vote (Ja, Nej, Avstå, Frånvarande)
# //iid = some personal id
# sz = number of rows to request
# utformat = format of the response

request <- paste0(path, "rm=2018/19", "&sz=5", "&utformat=xml")

#path <- "http://data.riksdagen.se/voteringlista/?rm=2018%2F19&sz=5&utformat=xml"
response <- read_xml(request)

df <- data.frame()
i <- 1
for(child in xml_children(response)){
  values <- vector()
  for(subchild in xml_children(child)){
    values <- append(values, xml_text(subchild))
  }
  df[i,1:length(values)] <- values
  i <- i+1
}


