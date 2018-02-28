library(ggmap)
cta = read.csv("/Users/Ajay/Downloads/Webservice-CTA-master/share/cta_L_stops.csv")
chicago <- get_map(location = 'chicago', zoom=10)
colors = c()
line = c()
reds = c()
blues = c()
greens = c()
pinks = c()
purples = c()
browns = c()
yellows = c()
oranges = c()

for (row in 1:nrow(cta)){
  if (!(grepl("&", cta$STATION_DESCRIPTIVE_NAME[row]))){
    if (cta$Red[row] == 1){
      colors = append(colors, "red")
    }
    else if (cta$Blue[row] == 1){
      colors = append(colors, "deepskyblue1")
    }
    else if (cta$Brn[row] == 1){
      colors = append(colors, "brown")
    }
    else if (cta$G[row] == 1){
      colors = append(colors, "forestgreen")
    }
    else if (cta$P[row] == 1 | cta$Pexp[row] == 1){
      colors = append(colors, "purple")
    }
    else if (cta$Y[row] == 1){
      colors = append(colors, "gold")
    }
    else if (cta$Org[row] == 1){
      colors = append(colors, "orange")
    }
    else{
      colors = append(colors, "pink")
    }
  }
  else{
    colors = append(colors, "black")
  }
  
  if (cta$Red[row] == 1){
    if (!(cta$PARENT_STOP_ID[row] %in% reds)){
      line = append(line, "Red")
      reds = append(reds, cta$PARENT_STOP_ID[row])
      #reds = append(reds, 1)
    }
  }
  if (cta$Blue[row] == 1){
    if (!(cta$PARENT_STOP_ID[row] %in% blues)){
      line = append(line, "Blue")
      blues = append(blues, cta$PARENT_STOP_ID[row])
    }
  }
  if (cta$Brn[row] == 1){
    if (!(cta$PARENT_STOP_ID[row] %in% browns)){
      line = append(line, "Brown")
      browns = append(browns, cta$PARENT_STOP_ID[row])
    }
  }
  if (cta$G[row] == 1){
    if (!(cta$PARENT_STOP_ID[row] %in% greens)){
      line = append(line, "Green")
      greens = append(greens, cta$PARENT_STOP_ID[row])
    }
  }
  if (cta$P[row] == 1 | cta$Pexp[row] == 1){
    if (!(cta$PARENT_STOP_ID[row] %in% purples)){
      line = append(line, "Purple")
      purples = append(purples, cta$PARENT_STOP_ID[row])
    }
  }
  if (cta$Y[row] == 1){
    if (!(cta$PARENT_STOP_ID[row] %in% yellows)){
      line = append(line, "Yellow")
      yellows = append(yellows, cta$PARENT_STOP_ID[row])
    }
  }
  if (cta$Pink[row] == 1){
    if (!(cta$PARENT_STOP_ID[row] %in% pinks)){
      line = append(line, "Pink")
      pinks = append(pinks, cta$PARENT_STOP_ID[row])
    }
  }
  if (cta$Org[row] == 1){
    if (!(cta$PARENT_STOP_ID[row] %in% oranges)){
      line = append(line, "Orange")
      oranges = append(oranges, cta$PARENT_STOP_ID[row])
    }
  }
}
cta$colors = colors
ggmap(chicago) + geom_point(data=cta, aes(x=LON, y = LAT), color = colors)
values = c("deepskyblue1", "brown", "forestgreen", "orange", "pink", "purple", "red", "gold")

ggplot() + geom_bar(aes(x = line), fill = values) + xlab("CTA Line") + ylab("Number of Stations")

cta$ADA[cta$ADA == 0] <- "Not ADA compliant"
cta$ADA[cta$ADA == 1] <- "ADA compliant"
adaColors = c("seagreen3", "slateblue3")
ggplot(cta) + geom_bar(aes(x = ADA), fill = adaColors) + xlab("ADA compliance") + ylab("Number of CTA Stations")
