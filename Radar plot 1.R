library(plotly)
library(dplyr)
#Data Input
setwd("C://Users//jeff//Desktop//Study//Results")

BB = "<b>Normal Dist.
ρ: 0.1
(One Peak)</b>"
BB.data = read.csv("GSA tools power simulation multivariate normal distribution(rho=0.1 and 0.9).csv",
                   header = TRUE)[3,-c(1, 3)]

CC = "<b>Normal Dist. 
ρ: 0.9
(One Peak)</b>"
CC.data = read.csv("GSA tools power simulation multivariate normal distribution(rho=0.1 and 0.9).csv",
                   header = TRUE)[10,-c(1, 3)]

DD = "<b>t-Dist. 
ρ: 0.9
(One Peak)</b>"
DD.data = read.csv("GSA tools power simulation(multivariate t).csv",
                   header = TRUE)[3,-c(1, 3)]

EE = "<b>Mix of Normal
Same Mean
ρ: 0.1, 0.9
(One Peak)</b>"
EE.data = read.csv("GSA tools power simulation(MON with same Mean).csv",
                   header = TRUE)[3,-c(1, 3)]

FF = "<b>Mix of Normal 
Mean: 0, 1
ρ: 0.1, 0.9
(Two Peaks)</b>"
FF.data = read.csv("GSA tools power simulation(MON with Different Mean(0, 1)).csv",
                   header = TRUE)[10 ,-c(1, 3)]

AA = "<b>Mix of Normal
Mean: 0, 2
ρ: 0.1, 0.9
(Two Peaks)</b>"
AA.data = read.csv("GSA tools power simulation(MON with Different Mean(0, 2)).csv",
                   header = TRUE)[10 ,-c(1, 3)]

data = rbind(AA.data, BB.data, CC.data, DD.data, EE.data, FF.data)
colnames(data) = c("<b>Hotelling's T<sup>2</sup></b>",
                   "<b>Global Test</b>",
                   "<b>GlobalANCOVA</b>",
                   "<b>Energy Test(N-statistic)</b>",
                   "<b>GSEA(Category)</b>")
data = as.data.frame(data)
########################################################################################
#Data Input
#可調參數 An Data Example
'data = data.frame(
  "Rich" = c(0.109, 0.26, 0.85, 0.12, 0.95, 0.3),
  "Andy" = c(0.1, 0.3, 0.8, 0.9, 0.65, 0.8),
  "Aimee" = c(0.7, 0.4, 0.8, 0.7, 0.65, 1),
  "ρ<sup>2</sup>" = c(0.1, 0.8, 0.8, 0.7, 0.65, 0.9),
  "μ<sub>3</sub>" = c(0.9, 0.9, 0.1, 0.7, 0.8, 0.9)
)'
#radar plot 每個individual要填充的色彩
fill.colors = c("rgba(135, 97, 88, 0.35)", "rgba(217, 154, 36, 0.35)",
               "rgba(59, 163, 59, 0.35)", "rgba(249, 133, 32, 0.35)",
               "rgba(93, 148, 186, 0.35)")
#radar plot 每個individual要畫邊界所使用的色彩
border.colors = c("rgba(135, 97, 88, 1)", "rgba(217, 154, 36, 1)",
               "rgba(59, 163, 59, 1)", "rgba(249, 133, 32, 1)",
               "rgba(93, 148, 186, 1)")
#radar plot 每個individual要畫邊界(點)所使用的符號(symbols)
dot.symbols = c("circle", "square", "diamond", "triangle-up", "star")
#radar plot 每個individual要畫邊界(點)所使用的大小
dot.size = 13








#Radar Chart Program Start
#data polar coordinate transformation
#program
getPolarCoord <- function(r, matrix = F, na = F){
  # Get starting angle and angle increments
  theta <- 0
  dtheta <- 360 / length(r)
  dtheta <- (pi / 180) * dtheta  # in radians
  # Get polar coordinates
  x <- c()
  y <- c()
  for(i in 1:length(r)){
    x <- c(x, r[i] * cos(theta))
    y <- c(y, r[i] * sin(theta))
    theta <- theta + dtheta
  }
  x[length(x) + 1] <- x[1]
  y[length(y) + 1] <- y[1]
  if(na == T){
    x[length(x) + 1] <- NA
    y[length(y) + 1] <- NA
  }
  if(matrix == T){
    return(cbind(x, y))
  }else{
    return(list(x = x, 
                y = y))
  }
}
#Transform data
draw.data = c()
individual = c()
for(i in c(1:ncol(data))) {
  temp = data[,i]
  for(j in c(1:(nrow(data)+1))){
    j = j%%nrow(data)
    if(j == 0){j = 6}
    draw.data = rbind(draw.data, c(sapply(getPolarCoord(rep(temp[j], nrow(data))), '[[', j), 
                                   "text" = temp[j]))
    individual = rbind(individual, names(data)[i])
  }
}
draw.data = as.data.frame(draw.data)
draw.data[,4] = as.factor(individual)
names(draw.data)[4] = "individual"
##Copypaste start
p = plot_ly()
for(i in 1: ncol(data)){#畫各個individual的radar圖
  p = add_trace(p,
                x = draw.data[draw.data$individual == names(data)[i],]$x, 
                y = draw.data[draw.data$individual == names(data)[i],]$y, 
                mode = "lines+markers",
                fill = "toself",
                fillcolor = fill.colors[i],
                marker = list(symbol = dot.symbols[i], size = dot.size, color = border.colors[i]),
                line = list(smoothing = 0.5, shape = "spline", color = border.colors[i]),
                hoverinfo = "name+text",
                name = names(data)[i],
                text = draw.data[draw.data$individual == names(data)[i],]$text)
}
##Copypaste end
print(p)

# Plot
# Add grids 繪製圓圈，制定要幾圈(似一般plot中的y軸的label、畫圓形邊界)
#可調參數
#注意: getPolarCoord(rep(圓圈半徑, 在一個圓周上的打點個數))
grid <- rbind(getPolarCoord(rep(0.10, 80), matrix = T, na = T),
              getPolarCoord(rep(0.20, 170), matrix = T, na = T),
              getPolarCoord(rep(0.30, 250), matrix = T, na = T),
              getPolarCoord(rep(0.40, 320), matrix = T, na = T),
              getPolarCoord(rep(0.50, 400), matrix = T, na = T),
              getPolarCoord(rep(0.60, 490), matrix = T, na = T),
              getPolarCoord(rep(0.70, 590), matrix = T, na = T),
              getPolarCoord(rep(0.80, 700), matrix = T, na = T),
              getPolarCoord(rep(0.90, 790), matrix = T, na = T),
              getPolarCoord(rep(1.00, 900), matrix = T, na = T))


#program
grid <- as.data.frame(grid)
##Copypaste start
plot_ly(x= grid$x, y = grid$y, mode = "lines",
        line = list(color = rgb(0.6, 0.6, 0.6), dash = "4px", width = 1),
        showlegend = F,
        hoverinfo = "none")
##Copypaste end


#標出要強調的圓圈刻度
#可調參數
grid.marknumber = 2
grid.mark = c(0.5, 0.8)
grid.mark.color = c(rgb(212/255, 54/255, 55/255, 0.5), rgb(0.4, 0.4, 0.4, 0.5))
grid.mark.width = c(2.5, 2.5)
grid.mark.legend = c("<b>power: 0.5</b>", "<b>power: 0.8</b>")
show.grid.mark.legend = c(TRUE, TRUE)

#program
grid.marks = list()
for(i in 1:grid.marknumber){
  grid.marks[[i]] = as.data.frame(getPolarCoord(rep(grid.mark[i], 900), matrix = T, na = T))
}
##Copypaste start
p = plot_ly()
for(i in 1:grid.marknumber){
  p = add_trace(p, x= grid.marks[[i]]$x, y = grid.marks[[i]]$y, mode = "lines",
                line = list(color = grid.mark.color[i], dash = "solid", 
                            width = grid.mark.width[i]), 
                showlegend = show.grid.mark.legend[i],
                name = grid.mark.legend[i],
                hoverinfo = "none")
}
print(p)
##Copypaste end


#制定軸線(每一個Y軸)的起始半徑及軸的個數(幾邊形) (畫各個垂直圓形邊界的軸線)
#可調參數
inner = getPolarCoord(rep(0.1, nrow(data)))   #各個軸的起始位置距離圓心離多少(半徑)
outer = getPolarCoord(rep(1.025, nrow(data)))   #各個軸的終點位置距離圓心離多少(半徑)


#program
x = t(cbind(inner$x, outer$x))
y = t(cbind(inner$y, outer$y))
x <- as.numeric(apply(x, 2, function(vec){
  return(c(vec, NA))
}))
y <- as.numeric(apply(y, 2, function(vec){
  return(c(vec, NA))
}))
linegrid <- data.frame(x = x, y = y)
##Copypaste start
plot_ly(x = linegrid$x, y = linegrid$y, mode = "lines",
        line = list(color = "#57788e", dash = "4px", width = 1),
        showlegend = F,
        hoverinfo = "none")
##Copypaste end

#繪製各個軸的名稱以及擺放位置，注意加一個空格當最後一個軸的名稱(因為會跟第一個軸重疊)
#可調參數
axis.name = c(AA,
              BB,
              CC,
              DD,
              EE,
              FF,"")  #各個軸的名稱(e.g.: 情境名稱，科目名稱)
axis.name.radius = 1.18  #軸名稱位置距離圓心離多少(半徑)
axis.name.size = 12 #軸名稱字體大小

#program
axis.labels <- paste0("", axis.name, "")
axis.pos = getPolarCoord(rep(axis.name.radius, nrow(data)))
##Copypaste start
plot_ly(x = axis.pos$x, y = axis.pos$y, 
        mode = "text", text = axis.labels,
        showlegend = F,
        hoverinfo = "none",
        textfont = list(family = "serif", size = axis.name.size, color = "#808080"))
##Copypaste end

#標出每個圓圈所代表的刻度
#可調參數
Y.label = seq(0.1, 1.1, 0.1) #要標上去的刻度 要預留一格標示連續變量的意涵(e.g."power, score") 所以才會是到1.1
Y.label.mean = "    <b>Power</b>" #連續變量的意涵(e.g."power, score")
Y.label.pos = 4    #選擇刻度要放的極座標角度(1~36) 每增加一個數值代表跳了10度
Y.label.margin = 0.04    #放的位置距離每個標線多少距離


#program
Y.labels = c(Y.label, "")
Y.labels = paste0("", Y.labels, "")
Y.labels[length(Y.label)] = Y.label.mean
Y.label.position = c()
for (i in Y.label) {
  Y.label.position = rbind(Y.label.position, round(sapply(getPolarCoord(rep(i-Y.label.margin, 36)), 
                                                          '[[', Y.label.pos), digits = 3))
}
Y.label.position = rbind(Y.label.position, NA)
##Copypaste start
plot_ly(x = Y.label.position[,"x"], y = Y.label.position[,"y"], 
        mode = "text", text = Y.labels,
        showlegend = F,
        hoverinfo = "none",
        textfont = list(family = "serif", size = 12, color = "#808080"))
##Copypaste end

# Add a gray circle#畫圓圈內的灰圓(仿照ggplot2的效果)
#可調參數
biggraycircle = getPolarCoord(rep(0.98, 200)) #getPolarCoord(rep(灰圓半徑, 在一個圓周上的打點個數))

#program
##Copypaste start
plot_ly(x = biggraycircle$x, y = biggraycircle$y,
        fill = "toself",
        fillcolor = "rgba(200, 200, 200, 0.3)",
        line = list(color = "transparent"),
        mode = "lines",
        hoverinfo = "none",
        showlegend = F)
##Copypaste end











#PLOT PROGRAM START
p = plot_ly() %>% #畫圓形邊界
  add_trace(x= grid$x, y = grid$y, mode = "lines",
            line = list(color = rgb(0.6, 0.6, 0.6), dash = "4px", width = 1),
            showlegend = F,
            hoverinfo = "none") %>% #畫各個垂直圓形邊界的軸線
  add_trace(x = linegrid$x, y = linegrid$y, mode = "lines",
            line = list(color = "#57788e", dash = "4px", width = 1),
            showlegend = F,
            hoverinfo = "none") %>% #畫圓圈內的灰圓(仿照ggplot2的效果)
  add_trace(p, x = biggraycircle$x, y = biggraycircle$y, 
            fill = "toself",
            fillcolor = "rgba(200, 200, 200, 0.3)",
            line = list(color = "transparent"),
            mode = "lines",
            hoverinfo = "none",
            showlegend = F) %>% #標出各個軸線的名稱(e.g.:情境，科目)
  add_trace(x = axis.pos$x, y = axis.pos$y, 
            mode = "text", text = axis.labels,
            showlegend = F,
            hoverinfo = "none",
            textfont = list(family = "MS Reference Sans Serif", size = axis.name.size, color = "#808080")) %>% #標出每個圓圈所代表的刻度
  add_trace(x = Y.label.position[,"x"], y = Y.label.position[,"y"], 
            mode = "text", text = Y.labels,
            showlegend = F,
            hoverinfo = "none",
            textfont = list(family = "MS Reference Sans Serif", size = 12, color = "#808080"))
#校正圓變橢圓因此在四邊各加了一個透明點
p = add_trace(p, x = c(1.3, -1.3) , y = c(0, 0), 
              mode = "markers", 
              marker = list(color = "rgba(242.25, 242.25, 242.25, 0)"), 
              showlegend = F, 
              hoverinfo = "none")

p = add_trace(p, x = 0, y = -1.015, 
              mode = "markers", 
              marker = list(color = "rgba(242.25, 242.25, 242.25, 0)"), 
              showlegend = F, 
              hoverinfo = "none") 
#標出要強調的圓圈刻度
for(i in 1:grid.marknumber){
  p = add_trace(p, x= grid.marks[[i]]$x, y = grid.marks[[i]]$y, mode = "lines",
                line = list(color = grid.mark.color[i], dash = "solid", 
                            width = grid.mark.width[i]), 
                showlegend = show.grid.mark.legend[i],
                name = grid.mark.legend[i],
                hoverinfo = "none")
}
#畫各個individual的radar圖
for(i in 1: ncol(data)){
  p = add_trace(p,
                x = draw.data[draw.data$individual == names(data)[i],]$x, 
                y = draw.data[draw.data$individual == names(data)[i],]$y, 
                mode = "lines+markers",
                fill = "toself",
                fillcolor = fill.colors[i],
                marker = list(symbol = dot.symbols[i], size = dot.size, color = border.colors[i]),
                line = list(smoothing = 0.5, shape = "spline", color = border.colors[i]),
                hoverinfo = "name+text",
                name = names(data)[i],
                text = draw.data[draw.data$individual == names(data)[i],]$text)
}
#決定整個最大圖層紙的背景底色, 圖表size 以及X軸和Y軸的形式
LL = layout(p, xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
                            domain = c(0, 0.6)), #radar chart 圖框橫向邊界
            yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F,
                         domain = c(0, 0.93)), #radar chart 圖框直向邊界
            font = list(family = "serif", size = 15),
            legend = list(x = 0.6, y = 0.93, bgcolor = "transparent"),
            plot_bgcolor = toRGB(rgb(0.95, 0.95, 0.95)), #radar chart 圖框內的圖層底色
            paper_bgcolor = toRGB(rgb(0.95, 0.95, 0.95))) #最大圖層紙的顏色
# Add titles, description etc
#可調參數
final = layout(LL, annotations = list(
  list(xref = "paper", yref = "paper", 
       xanchor = "left", yanchor = "top",
       x = 0.02, y = 1.04, 
       showarrow = F, 
       text = "<b>The Statistical Performance of Five GSA Tools on Six Different Scenarios</b>",
       font = list(family = "MS Reference Sans Serif",
                   size = 27, 
                   color = "#4080bf")),
  
  list(xref = "paper", yref = "paper", 
       xanchor = "left", yanchor = "top",
       x = 0.02, y = 0.98, 
       showarrow = F, 
       align = "left",
       text = '<em>Remark: The performance result is based on a difference of 0.5 on the mean of the six distributions between case and control group on every dimension.</em>',
       font = list(family = "MS Reference Sans Serif",
                   size = 13, 
                   color = "#679bcb")),
  #字首縮排(&nbsp;  代表空白鍵)
  list(xref = "paper", yref = "paper", 
       xanchor = "left", yanchor = "top",
       x = 0.65, y = 0.53, 
       showarrow = F, 
       align = "left",
       text = '● The performance of Hotelling\'s T<sup>2</sup> is the
&nbsp;&nbsp; worst among all the five GSA Tools in all                     
&nbsp;&nbsp; scenarios.
● The performance of energy test outperforms
&nbsp;&nbsp; than all the other tests in the multivariate                    
&nbsp;&nbsp; t-distribution scenario.
● In the multivariate normal distribution 
&nbsp;&nbsp; scenario, the correlation among  variables
&nbsp;&nbsp; significantly influences the performance of 
&nbsp;&nbsp; every tests.
● The more different the scenario is from a  
&nbsp;&nbsp; multivariate standard normal distribution, 
&nbsp;&nbsp; the worse the performance of GSA tools is.',
       font = list(family = "arial",
                   size = 18))
),

#次大圖層紙的範圍以及顏色
shapes = list(
  list(
    xref = "paper", yref = "paper",
    x0 = 0, x1 = 0.98,
    y0 = 0, y1 = 1,
    type = "rect",
    layer = "above",
    fillcolor = "rgba(242, 242, 242, 0.1)",
    line = list(color = "transparent"))                  
))

print(final)


  


