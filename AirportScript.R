data.port <- read.csv('airports.dat',F)
data.line <- read.csv('routes.dat',F)

# 机场数据整理，除去重复的和没有编号的机场
airports <-data.port[data.port$V5!='',
                     c('V5','V4','V3')]
names(airports) <- c('name','country','city')
airports$city <- as.character(airports$city)
airports <- airports[!duplicated(airports$name),]

# 航线数据整理，只保留起飞和降落地点
goodlines <- (data.line[,'V3'] %in% airports$name) &
  (data.line[,'V5'] %in% airports$name)
airlines <- data.line[goodlines,c('V3','V5')]
names(airlines) <- c('from','to')
# 加载igraph包，生成网络图对象
library(igraph)
g <- graph.data.frame(airlines, vertices=airports,directed=F)
# 删除没有航线的机场
g<- g - V(g)[degree(g)==0]

# 合并航线，将同一航线的频次放入weight属性
is.multiple(g)
E(g)$weight <- count.multiple(g)
g1 <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE,
               edge.attr.comb = 'mean')
summary(g1)

# 从上面观察整个机场群共有3218个机场，16822条航线，来判断这些机场整体是否连通？

is.connected(g1)
# 存在一些不能连通的机场，观察有哪些相互断开的机场群？
clusters(g1)$csize
V(g1)[clusters(g1)$membership==2]$country
# 发现有一些机场属于西南太平洋上的岛国，去掉这些处于边缘状态的机场
g2 <- g1 - V(g1)[clusters(g1)$membership!=1]

# 观察哪一对机场之间航班频次最多，原来是香港和曼谷，它们之间的航班达到28次，果然是有基情。
E(g2)[which.max(E(g2)$weight)]
V(g2)['BKK']$city
V(g2)['HKG']$city

# 哪个机场的直航城市最多，法兰克福位于首位，它与237个机场间有直飞航班。从下面的分布图来看，大部分机场的直航城市并不多，只有少数机场特别突出，我们还可以列出直航城市最多的十大机场。
V(g2)[which.max(degree(g2))]$city
plot(degree.distribution(g2), log="xy")
V(g2)$city[order(degree(g2),decreasing=T)][1:10]

# Google使用的PageRank值代表了一个网站的重要性。PageRank根据网页之间的超链接来确定一个页面的等级。那么机场的重要性也可以从航线的多寡来确定。 根据pagerank计算出机场前十强，分别是"Chicago" "Denver" "Los Angeles" "Houston"  "London" "Frankfurt" "Paris"  "Beijing" "Singapore" "Bangkok" 

V(g2)$city[order(page.rank(g2)$vector,decreasing=T)][1:10]

# 观察两个机场之间是否连通直航，这里判断的是武汉到开罗，结果是否
are.connected(g2,'WUH','CAI')

# 那么应当如何转机呢，这实际上是寻找网络中的最短连线wuhan->cairo，给出的结果是先从武汉到东京，再到莫斯科，最后到开罗。
# V(g2)[get.shortest.paths(g2,'WUH','CAI')[[1]]]$city

# 还可以使用网络分析方法中的社群探测，发现网络中有三个主要的社群，观察社群特征，它们分别对应了美洲，欧洲与中东地区，亚太地区
commu <-  fastgreedy.community(g2)
commu$membership
sizes(commu)
V(g2)[commu$membership==1]$country
V(g2)[commu$membership==2]$country
V(g2)[commu$membership==3]$country

# 绘制网络图，由于机场数量太多，所以略去了连线，只留下圆点来表示机场，圆的大小表示机场的重要性，不同的颜色表示不同的社群
rank.max <- max(page.rank(g2)$vector)
rank.min <- min(page.rank(g2)$vector)
size <- 20*(page.rank(g2)$vector -rank.min)/(rank.max-rank.min)+1
V(g2)$size <- size
V(g2)$color <- 'grey'
V(g2)$frame.color <- 'black'
V(g2)[commu$membership==1]$color <- 'red4'
V(g2)[commu$membership==2]$color <- 'darkseagreen3'
V(g2)[commu$membership==3]$color <- 'darkslategray3'
V(g2)$label <- NA
E(g2)$color <- 'white'
plot(g2, layout=layout.sphere)

