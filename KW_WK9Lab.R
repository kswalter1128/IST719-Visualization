#Author Kyle Walter
# Purpose: Week 9 Lab
# RGL 3D visualization

require(igraph)
require(rgl)
setwd("J:/My Drive/Graduate School/IST719 Vizualization/")
load(file = "Files for Class from 2U/ist719networkobject.rda")

g

coords <- layout_with_kk(g, dim = 3)
rglplot(g, layout=coords)

l <- layout_with_kk(g)
V(g)$x <- coords[,1]
V(g)$y <- coords[,2]
V(g)$z <- V(g)$bet

rglplot(g)

V(g)$label <- ""
V(g)$x <- coords[,1]
V(g)$y <- coords[,2]
V(g)$z <- coords[,3]

rglplot(g)
par3d(windowRect = c(100,100,640, 640))
rgl.bringtotop()
rgl.bg(color = 'black')
rgl.viewpoint(0,20)

E(g)$color <-  'yellow'
E(g)$width <-  .25
V(g)$label <- ''
rglplot(g)

#9.1.3
require(stringr)
require(animation)
require(plotrix)

rglplot(g)
rgl.snapshot(filename = "Week 9/networkviz.png")


rglplot(g)
par3d(windowRect = c(100,100,500,500))
rgl.bringtotop()
rgl.bg(color = 'black')
rgl.viewpoint(0,0, zoom = .7)
max.loops <- 600
my.angle <- rescale(1:max.loops, c(-90,90))
for(i in 1:max.loops){
  rgl.viewpoint(theta = -my.angle[i], phi = my.angle[i]*.7,
                zoom = .75-1/(max.loops*1.7))
  #Theta is the angle spinning around Y
  #Phi is the angle spinning around X
  snapshotFname <- paste0("Week 9/out/","Network",
                         str_pad(i, width = 4, side = 'left',pad = '0'),".png")
  rgl.snapshot(filename = snapshotFname)
}

ani.options(interval = .1)
imgs <- list.files("Week 9/out/")

saveVideo({
  for(img in imgs){
    im <- magick::image_read(paste0("Week 9/out/", img))
    print(im)
  }
}, video.name = paste0("Week 9/","classnetwork.mp4"), other.opts = '-vf format = yuv420p')

#9.2.1

require(scatterplot3d)

n <- 1000
x <- rnorm(n)
y <- ((2*x)^2)/10 + rnorm(n, mean = 0, sd = .2)
z <- sqrt(abs(x))+ rnorm(n, mean = 0, sd = .2)

scatterplot3d(x,y,z, pch = 16, type = 'h')
plot3d(x,y,z, col = 'red', size = 3)
plot3d(x,y,z, col = 'gold', size = 1, type = 's')
rgl.bg(color = 'black')
rgl.clear(type = 'shapes')
spheres3d(4*x, 4*y,4*z, radius = .1, col = 'gold')
rgl.light(theta = 0, phi = 0, viewpoint.rel = T,
          ambient = '#FFFFFF',
          diffuse = '#FFFFFF',
          specular = '#FFFFFF')
rgl.clear(type = 'lights')
light3d(diffuse = 'gray75', 
        specular = 'gray75',
        viewpoint.rel = F)
rgl.light(ambient = '#444444',
          diffuse = '#0000FF',
          specular = '#FF0000')

#9.2.3
open3d()
par3d()$windowRect

lines3d(x=c(-2,2), y = c(0,0), z = c(0,0),
        col = 'red', lwd=1)
text3d(2.2,0,0, "x", col = 'red', cex=1, adj = 0)

lines3d(x=c(0,0), y = c(-2,2), z = c(0,0),
        col = 'green', lwd=1)
text3d(0,2.2,0, "Y", col = 'green', cex=1, adj = 0)

lines3d(x=c(0,0), y = c(0,0), z = c(-2,2),
        col = 'blue', lwd=1)
text3d(0,0,2.2, "Z", col = 'blue', cex=1, adj = 0)

wire3d(cube3d())
wire3d(scale3d(cube3d(), 2,2,2), col ='red')

rgl.ids()
rgl.clear(type = 'shapes')

shapelist3d(tetrahedron3d(), 2,0,0,
            size = .5, color ='tan')
material3d(alpha = .2, shininess = 75,
           emision ='blue')
shapelist3d(cube3d(), 0,2,0,
            size = .2, color ='cadetblue')

material3d(alpha = 1, shininess = 5,
           emision ='black')
shapelist3d(octahedron3d(), 0,0,2.5,
            size = .9, color ='red')
material3d(alpha = .5, shininess = 0,
           emision ='black')
shapelist3d(icosahedron3d(), 2,0,2.5,
            size = .9, color ='orange')

material3d(alpha = .75, shininess = 0,
           emision ='black')
shapelist3d(icosahedron3d(), 2,0,2.5,
            size = .9, color ='orange')

#9.2.5 Creating a scene
open3d()
material3d(alpha =1 , shininess = 50,
           emission ='black')
rgl.bg(colore = 'black')
rgl.clear(type = 'shapes')
n <- 10
x <- 0
y <- 0
z.start <- 0
z.end <- 10

M <- matrix(c(rep(x,n), rep(y,n), 
            seq(from = z.start, to=z.end,
            length.out = n)), nrow = n, byrow = F)
m2 <- cylinder3d(center = M, radius = .2, sides = 50,
                 closed = -2)
shade3d(m2, alpha = 1, color = 'gray45')

z.end = .25
M <- matrix(c(rep(x,n), rep(y,n), 
              seq(from = z.start, to=z.end,
                  length.out = n)), nrow = n, byrow = F)
m2 <- cylinder3d(center = M, radius = 1.5, sides = 50,
                 closed = -2)
shade3d(m2, alpha = 1, color = 'gray45')

material3d(alpha = 1, shininess = 50,
           emission = 'tan')
spheres3d(x, y, 10.5, radius = .25,
          col='yellow')
material3d(alpha = .3, shininess = 50,
           emission = 'black')
shapelist3d(cuboctahedron3d(), x, y, 10.5,
            size = .575, color = 'gold')
material3d(alpha = 1, shininess = 50,
           emission = 'black')
M <- matrix(c(rep(x,n), rep(y,n), 
              seq(from = 11, to=11.15,
                  length.out = n)), nrow = n, byrow = F)
m4 <- cylinder3d(center = M, radius = .75, sides = 50,
                 closed = -2)
shade3d(m4, alpha = 1, color = 'gray45')
ns <- 100
xs <- rnorm(ns, 0,3)
ys <- rnorm(ns, 0,3)
zs <- rnorm(ns, 9,2)
rs <- rnorm(ns, 1,.3)
material3d(alpha = 1, shininess = 100,
           emission = 'black')
particles3d(xs, ys, zs, radius = rs, color = 'white')
writeWebGL(dir = "Week 9/", filename = "index.html")
filename <- writeWebGL(dir = "Week 9/", width = 500, reuse = T)

#9.3 Other Topics - Package Management
writeOBJ()
readOBJ()

readSTL()
writeSTL()

tmp <- installed.packages()
ip <- as.data.frame(tmp, stringsAsFactors = F)

table(ip$LibPath)

.libPaths()
ip$Package[grep(pattern = "^gg", ip$Package)]
tmp <- sessionInfo()
tmp
gc()
memory.size()
