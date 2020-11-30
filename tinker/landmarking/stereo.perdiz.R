library(here)
library(StereoMorph)
library(geomorph)

# digitize dataset
digitizeImage(image.file = 'points', 
              shapes.file = 'shapes',
              landmarks.ref = 'lm.txt', 
              curves.ref = 'curve.txt')

read_shapes <- readShapes('shapes/01.txt')
read_shapes
read_shapes$landmarks.pixel
read_shapes$scaling
read_shapes$landmarks.scaled

# read data and define number of sLMs
shapes <- readShapes("shapes")
shapesGM <- readland.shapes(shapes, nCurvePts = c(10,3,5,5,3,10))

# gpa
Y.gpa <- gpagen(shapesGM, print.progress = FALSE)
plot(Y.gpa)
