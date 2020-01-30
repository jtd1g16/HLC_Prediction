# load "ref2477-85-01-8.gnu"
# chem = "phenanthrene"

set terminal postscript eps color
set title "ref = 2477; chem = phenanthrene; casrn = 85-01-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2328871     * exp(  -5996.511    *(1/   298.    -1/T))

set label "" at    277.2500    ,    1.063830     point
set label "" at    284.1500    ,   0.6250000     point
set label "" at    291.1500    ,   0.3773585     point
set label "" at    298.1500    ,   0.2331002     point
set label "" at    304.1500    ,   0.1567398     point
set label "" at    298.1500    ,   0.2328871     point ps 2 pt 6

plot [270:310] H(T)
