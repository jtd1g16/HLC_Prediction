# load "ref2477-90-12-0.gnu"
# chem = "1-methylnaphthalene"

set terminal postscript eps color
set title "ref = 2477; chem = 1-methylnaphthalene; casrn = 90-12-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2090823E-01 * exp(  -6138.700    *(1/   298.    -1/T))

set label "" at    277.2500    ,   0.9900990E-01 point
set label "" at    284.1500    ,   0.5747126E-01 point
set label "" at    291.1500    ,   0.3424658E-01 point
set label "" at    298.1500    ,   0.2092050E-01 point
set label "" at    304.1500    ,   0.1394700E-01 point
set label "" at    298.1500    ,   0.2090823E-01 point ps 2 pt 6

plot [270:310] H(T)
