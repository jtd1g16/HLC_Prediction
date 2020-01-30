# load "ref2477-56-55-3.gnu"
# chem = "benz[a]anthracene"

set terminal postscript eps color
set title "ref = 2477; chem = benz[a]anthracene; casrn = 56-55-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8186066     * exp(  -8289.113    *(1/   298.    -1/T))

set label "" at    277.2500    ,    6.666667     point
set label "" at    284.1500    ,    3.225806     point
set label "" at    291.1500    ,    1.587302     point
set label "" at    298.1500    ,   0.8196721     point
set label "" at    304.1500    ,   0.4739336     point
set label "" at    298.1500    ,   0.8186066     point ps 2 pt 6

plot [270:310] H(T)
