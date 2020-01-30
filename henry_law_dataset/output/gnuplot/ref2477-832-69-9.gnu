# load "ref2477-832-69-9.gnu"
# chem = "1-methylphenanthrene"

set terminal postscript eps color
set title "ref = 2477; chem = 1-methylphenanthrene; casrn = 832-69-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1997416     * exp(  -4557.666    *(1/   298.    -1/T))

set label "" at    277.2500    ,   0.6329114     point
set label "" at    284.1500    ,   0.4237288     point
set label "" at    291.1500    ,   0.2881844     point
set label "" at    298.1500    ,   0.2000000     point
set label "" at    304.1500    ,   0.1477105     point
set label "" at    298.1500    ,   0.1997416     point ps 2 pt 6

plot [270:310] H(T)
