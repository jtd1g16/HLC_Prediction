# load "ref1229-103-65-1.gnu"
# chem = "propylbenzene"

set terminal postscript eps color
set title "ref = 1229; chem = propylbenzene; casrn = 103-65-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8608766E-03 * exp(  -5393.468    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.2268789E-02 point
set label "" at    288.1500    ,   0.1589248E-02 point
set label "" at    293.1500    ,   0.1179120E-02 point
set label "" at    298.1500    ,   0.8507959E-03 point
set label "" at    303.1500    ,   0.6450479E-03 point
set label "" at    298.1500    ,   0.8608766E-03 point ps 2 pt 6

plot [280:310] H(T)
