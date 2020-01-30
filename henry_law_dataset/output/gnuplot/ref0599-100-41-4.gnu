# load "ref0599-100-41-4.gnu"
# chem = "ethylbenzene"

set terminal postscript eps color
set title "ref = 599; chem = ethylbenzene; casrn = 100-41-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1261011E-02 * exp(  -4624.082    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.1268539E-02 point
set label "" at    303.1500    ,   0.9675718E-03 point
set label "" at    313.1500    ,   0.6017825E-03 point
set label "" at    298.1500    ,   0.1261011E-02 point ps 2 pt 6

plot [290:320] H(T)
