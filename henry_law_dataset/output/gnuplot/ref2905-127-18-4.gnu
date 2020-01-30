# load "ref2905-127-18-4.gnu"
# chem = "tetrachloroethene"

set terminal postscript eps color
set title "ref = 2905; chem = tetrachloroethene; casrn = 127-18-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5549933E-03 * exp(  -4205.398    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.9675859E-03 point
set label "" at    298.1500    ,   0.4837929E-03 point
set label "" at    308.1500    ,   0.3769744E-03 point
set label "" at    298.1500    ,   0.5549933E-03 point ps 2 pt 6

plot [280:310] H(T)
