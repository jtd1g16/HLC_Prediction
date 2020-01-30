# load "ref2896-127-18-4.gnu"
# chem = "tetrachloroethene"

set terminal postscript eps color
set title "ref = 2896; chem = tetrachloroethene; casrn = 127-18-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6004687E-03 * exp(  -4077.312    *(1/   298.    -1/T))

set label "" at    303.1500    ,   0.4959276E-03 point
set label "" at    313.1500    ,   0.2947603E-03 point
set label "" at    323.1500    ,   0.2099196E-03 point
set label "" at    333.1500    ,   0.1449280E-03 point
set label "" at    298.1500    ,   0.6004687E-03 point ps 2 pt 6

plot [300:340] H(T)
