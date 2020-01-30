# load "ref2982-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 2982; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1262593E-02 * exp(  -2685.429    *(1/   298.    -1/T))

set label "" at    322.0000    ,   0.6472738E-03 point
set label "" at    342.0000    ,   0.3987169E-03 point
set label "" at    362.0000    ,   0.2575240E-03 point
set label "" at    298.1500    ,   0.1262593E-02 point ps 2 pt 6

plot [320:370] H(T)
