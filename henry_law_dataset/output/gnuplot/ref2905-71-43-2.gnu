# load "ref2905-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2905; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1449461E-02 * exp(  -3308.954    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2048341E-02 point
set label "" at    298.1500    ,   0.1572327E-02 point
set label "" at    308.1500    ,   0.9694619E-03 point
set label "" at    298.1500    ,   0.1449461E-02 point ps 2 pt 6

plot [280:310] H(T)
