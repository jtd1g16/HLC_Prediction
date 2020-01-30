# load "ref2982-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2982; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1401650E-02 * exp(  -2383.487    *(1/   298.    -1/T))

set label "" at    322.0000    ,   0.7827710E-03 point
set label "" at    342.0000    ,   0.4928042E-03 point
set label "" at    362.0000    ,   0.3458869E-03 point
set label "" at    298.1500    ,   0.1401650E-02 point ps 2 pt 6

plot [320:370] H(T)
