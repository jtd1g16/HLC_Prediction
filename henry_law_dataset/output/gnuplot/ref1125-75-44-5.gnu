# load "ref1125-75-44-5.gnu"
# chem = "CCl2O"

set terminal postscript eps color
set title "ref = 1125; chem = CCl2O; casrn = 75-44-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5883717E-03 * exp(  -3795.459    *(1/   298.    -1/T))

set label "" at    278.0000    ,   0.1480385E-02 point
set label "" at    298.0000    ,   0.5921540E-03 point
set label "" at    298.1500    ,   0.5883717E-03 point ps 2 pt 6

plot [270:300] H(T)
