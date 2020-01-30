# load "ref2993-25291-17-2.gnu"
# chem = "6:2 FTO"

set terminal postscript eps color
set title "ref = 2993; chem = 6:2 FTO; casrn = 25291-17-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5287711E-06 * exp(  -4877.318    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1760885E-05 point
set label "" at    288.1500    ,   0.8825332E-06 point
set label "" at    298.1500    ,   0.5441655E-06 point
set label "" at    298.1500    ,   0.5287711E-06 point ps 2 pt 6

plot [270:300] H(T)
