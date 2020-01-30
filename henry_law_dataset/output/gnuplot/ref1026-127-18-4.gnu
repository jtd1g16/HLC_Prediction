# load "ref1026-127-18-4.gnu"
# chem = "tetrachloroethene"

set terminal postscript eps color
set title "ref = 1026; chem = tetrachloroethene; casrn = 127-18-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4126600E-03 * exp(  -5259.372    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1465766E-02 point
set label "" at    283.1500    ,   0.1051400E-02 point
set label "" at    288.1500    ,   0.7616697E-03 point
set label "" at    293.1500    ,   0.5574400E-03 point
set label "" at    298.1500    ,   0.4124698E-03 point
set label "" at    298.1500    ,   0.4126600E-03 point ps 2 pt 6

plot [270:300] H(T)
