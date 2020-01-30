# load "ref1026-74-87-3.gnu"
# chem = "chloromethane"

set terminal postscript eps color
set title "ref = 1026; chem = chloromethane; casrn = 74-87-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8831338E-03 * exp(  -3157.331    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1888214E-02 point
set label "" at    283.1500    ,   0.1550239E-02 point
set label "" at    288.1500    ,   0.1276437E-02 point
set label "" at    293.1500    ,   0.1057412E-02 point
set label "" at    298.1500    ,   0.8827035E-03 point
set label "" at    298.1500    ,   0.8831338E-03 point ps 2 pt 6

plot [270:300] H(T)
