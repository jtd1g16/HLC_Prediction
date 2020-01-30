# load "ref1026-79-01-6.gnu"
# chem = "trichloroethene"

set terminal postscript eps color
set title "ref = 1026; chem = trichloroethene; casrn = 79-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7586705E-03 * exp(  -4918.126    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.2485064E-02 point
set label "" at    283.1500    ,   0.1815237E-02 point
set label "" at    288.1500    ,   0.1346435E-02 point
set label "" at    293.1500    ,   0.1005578E-02 point
set label "" at    298.1500    ,   0.7582622E-03 point
set label "" at    298.1500    ,   0.7586705E-03 point ps 2 pt 6

plot [270:300] H(T)
