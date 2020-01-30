# load "ref2645-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2645; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1702860E-02 * exp(  -4211.528    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.4700012E-02 point
set label "" at    288.1500    ,   0.2782633E-02 point
set label "" at    298.1500    ,   0.1702091E-02 point
set label "" at    298.1500    ,   0.1702860E-02 point ps 2 pt 6

plot [270:300] H(T)
