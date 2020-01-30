# load "ref2431-994-05-8.gnu"
# chem = "2-methoxy-2-methylbutane"

set terminal postscript eps color
set title "ref = 2431; chem = 2-methoxy-2-methylbutane; casrn = 994-05-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4980191E-02 * exp(  -7566.688    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.3088579E-01 point
set label "" at    298.1500    ,   0.4980191E-02 point
set label "" at    298.1500    ,   0.4980191E-02 point ps 2 pt 6

plot [270:300] H(T)
