# load "ref2993-647-42-7.gnu"
# chem = "6:2 FTOH"

set terminal postscript eps color
set title "ref = 2993; chem = 6:2 FTOH; casrn = 647-42-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1819996E-02 * exp(  -8030.113    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1305362E-01 point
set label "" at    288.1500    ,   0.4322460E-02 point
set label "" at    298.1500    ,   0.1886822E-02 point
set label "" at    298.1500    ,   0.1819996E-02 point ps 2 pt 6

plot [270:300] H(T)
