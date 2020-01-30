# load "ref2647-1634-04-4.gnu"
# chem = "methyl {tert}-butyl ether"

set terminal postscript eps color
set title "ref = 2647; chem = methyl {tert}-butyl ether; casrn = 1634-04-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1414803E-01 * exp(  -4548.589    *(1/   298.    -1/T))

set label "" at    276.1500    ,   0.4786074E-01 point
set label "" at    278.1500    ,   0.3860724E-01 point
set label "" at    283.1500    ,   0.3630475E-01 point
set label "" at    288.1500    ,   0.2358164E-01 point
set label "" at    293.1500    ,   0.1831589E-01 point
set label "" at    298.1500    ,   0.1381491E-01 point
set label "" at    298.1500    ,   0.1414803E-01 point ps 2 pt 6

plot [270:300] H(T)
