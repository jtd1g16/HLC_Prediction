# load "ref2993-21652-58-4.gnu"
# chem = "8:2 FTO"

set terminal postscript eps color
set title "ref = 2993; chem = 8:2 FTO; casrn = 21652-58-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1365171E-06 * exp(  -5741.788    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.5698113E-06 point
set label "" at    288.1500    ,   0.2430698E-06 point
set label "" at    298.1500    ,   0.1431301E-06 point
set label "" at    298.1500    ,   0.1365171E-06 point ps 2 pt 6

plot [270:300] H(T)
