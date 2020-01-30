# load "ref3008-78-79-5.gnu"
# chem = "isoprene"

set terminal postscript eps color
set title "ref = 3008; chem = isoprene; casrn = 78-79-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3432846E-03 * exp(  -4359.046    *(1/   298.    -1/T))

set label "" at    298.0000    ,   0.3552924E-03 point
set label "" at    293.0000    ,   0.4342462E-03 point
set label "" at    288.0000    ,   0.5526770E-03 point
set label "" at    283.0000    ,   0.7796694E-03 point
set label "" at    278.0000    ,   0.9869233E-03 point
set label "" at    298.1500    ,   0.3432846E-03 point ps 2 pt 6

plot [270:300] H(T)
