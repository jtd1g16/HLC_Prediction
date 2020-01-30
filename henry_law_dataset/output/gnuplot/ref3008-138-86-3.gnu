# load "ref3008-138-86-3.gnu"
# chem = "limonene"

set terminal postscript eps color
set title "ref = 3008; chem = limonene; casrn = 138-86-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4809497E-03 * exp(  -4583.376    *(1/   298.    -1/T))

set label "" at    298.0000    ,   0.4737232E-03 point
set label "" at    293.0000    ,   0.6513694E-03 point
set label "" at    288.0000    ,   0.8191463E-03 point
set label "" at    283.0000    ,   0.1105354E-02 point
set label "" at    278.0000    ,   0.1450777E-02 point
set label "" at    298.1500    ,   0.4809497E-03 point ps 2 pt 6

plot [270:300] H(T)
