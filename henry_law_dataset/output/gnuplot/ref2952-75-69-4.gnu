# load "ref2952-75-69-4.gnu"
# chem = "trichlorofluoromethane"

set terminal postscript eps color
set title "ref = 2952; chem = trichlorofluoromethane; casrn = 75-69-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1091939E-03 * exp(  -2101.407    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.1633714E-03 point
set label "" at    288.1500    ,   0.1346435E-03 point
set label "" at    293.1500    ,   0.1206694E-03 point
set label "" at    298.1500    ,   0.1120543E-03 point
set label "" at    298.1500    ,   0.1091939E-03 point ps 2 pt 6

plot [280:300] H(T)
