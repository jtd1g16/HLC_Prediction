# load "ref2830-75-69-4.gnu"
# chem = "trichlorofluoromethane"

set terminal postscript eps color
set title "ref = 2830; chem = trichlorofluoromethane; casrn = 75-69-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7846832E-04 * exp(  -3869.425    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2655107E-03 point
set label "" at    275.7500    ,   0.2276435E-03 point
set label "" at    276.6500    ,   0.2170759E-03 point
set label "" at    278.3500    ,   0.1985826E-03 point
set label "" at    278.4500    ,   0.1972616E-03 point
set label "" at    278.5500    ,   0.2025454E-03 point
set label "" at    280.1500    ,   0.1770071E-03 point
set label "" at    280.3500    ,   0.1827312E-03 point
set label "" at    280.5500    ,   0.1809699E-03 point
set label "" at    281.0500    ,   0.1717233E-03 point
set label "" at    283.1500    ,   0.1457447E-03 point
set label "" at    283.3500    ,   0.1510284E-03 point
set label "" at    283.5500    ,   0.1514688E-03 point
set label "" at    283.6500    ,   0.1549913E-03 point
set label "" at    285.4500    ,   0.1378190E-03 point
set label "" at    288.1500    ,   0.1215273E-03 point
set label "" at    288.2500    ,   0.1206466E-03 point
set label "" at    288.3500    ,   0.1206466E-03 point
set label "" at    291.0500    ,   0.1087581E-03 point
set label "" at    291.1500    ,   0.1025937E-03 point
set label "" at    292.8500    ,   0.1017130E-03 point
set label "" at    293.3500    ,   0.9334703E-04 point
set label "" at    293.8500    ,   0.9598893E-04 point
set label "" at    298.3500    ,   0.7793596E-04 point
set label "" at    298.6500    ,   0.7705533E-04 point
set label "" at    301.0500    ,   0.6956996E-04 point
set label "" at    304.7500    ,   0.6252490E-04 point
set label "" at    298.1500    ,   0.7846832E-04 point ps 2 pt 6

plot [270:310] H(T)
