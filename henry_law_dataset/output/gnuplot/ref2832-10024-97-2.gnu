# load "ref2832-10024-97-2.gnu"
# chem = "dinitrogen monoxide"

set terminal postscript eps color
set title "ref = 2832; chem = dinitrogen monoxide; casrn = 10024-97-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2500518E-03 * exp(  -2671.576    *(1/   298.    -1/T))

set label "" at    273.4400    ,   0.5793240E-03 point
set label "" at    273.4400    ,   0.5781396E-03 point
set label "" at    273.4400    ,   0.5793240E-03 point
set label "" at    273.4400    ,   0.5793240E-03 point
set label "" at    273.4400    ,   0.5781396E-03 point
set label "" at    283.1600    ,   0.3963484E-03 point
set label "" at    283.1500    ,   0.3964471E-03 point
set label "" at    283.1500    ,   0.3954602E-03 point
set label "" at    283.1500    ,   0.3957562E-03 point
set label "" at    283.1600    ,   0.3960523E-03 point
set label "" at    283.1500    ,   0.3966445E-03 point
set label "" at    293.1100    ,   0.2844313E-03 point
set label "" at    293.1300    ,   0.2842339E-03 point
set label "" at    293.1300    ,   0.2835431E-03 point
set label "" at    293.1200    ,   0.2840365E-03 point
set label "" at    293.1200    ,   0.2841352E-03 point
set label "" at    293.1300    ,   0.2837404E-03 point
set label "" at    303.3500    ,   0.2127807E-03 point
set label "" at    303.3400    ,   0.2124846E-03 point
set label "" at    303.3600    ,   0.2126820E-03 point
set label "" at    303.3600    ,   0.2120898E-03 point
set label "" at    303.3700    ,   0.2122872E-03 point
set label "" at    313.2300    ,   0.1670861E-03 point
set label "" at    313.2300    ,   0.1669874E-03 point
set label "" at    313.2400    ,   0.1668887E-03 point
set label "" at    313.2300    ,   0.1673822E-03 point
set label "" at    313.2400    ,   0.1671848E-03 point
set label "" at    298.1500    ,   0.2500518E-03 point ps 2 pt 6

plot [270:320] H(T)
