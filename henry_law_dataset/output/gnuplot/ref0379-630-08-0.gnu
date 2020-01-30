# load "ref0379-630-08-0.gnu"
# chem = "carbon monoxide"

set terminal postscript eps color
set title "ref = 379; chem = carbon monoxide; casrn = 630-08-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9403698E-05 * exp(  -1593.378    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1557398E-04 point
set label "" at    274.1500    ,   0.1521292E-04 point
set label "" at    275.1500    ,   0.1486067E-04 point
set label "" at    276.1500    ,   0.1451722E-04 point
set label "" at    277.1500    ,   0.1418699E-04 point
set label "" at    278.1500    ,   0.1386556E-04 point
set label "" at    279.1500    ,   0.1355293E-04 point
set label "" at    280.1500    ,   0.1324911E-04 point
set label "" at    281.1500    ,   0.1295410E-04 point
set label "" at    282.1500    ,   0.1267230E-04 point
set label "" at    283.1500    ,   0.1239930E-04 point
set label "" at    284.1500    ,   0.1213952E-04 point
set label "" at    285.1500    ,   0.1189294E-04 point
set label "" at    286.1500    ,   0.1165077E-04 point
set label "" at    287.1500    ,   0.1141740E-04 point
set label "" at    288.1500    ,   0.1119724E-04 point
set label "" at    289.1500    ,   0.1098149E-04 point
set label "" at    290.1500    ,   0.1077894E-04 point
set label "" at    291.1500    ,   0.1057639E-04 point
set label "" at    292.1500    ,   0.1039146E-04 point
set label "" at    293.1500    ,   0.1021093E-04 point
set label "" at    294.1500    ,   0.1004361E-04 point
set label "" at    295.1500    ,   0.9880695E-05 point
set label "" at    296.1500    ,   0.9722181E-05 point
set label "" at    297.1500    ,   0.9572474E-05 point
set label "" at    298.1500    ,   0.9431572E-05 point
set label "" at    299.1500    ,   0.9290671E-05 point
set label "" at    300.1500    ,   0.9158576E-05 point
set label "" at    301.1500    ,   0.9030885E-05 point
set label "" at    302.1500    ,   0.8911999E-05 point
set label "" at    303.1500    ,   0.8797517E-05 point
set label "" at    298.1500    ,   0.9403698E-05 point ps 2 pt 6

plot [270:310] H(T)
