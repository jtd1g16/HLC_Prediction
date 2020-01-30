# load "ref0379-7783-06-4.gnu"
# chem = "hydrogen sulfide"

set terminal postscript eps color
set title "ref = 379; chem = hydrogen sulfide; casrn = 7783-06-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1001994E-02 * exp(  -2299.287    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2056277E-02 point
set label "" at    274.1500    ,   0.1991110E-02 point
set label "" at    275.1500    ,   0.1928145E-02 point
set label "" at    276.1500    ,   0.1867381E-02 point
set label "" at    277.1500    ,   0.1808379E-02 point
set label "" at    278.1500    ,   0.1751137E-02 point
set label "" at    279.1500    ,   0.1696098E-02 point
set label "" at    280.1500    ,   0.1643260E-02 point
set label "" at    281.1500    ,   0.1592183E-02 point
set label "" at    282.1500    ,   0.1543308E-02 point
set label "" at    283.1500    ,   0.1496635E-02 point
set label "" at    284.1500    ,   0.1453043E-02 point
set label "" at    285.1500    ,   0.1411654E-02 point
set label "" at    286.1500    ,   0.1371585E-02 point
set label "" at    287.1500    ,   0.1333277E-02 point
set label "" at    288.1500    ,   0.1296731E-02 point
set label "" at    289.1500    ,   0.1261506E-02 point
set label "" at    290.1500    ,   0.1228042E-02 point
set label "" at    291.1500    ,   0.1196339E-02 point
set label "" at    292.1500    ,   0.1165517E-02 point
set label "" at    293.1500    ,   0.1136896E-02 point
set label "" at    294.1500    ,   0.1108276E-02 point
set label "" at    295.1500    ,   0.1081417E-02 point
set label "" at    296.1500    ,   0.1054998E-02 point
set label "" at    297.1500    ,   0.1029459E-02 point
set label "" at    298.1500    ,   0.1004802E-02 point
set label "" at    299.1500    ,   0.9814647E-03 point
set label "" at    300.1500    ,   0.9585683E-03 point
set label "" at    301.1500    ,   0.9369928E-03 point
set label "" at    302.1500    ,   0.9162980E-03 point
set label "" at    303.1500    ,   0.8969240E-03 point
set label "" at    298.1500    ,   0.1001994E-02 point ps 2 pt 6

plot [270:310] H(T)
