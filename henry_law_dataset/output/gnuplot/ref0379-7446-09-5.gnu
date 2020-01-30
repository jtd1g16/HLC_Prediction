# load "ref0379-7446-09-5.gnu"
# chem = "sulfur dioxide"

set terminal postscript eps color
set title "ref = 379; chem = sulfur dioxide; casrn = 7446-09-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1495460E-01 * exp(  -2887.104    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3534545E-01 point
set label "" at    274.1500    ,   0.3421855E-01 point
set label "" at    275.1500    ,   0.3311823E-01 point
set label "" at    276.1500    ,   0.3204358E-01 point
set label "" at    277.1500    ,   0.3099505E-01 point
set label "" at    278.1500    ,   0.2997263E-01 point
set label "" at    279.1500    ,   0.2897587E-01 point
set label "" at    280.1500    ,   0.2800475E-01 point
set label "" at    281.1500    ,   0.2705972E-01 point
set label "" at    282.1500    ,   0.2614122E-01 point
set label "" at    283.1500    ,   0.2524835E-01 point
set label "" at    284.1500    ,   0.2438112E-01 point
set label "" at    285.1500    ,   0.2354040E-01 point
set label "" at    286.1500    ,   0.2272532E-01 point
set label "" at    287.1500    ,   0.2193586E-01 point
set label "" at    288.1500    ,   0.2117249E-01 point
set label "" at    289.1500    ,   0.2043520E-01 point
set label "" at    290.1500    ,   0.1972402E-01 point
set label "" at    291.1500    ,   0.1903940E-01 point
set label "" at    292.1500    ,   0.1838002E-01 point
set label "" at    293.1500    ,   0.1774634E-01 point
set label "" at    294.1500    ,   0.1713929E-01 point
set label "" at    295.1500    ,   0.1655486E-01 point
set label "" at    296.1500    ,   0.1598716E-01 point
set label "" at    297.1500    ,   0.1543665E-01 point
set label "" at    298.1500    ,   0.1490197E-01 point
set label "" at    299.1500    ,   0.1438403E-01 point
set label "" at    300.1500    ,   0.1388374E-01 point
set label "" at    301.1500    ,   0.1340753E-01 point
set label "" at    302.1500    ,   0.1293254E-01 point
set label "" at    303.1500    ,   0.1248210E-01 point
set label "" at    298.1500    ,   0.1495460E-01 point ps 2 pt 6

plot [270:310] H(T)
