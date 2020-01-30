# load "ref0379-10102-43-9.gnu"
# chem = "nitrogen monoxide"

set terminal postscript eps color
set title "ref = 379; chem = nitrogen monoxide; casrn = 10102-43-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1893158E-04 * exp(  -1674.370    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3249974E-04 point
set label "" at    274.1500    ,   0.3163231E-04 point
set label "" at    275.1500    ,   0.3079131E-04 point
set label "" at    276.1500    ,   0.2998113E-04 point
set label "" at    277.1500    ,   0.2920177E-04 point
set label "" at    278.1500    ,   0.2844883E-04 point
set label "" at    279.1500    ,   0.2773111E-04 point
set label "" at    280.1500    ,   0.2703541E-04 point
set label "" at    281.1500    ,   0.2637494E-04 point
set label "" at    282.1500    ,   0.2574088E-04 point
set label "" at    283.1500    ,   0.2513765E-04 point
set label "" at    284.1500    ,   0.2460046E-04 point
set label "" at    285.1500    ,   0.2408529E-04 point
set label "" at    286.1500    ,   0.2358774E-04 point
set label "" at    287.1500    ,   0.2311660E-04 point
set label "" at    288.1500    ,   0.2266307E-04 point
set label "" at    289.1500    ,   0.2223156E-04 point
set label "" at    290.1500    ,   0.2182207E-04 point
set label "" at    291.1500    ,   0.2143459E-04 point
set label "" at    292.1500    ,   0.2106913E-04 point
set label "" at    293.1500    ,   0.2072128E-04 point
set label "" at    294.1500    ,   0.2036462E-04 point
set label "" at    295.1500    ,   0.2001237E-04 point
set label "" at    296.1500    ,   0.1967773E-04 point
set label "" at    297.1500    ,   0.1935190E-04 point
set label "" at    298.1500    ,   0.1903487E-04 point
set label "" at    299.1500    ,   0.1873105E-04 point
set label "" at    300.1500    ,   0.1844044E-04 point
set label "" at    301.1500    ,   0.1815864E-04 point
set label "" at    302.1500    ,   0.1789005E-04 point
set label "" at    303.1500    ,   0.1763026E-04 point
set label "" at    298.1500    ,   0.1893158E-04 point ps 2 pt 6

plot [270:310] H(T)
