# load "ref0379-74-85-1.gnu"
# chem = "ethene"

set terminal postscript eps color
set title "ref = 379; chem = ethene; casrn = 74-85-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4758636E-04 * exp(  -2329.998    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.9951146E-04 point
set label "" at    274.1500    ,   0.9642924E-04 point
set label "" at    275.1500    ,   0.9290671E-04 point
set label "" at    276.1500    ,   0.8982450E-04 point
set label "" at    277.1500    ,   0.8674229E-04 point
set label "" at    278.1500    ,   0.8410039E-04 point
set label "" at    279.1500    ,   0.8101818E-04 point
set label "" at    280.1500    ,   0.7837628E-04 point
set label "" at    281.1500    ,   0.7617470E-04 point
set label "" at    282.1500    ,   0.7353280E-04 point
set label "" at    283.1500    ,   0.7133122E-04 point
set label "" at    284.1500    ,   0.6912964E-04 point
set label "" at    285.1500    ,   0.6692806E-04 point
set label "" at    286.1500    ,   0.6516679E-04 point
set label "" at    287.1500    ,   0.6296521E-04 point
set label "" at    288.1500    ,   0.6120395E-04 point
set label "" at    289.1500    ,   0.5988300E-04 point
set label "" at    290.1500    ,   0.5812173E-04 point
set label "" at    291.1500    ,   0.5680079E-04 point
set label "" at    292.1500    ,   0.5503952E-04 point
set label "" at    293.1500    ,   0.5371857E-04 point
set label "" at    294.1500    ,   0.5239762E-04 point
set label "" at    295.1500    ,   0.5107668E-04 point
set label "" at    296.1500    ,   0.5019604E-04 point
set label "" at    297.1500    ,   0.4887510E-04 point
set label "" at    298.1500    ,   0.4755415E-04 point
set label "" at    299.1500    ,   0.4667351E-04 point
set label "" at    300.1500    ,   0.4579288E-04 point
set label "" at    301.1500    ,   0.4491225E-04 point
set label "" at    302.1500    ,   0.4403162E-04 point
set label "" at    303.1500    ,   0.4315098E-04 point
set label "" at    298.1500    ,   0.4758636E-04 point ps 2 pt 6

plot [270:310] H(T)
