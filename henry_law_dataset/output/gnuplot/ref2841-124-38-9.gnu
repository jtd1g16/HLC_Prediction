# load "ref2841-124-38-9.gnu"
# chem = "carbon dioxide"

set terminal postscript eps color
set title "ref = 2841; chem = carbon dioxide; casrn = 124-38-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3457417E-03 * exp(  -2364.443    *(1/   298.    -1/T))

set label "" at    273.2500    ,   0.7512675E-03 point
set label "" at    276.9500    ,   0.6570398E-03 point
set label "" at    279.9500    ,   0.5905080E-03 point
set label "" at    285.4500    ,   0.4836873E-03 point
set label "" at    287.9500    ,   0.4505315E-03 point
set label "" at    292.0500    ,   0.4019206E-03 point
set label "" at    294.3500    ,   0.3741367E-03 point
set label "" at    295.7500    ,   0.3582853E-03 point
set label "" at    299.3500    ,   0.3220472E-03 point
set label "" at    304.2500    ,   0.2856331E-03 point
set label "" at    310.4500    ,   0.2478540E-03 point
set label "" at    313.5500    ,   0.2335437E-03 point
set label "" at    317.0500    ,   0.2159751E-03 point
set label "" at    322.9500    ,   0.1929465E-03 point
set label "" at    334.5500    ,   0.1544629E-03 point
set label "" at    298.1500    ,   0.3457417E-03 point ps 2 pt 6

plot [270:340] H(T)
