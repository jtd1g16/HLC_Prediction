# load "ref2458-91-20-3.gnu"
# chem = "naphthalene"

set terminal postscript eps color
set title "ref = 2458; chem = naphthalene; casrn = 91-20-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2179609E-01 * exp(  -5355.787    *(1/   298.    -1/T))

set label "" at    295.0500    ,   0.2731147E-01 point
set label "" at    298.4500    ,   0.2135847E-01 point
set label "" at    289.7500    ,   0.3735811E-01 point
set label "" at    281.5500    ,   0.6236819E-01 point
set label "" at    283.8500    ,   0.5254104E-01 point
set label "" at    291.6500    ,   0.3299088E-01 point
set label "" at    303.4500    ,   0.1545764E-01 point
set label "" at    296.2500    ,   0.2435896E-01 point
set label "" at    298.1500    ,   0.2179609E-01 point ps 2 pt 6

plot [280:310] H(T)
