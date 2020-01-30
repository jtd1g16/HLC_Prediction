# load "ref2936-590-73-8.gnu"
# chem = "2,2-dimethylhexane"

set terminal postscript eps color
set title "ref = 2936; chem = 2,2-dimethylhexane; casrn = 590-73-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2636104E-05 * exp(  -5145.739    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1458001E-04 point
set label "" at    278.1500    ,   0.9359331E-05 point
set label "" at    283.1500    ,   0.6283514E-05 point
set label "" at    288.1500    ,   0.4412209E-05 point
set label "" at    293.1500    ,   0.3230518E-05 point
set label "" at    298.1500    ,   0.2444821E-05 point
set label "" at    303.1500    ,   0.1925932E-05 point
set label "" at    308.1500    ,   0.1567488E-05 point
set label "" at    313.1500    ,   0.1310828E-05 point
set label "" at    298.1500    ,   0.2636104E-05 point ps 2 pt 6

plot [270:320] H(T)
