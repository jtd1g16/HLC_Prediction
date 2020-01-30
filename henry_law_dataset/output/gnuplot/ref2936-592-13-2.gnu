# load "ref2936-592-13-2.gnu"
# chem = "2,5-dimethylhexane"

set terminal postscript eps color
set title "ref = 2936; chem = 2,5-dimethylhexane; casrn = 592-13-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2662314E-05 * exp(  -4664.093    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1302711E-04 point
set label "" at    278.1500    ,   0.8461861E-05 point
set label "" at    283.1500    ,   0.5786996E-05 point
set label "" at    288.1500    ,   0.4132624E-05 point
set label "" at    293.1500    ,   0.3108150E-05 point
set label "" at    298.1500    ,   0.2444821E-05 point
set label "" at    303.1500    ,   0.1983710E-05 point
set label "" at    308.1500    ,   0.1682347E-05 point
set label "" at    313.1500    ,   0.1465926E-05 point
set label "" at    298.1500    ,   0.2662314E-05 point ps 2 pt 6

plot [270:320] H(T)
