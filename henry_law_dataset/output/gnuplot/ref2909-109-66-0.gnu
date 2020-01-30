# load "ref2909-109-66-0.gnu"
# chem = "pentane"

set terminal postscript eps color
set title "ref = 2909; chem = pentane; casrn = 109-66-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8243684E-05 * exp(  -3633.371    *(1/   298.    -1/T))

set label "" at    288.2500    ,   0.1280958E-04 point
set label "" at    288.2500    ,   0.1280958E-04 point
set label "" at    293.1500    ,   0.9928675E-05 point
set label "" at    293.1500    ,   0.1025690E-04 point
set label "" at    298.1500    ,   0.7946891E-05 point
set label "" at    298.1500    ,   0.7825872E-05 point
set label "" at    303.1500    ,   0.6942986E-05 point
set label "" at    308.1500    ,   0.5698447E-05 point
set label "" at    298.1500    ,   0.8243684E-05 point ps 2 pt 6

plot [280:310] H(T)
