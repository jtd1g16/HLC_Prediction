# load "ref2909-142-82-5.gnu"
# chem = "heptane"

set terminal postscript eps color
set title "ref = 2909; chem = heptane; casrn = 142-82-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4153889E-05 * exp(  -4746.484    *(1/   298.    -1/T))

set label "" at    288.4500    ,   0.7130031E-05 point
set label "" at    288.4500    ,   0.7338511E-05 point
set label "" at    288.4500    ,   0.7338511E-05 point
set label "" at    293.2000    ,   0.5250635E-05 point
set label "" at    293.2000    ,   0.5332676E-05 point
set label "" at    298.1900    ,   0.3993080E-05 point
set label "" at    302.9500    ,   0.3116481E-05 point
set label "" at    302.9500    ,   0.3156182E-05 point
set label "" at    308.2000    ,   0.2567788E-05 point
set label "" at    307.9800    ,   0.2464181E-05 point
set label "" at    307.9800    ,   0.2632105E-05 point
set label "" at    298.1500    ,   0.4153889E-05 point ps 2 pt 6

plot [280:310] H(T)
