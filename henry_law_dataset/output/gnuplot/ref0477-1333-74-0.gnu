# load "ref0477-1333-74-0.gnu"
# chem = "hydrogen"

set terminal postscript eps color
set title "ref = 477; chem = hydrogen; casrn = 1333-74-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7745636E-05 * exp(  -496.7687    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.8247351E-05 point
set label "" at    293.1500    ,   0.7946951E-05 point
set label "" at    298.1500    ,   0.7706631E-05 point
set label "" at    303.1500    ,   0.7520929E-05 point
set label "" at    308.1500    ,   0.7373459E-05 point
set label "" at    298.1500    ,   0.7745636E-05 point ps 2 pt 6

plot [280:310] H(T)
