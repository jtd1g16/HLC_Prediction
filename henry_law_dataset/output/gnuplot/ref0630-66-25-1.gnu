# load "ref0630-66-25-1.gnu"
# chem = "hexanal"

set terminal postscript eps color
set title "ref = 630; chem = hexanal; casrn = 66-25-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4854960E-01 * exp(  -6549.605    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.1569208     point
set label "" at    298.1500    ,   0.5132001E-01 point
set label "" at    303.1500    ,   0.3158154E-01 point
set label "" at    308.1500    ,   0.2269924E-01 point
set label "" at    318.1500    ,   0.1283000E-01 point
set label "" at    298.1500    ,   0.4854960E-01 point ps 2 pt 6

plot [280:320] H(T)
