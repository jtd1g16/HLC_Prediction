# load "ref2999-111-27-3.gnu"
# chem = "1-hexanol"

set terminal postscript eps color
set title "ref = 2999; chem = 1-hexanol; casrn = 111-27-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5718961     * exp(  -7251.091    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.5882353     point
set label "" at    305.5500    ,   0.3225806     point
set label "" at    323.1500    ,   0.7874016E-01 point
set label "" at    343.1500    ,   0.2493766E-01 point
set label "" at    298.1500    ,   0.5718961     point ps 2 pt 6

plot [290:350] H(T)
