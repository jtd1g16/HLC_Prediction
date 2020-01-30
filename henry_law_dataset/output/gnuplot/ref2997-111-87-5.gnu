# load "ref2997-111-87-5.gnu"
# chem = "1-octanol"

set terminal postscript eps color
set title "ref = 2997; chem = 1-octanol; casrn = 111-87-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1939790     * exp(  -5961.478    *(1/   298.    -1/T))

set label "" at    323.1500    ,   0.3906250E-01 point
set label "" at    333.1500    ,   0.2493766E-01 point
set label "" at    343.1500    ,   0.1488095E-01 point
set label "" at    353.1500    ,   0.8403361E-02 point
set label "" at    363.1500    ,   0.5291005E-02 point
set label "" at    298.1500    ,   0.1939790     point ps 2 pt 6

plot [320:370] H(T)
