# load "ref2905-156-59-2.gnu"
# chem = "($Z$)-1,2-dichloroethene"

set terminal postscript eps color
set title "ref = 2905; chem = ($Z$)-1,2-dichloroethene; casrn = 156-59-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1273768E-02 * exp(  -3090.674    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1798238E-02 point
set label "" at    298.1500    ,   0.1313715E-02 point
set label "" at    308.1500    ,   0.8954957E-03 point
set label "" at    298.1500    ,   0.1273768E-02 point ps 2 pt 6

plot [280:310] H(T)
