# load "ref2905-156-60-5.gnu"
# chem = "($E$)-1,2-dichloroethene"

set terminal postscript eps color
set title "ref = 2905; chem = ($E$)-1,2-dichloroethene; casrn = 156-60-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1793266E-02 * exp(  -6229.146    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.4043672E-02 point
set label "" at    298.1500    ,   0.1494545E-02 point
set label "" at    308.1500    ,   0.1000200E-02 point
set label "" at    298.1500    ,   0.1793266E-02 point ps 2 pt 6

plot [280:310] H(T)
