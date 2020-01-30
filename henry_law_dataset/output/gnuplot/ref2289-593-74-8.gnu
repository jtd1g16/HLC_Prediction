# load "ref2289-593-74-8.gnu"
# chem = "dimethylmercury"

set terminal postscript eps color
set title "ref = 2289; chem = dimethylmercury; casrn = 593-74-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1301276E-02 * exp(  -2650.088    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2935441E-02 point
set label "" at    298.1500    ,   0.1301276E-02 point
set label "" at    298.1500    ,   0.1301276E-02 point ps 2 pt 6

plot [270:300] H(T)
