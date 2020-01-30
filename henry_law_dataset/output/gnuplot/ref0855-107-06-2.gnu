# load "ref0855-107-06-2.gnu"
# chem = "1,2-dichloroethane"

set terminal postscript eps color
set title "ref = 855; chem = 1,2-dichloroethane; casrn = 107-06-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8538088E-02 * exp(  -3940.679    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1087267E-01 point
set label "" at    308.1500    ,   0.5373001E-02 point
set label "" at    323.1500    ,   0.3126662E-02 point
set label "" at    298.1500    ,   0.8538088E-02 point ps 2 pt 6

plot [290:330] H(T)
