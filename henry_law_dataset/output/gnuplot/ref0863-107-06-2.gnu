# load "ref0863-107-06-2.gnu"
# chem = "1,2-dichloroethane"

set terminal postscript eps color
set title "ref = 863; chem = 1,2-dichloroethane; casrn = 107-06-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8606952E-02 * exp(  -4388.145    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3390812E-01 point
set label "" at    283.1500    ,   0.1830041E-01 point
set label "" at    293.1500    ,   0.1079076E-01 point
set label "" at    303.1500    ,   0.6931218E-02 point
set label "" at    298.1500    ,   0.8606952E-02 point ps 2 pt 6

plot [270:310] H(T)
