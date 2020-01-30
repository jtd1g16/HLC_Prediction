# load "ref0863-75-34-3.gnu"
# chem = "1,1-dichloroethane"

set terminal postscript eps color
set title "ref = 863; chem = 1,1-dichloroethane; casrn = 75-34-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1831541E-02 * exp(  -4353.440    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.7100759E-02 point
set label "" at    283.1500    ,   0.3909406E-02 point
set label "" at    293.1500    ,   0.2283811E-02 point
set label "" at    303.1500    ,   0.1476000E-02 point
set label "" at    298.1500    ,   0.1831541E-02 point ps 2 pt 6

plot [270:310] H(T)
