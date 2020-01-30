# load "ref0583-75-27-4.gnu"
# chem = "bromodichloromethane"

set terminal postscript eps color
set title "ref = 583; chem = bromodichloromethane; casrn = 75-27-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3916947E-02 * exp(  -4944.363    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1789903E-01 point
set label "" at    283.1500    ,   0.9397468E-02 point
set label "" at    293.1500    ,   0.5206546E-02 point
set label "" at    298.1500    ,   0.3916947E-02 point ps 2 pt 6

plot [270:300] H(T)
