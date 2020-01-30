# load "ref1342-74-87-3.gnu"
# chem = "chloromethane"

set terminal postscript eps color
set title "ref = 1342; chem = chloromethane; casrn = 74-87-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1080300E-02 * exp(  -3046.105    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2751976E-02 point
set label "" at    295.1500    ,   0.1198517E-02 point
set label "" at    298.1500    ,   0.1080300E-02 point ps 2 pt 6

plot [270:300] H(T)
