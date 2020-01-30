# load "ref1342-74-88-4.gnu"
# chem = "iodomethane"

set terminal postscript eps color
set title "ref = 1342; chem = iodomethane; casrn = 74-88-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1992530E-02 * exp(  -3744.898    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.6290231E-02 point
set label "" at    295.1500    ,   0.2263865E-02 point
set label "" at    298.1500    ,   0.1992530E-02 point ps 2 pt 6

plot [270:300] H(T)
