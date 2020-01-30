# load "ref0583-74-87-3.gnu"
# chem = "chloromethane"

set terminal postscript eps color
set title "ref = 583; chem = chloromethane; casrn = 74-87-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9287376E-03 * exp(  -3271.199    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2548126E-02 point
set label "" at    276.1500    ,   0.2202998E-02 point
set label "" at    279.1500    ,   0.1970060E-02 point
set label "" at    298.1500    ,   0.9287376E-03 point ps 2 pt 6

plot [270:280] H(T)
