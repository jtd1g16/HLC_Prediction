# load "ref2471-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2471; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1794406E-02 * exp(  -4067.241    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3696342E-02 point
set label "" at    298.1500    ,   0.1794406E-02 point
set label "" at    298.1500    ,   0.1794406E-02 point ps 2 pt 6

plot [280:300] H(T)
