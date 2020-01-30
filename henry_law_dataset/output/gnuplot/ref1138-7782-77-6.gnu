# load "ref1138-7782-77-6.gnu"
# chem = "nitrous acid"

set terminal postscript eps color
set title "ref = 1138; chem = nitrous acid; casrn = 7782-77-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3658797     * exp(  -9003.734    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.3658797     point
set label "" at    288.1500    ,    1.043487     point
set label "" at    298.1500    ,   0.3658797     point ps 2 pt 6

plot [280:300] H(T)
