# load "ref1943-7726-95-6.gnu"
# chem = "bromine (molecular)"

set terminal postscript eps color
set title "ref = 1943; chem = bromine (molecular); casrn = 7726-95-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8284270E-02 * exp(  -4126.514    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3020000E-01 point
set label "" at    293.1500    ,   0.1000000E-01 point
set label "" at    323.1500    ,   0.2900000E-02 point
set label "" at    298.1500    ,   0.8284270E-02 point ps 2 pt 6

plot [270:330] H(T)
