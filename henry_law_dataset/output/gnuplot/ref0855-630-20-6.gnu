# load "ref0855-630-20-6.gnu"
# chem = "1,1,1,2-tetrachloroethane"

set terminal postscript eps color
set title "ref = 855; chem = 1,1,1,2-tetrachloroethane; casrn = 630-20-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3920516E-02 * exp(  -4807.205    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.5077239E-02 point
set label "" at    303.1500    ,   0.3109096E-02 point
set label "" at    313.1500    ,   0.1779483E-02 point
set label "" at    298.1500    ,   0.3920516E-02 point ps 2 pt 6

plot [290:320] H(T)
