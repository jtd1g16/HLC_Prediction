# load "ref0856-630-20-6.gnu"
# chem = "1,1,1,2-tetrachloroethane"

set terminal postscript eps color
set title "ref = 856; chem = 1,1,1,2-tetrachloroethane; casrn = 630-20-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4494459E-02 * exp(  -4562.263    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.5805431E-02 point
set label "" at    303.1500    ,   0.3524726E-02 point
set label "" at    308.1500    ,   0.2741454E-02 point
set label "" at    313.1500    ,   0.2145485E-02 point
set label "" at    298.1500    ,   0.4494459E-02 point ps 2 pt 6

plot [290:320] H(T)
