# load "ref0856-107-06-2.gnu"
# chem = "1,2-dichloroethane"

set terminal postscript eps color
set title "ref = 856; chem = 1,2-dichloroethane; casrn = 107-06-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8042650E-02 * exp(  -3598.658    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.9869233E-02 point
set label "" at    303.1500    ,   0.6579488E-02 point
set label "" at    308.1500    ,   0.5482907E-02 point
set label "" at    313.1500    ,   0.4486015E-02 point
set label "" at    298.1500    ,   0.8042650E-02 point ps 2 pt 6

plot [290:320] H(T)
