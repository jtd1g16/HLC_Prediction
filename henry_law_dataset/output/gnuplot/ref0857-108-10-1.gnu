# load "ref0857-108-10-1.gnu"
# chem = "4-methyl-2-pentanone"

set terminal postscript eps color
set title "ref = 857; chem = 4-methyl-2-pentanone; casrn = 108-10-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4266469E-01 * exp(  -4569.070    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.2085515E-01 point
set label "" at    333.1500    ,   0.8231157E-02 point
set label "" at    343.1500    ,   0.5678019E-02 point
set label "" at    353.1500    ,   0.4018728E-02 point
set label "" at    298.1500    ,   0.4266469E-01 point ps 2 pt 6

plot [310:360] H(T)
