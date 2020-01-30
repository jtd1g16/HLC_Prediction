# load "ref2445-76-38-0.gnu"
# chem = "2,2-dichloro-1,1-difluoro-1-methoxyethane"

set terminal postscript eps color
set title "ref = 2445; chem = 2,2-dichloro-1,1-difluoro-1-methoxyethane; casrn = 76-38-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2754663E-02 * exp(  -3314.043    *(1/   298.    -1/T))

set label "" at    281.1500    ,   0.5138490E-02 point
set label "" at    293.1500    ,   0.3557755E-02 point
set label "" at    303.1500    ,   0.2412933E-02 point
set label "" at    310.1500    ,   0.1673201E-02 point
set label "" at    298.1500    ,   0.2754663E-02 point ps 2 pt 6

plot [280:320] H(T)
