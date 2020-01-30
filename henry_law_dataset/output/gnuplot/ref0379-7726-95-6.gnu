# load "ref0379-7726-95-6.gnu"
# chem = "molecular bromine"

set terminal postscript eps color
set title "ref = 379; chem = molecular bromine; casrn = 7726-95-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7475481E-02 * exp(  -4097.824    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2663913E-01 point
set label "" at    275.1500    ,   0.2382110E-01 point
set label "" at    277.1500    ,   0.2126727E-01 point
set label "" at    279.1500    ,   0.1906569E-01 point
set label "" at    281.1500    ,   0.1712830E-01 point
set label "" at    283.1500    ,   0.1545510E-01 point
set label "" at    285.1500    ,   0.1386996E-01 point
set label "" at    287.1500    ,   0.1250498E-01 point
set label "" at    289.1500    ,   0.1131613E-01 point
set label "" at    291.1500    ,   0.1030340E-01 point
set label "" at    293.1500    ,   0.9378734E-02 point
set label "" at    295.1500    ,   0.8542134E-02 point
set label "" at    297.1500    ,   0.7793596E-02 point
set label "" at    299.1500    ,   0.7177154E-02 point
set label "" at    301.1500    ,   0.6604743E-02 point
set label "" at    303.1500    ,   0.6076363E-02 point
set label "" at    298.1500    ,   0.7475481E-02 point ps 2 pt 6

plot [270:310] H(T)
