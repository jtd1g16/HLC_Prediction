# load "ref2997-111-70-6.gnu"
# chem = "1-heptanol"

set terminal postscript eps color
set title "ref = 2997; chem = 1-heptanol; casrn = 111-70-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3641246     * exp(  -6325.799    *(1/   298.    -1/T))

set label "" at    323.1500    ,   0.7042254E-01 point
set label "" at    333.1500    ,   0.3984064E-01 point
set label "" at    343.1500    ,   0.2197802E-01 point
set label "" at    353.1500    ,   0.1345895E-01 point
set label "" at    363.1500    ,   0.8196721E-02 point
set label "" at    298.1500    ,   0.3641246     point ps 2 pt 6

plot [320:370] H(T)
