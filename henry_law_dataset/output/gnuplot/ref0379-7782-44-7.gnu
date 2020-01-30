# load "ref0379-7782-44-7.gnu"
# chem = "oxygen"

set terminal postscript eps color
set title "ref = 379; chem = oxygen; casrn = 7782-44-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1242703E-04 * exp(  -1726.113    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2152706E-04 point
set label "" at    274.1500    ,   0.2095024E-04 point
set label "" at    275.1500    ,   0.2039985E-04 point
set label "" at    276.1500    ,   0.1986707E-04 point
set label "" at    277.1500    ,   0.1936070E-04 point
set label "" at    278.1500    ,   0.1887635E-04 point
set label "" at    279.1500    ,   0.1840522E-04 point
set label "" at    280.1500    ,   0.1796490E-04 point
set label "" at    281.1500    ,   0.1753779E-04 point
set label "" at    282.1500    ,   0.1713270E-04 point
set label "" at    283.1500    ,   0.1674082E-04 point
set label "" at    284.1500    ,   0.1637096E-04 point
set label "" at    285.1500    ,   0.1601430E-04 point
set label "" at    286.1500    ,   0.1567085E-04 point
set label "" at    287.1500    ,   0.1534942E-04 point
set label "" at    288.1500    ,   0.1503680E-04 point
set label "" at    289.1500    ,   0.1474179E-04 point
set label "" at    290.1500    ,   0.1445558E-04 point
set label "" at    291.1500    ,   0.1417818E-04 point
set label "" at    292.1500    ,   0.1391839E-04 point
set label "" at    293.1500    ,   0.1365861E-04 point
set label "" at    294.1500    ,   0.1340322E-04 point
set label "" at    295.1500    ,   0.1315665E-04 point
set label "" at    296.1500    ,   0.1291888E-04 point
set label "" at    297.1500    ,   0.1268551E-04 point
set label "" at    298.1500    ,   0.1246535E-04 point
set label "" at    299.1500    ,   0.1225400E-04 point
set label "" at    300.1500    ,   0.1204705E-04 point
set label "" at    301.1500    ,   0.1184891E-04 point
set label "" at    302.1500    ,   0.1166398E-04 point
set label "" at    303.1500    ,   0.1148345E-04 point
set label "" at    298.1500    ,   0.1242703E-04 point ps 2 pt 6

plot [270:310] H(T)
