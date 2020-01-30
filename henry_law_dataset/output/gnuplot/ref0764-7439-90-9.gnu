# load "ref0764-7439-90-9.gnu"
# chem = "krypton"

set terminal postscript eps color
set title "ref = 764; chem = krypton; casrn = 7439-90-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2432887E-04 * exp(  -1453.001    *(1/   298.    -1/T))

set label "" at    279.7500    ,   0.3599761E-04 point
set label "" at    281.1500    ,   0.3485622E-04 point
set label "" at    285.7500    ,   0.3051017E-04 point
set label "" at    288.6500    ,   0.2879809E-04 point
set label "" at    291.3500    ,   0.2708601E-04 point
set label "" at    294.3500    ,   0.2554952E-04 point
set label "" at    297.8500    ,   0.2348624E-04 point
set label "" at    303.3500    ,   0.2146687E-04 point
set label "" at    308.5500    ,   0.1944749E-04 point
set label "" at    314.5500    ,   0.1782321E-04 point
set label "" at    319.1500    ,   0.1681352E-04 point
set label "" at    325.0500    ,   0.1575993E-04 point
set label "" at    331.0500    ,   0.1461854E-04 point
set label "" at    332.9500    ,   0.1439904E-04 point
set label "" at    337.8500    ,   0.1387225E-04 point
set label "" at    340.6500    ,   0.1347715E-04 point
set label "" at    344.6500    ,   0.1303816E-04 point
set label "" at    347.0500    ,   0.1295036E-04 point
set label "" at    348.0500    ,   0.1286256E-04 point
set label "" at    298.1500    ,   0.2432887E-04 point ps 2 pt 6

plot [270:350] H(T)
