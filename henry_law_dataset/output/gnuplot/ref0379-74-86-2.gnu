# load "ref0379-74-86-2.gnu"
# chem = "ethyne"

set terminal postscript eps color
set title "ref = 379; chem = ethyne; casrn = 74-86-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4069194E-03 * exp(  -1981.664    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.7617470E-03 point
set label "" at    274.1500    ,   0.7397312E-03 point
set label "" at    275.1500    ,   0.7177154E-03 point
set label "" at    276.1500    ,   0.6956996E-03 point
set label "" at    277.1500    ,   0.6736837E-03 point
set label "" at    278.1500    ,   0.6560711E-03 point
set label "" at    279.1500    ,   0.6384585E-03 point
set label "" at    280.1500    ,   0.6208458E-03 point
set label "" at    281.1500    ,   0.6032332E-03 point
set label "" at    282.1500    ,   0.5900237E-03 point
set label "" at    283.1500    ,   0.5768142E-03 point
set label "" at    284.1500    ,   0.5592015E-03 point
set label "" at    285.1500    ,   0.5459921E-03 point
set label "" at    286.1500    ,   0.5327826E-03 point
set label "" at    287.1500    ,   0.5195731E-03 point
set label "" at    288.1500    ,   0.5063636E-03 point
set label "" at    289.1500    ,   0.4975573E-03 point
set label "" at    290.1500    ,   0.4843478E-03 point
set label "" at    291.1500    ,   0.4755415E-03 point
set label "" at    292.1500    ,   0.4623320E-03 point
set label "" at    293.1500    ,   0.4535257E-03 point
set label "" at    294.1500    ,   0.4447193E-03 point
set label "" at    295.1500    ,   0.4359130E-03 point
set label "" at    296.1500    ,   0.4271067E-03 point
set label "" at    297.1500    ,   0.4183004E-03 point
set label "" at    298.1500    ,   0.4094940E-03 point
set label "" at    299.1500    ,   0.4006877E-03 point
set label "" at    300.1500    ,   0.3918814E-03 point
set label "" at    301.1500    ,   0.3830751E-03 point
set label "" at    302.1500    ,   0.3742687E-03 point
set label "" at    303.1500    ,   0.3698656E-03 point
set label "" at    298.1500    ,   0.4069194E-03 point ps 2 pt 6

plot [270:310] H(T)
