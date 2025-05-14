Failų turinys ir paskirtis:

**1 Q duomenų konvertavimas į M** - išbandyti ir palyginti įvairūs duomenų dažnio keitimo būdai. Tikslas - modeliavime naudoti vienodą kiekį tos pačios struktūros (duomenų dažnio) stebėjimų.
**2 pradinė analizė (sklaida, sezoniškumas, koreliacija)** - sugeneruotos rodiklių duomenų kitimo dinamikos vizualizacijos, jų tikslas - susipažinti su duomenimis prieš modeliavimą. Taip pat, atliktas sezoniškumo komponentės šalinimas ir sugeneruota koreliacijos tarp tyrime naudotų rodiklių matrica.
**3 modeliavimas su tiesine regresija (1-3 modeliai)** - tiesinės regresijos modelių įvertinimas ir prognozių palyginimas.
**4 modeliavimas su kitomis regresijomis (4-12 modeliai)** - Ridge, LASSO, Elastic Net modelių įvertinimas ir prognozių palyginimas.
**KOREKCIJOS 1 modelio** - išbandytos pradinio tiesinės regresijos modelio liekanų autokoreliacijos korekcijos.
**KOREKCIJOS 3 modelio** - išbandytos tiesinės regresijos su BVP komponente liekanų autokoreliacijos korekcijos.
**geriausias modelis** - atskirame faile atkurtas geriausias (išrinktas) modelis, jo rezultatai. Taip pat, sugeneruotos vizualizacijos, kurios panaudotos baigiamajame darbe.

**pradiniai_duomenys** - atskiruose Excel failo lapuose pateikti duomenys, naudoti tyrimo metu. Kiekvienas lapas žymi atskirų analizės etapų rezultatus (ORIG_MĖN - originalūs mėnesio dažnio rodiklių duomenys, ORIG_KETV - originalūs ketvirčio dažnio rodiklių duomenys, KOREG_KETV - rezultatai, gauti pakeitus originalių ketvirčio dažnio rodiklių duomenis į mėnesio dažnio duomenis, SUJUNGTA - prie originalių mėnesio dažnio rodiklių duomenų prijungti transformuoti (KOREG_KETV) duomenys, BE SEZONIŠKUMO - duomenys, gauti atlikus sezoniškumo komponentės šalinimą. Šie duomenys naudoti modeliavimo metu).
