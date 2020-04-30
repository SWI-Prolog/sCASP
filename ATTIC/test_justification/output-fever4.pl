-------------------- Example 1 --------------------------------------
RENAMED_QUERY:	?- diagnosis(Luisa,A).

	ANSWER:	1 (in 0.94 ms)

BEGIN HTML JUSTIFICATION and END

JUSTIFICATION_TREE:
The diagnosis of the patient Luisa is fever
    There is a fever for the patient Luisa with a temperature B greater than 38
        The temperature for the patient Luisa has a value B greater than 38
            There is no valid register for the patient Luisa
                First: There is no valid register for the patient Luisa
                    For any possible value:		 forall(E,forall(F,not o_reg_person.1(Luisa,E,F)))
                        For any possible value:		 forall(F,not o_reg_person.1(Luisa,C,F))
                            First: For this conclusion there is no text		 not reg_person(Luisa,C,D)
                                There is no register, for the patient Luisa, with any temperature, at any time point
                                    First: There is no register, for the patient Luisa, with any temperature, at any time point
                                        Luisa is not equal Juan
                                    Second: There is no register, for the patient Luisa, with any temperature, at any time point
                                        Luisa is not equal Pedro
                                    Third: There is no register, for the patient Luisa, with any temperature, at any time point
                                        Luisa is not equal Jose
                                    Then (4): There is no register, for the patient Luisa, with any temperature, at any time point
                                        Luisa is not equal Jose
        A temperature with a value B greater than 38 is high
            B is greater than 38
        For the patient Luisa with a temperature B greater than 38 we conclude that there is a fever
            First: For the patient Luisa with a temperature B greater than 38 we conclude that there is a fever
                The temperature for the patient Luisa has a value B greater than 38
                    There is no valid register for the patient Luisa, as we saw before
                A temperature with a value B greater than 38 is high
                    B is greater than 38
The global constraints hold
    There are no global constraints to be checked


MODEL:
diagnosis(Luisa,fever) ,  fever(Luisa,B │{B #> 38}) ,  temp(Luisa,B │{B #> 38}) ,  not reg_person(Luisa) ,  not reg_temp(Luisa,C,D) ,  high_temp(B │{B #> 38}) ,  not no_fever(Luisa,B │{B #> 38}) ,  temp(Luisa,B │{B #> 38}) ,  high_temp(B │{B #> 38})

BINDINGS: 
A is fever ? ;


	ANSWER:	2 (in 1.799 ms)

BEGIN HTML JUSTIFICATION and END

JUSTIFICATION_TREE:
The diagnosis of the patient Luisa is no fever
    For the patient Luisa with a temperature B less or equal 38 we conclude that there is no fever
        The temperature for the patient Luisa has a value B less or equal 38
            There is no valid register for the patient Luisa
                First: There is no valid register for the patient Luisa
                    For any possible value:		 forall(E,forall(F,not o_reg_person.1(Luisa,E,F)))
                        For any possible value:		 forall(F,not o_reg_person.1(Luisa,C,F))
                            First: For this conclusion there is no text		 not reg_person(Luisa,C,D)
                                There is no register, for the patient Luisa, with any temperature, at any time point
                                    First: There is no register, for the patient Luisa, with any temperature, at any time point
                                        Luisa is not equal Juan
                                    Second: There is no register, for the patient Luisa, with any temperature, at any time point
                                        Luisa is not equal Pedro
                                    Third: There is no register, for the patient Luisa, with any temperature, at any time point
                                        Luisa is not equal Jose
                                    Then (4): There is no register, for the patient Luisa, with any temperature, at any time point
                                        Luisa is not equal Jose
        A temperature with a value B less or equal 38 is not high
            First: A temperature with a value B less or equal 38 is not high
                B is less or equal 38
        There is no fever for the patient Luisa with a temperature B less or equal 38
            First: There is no fever for the patient Luisa with a temperature B less or equal 38
                The temperature for the patient Luisa has a value B less or equal 38
                    There is no valid register for the patient Luisa, as we saw before
                A temperature with a value B less or equal 38 is not high
                    First: A temperature with a value B less or equal 38 is not high
                        B is less or equal 38
The global constraints hold
    There are no global constraints to be checked


MODEL:
diagnosis(Luisa,no fever) ,  no_fever(Luisa,B │{B #=< 38}) ,  temp(Luisa,B │{B #=< 38}) ,  not reg_person(Luisa) ,  not reg_temp(Luisa,C,D) ,  not high_temp(B │{B #=< 38}) ,  not fever(Luisa,B │{B #=< 38}) ,  temp(Luisa,B │{B #=< 38}) ,  not high_temp(B │{B #=< 38})

BINDINGS: 
A is no fever ? ;

-------------------- Example 2 --------------------------------------
RENAMED_QUERY:	?- diagnosis(Juan,A).

	ANSWER:	1 (in 2.135 ms)

BEGIN HTML JUSTIFICATION and END

JUSTIFICATION_TREE:
The diagnosis of the patient Juan is no fever
    For the patient Juan with the temperature 37 we conclude that there is no fever
        The temperature for the patient Juan has the value 37
            There is a register, for the patient Juan, with the temperature 37, at the time point -2
            A register at the time point -2 is recent
                -2 is greater than -3
        A temperature with the value 37 is not high
            First: A temperature with the value 37 is not high
                37 is less or equal 38
        There is no fever for the patient Juan with the temperature 37
            First: There is no fever for the patient Juan with the temperature 37
                The temperature for the patient Juan has the value 37, as we saw before
                A temperature with the value 37 is not high, as we saw before
The global constraints hold
    There are no global constraints to be checked


MODEL:
diagnosis(Juan,no fever) ,  no_fever(Juan,37) ,  temp(Juan,37) ,  reg_temp(Juan,37,-2) ,  recent_reg(-2) ,  not high_temp(37) ,  not fever(Juan,37)

BINDINGS: 
A is no fever ? ;

-------------------- Example 3 --------------------------------------
RENAMED_QUERY:	?- diagnosis(Pedro,A).

	ANSWER:	1 (in 0.425 ms)

BEGIN HTML JUSTIFICATION and END

JUSTIFICATION_TREE:
The diagnosis of the patient Pedro is fever
    There is a fever for the patient Pedro with the temperature 39
        The temperature for the patient Pedro has the value 39
            There is a register, for the patient Pedro, with the temperature 39, at the time point -2
            A register at the time point -2 is recent
                -2 is greater than -3
        A temperature with the value 39 is high
            39 is greater than 38
        For the patient Pedro with the temperature 39 we conclude that there is a fever
            First: For the patient Pedro with the temperature 39 we conclude that there is a fever
                The temperature for the patient Pedro has the value 39, as we saw before
                A temperature with the value 39 is high, as we saw before
The global constraints hold
    There are no global constraints to be checked


MODEL:
diagnosis(Pedro,fever) ,  fever(Pedro,39) ,  temp(Pedro,39) ,  reg_temp(Pedro,39,-2) ,  recent_reg(-2) ,  high_temp(39) ,  not no_fever(Pedro,39)

BINDINGS: 
A is fever ? ;

-------------------- Example 4 --------------------------------------
RENAMED_QUERY:	?- diagnosis(Jose,A).

	ANSWER:	1 (in 5.713 ms)

BEGIN HTML JUSTIFICATION and END

JUSTIFICATION_TREE:
The diagnosis of the patient Jose is fever
    There is a fever for the patient Jose with a temperature B greater than 38
        The temperature for the patient Jose has a value B greater than 38
            There is no valid register for the patient Jose
                First: There is no valid register for the patient Jose
                    For any possible value:		 forall(G,forall(H,not o_reg_person.1(Jose,G,H)))
                        For any possible value:		 forall(H,not o_reg_person.1(Jose,C │{C \= 37,C \= 39},H))
                            First: For this conclusion there is no text		 not reg_person(Jose,C │{C \= 37,C \= 39},D)
                                There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                    First: There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                        Jose is not equal Juan
                                    Second: There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                        Jose is not equal Pedro
                                    Third: There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                        Jose is Jose
                                        C is not equal 39
                                    Then (4): There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                        Jose is Jose
                                        C is not equal 37
                        For any possible value:		 forall(H,not o_reg_person.1(Jose,37,H))
                            First: For this conclusion there is no text		 not reg_person(Jose,37,E │{E \= -6})
                                There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                    First: There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                        Jose is not equal Juan
                                    Second: There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                        Jose is not equal Pedro
                                    Third: There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                        Jose is Jose
                                        37 is not equal 39
                                    Then (4): There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                        Jose is Jose
                                        37 is 37
                                        E is not equal -6
                            First: For this conclusion there is no text		 not reg_person(Jose,37,-6)
                                There is a register, for the patient Jose, with the temperature 37, at the time point -6
                                A register at the time point -6 is not recent
                                    First: A register at the time point -6 is not recent
                                        -6 is less or equal -3
                        For any possible value:		 forall(H,not o_reg_person.1(Jose,39,H))
                            First: For this conclusion there is no text		 not reg_person(Jose,39,F │{F \= -4})
                                There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                    First: There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                        Jose is not equal Juan
                                    Second: There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                        Jose is not equal Pedro
                                    Third: There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                        Jose is Jose
                                        39 is 39
                                        F is not equal -4
                                    Then (4): There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                        Jose is Jose
                                        39 is not equal 37
                            First: For this conclusion there is no text		 not reg_person(Jose,39,-4)
                                There is a register, for the patient Jose, with the temperature 39, at the time point -4
                                A register at the time point -4 is not recent
                                    First: A register at the time point -4 is not recent
                                        -4 is less or equal -3
        A temperature with a value B greater than 38 is high
            B is greater than 38
        For the patient Jose with a temperature B greater than 38 we conclude that there is a fever
            First: For the patient Jose with a temperature B greater than 38 we conclude that there is a fever
                The temperature for the patient Jose has a value B greater than 38
                    There is no valid register for the patient Jose, as we saw before
                A temperature with a value B greater than 38 is high
                    B is greater than 38
The global constraints hold
    There are no global constraints to be checked


MODEL:
diagnosis(Jose,fever) ,  fever(Jose,B │{B #> 38}) ,  temp(Jose,B │{B #> 38}) ,  not reg_person(Jose) ,  not reg_temp(Jose,C │{C \= 37,C \= 39},D) ,  not reg_temp(Jose,37,E │{E \= -6}) ,  reg_temp(Jose,37,-6) ,  not recent_reg(-6) ,  not reg_temp(Jose,39,F │{F \= -4}) ,  reg_temp(Jose,39,-4) ,  not recent_reg(-4) ,  high_temp(B │{B #> 38}) ,  not no_fever(Jose,B │{B #> 38}) ,  temp(Jose,B │{B #> 38}) ,  high_temp(B │{B #> 38})

BINDINGS: 
A is fever ? ;


	ANSWER:	2 (in 7.11 ms)

BEGIN HTML JUSTIFICATION and END

JUSTIFICATION_TREE:
The diagnosis of the patient Jose is no fever
    For the patient Jose with a temperature B less or equal 38 we conclude that there is no fever
        The temperature for the patient Jose has a value B less or equal 38
            There is no valid register for the patient Jose
                First: There is no valid register for the patient Jose
                    For any possible value:		 forall(G,forall(H,not o_reg_person.1(Jose,G,H)))
                        For any possible value:		 forall(H,not o_reg_person.1(Jose,C │{C \= 37,C \= 39},H))
                            First: For this conclusion there is no text		 not reg_person(Jose,C │{C \= 37,C \= 39},D)
                                There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                    First: There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                        Jose is not equal Juan
                                    Second: There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                        Jose is not equal Pedro
                                    Third: There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                        Jose is Jose
                                        C is not equal 39
                                    Then (4): There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                        Jose is Jose
                                        C is not equal 37
                        For any possible value:		 forall(H,not o_reg_person.1(Jose,37,H))
                            First: For this conclusion there is no text		 not reg_person(Jose,37,E │{E \= -6})
                                There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                    First: There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                        Jose is not equal Juan
                                    Second: There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                        Jose is not equal Pedro
                                    Third: There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                        Jose is Jose
                                        37 is not equal 39
                                    Then (4): There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                        Jose is Jose
                                        37 is 37
                                        E is not equal -6
                            First: For this conclusion there is no text		 not reg_person(Jose,37,-6)
                                There is a register, for the patient Jose, with the temperature 37, at the time point -6
                                A register at the time point -6 is not recent
                                    First: A register at the time point -6 is not recent
                                        -6 is less or equal -3
                        For any possible value:		 forall(H,not o_reg_person.1(Jose,39,H))
                            First: For this conclusion there is no text		 not reg_person(Jose,39,F │{F \= -4})
                                There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                    First: There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                        Jose is not equal Juan
                                    Second: There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                        Jose is not equal Pedro
                                    Third: There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                        Jose is Jose
                                        39 is 39
                                        F is not equal -4
                                    Then (4): There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                        Jose is Jose
                                        39 is not equal 37
                            First: For this conclusion there is no text		 not reg_person(Jose,39,-4)
                                There is a register, for the patient Jose, with the temperature 39, at the time point -4
                                A register at the time point -4 is not recent
                                    First: A register at the time point -4 is not recent
                                        -4 is less or equal -3
        A temperature with a value B less or equal 38 is not high
            First: A temperature with a value B less or equal 38 is not high
                B is less or equal 38
        There is no fever for the patient Jose with a temperature B less or equal 38
            First: There is no fever for the patient Jose with a temperature B less or equal 38
                The temperature for the patient Jose has a value B less or equal 38
                    There is no valid register for the patient Jose, as we saw before
                A temperature with a value B less or equal 38 is not high
                    First: A temperature with a value B less or equal 38 is not high
                        B is less or equal 38
The global constraints hold
    There are no global constraints to be checked


MODEL:
diagnosis(Jose,no fever) ,  no_fever(Jose,B │{B #=< 38}) ,  temp(Jose,B │{B #=< 38}) ,  not reg_person(Jose) ,  not reg_temp(Jose,C │{C \= 37,C \= 39},D) ,  not reg_temp(Jose,37,E │{E \= -6}) ,  reg_temp(Jose,37,-6) ,  not recent_reg(-6) ,  not reg_temp(Jose,39,F │{F \= -4}) ,  reg_temp(Jose,39,-4) ,  not recent_reg(-4) ,  not high_temp(B │{B #=< 38}) ,  not fever(Jose,B │{B #=< 38}) ,  temp(Jose,B │{B #=< 38}) ,  not high_temp(B │{B #=< 38})

BINDINGS: 
A is no fever ? ;




    ---------------------- Example human_short -----------------
scasp --human_short --html fever4.pl
RENAMED_QUERY:	?- diagnosis(Jose,A).

	ANSWER:	1 (in 7.656 ms)

BEGIN HTML JUSTIFICATION and END

JUSTIFICATION_TREE:
The diagnosis of the patient Jose is fever
            It is consider that the patient Jose has a temperature B greater than 38
                There is no valid register for the patient Jose
                                                There is no register, for the patient Jose, with a temperature C not equal 37, and not equal 39, at any time point
                                                There is no register, for the patient Jose, with the temperature 37, at a time point E not equal -6
                                                There is a register, for the patient Jose, with the temperature 37, at the time point -6
                                                A register at the time point -6 is not recent
                                                There is no register, for the patient Jose, with the temperature 39, at a time point F not equal -4
                                                There is a register, for the patient Jose, with the temperature 39, at the time point -4
                                                A register at the time point -4 is not recent
        It is high a temperature B greater than 38
The global constraints hold
                The diagnosis of the patient Pedro is fever
                            It is known that the patient Pedro has the temperature 39
                                There is a valid register for the patient Pedro with the temperature 39
                                    There is a register, for the patient Pedro, with the temperature 39, at the time point -2
                                    A register at the time point -2 is recent
                        It is high the temperature 39

MODEL:
diagnosis(Jose,fever) ,  temp(Jose,B │{B #> 38}) ,  reg_temp(Jose,37,-6) ,  reg_temp(Jose,39,-4) ,  diagnosis(Pedro,fever) ,  temp(Pedro,39) ,  reg_temp(Pedro,39,-2)

BINDINGS: 
A is fever ? 