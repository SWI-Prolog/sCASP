-------------------- Example 1 --------------------------------------
RENAMED_QUERY:	?- diagnosis(Luisa,A).

	ANSWER:	1 (in 1.134 ms)

JUSTIFICATION_TREE:
The diagnosis for Luisa with a temperature of A greater than 38 is that:
    Luisa has a fever
        
            There are no valid registerd temperature of Luisa
                
                    
                        
                            
                                We do not have a registered temperature for Luisa of B at time C
                                    
                                        
                                    
                                        
                                    
                                        
                                    
                                        
        
            
        
            
                
                    There are no valid registerd temperature of Luisa, as we saw before
                
                    

MODEL:
diagnosis(Luisa,A │{A#>38}) ,  fever(Luisa,A │{A#>38}) ,  temp(Luisa,A │{A#>38}) ,  not reg_person(Luisa) ,  not reg_temp(Luisa,B,C) ,  high_temp(A │{A#>38}) ,  not no_fever(Luisa,A │{A#>38}) ,  temp(Luisa,A │{A#>38}) ,  high_temp(A │{A#>38})

BINDINGS: 
A is A greater than 38 ? ;


	ANSWER:	2 (in 1.993 ms)

JUSTIFICATION_TREE:
The diagnosis for Luisa with a temperature of A less or equal 38 is that:
    Luisa has no fever
        
            There are no valid registerd temperature of Luisa
                
                    
                        
                            
                                We do not have a registered temperature for Luisa of B at time C
                                    
                                        
                                    
                                        
                                    
                                        
                                    
                                        
        
            
                
        
            
                
                    There are no valid registerd temperature of Luisa, as we saw before
                
                    
                        

MODEL:
diagnosis(Luisa,A │{A#=<38}) ,  no_fever(Luisa,A │{A#=<38}) ,  temp(Luisa,A │{A#=<38}) ,  not reg_person(Luisa) ,  not reg_temp(Luisa,B,C) ,  not high_temp(A │{A#=<38}) ,  not fever(Luisa,A │{A#=<38}) ,  temp(Luisa,A │{A#=<38}) ,  not high_temp(A │{A#=<38})

BINDINGS: 
A is A less or equal 38 ? ;

-------------------- Example 2 --------------------------------------
RENAMED_QUERY:	?- diagnosis(Juan,A).

	ANSWER:	1 (in 2.138 ms)

JUSTIFICATION_TREE:
The diagnosis for Juan with a temperature of 37 is that:
    Juan has no fever
        
            The registered temperatured is valid. -2 is greater than -3
                
            The registered temperarture of Juan is 37 at time -2
        
            
                
        
            
                
                

MODEL:
diagnosis(Juan,37) ,  no_fever(Juan,37) ,  temp(Juan,37) ,  recent_reg(-2) ,  reg_temp(Juan,37,-2) ,  not high_temp(37) ,  not fever(Juan,37)

BINDINGS: 
A is 37 ? ;

-------------------- Example 3 --------------------------------------
RENAMED_QUERY:	?- diagnosis(Pedro,A).

	ANSWER:	1 (in 0.478 ms)

JUSTIFICATION_TREE:
The diagnosis for Pedro with a temperature of 39 is that:
    Pedro has a fever
        
            The registered temperatured is valid. -2 is greater than -3
                
            The registered temperarture of Pedro is 39 at time -2
        
            
        
            
                
                

MODEL:
diagnosis(Pedro,39) ,  fever(Pedro,39) ,  temp(Pedro,39) ,  recent_reg(-2) ,  reg_temp(Pedro,39,-2) ,  high_temp(39) ,  not no_fever(Pedro,39)

BINDINGS: 
A is 39 ? ;

-------------------- Example 4 --------------------------------------
RENAMED_QUERY:	?- diagnosis(Jose,A).

	ANSWER:	1 (in 6.055 ms)

JUSTIFICATION_TREE:
The diagnosis for Jose with a temperature of A greater than 38 is that:
    Jose has a fever
        
            There are no valid registerd temperature of Jose
                
                    
                        
                            
                                We do not have a registered temperature for Jose of B not equal 37, and not equal 39 at time C
                                    
                                        
                                    
                                        
                                    
                                        
                                        
                                    
                                        
                                        
                        
                            
                                We do not have a registered temperature for Jose of 37 at time D not equal -6
                                    
                                        
                                    
                                        
                                    
                                        
                                        
                                    
                                        
                                        
                                        
                            
                                The registered temperarture of Jose is 37 at time -6
                                The registered temperatured has expired. -6 is not greater than -3
                                    
                                        
                        
                            
                                We do not have a registered temperature for Jose of 39 at time E not equal -4
                                    
                                        
                                    
                                        
                                    
                                        
                                        
                                        
                                    
                                        
                                        
                            
                                The registered temperarture of Jose is 39 at time -4
                                The registered temperatured has expired. -4 is not greater than -3
                                    
                                        
        
            
        
            
                
                    There are no valid registerd temperature of Jose, as we saw before
                
                    

MODEL:
diagnosis(Jose,A │{A#>38}) ,  fever(Jose,A │{A#>38}) ,  temp(Jose,A │{A#>38}) ,  not reg_person(Jose) ,  not reg_temp(Jose,B │{B\=37,B\=39},C) ,  not reg_temp(Jose,37,D │{D\= -6}) ,  reg_temp(Jose,37,-6) ,  not recent_reg(-6) ,  not reg_temp(Jose,39,E │{E\= -4}) ,  reg_temp(Jose,39,-4) ,  not recent_reg(-4) ,  high_temp(A │{A#>38}) ,  not no_fever(Jose,A │{A#>38}) ,  temp(Jose,A │{A#>38}) ,  high_temp(A │{A#>38})

BINDINGS: 
A is A greater than 38 ? ;


	ANSWER:	2 (in 7.286 ms)

JUSTIFICATION_TREE:
The diagnosis for Jose with a temperature of A less or equal 38 is that:
    Jose has no fever
        
            There are no valid registerd temperature of Jose
                
                    
                        
                            
                                We do not have a registered temperature for Jose of B not equal 37, and not equal 39 at time C
                                    
                                        
                                    
                                        
                                    
                                        
                                        
                                    
                                        
                                        
                        
                            
                                We do not have a registered temperature for Jose of 37 at time D not equal -6
                                    
                                        
                                    
                                        
                                    
                                        
                                        
                                    
                                        
                                        
                                        
                            
                                The registered temperarture of Jose is 37 at time -6
                                The registered temperatured has expired. -6 is not greater than -3
                                    
                                        
                        
                            
                                We do not have a registered temperature for Jose of 39 at time E not equal -4
                                    
                                        
                                    
                                        
                                    
                                        
                                        
                                        
                                    
                                        
                                        
                            
                                The registered temperarture of Jose is 39 at time -4
                                The registered temperatured has expired. -4 is not greater than -3
                                    
                                        
        
            
                
        
            
                
                    There are no valid registerd temperature of Jose, as we saw before
                
                    
                        

MODEL:
diagnosis(Jose,A │{A#=<38}) ,  no_fever(Jose,A │{A#=<38}) ,  temp(Jose,A │{A#=<38}) ,  not reg_person(Jose) ,  not reg_temp(Jose,B │{B\=37,B\=39},C) ,  not reg_temp(Jose,37,D │{D\= -6}) ,  reg_temp(Jose,37,-6) ,  not recent_reg(-6) ,  not reg_temp(Jose,39,E │{E\= -4}) ,  reg_temp(Jose,39,-4) ,  not recent_reg(-4) ,  not high_temp(A │{A#=<38}) ,  not fever(Jose,A │{A#=<38}) ,  temp(Jose,A │{A#=<38}) ,  not high_temp(A │{A#=<38})

BINDINGS: 
A is A less or equal 38 ? ;

