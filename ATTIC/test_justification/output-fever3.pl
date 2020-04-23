[22:30] joaquin@:test_justification$ scasp --human fever3.pl
RENAMED_QUERY:	?- diagnosis(Pedro,A).

	ANSWER:	1 (in 0.304 ms)

JUSTIFICATION_TREE:
The diagnosis for Pedro with a temperature of 39 is that:
    Pedro has a fever
        
            The registered temperarture of Pedro is 39
        
            
        
            
                
                

MODEL:
diagnosis(Pedro,39) ,  fever(Pedro,39) ,  temp(Pedro,39) ,  reg_temp(Pedro,39) ,  high_temp(39) ,  not no_fever(Pedro,39)

BINDINGS: 
A is 39 ? ;

[22:30] joaquin@:test_justification$ scasp --human fever3.pl
RENAMED_QUERY:	?- diagnosis(Juan,A).

	ANSWER:	1 (in 0.908 ms)

JUSTIFICATION_TREE:
The diagnosis for Juan with a temperature of 37 is that:
    Juan has no fever
        
            The registered temperarture of Juan is 37
        
            
                
        
            
                
                

MODEL:
diagnosis(Juan,37) ,  no_fever(Juan,37) ,  temp(Juan,37) ,  reg_temp(Juan,37) ,  not high_temp(37) ,  not fever(Juan,37)

BINDINGS: 
A is 37 ? ;

[22:30] joaquin@:test_justification$ scasp --human fever3.pl
RENAMED_QUERY:	?- diagnosis(Luisa,A).

	ANSWER:	1 (in 0.756 ms)

JUSTIFICATION_TREE:
The diagnosis for Luisa with a temperature of A greater than 38 is that:
    Luisa has a fever
        
            There are no registerd temperature of Luisa
                
                    
                        
                            We do not have a registered temperature for Luisa of B
                                
                                    
                                
                                    
        
            
        
            
                
                    There are no registerd temperature of Luisa, as we saw before
                
                    

MODEL:
diagnosis(Luisa,A │{A#>38}) ,  fever(Luisa,A │{A#>38}) ,  temp(Luisa,A │{A#>38}) ,  not reg_person(Luisa) ,  not reg_temp(Luisa,B) ,  high_temp(A │{A#>38}) ,  not no_fever(Luisa,A │{A#>38}) ,  temp(Luisa,A │{A#>38}) ,  high_temp(A │{A#>38})

BINDINGS: 
A is A greater than 38 ? ;


	ANSWER:	2 (in 1.135 ms)

JUSTIFICATION_TREE:
The diagnosis for Luisa with a temperature of A less or equal 38 is that:
    Luisa has no fever
        
            There are no registerd temperature of Luisa
                
                    
                        
                            We do not have a registered temperature for Luisa of B
                                
                                    
                                
                                    
        
            
                
        
            
                
                    There are no registerd temperature of Luisa, as we saw before
                
                    
                        

MODEL:
diagnosis(Luisa,A │{A#=<38}) ,  no_fever(Luisa,A │{A#=<38}) ,  temp(Luisa,A │{A#=<38}) ,  not reg_person(Luisa) ,  not reg_temp(Luisa,B) ,  not high_temp(A │{A#=<38}) ,  not fever(Luisa,A │{A#=<38}) ,  temp(Luisa,A │{A#=<38}) ,  not high_temp(A │{A#=<38})

BINDINGS: 
A is A less or equal 38 ? ;

[22:30] joaquin@:test_justification$ 