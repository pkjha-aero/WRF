
  �   [   k820309    a          16.0        -\|[                                                                                                           
       module_diag_afwa_hail.f90 MODULE_DIAG_AFWA_HAIL #         @                                                       #TCA    #H1D    #HT    #PA    #RHO1D    #RA    #QI1D 	   #QC1D 
   #QR1D    #QS1D    #QG1D    #NG1D    #VUU    #WDUR    #NZ    #DHAIL1    #DHAIL2    #DHAIL3    #DHAIL4    #DHAIL5             
@ @                                                   	    p          5 � p        r        5 � p        r                               
                                                      	    p          5 � p        r        5 � p        r                                
                                       	               
@ @                                                   	    p          5 � p        r        5 � p        r                               
                                                      	    p          5 � p        r        5 � p        r                               
@ @                                                   	    p          5 � p        r        5 � p        r                               
                                  	                    	    p          5 � p        r        5 � p        r                               
                                  
                    	    p          5 � p        r        5 � p        r                               
                                                      	 	   p          5 � p        r        5 � p        r                               
                                                      	 
   p          5 � p        r        5 � p        r                               
                                                      	    p          5 � p        r        5 � p        r                               
                                                      	    p          5 � p        r        5 � p        r                               
@ @                                                   	    p          5 � p        r        5 � p        r                                
  @                                    	                
@ @                                                    
D                                      	                 
D                                      	                 
D                                      	                 
D                                      	                 
D                                      	       #         @                                                      #AA    #A    #P    #IFOUT    #PA    #ITEL                                                                  	     p          5 � p        r        5 � p        r                                D                                      	                                                       	                 D                                                                                                           	     p          5 � p        r        5 � p        r                                                       @                     #         @                                                      #DENSA    #DENSE    #D     #VT !   #TC "                                                   	                                                       	                                                       
                 D                                 !     	                                                  "     	       #         @                                  #                    #DELRW $   #PC %   #TS &   #TC '   #ITYPE (             D                                 $     	                                                  %     	                                                  &     	                                                  '     	                                                  (            #         @                                  )                    #D *   #GM +   #GM1 ,   #GMW -   #GMI .   #DGM /   #DGMW 0   #DGMI 1   #DI 2   #TC 3   #TS 4   #P 5   #DENSE 6   #FW 7   #VT 8   #XW 9   #XI :   #SEKDEL ;   #ITYPE <             D                                *     
                 D                                 +     	                 D                                 ,     	                 D                                 -     	                 D                                 .     	                 D                                 /     	                 D                                 0     	                 D                                 1     	                 D                                 2     	                                                  3     	                                                  4     	                                                  5     	                 D                                 6     	                                                  7     	                                                  8     	                                                  9     	                                                  :     	                                                  ;     	                                                  <            #         @                                  =                    #TS >   #FW ?   #TC @   #VT A   #DELRW B   #D C   #DENSA D   #GM1 E   #DGM F   #DGMW G   #DGMI H   #GMW I   #GMI J   #DI K   #SEKDEL L   #ITYPE M   #P N             D                                 >     	                 D                                 ?     	                                                  @     	                                                  A     	                                                  B     	                                                 C     
                                                  D     	                                                  E     	                                                  F     	                                                  G     	                                                  H     	                                                  I     	                                                  J     	                                                  K     	                                                  L     	                                                  M                                                       N     	       #         @                                  O                    #DENSE P   #D Q   #GM R   #FW S             D                                 P     	                 D                                Q     
                 D                                 R     	                 D                                 S     	       #         @                                  T                    #D U   #TLAYER V   #PLAYER W   #RLAYER X   #LDEPTH Y   #VT Z             D                                U     
                                                  V     	                                                  W     	                                                  X     	                                                  Y     	                                                  Z     	          �   8      fn#fn !   �         HAILSTONE_DRIVER %   �  �   a   HAILSTONE_DRIVER%TCA %   �  �   a   HAILSTONE_DRIVER%H1D $   P  @   a   HAILSTONE_DRIVER%HT $   �  �   a   HAILSTONE_DRIVER%PA '   D  �   a   HAILSTONE_DRIVER%RHO1D $   �  �   a   HAILSTONE_DRIVER%RA &   �  �   a   HAILSTONE_DRIVER%QI1D &   `  �   a   HAILSTONE_DRIVER%QC1D &     �   a   HAILSTONE_DRIVER%QR1D &   �  �   a   HAILSTONE_DRIVER%QS1D &   |  �   a   HAILSTONE_DRIVER%QG1D &   0	  �   a   HAILSTONE_DRIVER%NG1D %   �	  �   a   HAILSTONE_DRIVER%VUU &   �
  @   a   HAILSTONE_DRIVER%WDUR $   �
  @   a   HAILSTONE_DRIVER%NZ (     @   a   HAILSTONE_DRIVER%DHAIL1 (   X  @   a   HAILSTONE_DRIVER%DHAIL2 (   �  @   a   HAILSTONE_DRIVER%DHAIL3 (   �  @   a   HAILSTONE_DRIVER%DHAIL4 (     @   a   HAILSTONE_DRIVER%DHAIL5    X  {       INTERP    �  �   a   INTERP%AA    �  @   a   INTERP%A    �  @   a   INTERP%P      @   a   INTERP%IFOUT    G  �   a   INTERP%PA    �  @   a   INTERP%ITEL    ;  u       TERMINL    �  @   a   TERMINL%DENSA    �  @   a   TERMINL%DENSE    0  @   a   TERMINL%D    p  @   a   TERMINL%VT    �  @   a   TERMINL%TC    �  v       VAPORCLOSE !   f  @   a   VAPORCLOSE%DELRW    �  @   a   VAPORCLOSE%PC    �  @   a   VAPORCLOSE%TS    &  @   a   VAPORCLOSE%TC !   f  @   a   VAPORCLOSE%ITYPE    �  �       MASSAGR    �  @   a   MASSAGR%D    �  @   a   MASSAGR%GM      @   a   MASSAGR%GM1    V  @   a   MASSAGR%GMW    �  @   a   MASSAGR%GMI    �  @   a   MASSAGR%DGM      @   a   MASSAGR%DGMW    V  @   a   MASSAGR%DGMI    �  @   a   MASSAGR%DI    �  @   a   MASSAGR%TC      @   a   MASSAGR%TS    V  @   a   MASSAGR%P    �  @   a   MASSAGR%DENSE    �  @   a   MASSAGR%FW      @   a   MASSAGR%VT    V  @   a   MASSAGR%XW    �  @   a   MASSAGR%XI    �  @   a   MASSAGR%SEKDEL      @   a   MASSAGR%ITYPE    V  �       HEATBUD    9  @   a   HEATBUD%TS    y  @   a   HEATBUD%FW    �  @   a   HEATBUD%TC    �  @   a   HEATBUD%VT    9  @   a   HEATBUD%DELRW    y  @   a   HEATBUD%D    �  @   a   HEATBUD%DENSA    �  @   a   HEATBUD%GM1    9  @   a   HEATBUD%DGM    y  @   a   HEATBUD%DGMW    �  @   a   HEATBUD%DGMI    �  @   a   HEATBUD%GMW    9  @   a   HEATBUD%GMI    y  @   a   HEATBUD%DI    �  @   a   HEATBUD%SEKDEL    �  @   a   HEATBUD%ITYPE    9  @   a   HEATBUD%P    y  j       BREAKUP    �  @   a   BREAKUP%DENSE    #  @   a   BREAKUP%D    c  @   a   BREAKUP%GM    �  @   a   BREAKUP%FW    �  �       MELT    j  @   a   MELT%D    �  @   a   MELT%TLAYER    �  @   a   MELT%PLAYER    *   @   a   MELT%RLAYER    j   @   a   MELT%LDEPTH    �   @   a   MELT%VT 