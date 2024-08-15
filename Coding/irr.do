/* 
Author: José Gustavo Varela. Departamento de Economía, FCS-UDELAR. 
*/

*------------------------------------------------------------------------------------------------
*-------------------------- cálculo de la TIR (bisection method)----------------------------------
*------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------

*g neg_ssw_period = -social_security_wealth_period
*	g flufon = social_security_wealth_period

	g flufon = sscf_id
	
*.....Se definen las variables a utilizar localmente en el calculo de la TIR............................................

	scalar delta = 0.000000001			// Error admitido para la TIR. Original.
	*scalar delta = 0.000001			// Error admitido para la TIR. 

	g double tirinf = 0		// Cota inf del intervalo 
	g double tirsup = 0		// Cota sup del intervalo 

	g conrai = 1		// Variable para con_trolar que el intervalo tenga la raí_z

	g conite = 50		// Variable para con_trolar ite_ración. Se admitiran 50 iteraciones como máximo. Original.
	*g conite = 10		// Variable para con_trolar ite_ración. Se admitiran 10 iteraciones como máximo
	g double tirite = 0		// Crea una tir_  que se utilizará en ite_ración	


*.....Determinación del extremo inferior del intervalo de busqueda de la raiz en pasos del 5%............................	
	
	while conrai {

	g double mono = flufon*(1+tirinf)^(20-age)	// Calcula valor de los monomios  
	egen double poli = sum(mono)				// Calcula el valor del polinomio 
	
	replace tirinf = tirinf - 0.05 if poli < 0
	replace conrai = 0 if poli >= 0		//Determina que fue la última pasada del while y desabilita mover los extremos del intervalo 
	replace tirsup = tirinf if poli == 0	//Igualamos ambos extremos del intervalo porque justo caimos en la raíz
	
	drop mono poli		// OJO este drop no se debe comentar porque sino el while no funciona

	}
	
*.....Determinación de extremo superior del intervalo de busqueda de la raíz en pasos del 5%..............................	

	replace conrai = 1 if tirsup == 0		//Habilita mover el extremo superior si no caimos justo en la raíz con el extremo inferior
	
	while conrai {

	g double mono = flufon*(1+tirsup)^(20-age)	//Calcula valor de los monomios  
	egen double poli = sum(mono)				//Calcula el valor del polinomio
	
	replace tirsup = tirsup + 0.05 if poli > 0

	replace conrai = 0 if poli <= 0		//Determina que fue la última pasada del while		
	replace tirinf = tirsup if poli == 0	//Igualamos ambos extremos del intervalo porque justo caimos en la raíz

	drop mono poli		//OJO este drop no se debe comentar porque sino el while no funciona

	}

*.....Iteración para hallar los extremos de un intervalo menor a "delta"...................................................	

	replace conite = 0 if tirinf == tirsup	//Deshabilita hacer la iteración porque caimos justo en la raíz con algun extremo

	while conite {
	replace tirite = tirinf/2+tirsup/2

	g double mono = flufon*(1+tirite)^(20-age)	//Calcula valor de los monomios
	egen double poli = sum(mono)				//Calcula el valor del polinomio
	
	replace tirinf = tirite if poli >= 0 	
	replace tirsup = tirite if poli <= 0	

	replace conite = 0 if tirsup - tirinf <= delta		//Determina que fue la última pasada del while
 	replace conite = conite - 1 if conite > 0			//Se admitiran cierto número de iteraciones como máximo
	
	drop mono poli		// OJO este drop no se debe comentar porque sino el while no funciona
	}

*.....Carga del valor de la TIR como el promedio de los extremos del intervalo menor a "delta"............................


	replace irr_id= (tirinf/2+tirsup/2)
	format irr_id %-9.3f			//Dejar con 3 cifras decimales

* Alvaro Forteza: El programa está arrojando - 100 cuando el flujo de fondos sólo tiene valores negativos. La TIR en ese caso no está definida, así que debería dar un missing. Lo sustituimos en la siguiente sentencia.	
	
	replace irr_id = . if irr_id == -1
	
	drop flufon tirinf tirsup conrai conite tirite	//Se borran las variables locales del cálculo de la TIR


*----------------------------------------------------------------------------------------------
*----------------------Fin de cálculo de la TIR------------------------------------------------
*----------------------------------------------------------------------------------------------
