 ;ultima_funcao______varGlobal
 ;remove_letras_string
 ;remove_letras_string_____STRING1
 ;remove_numeros_string
 ;remove_numeros_string_____STRING1
 ;CriarLink
 ;criarlink
 ;CriarLink_____ent_link_codigo
 ;criarlink_____ent_link_codigo
 ;GetId
 ;GetId_____entd_nomeLink
 ;getid
 ;getid_____entd_nomeLink
 ;faz_janela
 ;faz_janela_____COORD_TIPO_PROPRIEDADE_TAMANHO
 ;desenha_linha
 ;desenha_linha_____p1_p2_layer_cor
 ;desenha_circulo
 ;desenha_circulo_____ponto_raio_layer_cor
 ;quantidade_elementos_array
 ;quantidade_elementos_array_____array1
 ;pega_propriedade_objeto
 ;pega_propriedade_objeto_____obj_prop
 ;gerar_lista_elementos
 ;gerar_lista_elementos_____layer_tipo_elementos
 ;retorna_attrib
 ;retorna_attrib_____BLOCO_numATTRIB
 ;gera_layer
 ;gera_layer_____nomeLayer_Cor
 ;arredonda_coordenada
 ;arredonda_coordenada_____coord_casas
 ;exibir_porcentagem
 ;exibir_porcentagem_____Qtd
 ;verxdata
 ;verxdata_____semParametros
 ;ViewExtents
 ;ViewExtents_____semParametros
 ;viewextents
 ;viewextents_____semParametros
 ;distancia_ponto_reta
 ;distancia_ponto_reta_____p1,p2,ponto_insercao
 
 ;cuidado ao usar a variavel contador

 (defun formataroZero(p)
	
	(if (= p "1")
		(progn
			(setq p "01")
		)
	)
	(if (= p "2")
		(progn
			(setq p "02")
		)
	)
	(if (= p "3")
		(progn
			(setq p "03")
		)
	)
	(if (= p "4")
		(progn
			(setq p "04")
		)
	)
	(if (= p "5")
		(progn
			(setq p "05")
		)
	)
	(if (= p "6")
		(progn
			(setq p "06")
		)
	)
	(if (= p "7")
		(progn
			(setq p "07")
		)
	)
	(if (= p "8")
		(progn
			(setq p "08")
		)
	)
	(if (= p "9")
		(progn
			(setq p "09")
		)
	)
	
	p
 )

(defun c:agridcont()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	
	(setq totaldeGrids 7)
	(setq qtdDeDifusores 10)
	(setq nivelDAgua 10)
	
	
	(setq totaldeGrids (atoi  (getstring "\nTotal de grids: ")))
	(setq qtdDeDifusores (atoi  (getstring "\nQuantidade de difusores em 1 grid: ")))
	(setq nivelDAgua (atoi  (getstring "\nNível da água: ")))
	
	(setq pontoInsercao (getpoint "\nDefina um ponto de inserção: "))
	
	(setq qtdLinas 12)
	(setq distancia_cima_baixo 3.35632)
	;(setq distanciaHorizontal 109.012)
	(setq distanciaHorizontal 108.982)
	(setq distanciaHorizontalTotalQtd 122.598)
	(setq _varAngulo270 (* (/ pi 2) 3))
	(setq _varAngulo90 (/ pi 2))
	
	(setq contador2 0)
	
	(setq listaMateriais1 
		(list 
			"PARAFUSO SEXTAVADO INOX AISI 304 Ø 3/8\" X 1 1/2\" C/ 1PO/2AR"
			"NIPLE INOX C/ ROSCA Ø 3/4\" X 50MM"
			"FLUTUANTE A01 C/ HASTE / PORCA / OLHAL M16"
			"ABRAÇADEIRA INOX \"O\" EMBORRACHADA Ø 85MM"
			"PARAFUSO PRISIONEIRO INOX AISI 304 Ø 1/4\" X 122MM C/ 4PO/AR"
			"GRAMPO INOX AISI 304 1/4\""
			"CABO INOX AISI 304 Ø 1/4''"
			"ABRAÇADEIRA INOX MANGUEIRA 32MM Ø 54-62MM (2 1/8\"-2 1/2\")"
			"MANGUEIRA BORRACHA Ø INT 2\" X 4200MM - 80°C 2 LONAS"
			"GRADE INOX AISI 304 5000x2180mm"
			"DIFUSOR BOLHA FINA TUBULAR MEMBRANA EPDM Ø 90 X 1000MM GU P/ SUPORTE"
			"TUBO INOX AISI 304 ØOD. 2\" C/ 4900MM; 05 TE. Ø3/4\"; 01 CURVA 45º"
		)
	)
	
	(while (< contador2 qtdLinas)
		
		(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
		(if (= (nth contador2 listaMateriais1) "TUBO INOX AISI 304 ØOD. 2\" C/ 4900MM; 05 TE. Ø3/4\"; 01 CURVA 45º")
			(progn
				(setq pecaQtd "01 pç.") ;01 por grid
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
				
				;Total de grid
				(setq pecaQtd (itoa totaldeGrids))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "--.--.--.-----")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "01.01")
				
			)
		)
		(if (= (nth contador2 listaMateriais1) "DIFUSOR BOLHA FINA TUBULAR MEMBRANA EPDM Ø 90 X 1000MM GU P/ SUPORTE")
			(progn
				;qtd de difusores
				(setq pecaQtd (itoa qtdDeDifusores))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
				
				
				(setq pecaQtd (itoa (* qtdDeDifusores totaldeGrids)))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "02.07.06.00250")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "02.01")
				
			)
		)
		(if (= (nth contador2 listaMateriais1) "GRADE INOX AISI 304 5000x2180mm")
			(progn
				(setq pecaQtd "01 pç.") ;01 por grid
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
			
				(setq pecaQtd (itoa totaldeGrids))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "--.--.--.-----")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "03.01")
				
			)
		)
		(if (= (nth contador2 listaMateriais1) "MANGUEIRA BORRACHA Ø INT 2\" X 4200MM - 80°C 2 LONAS")
			(progn
				(setq pecaQtd "01 pç.") ;01 por grid
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
				
				
				;Total de grid
				(setq pecaQtd (itoa totaldeGrids))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "--.--.--.-----")
				
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "04.01")
				
			)
		)
		(if (= (nth contador2 listaMateriais1) "ABRAÇADEIRA INOX MANGUEIRA 32MM Ø 54-62MM (2 1/8\"-2 1/2\")")
			(progn
				(setq pecaQtd "02 pç.") ;02 por grid
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
			
				;Total de grid
				(setq pecaQtd (itoa (* totaldeGrids 2)))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "02.02.03.00028")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "05.01")
				
			)
		)
		
		(if (= (nth contador2 listaMateriais1) "CABO INOX AISI 304 Ø 1/4''")
			(progn
				;nivel da agua + 0.5 * 4
				(setq pecaQtd (* (+ nivelDAgua 0.5) 4))
				(setq pecaQtd (strcat (rtos pecaQtd 2 2) " m."))
				
				
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
			
			
				;Total de grid
				(setq pecaQtd (rtos (* totaldeGrids (* (+ nivelDAgua 0.5) 4)) 2 2))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " m."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "02.02.99.00048")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "06.01")
				
			)
		)
		
		(if (= (nth contador2 listaMateriais1) "GRAMPO INOX AISI 304 1/4\"")
			(progn
				(setq pecaQtd "16 pç.") ;por grid
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
			
				
				
				;Total de grid
				(setq pecaQtd (itoa (* totaldeGrids 16)))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "02.02.03.00172")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "07.01")
				
			)
		)
		
		
		(if (= (nth contador2 listaMateriais1) "PARAFUSO PRISIONEIRO INOX AISI 304 Ø 1/4\" X 122MM C/ 4PO/AR")
			(progn
				;qtd de difusores
				(setq pecaQtd (itoa qtdDeDifusores))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
			
				;Total de grid
				(setq pecaQtd (itoa (* qtdDeDifusores totaldeGrids)))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "02.02.06.00212")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "08.01")
				
			)
		)
		
		(if (= (nth contador2 listaMateriais1) "PARAFUSO SEXTAVADO INOX AISI 304 Ø 3/8\" X 1 1/2\" C/ 1PO/2AR")
			(progn
				;por grid
				(setq pecaQtd "08 pç.")
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
			
				
				;Total de grid
				(setq pecaQtd (itoa (* 8 totaldeGrids)))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "02.02.06.00213")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "12.01")
				
				
				
			)
		)
		(if (= (nth contador2 listaMateriais1) "NIPLE INOX C/ ROSCA Ø 3/4\" X 50MM")
			(progn
				;qtd de difusores
				(setq pecaQtd (itoa qtdDeDifusores))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
			
				;Total de grid
				(setq pecaQtd (itoa (* qtdDeDifusores totaldeGrids)))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "02.02.05.00185")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "11.01")
				
			)
		)
		(if (= (nth contador2 listaMateriais1) "FLUTUANTE A01 C/ HASTE / PORCA / OLHAL M16")
			(progn
				;por grid
				(setq pecaQtd "01 pç.")
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
			
				;Total de grid
				(setq pecaQtd (itoa (* 1 totaldeGrids)))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "02.07.06.00389")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "10.01")
				
			)
		)
		(if (= (nth contador2 listaMateriais1) "ABRAÇADEIRA INOX \"O\" EMBORRACHADA Ø 85MM")
			(progn
				;qtd de difusores
				(setq pecaQtd (itoa qtdDeDifusores))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 1.6817 0 (nth contador2 listaMateriais1))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontal) 1.6817 0 pecaQtd)
			
				;Total de grid
				(setq pecaQtd (itoa (* qtdDeDifusores totaldeGrids)))
				(setq pecaQtd (formataroZero pecaQtd))
				(setq pecaQtd (strcat pecaQtd " pç."))
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) 0 distanciaHorizontalTotalQtd) 1.6817 0 pecaQtd)
				
				;Insere código do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 18.7721) 1.6817 0 "02.02.03.00038")
				
				;Insere código referencia do material
				(command "text" (polar (polar pontoInsercao _varAngulo270 (* distancia_cima_baixo contador2)) pi 26.6751) 1.6817 0 "09.01")
			)
		)
		
		
		
		(setq contador2 (+ contador2 1))
	)
	
	
	(princ)
)







