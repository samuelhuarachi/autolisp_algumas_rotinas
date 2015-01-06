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


(defun saveVars()
	(setq qtdbf  (atof (get_tile "qtdbf")))
	(setq espaco (atof (get_tile "espaco")))
)
 
(defun c:aefixo2()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	;(setq qtdbf (atof (getstring "\nQuantidade de ramais: ")))
	;(setq espaco (atof (getstring "\nEspaço entre eles: ")))
	
	;DCL start
	(if (not(setq dcl_id (load_dialog "C:\\bfdias\\aero fixo\\dcl\\desenha_aero_fixo2.dcl")))
		(progn
			(alert "O arquivo da DCL não pode ser carregado!")
			(exit)
		)
		(progn
			;;;--- Load the DCL definition inside the DCL file
			(if (not (new_dialog "SAMPLE1" dcl_id))
				(progn
					(alert "A definição SAMPLE1 não pode ser encontrada dentro do arquivo DCL!")
					(exit)
				)
				;;;--- Else, the definition was loaded
				(progn
					(action_tile "accept" "(saveVars)(done_dialog 2)")
					;;;--- If an action event occurs, do this function
					(action_tile "cancel" "(done_dialog 1)")
					;;;--- Display the dialog box
					(setq ddiag  (start_dialog))
					;;;--- Unload the dialog box
					(unload_dialog dcl_id)
					
					;;;--- If the user pressed the Cancel button
					(if(= ddiag 1)(exit))
					;;;--- If the user pressed the Okay button
					(if(= ddiag 2)
						(progn
							;.(princ "\n The user pressed Okay!")
						)
					)
				)
			)
		)
	)
	
	
	
	
	(setq pontoInsert (getpoint "\nPonto de inserção: "))
	
	(setq contador 1)
	(while (< contador qtdbf)
		
		(if (= contador 1)
			(progn
				(command "layer" "m" "bfesquerda_layer" "c" "white" "" "")
				(command "insert" "C:\\bfdias\\blocos\\CURVA CORTE DIFUSOR2.dwg" pontoInsert "" "" "")
				
				(setq p1Inicio (polar pontoInsert 0.3717672569069454 154.5583735859689))
				(setq p1Fim (polar (polar pontoInsert 0 espaco) 2.734553731958625 141.5663819822644))
				
				(setq p2Inicio (polar pontoInsert 5.925299347551002 153.7411214377903))
				(setq p2Fim (polar (polar pontoInsert 0 espaco) 3.534995714199270 140.7520638777298))
				
				(command "layer" "m" "BF-04-SA-TUB" "c" "30" "" "")
				(command "line" p1Inicio p1Fim "")
				(command "line" p2Inicio p2Fim "")
				
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" pontoInsert (polar pontoInsert 0 espaco) (polar (polar pontoInsert 0 (/ (distance pontoInsert (polar pontoInsert 0 espaco)) 2)) (+ pi (/ pi 2)) 1000 ) )
				
				;(command "layer" "m" "bfmeio_layer" "c" "white" "" "")
				;(command "insert" "C:\\bfdias\\blocos\\bfmeio_bloco.dwg" (polar pontoInsert 0 espaco	) "" "" "")
				
			)	
		)
		
		(if (/= contador 1) 
			(progn
				
				(command "layer" "m" "bfmeio_layer" "c" "white" "" "")
				;(command "insert" "C:\\bfdias\\blocos\\bfmeio_bloco.dwg" (polar pontoInsert 0 (* espaco (- contador 1) )) "" "" "")
				(command "insert" "C:\\bfdias\\blocos\\TE CORTE DIFUSOR.dwg" (polar pontoInsert 0 (* espaco (- contador 1) )) "" "" "")
				
				;Insere tubulação meio
				(setq p1Inicio (polar (polar pontoInsert 0 (* espaco (- contador 1) )) 0.4070389216354575 141.5663819823995))
				(setq p1Fim (polar (polar pontoInsert 0 (* espaco contador ))  2.734553731958625 141.5663819822644))
				
				(setq p2Inicio (polar (polar pontoInsert 0 (* espaco (- contador 1) )) 5.889782246564825 140.7520638779124))
				(setq p2Fim (polar (polar pontoInsert 0 (* espaco contador )) 3.534995714198730 140.7520638777613))
				
				(if (/= (+ contador 1) qtdbf)
					(progn
						(command "layer" "m" "BF-04-SA-TUB" "c" "30" "" "")
						(command "line" p1Inicio p1Fim "")
						(command "line" p2Inicio p2Fim "")
					)
					(progn
						(setq p1Fim (polar (polar pontoInsert 0 (* espaco contador ))  2.770635850899703 154.6037374392899))
						(setq p2Fim (polar (polar pontoInsert 0 (* espaco contador )) 3.499889332015266 153.8584369108029))
						
						(command "layer" "m" "BF-04-SA-TUB" "c" "30" "" "")
						(command "line" p1Inicio p1Fim "")
						(command "line" p2Inicio p2Fim "")
					)
				)
				
				
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" (polar pontoInsert 0 (* espaco (- contador 1) )) (polar pontoInsert 0 (* espaco contador )) (polar (polar (polar pontoInsert 0 (* espaco (- contador 1) )) 0 (/ (distance (polar pontoInsert 0 (* espaco (- contador 1) )) (polar pontoInsert 0 (* espaco contador ))) 2)) (+ pi (/ pi 2)) 1000))
				
			)
		)
		
		
		(setq contador (+ contador 1))
		
		(if (= contador qtdbf)
			(progn
				(command "layer" "m" "bfdireita_layer" "c" "white" "" "")
				(command "insert" "C:\\bfdias\\blocos\\CURVA CORTE DIFUSOR.dwg" (polar pontoInsert 0 (* espaco (- qtdbf 1))) "" "" "")
				
			)
		)
	)
	
	
	(command "layer" "m" "BF-08-SA-LCE" "c" "red" "" "")
	(command "line" pontoInsert (polar pontoInsert 0 (* espaco (- qtdbf 1))) "")
	
	
	
	;Faz legenda
	(setq pILeng (polar pontoInsert (/ pi 2) 800))
	
	(command "layer" "m" "Legenda" "c" "33" "" "")
	(command "text" pILeng 70 0 "Desenha_aero_fixo2.fas")
	
	(command "text" (polar pILeng (* (/ pi 2) 3) 120) 70 0 (strcat "Quantidade de ramais - " (rtos qtdbf 2 2)))
	(setq ultimo (polar pILeng (* (/ pi 2) 3) 120))
	
	(command "text" (polar ultimo (* (/ pi 2) 3) 120) 70 0 (strcat "Espaçamento entre eles - " (rtos espaco 2 2)))
	
	
	;BF-08-SA-LCE
	
	(princ)
)























