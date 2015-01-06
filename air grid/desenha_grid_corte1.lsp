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



(defun dgrid_corte1_saveVars()
	(setq comprimento (atof (get_tile "comprimento")))
	(setq tamTubo (atof (get_tile "comLastro")))
	(setq qtdDeLastros (atof (get_tile "qtdLastros")))
	(setq modeloLastro (atof (get_tile "quartCinque")))
	(setq larTuboInterno (atof (get_tile "larCentral")))
	(setq qtdDifusor (atof (get_tile "quantidadeDif"))) ;34 e 20, apresentam as colisões
)



(defun desenha_marcacao2(ponto)
	
	(command "layer" "m" "BF-07-SA-LCE" "c" "red" "" "")
	(sam_linha (polar ponto sam_parabaixo 14.92285590553365) (polar (polar ponto sam_parabaixo 14.92285590553365) sam_paracima 29.84571181106730))
	(setq obj1 (entlast))
	(sam_linha (polar ponto pi 14.92285590553365) (polar (polar ponto pi 14.92285590553365) 0 29.84571181106730))
	(command "layer" "m" "BF-SA-09-SUP" "c" "251" "" "")
	(command "circle" ponto 3)

	
)


(defun c:dd()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	;DCL start
	(if(not(setq dcl_id (load_dialog "C:\\bfdias\\air grid\\dcl\\desenha_grid.dcl")))
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
					(action_tile "accept" "(dgrid_corte1_saveVars)(done_dialog 2)")
					;;;--- If an action event occurs, do this function
					(action_tile "cancel" "(done_dialog 1)")
					;(action_tile "but1" "(doButton 1)")
					;(action_tile "but2" "(doButton 2)") 
					;(action_tile "but3" "(doButton 3)") 
					;;;--- Display the dialog box
					(setq ddiag  (start_dialog))
					;;;--- Unload the dialog box
					(unload_dialog dcl_id)
					
					;;;--- If the user pressed the Cancel button
					(if(= ddiag 1)
						;(princ "\n Sample2 cancelled!")
						(exit)
					)

					;;;--- If the user pressed the Okay button
					(if(= ddiag 2)
						(progn
							;(princ "\n The user pressed Okay!")
						)
					)
				)
			)
		)
	)
	
	(setq pontoInsercao (getpoint "\nPonto de insercao: "))
	
	
	
	;Faz os furos da fixação
	;(polar pontoInsercao 0 comprimento)
	;pontoInsercao
	;desenha_marcacao
	;qtdDifusor
	(setq qtdFurinhos (/ qtdDifusor 2))
	;(desenha_marcacao pontoInsercao)
	
	(setq distancia2 (/ comprimento qtdFurinhos))
	(setq sel nil)
	(setq sel (ssadd))

	(setq contador2 1)
	(while (> qtdFurinhos 0)
		(setq ponto55 (polar pontoInsercao 0 (* contador2 distancia2)))
		;(desenha_marcacao2 ponto55)
		
		(setq contador2 (+ contador2 1))
		(setq qtdFurinhos (- qtdFurinhos 1))
	)
	
	
	(command "layer" "m" "BF-SA-03-CNT" "c" "251" "" "")
	(command "rectangle" pontoInsercao (polar (polar pontoInsercao 0 comprimento) sam_paracima 2180) )
	(setq objRec (entlast))
	;offset de 6
	
	(command "offset" 6 objRec (polar pontoInsercao (/ pi 4) 10) "")
	
	(command "offset" 50 objRec (polar pontoInsercao (/ pi 4) 10) "")
	
	(setq pntLastroInicio (polar pontoInsercao  0.7853981633974482  70.71067811865475 ))
	
	(setq pntLastroInicio2 (polar pntLastroInicio 0 tamTubo))
	
	
	(command "line" pntLastroInicio2 (polar pntLastroInicio2 sam_paracima 2080) "")
	(command "line" pntLastroInicio2 (polar pntLastroInicio2 sam_paracima 2080) "")
	(setq ultLinha (entlast))
	(command "move" ultLinha "" pntLastroInicio2 (polar pntLastroInicio2 0 50))
	(command "line" pntLastroInicio2 (polar pntLastroInicio2 sam_paracima 2080) "")
	(setq ultLinha (entlast))
	(command "move" ultLinha "" pntLastroInicio2 (polar pntLastroInicio2 0 44.00000000088913))
	
	
	(setq pntLastroInicio3 (polar (polar pontoInsercao 0 comprimento) 2.356194490192344 70.71067811865475))
	(setq pLastro1 (polar pntLastroInicio3 pi tamTubo))
	(setq pLastro2 (polar pLastro1 pi 50))
	(setq pLastro3 (polar pLastro1 pi 44.00000000088913))
	
	(command "line" pLastro1 (polar pLastro1 sam_paracima 2080) "" )
	(command "line" pLastro2 (polar pLastro2 sam_paracima 2080) "" )
	(command "line" pLastro3 (polar pLastro3 sam_paracima 2080) "" )
	
	
	;Desenha o x interno
	(setq p1 (polar pntLastroInicio2 0 50))
	
	(setq p2 (polar pLastro2 sam_paracima 2080))
	
	(command "line" p1 p2 "")
	(command "line" (polar p1 sam_paracima 2080) (polar p2 sam_parabaixo 2080) "")
	
	(setq inicioLastro (polar pontoInsercao 1.451340253316036 50.34732325019136))
	
	;qtdDeLastros
	(setq contador qtdDeLastros)
	(setq espaco (/ 2080 qtdDeLastros))
	
	(setq sel nil)
	(setq sel (ssadd))
	
	(command "layer" "m" "BF-08-SA-LCE" "")
	(while (> contador 0)
		(setq ponto1 (polar inicioLastro sam_paracima (* espaco contador)))
		
		(command "line" ponto1 (polar ponto1 0 (+ 88.00000000088767 tamTubo)) "")
		(setq sel (ssadd (entlast) sel))
		(setq objLinha1 (entlast))
		
		(command "offset" 25.00000000000728 objLinha1 (polar ponto1 sam_paracima 10) "")
		(setq sel (ssadd (entlast) sel))
		(vl-cmdf "_.chprop" (entlast) "" "_la" "BF-SA-03-CNT" "")
		(command "offset" 25.00000000000728 objLinha1 (polar ponto1 sam_parabaixo 10) "")
		(setq sel (ssadd (entlast) sel))
		(vl-cmdf "_.chprop" (entlast) "" "_la" "BF-SA-03-CNT" "")
		
		
		(setq contador (- contador 1))
	)
	(command "move" sel "" ponto1 (polar ponto1 sam_parabaixo (/ espaco 2) ))
	
	
	(setq ponto2 (polar (polar pontoInsercao 0 comprimento) 1.690225252813234 50.35871324805668))
	(setq contador qtdDeLastros)
	(setq espaco (/ 2080 qtdDeLastros))
	
	(setq sel nil)
	(setq sel (ssadd))
	
	(setq sel2 nil)
	(setq sel2 (ssadd))
	
	(setq ponto2_ nil)
	
	(command "layer" "m" "BF-08-SA-LCE" "")
	(while (> contador 0)
		(setq ponto1 (polar ponto2 sam_paracima (* espaco contador)))
		
		(command "layer" "m" "BF-08-SA-LCE" "")
		(command "line" ponto1 (polar ponto1 pi (+ 88.00000000088767 tamTubo)) "")
		(setq sel (ssadd (entlast) sel))
		(setq objLinha1 (entlast))
		
		
		;(command "layer" "m" "BF-SA-03-CNT" "c" "251" "" "")
		(command "offset" 25.00000000000728 objLinha1 (polar ponto1 sam_paracima 10) "")
		(setq sel (ssadd (entlast) sel))
		(vl-cmdf "_.chprop" (entlast) "" "_la" "BF-SA-03-CNT" "")
		(command "offset" 25.00000000000728 objLinha1 (polar ponto1 sam_parabaixo 10) "")
		(setq sel (ssadd (entlast) sel))
		(vl-cmdf "_.chprop" (entlast) "" "_la" "BF-SA-03-CNT" "")
		
		
		(if (/= ponto2_ nil)
			(progn
				(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
				(command "dimlinear" ponto1 ponto2_ (polar (sam_metade ponto1 ponto2_) 0 200) )
				(setq sel2 (ssadd (entlast) sel2))
			)
		)
		
		(setq ponto2_ ponto1)
		
		(setq contador (- contador 1))
	)
	(command "move" sel "" ponto1 (polar ponto1 sam_parabaixo (/ espaco 2) ))
	(command "move" sel2 "" ponto1 (polar ponto1 sam_parabaixo (/ espaco 2) ))
	
	
	(setq ponto33 (polar pontoInsercao 0 comprimento))
	(setq ponto44 (polar ponto33 sam_paracima 2180))
	(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
	(command "dimlinear" ponto33 ponto44 (polar (sam_metade ponto33 ponto44) 0 400) )
	
	(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
	(command "dimlinear" pontoInsercao ponto33 (polar (sam_metade pontoInsercao ponto33) sam_parabaixo 290) )
	
	
	
	
	(princ)
	
	
)







