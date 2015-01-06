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



(defun insere_difusores_air3(pontoInsercao qtdDifusores comprimento)
	
	;Distancia entre difusores
	(setq distEntreDif (/ comprimento qtdDifusores))
	
	;Inserção do primeiro difusor
	(command "layer" "m" "difusores1_temp" "c" "red" "" "")
	;(command "circle" (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 10)
	(command "insert" "C:\\bfdias\\blocos\\desenha_air_grid3\\difusor1_aeg3_bloco.dwg" (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) sam_paraCima 77.5505) "" "" "")
	
	(setq qtdDifusores (- qtdDifusores 1))
	
	;Loop para a inserção dos difusores restantes =D
	(while (> qtdDifusores 0)
		
		(command "layer" "m" "difusores1_temp" "c" "red" "" "")
		;(command "circle" (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif qtdDifusores)) 10)
		(command "insert" "C:\\bfdias\\blocos\\desenha_air_grid3\\difusor1_aeg3_bloco.dwg" (polar (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif qtdDifusores)) sam_paraCima 77.5505) "" "" "")
	
		
		(setq qtdDifusores (- qtdDifusores 1))
		
		;77.5505
	)
)


(defun saveVarsDGRID3()
	(setq comprimento (atof (get_tile "comprimento")))
	(setq qtdDifusores (atof (get_tile "qtdDifusores")))
	(setq compLastros (atof (get_tile "compLastros")))
	(setq distanciaGancho (atof (get_tile "distanciaGancho")))
)


(defun c:dgrid3()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	;DCL start
	(if(not(setq dcl_id (load_dialog "C:\\bfdias\\air grid\\dcl\\desenha_air_grid3.dcl")))
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
					(action_tile "accept" "(saveVarsDGRID3)(done_dialog 2)")
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
							;.(princ "\n The user pressed Okay!")
						)
					)
				)
			)
		)
	)
	(setq pontoInsercao (getpoint "Ponto inserção: "))
	
	
	;(command "circle" ponto2 20)
	;pontoInsercao
	(setq ponto2 (polar pontoInsercao 0 comprimento))
	;Faz primeira cota
	;BF-07-SA-COT 250
	(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
	(command "dimlinear" pontoInsercao ponto2 (polar (sam_metade pontoInsercao ponto2) sam_paraBaixo 615)  )
	
	
	(command "layer" "m" "layer_temp_extr" "c" "cyan" "" "")
	(command "insert" "C:\\bfdias\\blocos\\desenha_air_grid3\\extremidade_agrid31_bloco.dwg" (polar pontoInsercao sam_paraCima 80.8000) "" "" "")
	
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	(command "line" (polar pontoInsercao sam_paraCima 80.8000) (polar (polar pontoInsercao sam_paraCima 80.8000) 0 comprimento) "")
	
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	(command "line" (polar pontoInsercao sam_paraCima 30.0000) (polar (polar pontoInsercao sam_paraCima 30.0000) 0 comprimento) "")
	
	(setq pontoInsertExtr2 (polar (polar pontoInsercao sam_paraCima 80.8000) 0 comprimento))
	(command "layer" "m" "layer_temp_extr" "c" "cyan" "" "")
	(command "insert" "C:\\bfdias\\blocos\\desenha_air_grid3\\extremidade_agrid32_bloco.dwg" pontoInsertExtr2 "" "" "")
	
	(insere_difusores_air3 pontoInsercao qtdDifusores comprimento)
	
	
	;Desenha os ganchos
	(setq pontoPrimeiroGanho (polar (polar pontoInsercao sam_paraCima 80.8000) 0 distanciaGancho))
	(command "layer" "m" "layer_gancho1" "c" "cyan" "" "")
	(command "insert" "C:\\bfdias\\blocos\\desenha_air_grid3\\gancho_ae3_bloco.dwg" pontoPrimeiroGanho "" "" "")
	(command "insert" "C:\\bfdias\\blocos\\desenha_air_grid3\\gancho_ae3_bloco.dwg" (polar pontoInsertExtr2 pi (+ distanciaGancho 6)) "" "" "")
	
	;Desenha entrada de ar
	(setq ponto1EntradaDeAr (polar (polar pontoInsercao sam_paraCima 80.8000) 1.040956196779418 200.6349888482837))
	(setq ponto2EntradaDeAr (polar ponto1EntradaDeAr 0 (- comprimento 101.4000)))
	(setq ponto1_BEntradaDeAr (polar (polar pontoInsercao sam_paraCima 80.8000) 0.8786588684546298 158.8882876431706))
	(setq ponto2_BEntradaDeAr (polar ponto1_BEntradaDeAr 0 (- comprimento 101.4000)))
	
	(command "layer" "m" "BF-04-SA-TUB" "c" "250" "" "")
	(command "line" ponto1EntradaDeAr ponto2EntradaDeAr "")
	(command "line" ponto1_BEntradaDeAr ponto2_BEntradaDeAr "")
	(command "line" ponto1_BEntradaDeAr ponto1EntradaDeAr "")
	
	;Desenha linha vermelha meio da entrada de ar
	(command "layer" "m" "BF-SA-06-LCE" "c" "red" "" "")
	(setq pontoVermelhoMeio1 (sam_metade ponto1_BEntradaDeAr ponto1EntradaDeAr ))
	(setq pontoVermelhoMeio2 (polar pontoVermelhoMeio1 0 (- comprimento 101.4000)))
	(command "line" pontoVermelhoMeio1 pontoVermelhoMeio2 "")
	
	
	;compLastros
	;Desenha Lastros
	
	(setq pontoInicialLastro (polar (polar pontoInsercao sam_paraCima 80.8000) 0 50))
	(setq pontoInicioDesenhoLastro (polar pontoInicialLastro 0 compLastros))
	;Inicio do desenho das linhas do lastro
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	(command "line" pontoInicioDesenhoLastro (polar pontoInicioDesenhoLastro sam_paraBaixo 6) "")
	(setq ultimoPonto (polar pontoInicioDesenhoLastro sam_paraBaixo 6))
	
	(command "line" ultimoPonto (polar ultimoPonto 0 44) "")
	(setq ultimoPonto (polar ultimoPonto 0 44))
	
	(command "line" ultimoPonto (polar ultimoPonto sam_paraBaixo 44.8000) "")
	(setq ultimoPonto (polar ultimoPonto sam_paraBaixo 44.8000))
	
	(setq ultimoPonto (polar ultimoPonto 0 6))
	(command "line" ultimoPonto  (polar ultimoPonto sam_paraCima 50.8000) "")
	
	(command "layer" "m" "BF-SA-09-SUP" "c" "251" "" "")
	(command "line" ultimoPonto (polar ultimoPonto sam_paraCima 198.5255) "")
	(setq linhaSuporteTrim1 (entlast))
	(setq ultimoPonto (polar ultimoPonto sam_paraCima 198.5255))
	(command "line" ultimoPonto (polar ultimoPonto 0 6) "")
	(setq ultimoPonto (polar ultimoPonto 0 6))
	(command "line" ultimoPonto (polar ultimoPonto sam_paraBaixo 198.5255) "")
	(setq linhaSuporteTrim2 (entlast))
	(setq ultimoPontoBackup ultimoPonto)
	
	(setq sel2 nil)
	(setq sel2 (ssadd))
	(setq sel2 (ssadd linhaSuporteTrim1 sel2))
	(setq sel2 (ssadd linhaSuporteTrim2 sel2))
	
	
	;Desenha Lastros do outro lado da estrutura
	
	(setq pontoInicialLastro2 (polar pontoInsertExtr2 pi 50))
	
	(setq extr2Lastro (polar pontoInicialLastro2 pi compLastros))
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	(command "line" extr2Lastro (polar extr2Lastro sam_paraBaixo 6) "")
	(setq ultimoPonto (polar extr2Lastro sam_paraBaixo 6))
	(command "line" ultimoPonto (polar ultimoPonto pi 44.000) "")
	(setq ultimoPonto (polar ultimoPonto pi 44.000))
	(command "line" ultimoPonto (polar ultimoPonto sam_paraBaixo 44.8000) "")
	(setq ultimoPonto (polar (polar ultimoPonto sam_paraBaixo 44.8000) pi 6))
	;(command "circle" ultimoPonto 50)
	
	(command "line" ultimoPonto (polar ultimoPonto sam_paraCima 50.8000)  "")
	
	(command "layer" "m" "BF-SA-09-SUP" "c" "251" "" "")
	(command "line" ultimoPonto (polar ultimoPonto sam_paraCima 198.5255)  "")
	(setq linhaSuporteTrim1 (entlast)) ;<--- Linha suporte trim 1
	(setq ultimoPonto (polar ultimoPonto sam_paraCima 198.5255))
	(command "line" ultimoPonto (polar ultimoPonto pi 6)  "")
	(setq ultimoPonto (polar ultimoPonto pi 6))
	(command "line" ultimoPonto (polar ultimoPonto sam_paraBaixo 198.5255)  "")
	(setq linhaSuporteTrim2 (entlast)) ;<--- Linha suporte trim 2
	
	
	(setq sel nil)
	(setq sel (ssadd))
	(setq sel (ssadd linhaSuporteTrim1 sel))
	(setq sel (ssadd linhaSuporteTrim2 sel))
	
	
	(setq zoomInicial (viewextents))
	;Aplicar o trim
	(setq pontoZoomTrim  (polar ultimoPonto 4.829954554521022 25.57655176161385)   )
	(command "zoom" "c" pontoZoomTrim 10)
	(command "trim" sel "" pontoZoomTrim "")
	
	
	
	(setq pontoZoomTrim  (polar ultimoPontoBackup 4.594823406248356 25.57655176161385)   )
	(command "zoom" "c" pontoZoomTrim 10)
	(command "trim" sel2 "" pontoZoomTrim "")
	
	
	
	
	
	(command "zoom" "w" (nth 0 zoomInicial) (nth 1 zoomInicial))
	(princ)
)







