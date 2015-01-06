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


(defun insere_difusores33(pontoInsercao qtdDifusores comprimento)
	
	;Distancia entre difusores
	(setq distEntreDif (/ comprimento qtdDifusores))
	
	;Inserção do primeiro difusor
	(command "layer" "m" "difusores1_temp" "c" "red" "" "")
	;(command "circle" (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 10)
	(command "insert" "C:\\bfdias\\blocos\\desenha_air_grid3\\suporte_dif3_bloco.dwg" (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) sam_paraCima 77.5505) "" "" "")
	
	(setq qtdDifusores (- qtdDifusores 1))
	
	
	;Primeira cota
	(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
	(command "dimlinear" (polar pontoInsercao sam_paraCima 30) (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) sam_paraCima 77.5505) (polar (sam_metade (polar pontoInsercao sam_paraCima 30) (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) sam_paraCima 77.5505)) sam_paraBaixo (- 242.0431378389039 64.1753)))
	
	
	;Última cota
	(setq distanciaCota1 (distance (polar (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif qtdDifusores)) sam_paraCima 77.5505) (polar (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif (- qtdDifusores 1))) sam_paraCima 77.5505))  )
	(setq oooo (polar (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif qtdDifusores)) sam_paraCima 77.5505))
	(command "dimlinear" oooo (polar (polar oooo 0 (/ distanciaCota1 2)) sam_paraBaixo 128.3505)  (polar  (sam_metade oooo (polar (polar oooo 0 (/ distanciaCota1 2)) sam_paraBaixo 128.3505) ) sam_paraBaixo (- 242.0431378389039 64.1753) ) )
	
	
	
	
	;Inicia segunda linha das cotas e desenhando 
	;os suportes dos lastros =D 18/9/2013
	
	(command "layer" "m" "BF-SA-09-SUP" "c" "251" "" "")
	(setq p1 (polar (polar pontoInsercao sam_paraCima 30) 0 (+ comprimentoLastro 102)))
	(setq p1Back2 p1)
	(command "line" p1 (polar p1 0 6) "")
	(setq ultimo (polar p1 0 6))
	(command "line" ultimo (polar ultimo sam_paraCima 198.5255) "")
	(setq linhaSuporteLastro1 (entlast))
	(setq ultimo (polar ultimo sam_paraCima 198.5255))
	(command "line" ultimo (polar ultimo pi 6) "")
	(setq ultimo (polar ultimo pi 6))
	(command "line" ultimo (polar ultimo sam_paraBaixo 198.5255) "")
	(setq linhaSuporteLastro2 (entlast))
	(setq ultimo (polar ultimo sam_paraBaixo 198.5255))
	
	
	;Segunda linha de cota mais para baixo
	(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
	(command "dimlinear" (polar pontoInsercao sam_paraCima 30) ultimo  (polar (sam_metade (polar pontoInsercao sam_paraCima 30) ultimo) sam_paraBaixo 350) )
	(setq pontoZoomSuporteLastro (polar ultimo 1.553469591113429 173.1514907537635) )
	(setq pontoBack3 ultimo)
	
	
	;Loop para a inserção dos difusores restantes =D
	(while (> qtdDifusores 0)
		
		(command "layer" "m" "difusores1_temp" "c" "red" "" "")
		;(command "circle" (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif qtdDifusores)) 10)
		(command "insert" "C:\\bfdias\\blocos\\desenha_air_grid3\\suporte_dif3_bloco.dwg" (polar (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif qtdDifusores)) sam_paraCima 77.5505) "" "" "")
		
		
		;Make quota
		(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
		(command "dimlinear"  (polar (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif qtdDifusores)) sam_paraCima 77.5505)     (polar (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif (- qtdDifusores 1))) sam_paraCima 77.5505)    (polar  (sam_metade  (polar (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif qtdDifusores)) sam_paraCima 77.5505)     (polar (polar (polar (polar pontoInsercao sam_paraCima 80.8000) 0 (/ distEntreDif 2)) 0 (* distEntreDif (- qtdDifusores 1))) sam_paraCima 77.5505)    ) sam_paraBaixo 242.0431378389039))
		
		
		(setq qtdDifusores (- qtdDifusores 1))
		
	)
	
)

(defun saveVarsagric1()
	(setq comprimento (atof (get_tile "comprimento")))
	(setq qtdDifusores (atof (get_tile "qtdDifusores")))
	(setq comprimentoLastro (atof (get_tile "comprimentoLastro")))
	
)


(defun c:agric1()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	;DCL start
	(if (not(setq dcl_id (load_dialog "C:\\bfdias\\air grid\\dcl\\air_grid_corte1.dcl")))
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
					(action_tile "accept" "(saveVarsagric1)(done_dialog 2)")
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
	
	;(setq comprimento (atof (getstring "\nComprimento: "))) ;VAR IMPORTANTE
	;(setq qtdDifusores (atof (getstring "\nQuantidade de difusores: ")))
	;(setq comprimentoLastro (atof (getstring "\nComprimento do lastro: ")))
	
	
	
	(setq pontoInsercao (getpoint "\nPonto de inserção: "))
	
	(setq zoomInicial (viewextents))
	
	(insere_difusores33 pontoInsercao qtdDifusores comprimento)
	;(command "circle" pontoInsercao 20)
	
	
	(command "layer" "m" "BF-SA-09-SUP" "c" "251" "" "")
	;Faz ponto inicial extremidade "1"
	(setq p1 (polar pontoInsercao sam_paraCima 30))
	(setq p1Backup p1)
	
	
	
	(command "line" p1 (polar p1 sam_paraCima 223.9255) "")
	(setq pointBackup2 (polar p1 sam_paraCima 223.9255))
	
	(setq ultimo (polar p1 sam_paraCima 223.9255))
	(command "line" ultimo (polar ultimo pi 6) "" )
	(setq ultimo (polar ultimo pi 6))
	(command "line" ultimo (polar ultimo sam_paraBaixo 223.9255) "")
	(setq ultimo (polar ultimo sam_paraBaixo 223.9255))
	(command "line" ultimo (polar ultimo 0 6) "")
	(setq ultimo (polar ultimo 0 6))
	
	;Faz ponto inicial extremidade "2"
	(setq p1 (polar (polar pontoInsercao sam_paraCima 30) 0 (+ comprimento 6)))
	
	(command "line" p1 (polar p1 sam_paraCima 325) "")
	(setq ultimo (polar p1 sam_paraCima 325))
	(command "line" ultimo (polar ultimo pi 6) "" )
	(setq ultimo (polar ultimo pi 6))
	(command "line" ultimo (polar ultimo sam_paraBaixo 325) "")
	(setq ultimo (polar ultimo sam_paraBaixo 325))
	(setq pontoFinal ultimo)
	
	(command "line" ultimo (polar ultimo 0 6) "")
	(setq ultimo (polar ultimo 0 6))
	
	
	
	(setq pontoTubo1 (polar p1Backup sam_paraCima 173.1255))
	(command "line" pontoTubo1 (polar pontoTubo1 0 (- comprimento 101.4000)) "")
	(command "line"  (polar pontoTubo1 0 (- comprimento 101.4000)) (polar (polar pontoTubo1 0 (- comprimento 101.4000)) sam_paraCima 50.8000)  "")
	
	;Faz cota
	(command "dimaligned" pointBackup2 (polar (polar pontoTubo1 0 (- comprimento 101.4000)) sam_paraCima 50.8000) (sam_metade pointBackup2 (polar (polar pontoTubo1 0 (- comprimento 101.4000)) sam_paraCima 50.8000)))
	
	
	;(command "circle" pontoInicialLinhaMeio 20)
	
	(setq pontoInicialLinhaMeio (polar p1Backup sam_paraCima 198.5255))
	
	(command "layer" "m" "BF-SA-06-LCE" "c" "red" "" "")
	(command "line" pontoInicialLinhaMeio (polar pontoInicialLinhaMeio 0 (- comprimento 101.4000)) "")
	
	
	
	
	;Faz o trim no primeiro suporte do lastro
	(setq sel nil)
	(setq sel (ssadd))
	(setq sel (ssadd linhaSuporteLastro1 sel))
	(setq sel (ssadd linhaSuporteLastro2 sel))
	(command "zoom" "c" pontoZoomSuporteLastro 50)
	(command "trim" sel "" pontoZoomSuporteLastro "")
	
	
	;Desenha suporte lastro2
	(setq inicio (polar pontoFinal pi (+ comprimentoLastro 102)))
	(command "layer" "m" "BF-SA-09-SUP" "c" "251" "" "")
	(command "line" inicio (polar inicio pi 6) "")
	(setq ultimo (polar inicio pi 6))
	(command "line" ultimo (polar ultimo sam_paraCima 198.5255) "")
	(setq linhaSuporteLastro1 (entlast))
	(setq ultimo (polar ultimo sam_paraCima 198.5255))
	(command "line" ultimo (polar ultimo 0 6) "")
	(setq ultimo (polar ultimo 0 6))
	(command "line" ultimo (polar ultimo sam_paraBaixo 198.5255) "")
	(setq linhaSuporteLastro2 (entlast))
	(setq ultimo (polar ultimo sam_paraBaixo 198.5255))
	
	(setq sel nil)
	(setq sel (ssadd))
	(setq sel (ssadd linhaSuporteLastro1 sel))
	(setq sel (ssadd linhaSuporteLastro2 sel))
	(setq pontoZoomSuporteLastro (polar inicio 1.588123062476363 173.1514907537635))
	(command "zoom" "c" pontoZoomSuporteLastro 50)
	(command "trim" sel "" pontoZoomSuporteLastro "")
	
	(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
	(command "dimlinear" inicio pontoFinal (polar (sam_metade inicio pontoFinal) sam_paraBaixo 350) )
	(command "dimlinear" pontoBack3 inicio (polar (sam_metade pontoBack3 inicio) sam_paraBaixo 350))
	
	;Dimension from end to end
	(command "dimlinear" p1Backup pontoFinal (polar (sam_metade p1Backup pontoFinal) sam_paraBaixo 650))
	
	
	
	
	(command "zoom" "w" (nth 0 zoomInicial) (nth 1 zoomInicial))
	(princ)
	
)








