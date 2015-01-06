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

(defun saveVarsagric2()
	(setq comprimento (atof (get_tile "comprimento")))
	(setq compLastro (atof (get_tile "compLastro")))
	(setq distanciaGancho (atof (get_tile "distanciaGancho")))
)


(defun c:agric2()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	;DCL start
	(if (not(setq dcl_id (load_dialog "C:\\bfdias\\air grid\\dcl\\air_grid_corte2.dcl")))
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
					(action_tile "accept" "(saveVarsagric2)(done_dialog 2)")
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
	
	
	;(setq comprimento (atof (getstring "\nComprimento: "))) ;5000
	;(setq compLastro (atof (getstring "\nComprimento do lastro: "))) ;750
	;(setq distanciaGancho (atof (getstring "\nGancho distancia: ")))  ;195.5
	
	
	
	;(setq comprimento (atof "5000"))
	;(setq compLastro (atof "680"))
	;(setq distanciaGancho (atof "618.600000000006"))
	
	(setq pontoInsercao (getpoint "\nDefina o ponto de inserção"))
	
	
	;BF-03-SA-CNT 251
	(command "layer" "m" "BF-03-SA-CNT" "c" "251" "" "")
	
	(sam_linha pontoInsercao (polar pontoInsercao sam_paracima 106.0847))
	(sam_linha  pontoInsercao (polar pontoInsercao 0 25) )
	(sam_linha (polar pontoInsercao 0 25) (polar (polar pontoInsercao 0 25) 0.7853981633977392 35.35533905933767))
	(sam_linha (polar (polar pontoInsercao 0 25) 0.7853981633977392 35.35533905933767) (polar (polar (polar pontoInsercao 0 25) 0.7853981633977392 35.35533905933767) sam_paracima 25.0023))
	(sam_linha (polar pontoInsercao sam_paracima 106.0847) (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento))
	(sam_linha (polar pontoInsercao sam_paracima 56.0847) (polar (polar pontoInsercao sam_paracima 56.0847) 0 comprimento))
	(sam_linha (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento) (polar (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento) sam_parabaixo 106.0847))
	(sam_linha (polar (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento) sam_parabaixo 106.0847) (polar (polar (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento) sam_parabaixo 106.0847) pi 25))
	(sam_linha (polar (polar (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento) sam_parabaixo 106.0847) pi 25) (polar (polar (polar (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento) sam_parabaixo 106.0847) pi 25) 2.356194490192053 35.35533905933767))
	(sam_linha (polar (polar (polar (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento) sam_parabaixo 106.0847) pi 25) 2.356194490192053 35.35533905933767) (polar (polar (polar (polar (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento) sam_parabaixo 106.0847) pi 25) 2.356194490192053 35.35533905933767) sam_paracima 25.0023))
	
	(setq pontoTeste (polar pontoInsercao sam_paracima 56.0847))
	(setq pontoTeste (polar pontoTeste 0 (/ comprimento 2)))
	
	;Faz detalhe do meio
	(setq ponto1Det (polar pontoTeste pi 37.5))
	
	;31.0847
	(sam_linha ponto1Det (polar ponto1Det sam_parabaixo 31.0847))
	(setq lastPoint (polar ponto1Det sam_parabaixo 31.0847))
	(sam_linha lastPoint (polar lastPoint 5.497787143781846 35.35533905933767))
	(setq lastPoint (polar lastPoint 5.497787143781846 35.35533905933767))
	(sam_linha lastPoint (polar lastPoint 0 25))
	(setq lastPoint (polar lastPoint 0 25))
	(sam_linha lastPoint (polar lastPoint 0.7853981633977392 35.35533905933767))
	(setq lastPoint (polar lastPoint 0.7853981633977392 35.35533905933767))
	(sam_linha lastPoint (polar lastPoint sam_paracima 31.0847))
	(setq lastPoint (polar lastPoint sam_paracima 31.0847))
	;(command "circle" lastPoint 22)
	
	
	(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
	(setq pontoLegenda (polar lastPoint 3.753807873427421 40.83874381408266))
	(sam_linha pontoLegenda (polar pontoLegenda 1.763452457784091 189.2868986923690))
	(setq lastPoint (polar pontoLegenda 1.763452457784091 189.2868986923690))
	(sam_linha lastPoint (polar lastPoint pi 68.9170))
	(setq lastPoint (polar lastPoint pi 68.9170))
	(setq pontoCentroCirculo (polar lastPoint pi 88.15094314128145))
	(command "circle" pontoCentroCirculo 88.15094314128145)
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" "s" "BF-01-TXT"  (polar pontoCentroCirculo 3.942348754367712 45.89584747320030) 64.6800 0 "J")
	
	
	(command "layer" "m" "BF-03-SA-CNT" "c" "251" "" "")
		
	;Desenha o retângulo
	(setq pontoInicialRet (polar pontoInsercao 1.451368141768345 50.35663950689833))
	(sam_linha pontoInicialRet (polar pontoInicialRet sam_paracima 50))
	(setq linha1 (entlast))
	(sam_linha pontoInicialRet (polar pontoInicialRet 0 (+ compLastro 80)))
	(setq linha2 (entlast))
	(sam_linha (polar pontoInicialRet sam_paracima 50) (polar (polar pontoInicialRet sam_paracima 50) 0 (+ compLastro 80)))
	(setq linha3 (entlast))
	(sam_linha (polar (polar pontoInicialRet sam_paracima 50) 0 (+ compLastro 80)) (polar pontoInicialRet 0 (+ compLastro 80)) )
	(setq linha4 (entlast))
	
	(setq sel nil)
	(setq sel (ssadd))
	(setq sel (ssadd linha1 sel))
	(setq sel (ssadd linha2 sel))
	(setq sel (ssadd linha3 sel))
	(setq sel (ssadd linha4 sel))
	;transforma em polyline
	(command "pedit" linha4 "y" "J" sel "" "")
	
	
	(setq pontoInicialRet2 (polar (polar (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento) sam_parabaixo 106.0847) 1.690025121035790 50.44284105300913))
	(sam_linha pontoInicialRet2 (polar pontoInicialRet2 pi (+ compLastro 80)))
	(setq linha11 (entlast))
	(sam_linha pontoInicialRet2 (polar pontoInicialRet2 sam_paracima 50))
	(setq linha22 (entlast))
	(sam_linha (polar pontoInicialRet2 pi (+ compLastro 80)) (polar (polar pontoInicialRet2 pi (+ compLastro 80)) sam_paracima 50))
	(setq linha33 (entlast))
	(sam_linha (polar (polar pontoInicialRet2 pi (+ compLastro 80)) sam_paracima 50) (polar pontoInicialRet2 sam_paracima 50) )
	(setq linha44 (entlast))
	
	(setq sel nil)
	(setq sel (ssadd))
	(setq sel (ssadd linha11 sel))
	(setq sel (ssadd linha22 sel))
	(setq sel (ssadd linha33 sel))
	(setq sel (ssadd linha44 sel))
	;transforma em polyline
	(command "pedit" linha44 "y" "J" sel "" "")
	
	
	
	;Desenhando detalhes
	;(polar pontoInsercao sam_paracima 106.0847)
	;(command "circle" (polar pontoInsercao sam_paracima 106.0847) 10)
	(setq p1 (polar (polar pontoInsercao sam_paracima 106.0847) 0 50))
	(sam_linha p1 (polar p1 sam_parabaixo 6.0868))
	
	(setq ponto2 (polar (polar pontoInsercao sam_paracima 106.0847) 0 (+ compLastro 50) ) )
	
	(sam_linha ponto2 (polar ponto2 sam_parabaixo 6))
	(sam_linha (polar ponto2 sam_parabaixo 6) (polar (polar ponto2 sam_parabaixo 6) 0 44))
	(sam_linha (polar (polar ponto2 sam_parabaixo 6) 0 44) (polar (polar (polar ponto2 sam_parabaixo 6) 0 44) sam_parabaixo 44))
	
	(setq ponto3 (polar ponto2 0 50.0038))
	
	
	(setq ponto3Back2 ponto3)
	
	(sam_linha ponto3 (polar ponto3 sam_parabaixo 50))
	
	;Legenda
	(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
	(setq pontoILeg3 (polar ponto3Back2 pi 20.84307539684232))
	(sam_linha pontoILeg3 (polar pontoILeg3 1.678322956728932 146.2200342559316 ))
	(setq lastPoint3 (polar pontoILeg3 1.678322956728932 146.2200342559316 ))
	(sam_linha lastPoint3 (polar lastPoint3 pi 31.3399))
	(setq lastPoint3 (polar lastPoint3 pi 31.3399))
	(setq pontoICirculo (polar lastPoint3 pi 88.15094314128510))
	(command "circle" pontoICirculo 88.15094314128510)
	(setq pontoInsertLegenda (polar pontoICirculo 4.044960994771498 41.94951796055041))
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" "s" "BF-01-TXT" pontoInsertLegenda 64.6800 0 "B")
	
	
	;BF-07-SA-COT 250
	(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
	(command "dimlinear" pontoInsercao (polar ponto3 sam_parabaixo 50) (polar (sam_metade pontoInsercao (polar ponto3 sam_parabaixo 50) ) sam_parabaixo 300) )
	
	(setq ponto3Back (polar ponto3 sam_parabaixo 50))
	
	(setq ponto3 (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento))
	(setq ponto4 (polar ponto3 pi (+ compLastro 50)))
	
	(command "layer" "m" "BF-03-SA-CNT" "c" "251" "" "")
	(sam_linha ponto4 (polar ponto4 sam_parabaixo 6))
	(sam_linha  (polar ponto4 sam_parabaixo 6)  (polar (polar ponto4 sam_parabaixo 6) pi 44))
	(sam_linha (polar (polar ponto4 sam_parabaixo 6) pi 44) (polar (polar (polar ponto4 sam_parabaixo 6) pi 44) sam_parabaixo 44))
	
	(setq ponto5 (polar ponto4 pi 50))
	(sam_linha ponto5 (polar ponto5 sam_parabaixo 50))
	
	;Inserção de legenda 2
	(setq pnt1a (polar ponto4 pi 20.63671176151547))
	(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
	(sam_linha pnt1a (polar pnt1a 1.463269696860853 145.9396825099136))
	(setq pntUltimo (polar pnt1a 1.463269696860853 145.9396825099136))
	(sam_linha pntUltimo (polar pntUltimo 0 31.33989463164471))
	(setq pntUltimo (polar pntUltimo 0 31.33989463164471))
	(setq pntCenCir (polar pntUltimo 0 88.15094314128510))
	(command "circle" pntCenCir 88.15094314128510)
	(setq pntLetra (polar pntCenCir 3.992685482347045 43.81353798219985))
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" "s" "BF-01-TXT" pntLetra 64.6800 0 "B")
	
	;88.15094314128510
	
	
	;31.33989463164471
	
	
	(setq ponto7 (polar (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento) sam_parabaixo 106.0847))
	
	(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
	(command "dimlinear"  (polar ponto5 sam_parabaixo 50) ponto7 (polar (sam_metade (polar ponto5 sam_parabaixo 50) ponto7 ) sam_parabaixo 300 ) )
	(command "dimlinear" ponto3Back (polar ponto5 sam_parabaixo 50) (polar (sam_metade ponto3Back (polar ponto5 sam_parabaixo 50)) sam_parabaixo (+ 300 28.0423) ) )
	(command "dimlinear" pontoInsercao  ponto7  (polar (sam_metade pontoInsercao  ponto7) sam_parabaixo 600) )
	
	
	
	;Inserção do gancho
	;distanciaGancho
	
	(setq hookpoint (polar (polar pontoInsercao sam_paracima 106.0847) 0 distanciaGancho))
	
	;Inserção legenda  gancho
	(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
	(setq pontoILeg (polar hookpoint sam_paracima 36.75624903996504))
	(sam_linha pontoILeg (polar pontoILeg 1.927165417871261 115.6040014022982))
	(setq lastPoint2 (polar pontoILeg 1.927165417871261 115.6040014022982))
	(sam_linha lastPoint2 (polar lastPoint2 pi 31.3399))
	(setq lastPoint2 (polar lastPoint2 pi 31.3399))
	(setq pontoInternoCircle (polar lastPoint2 pi 88.15094314131419))
	(command "circle" pontoInternoCircle 88.15094314131419)
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" "s" "BF-01-TXT"  (polar pontoInternoCircle 4.343725494408493 33.21285878856097) 64.6800 0 "I")
	
	
	
	(command "layer" "m" "BF-01-SA-DIF" "c" "252" "" "")
	(sam_linha hookpoint (polar hookpoint sam_paracima 73.5125))
	(setq linhaSave3 (entlast))
	
	
	(command "_.ARC" "_C" (polar (polar hookpoint sam_paracima 73.5125) 0 4.5) (polar (polar hookpoint sam_paracima 73.5125) 0 9) (polar hookpoint sam_paracima 73.5125) )
	
	
	;(polar (polar hookpoint sam_paracima 73.5125) 0 9)
	(sam_linha (polar (polar hookpoint sam_paracima 73.5125) 0 9) (polar (polar (polar hookpoint sam_paracima 73.5125) 0 9) sam_parabaixo 73.5125))
	(setq linhaSave2 (entlast))
	
	;(command "circle" (polar hookpoint sam_paracima 73.5125) 22)
	
	(sam_linha hookpoint (polar (polar (polar hookpoint sam_paracima 73.5125) 0 9) sam_parabaixo 73.5125) )
	(setq linhaSave1 (entlast))
	
	
	
	(setq sel nil)
	(setq sel (ssadd))
	(setq sel (ssadd linhaSave3 sel))
	(setq sel (ssadd linhaSave2 sel))
	(setq sel (ssadd linhaSave1 sel))
	
	(command "pedit" linhaSave3 "y" "J" sel "" "")
	
	
	(setq hookpoint (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento)   )
	(setq hookpoint (polar hookpoint pi distanciaGancho))
	
	(command "layer" "m" "BF-01-SA-DIF" "c" "252" "" "")
	(sam_linha hookpoint (polar hookpoint sam_paracima 73.5125))
	(setq linhaSave3 (entlast))
	(command "_.ARC" "_C"  (polar (polar hookpoint sam_paracima 73.5125) pi 4.5)  (polar hookpoint sam_paracima 73.5125) (polar (polar hookpoint sam_paracima 73.5125) pi 9)  )
	
	(sam_linha (polar (polar hookpoint sam_paracima 73.5125) pi 9) (polar (polar (polar hookpoint sam_paracima 73.5125) pi 9)  sam_parabaixo 73.5125 ))
	(setq linhaSave2 (entlast))
	
	(sam_linha hookpoint (polar (polar (polar hookpoint sam_paracima 73.5125) pi 9)  sam_parabaixo 73.5125 ))
	(setq linhaSave1 (entlast))
	
	(setq sel nil)
	(setq sel (ssadd))
	(setq sel (ssadd linhaSave3 sel))
	(setq sel (ssadd linhaSave2 sel))
	(setq sel (ssadd linhaSave1 sel))
	
	(command "pedit" linhaSave3 "y" "J" sel "" "")
	
	
	;(command "circle" hookpoint 33)
	;Inserção da legenda no gancho, lá do outro lado =DD
	(setq pnt1 (polar hookpoint sam_paracima 36.75624903965218))
	(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
	(sam_linha pnt1 (polar pnt1 1.214427235718531 115.6040014022982)) ;<---pareiAQUI
	(setq pntEntlastPoint (polar pnt1 1.214427235718531 115.6040014022982))
	(sam_linha pntEntlastPoint (polar pntEntlastPoint 0 31.33989463164471))
	(setq pntEntlastPoint (polar pntEntlastPoint 0 31.33989463164471))
	;88.15094314128510
	(setq pontoCCircle (polar pntEntlastPoint 0 88.15094314128510))
	(command "circle" pontoCCircle 88.15094314128510)
	(setq pntIText (polar pontoCCircle 4.319662822362359 32.19112290251303))
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" "s" "BF-01-TXT" pntIText 64.6800 0 "I")
	
	
	
	
	;BF-05-SA-TXT  252
	
	;(command "circle" (polar pontoInsercao sam_parabaixo 747.5) 15)
	
	(command "text" "s" "BF-01-TXT"  (polar pontoInsercao sam_parabaixo 747.5) 100 0 "VISTA LATERAL")
	
	;(command "circle" (polar pontoInicialRet sam_parabaixo 14.7686) 15)
	
	; 250
	(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
	(sam_linha (polar pontoInicialRet sam_parabaixo 14.7686) (polar (polar pontoInicialRet sam_parabaixo 14.7686) pi 48.8624) )
	
	(command "circle" (polar (polar (polar pontoInicialRet sam_parabaixo 14.7686) pi 48.8624) pi 88.1509) 88.1509)
	
	;BF-05-SA-TXT
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" "s" "BF-01-TXT"  (polar (polar (polar (polar pontoInicialRet sam_parabaixo 14.7686) pi 48.8624) pi 88.1509) 3.942348754367712 45.89584747320030) 64.6800 0 "J")
	
	(setq nextPonto (polar (polar pontoInsercao sam_paracima 106.0847) 0 comprimento))
	
	(setq iPonto (polar nextPonto 4.624995243214551 68.74227686031176))
	;43.7172
	(command "layer" "m" "BF-05A-SA-TXT" "c" "250" "" "")
	(sam_linha iPonto (polar iPonto 0 43.7172))
	(setq lastPoint4 (polar iPonto 0 43.7172))
	(setq centerCircle (polar lastPoint4 0 88.15094314128510))
	(command "circle" centerCircle 88.15094314128510)
	(setq pontoTxt1 (polar centerCircle 3.992685482347483 43.81353798222174))
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" "s" "BF-01-TXT" pontoTxt1 64.6800 0 "J")
	
	
	(princ "\nPrograma finalizado!!!")
	;(command "circle" pontoInicialRet 15)
	
	
	;;transforma em polyline
	;(command "pedit" obj "y" "J" sel3 "")
	
	(princ)
)







