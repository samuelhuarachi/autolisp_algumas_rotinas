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
;verificaRangeNumero ---- meio ----- range2  / bolinha("A" aberto ou "F" fechado)
;cuidado ao usar a variavel contador




(defun faz_o_trim_nos_difusores_air_grid(ponto1)
	;lista_coordenadas_lastros
	(setq qtdElemArray (vl-list-length lista_coordenadas_lastros))
	
	(setq sel (ssadd))
	
	;Faz Linha auxiliar no difusor
	(setq p1Aux (polar ponto1 2.16168 80.7756))
	(command "layer" "m" "linhaAuxiliar" "c" "241" "" "")
	(command "line" p1Aux (polar p1Aux (/ pi 2) 959.8148) "")
	(setq linhaAux1 (entlast))
	
	
	(setq p2Aux (polar ponto1 0.979908 80.7756))
	(command "line" p2Aux (polar p2Aux (/ pi 2) 959.8148) "")
	(setq linhaAux2 (entlast))
	
	
	(setq p3Aux (polar ponto1 4.12202 80.8298))
	(command "line" p3Aux (polar p3Aux (* (/ pi 2)3) 959.8148) "")
	(setq linhaAux3 (entlast))
	
	
	(setq p4Aux (polar ponto1 5.30285 80.8348))
	(command "line" p4Aux (polar p4Aux (*(/ pi 2)3) 959.8148) "")
	(setq linhaAux4 (entlast))
	
	(setq sel (ssadd linhaAux1 sel))
	(setq sel (ssadd linhaAux2 sel))
	(setq sel (ssadd linhaAux3 sel))
	(setq sel (ssadd linhaAux4 sel))
	
	
	(while (> qtdElemArray 0)
		
		(setq coordenadasAtual (nth (- qtdElemArray 1) lista_coordenadas_lastros))
		(setq coord1 (nth 0 coordenadasAtual))
		(setq coord2 (nth 1 coordenadasAtual))
		
		;Define quem é a maior coordenada,
		;e quem é a menor coordenada!
		(if (< (nth 0 coord1) (nth 0 coord2) )
			(progn
				
			)
			(progn
				(setq coordAux coord1)
				(setq coord1 coord2)
				(setq coord2 coordAux)
			)
		)
		
		;pontoInsDifusor
		(> (nth 0 pontoInsDifusor) (nth 0 coord1))
		
		(if (and (> (nth 0 ponto1) (nth 0 coord1))  (< (nth 0 ponto1) (nth 0 coord2))   )
			(progn
				;Aplicar o trim
				(setq pontoZoomTrim  (list (nth 0 ponto1) (nth 1 coord1) 0.0)   )
				(command "zoom" "c" pontoZoomTrim 10)
				(command "trim" sel "" pontoZoomTrim "")
			)
		)
		
		(setq qtdElemArray (- qtdElemArray 1))
	)
	
	
	(command "erase" sel "")
)


(defun faz_trim_nas_linhas_cruzamento(ponto1)
	;lista_linhas_cruzamento
	;Faz Linha auxiliar no difusor
	(setq p1Aux (polar ponto1 2.16168 80.7756))
	(command "layer" "m" "linhaAuxiliar" "c" "241" "" "")
	(command "line" p1Aux (polar p1Aux (/ pi 2) 959.8148) "")
	(setq linhaAux1 (entlast))
	
	(setq p2Aux (polar ponto1 0.979908 80.7756))
	(command "line" p2Aux (polar p2Aux (/ pi 2) 959.8148) "")
	(setq linhaAux2 (entlast))
	
	(setq p3Aux (polar ponto1 4.12202 80.8298))
	(command "line" p3Aux (polar p3Aux (* (/ pi 2)3) 959.8148) "")
	(setq linhaAux3 (entlast))
	
	
	(setq p4Aux (polar ponto1 5.30285 80.8348))
	(command "line" p4Aux (polar p4Aux (*(/ pi 2)3) 959.8148) "")
	(setq linhaAux4 (entlast))
	
	(setq sel (ssadd linhaAux1 sel))
	(setq sel (ssadd linhaAux2 sel))
	(setq sel (ssadd linhaAux3 sel))
	(setq sel (ssadd linhaAux4 sel))
	
	(setq qtdElemArray (vl-list-length lista_linhas_cruzamento))
	
	(while (> qtdElemArray 0)
		
		(setq coord1 (nth 0 (nth (- qtdElemArray 1) lista_linhas_cruzamento)))
		(setq coord2 (nth 1 (nth (- qtdElemArray 1) lista_linhas_cruzamento)))
		
		(setq interseccao (inters p1Aux (polar p1Aux (/ pi 2) 959.8148) coord1 coord2 ))
		(setq interseccao2 (inters p2Aux (polar p2Aux (/ pi 2) 959.8148) coord1 coord2 ))
		
		(if (and (/= interseccao nil) (/= interseccao2 nil) )
			(progn
				(setq pontoZoomTrim  (polar interseccao (angle interseccao interseccao2)  (/ (distance interseccao interseccao2) 2))   )
				(command "zoom" "c" pontoZoomTrim 10)
				(command "trim" sel "" pontoZoomTrim "")
			)
		)
		
		(setq interseccao (inters p3Aux (polar p3Aux (* (/ pi 2)3) 959.8148) coord1 coord2 ))
		(setq interseccao2 (inters p4Aux (polar p4Aux (*(/ pi 2)3) 959.8148) coord1 coord2 ))
		(if (and (/= interseccao nil) (/= interseccao2 nil) )
			(progn
				(setq pontoZoomTrim  (polar interseccao (angle interseccao interseccao2)  (/ (distance interseccao interseccao2) 2))   )
				(command "zoom" "c" pontoZoomTrim 10)
				(command "trim" sel "" pontoZoomTrim "")
			)
		)
		
		(setq qtdElemArray (- qtdElemArray 1))
	)
	(command "erase" sel "")
)
 

;range 140
(defun verifica_se_nao_existe_difusor_nessa_posicao(ponto)
	(setq sobrescreveu 0)
	
	;Inserção dos difusores
	(setq qtdDifusorMetade (/ qtdDifusor 2))
	(setq distanciaEntreDifusor (/ comprimento qtdDifusorMetade))
	(setq distInicio (/ distanciaEntreDifusor 2))
	
	;Ponto inserção difusor
	(setq pontoInsDifusor (polar pontoInsercao (/ pi 2)  (/ 2180 2)))
	
	(setq contador3 0)
	(while (> qtdDifusorMetade 0)
		
		;(command "layer" "m" "samuelS2Renata" "c" "231" "" "")
		;(command "circle" (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ) 30)
		
		(setq pontoDif (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ))
		(setq range2 (polar pontoDif 0 70))
		(setq range1 (polar pontoDif pi 70))
		
		;(command "insert" "C:\\bfdias\\blocos\\90X1000 3.dwg" (polar (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ) 1.63749 1087.9) "" "" "" "")
		
		(if (and (> (nth 0 ponto) (nth 0 range1))   (< (nth 0 ponto) (nth 0 range2))   )
			(progn
				(setq sobrescreveu 1)
			)
		)
		
		(setq contador3 (+ contador3 1))
		(setq qtdDifusorMetade (- qtdDifusorMetade 1))
	)
	
	
	
	sobrescreveu
)

(defun ajusta_difusores_invertidos(lista)
	
	(setq totalDifusor (vl-list-length lista))
	(while (> totalDifusor 0)
		(setq objDif (nth 0 (nth (- totalDifusor 1) lista) )  )
		(setq coord (cdr (assoc 10 (entget objDif))))
		(vl-cmdf "._mirror" objDif "" coord (polar coord (/ pi 2) 100) "_y")
		(command "move" objDif "" coord (polar coord pi 145))
		
		(setq totalDifusor (- totalDifusor 1))
	)
)


;lastroFimHelp.png (C:\bfdias\lastroFimHelp.png)
(defun ajusta_trim_fim_base(ponto1 ponto2 pontoDifusor)
	
	(setq pontofim1Dif (polar pontoDifusor 2.378861371255418  62.24796498051237 ))
	(setq pontofim2Dif (polar pontoDifusor 0.7627816810226796 62.24468247109927 ))
	
	(if (and (> (nth 0 ponto1) (nth 0 pontofim1Dif))(<  (nth 0 ponto1)(nth 0 pontofim2Dif) ))
		(progn
			(princ "\n 1")
			
			
			
			(setq pontofim111Dif (polar pontoDifusor 3.904374334612493 62.24468247110053 ))
			(setq pontofim222Dif (polar pontoDifusor 5.520454024845189 62.24796498051362 ))
			(command "layer" "m" "linhaTemp" "c" "red" "" "")
			(command "line" pontofim111Dif pontofim222Dif "" )
			(setq linhaTempBase2 (entlast))
			
			(setq pontoZoomTrim (polar (list (nth 0 ponto1) (nth 1 pontofim111Dif) 0.0) (* (/ pi 2)3) 3))
			(command "zoom" "c" pontoZoomTrim 10)
			(command "trim" linhaTempBase2 "" pontoZoomTrim "")
			
			
			
			(setq pontoZoomTrim (polar (list (nth 0 ponto1) (nth 1 pontofim1Dif) 0.0) (/ pi 2) 3))
			(command "line" pontofim1Dif pontofim2Dif "" )
			(setq linhaTempBase (entlast))
			(command "zoom" "c" pontoZoomTrim 10)
			(command "trim" linhaTempBase "" pontoZoomTrim "")
			
			
			(command "erase" linhaTempBase2 "")
			(command "erase" linhaTempBase "")
			
			
		)
	)
	
	
	
	(setq ponto1 (polar ponto1 0 50))
	(if (and (> (nth 0 ponto1) (nth 0 pontofim1Dif))(<  (nth 0 ponto1)(nth 0 pontofim2Dif) ))
		(progn
			(princ "\n SEGUNDO")
			
			(setq ponto0009 ponto1)
			
			(setq pontofim111Dif (polar pontoDifusor 3.904374334612493 62.24468247110053 ))
			(setq pontofim222Dif (polar pontoDifusor 5.520454024845189 62.24796498051362 ))
			(command "layer" "m" "linhaTemp" "c" "red" "" "")
			(command "line" pontofim111Dif pontofim222Dif "" )
			(setq linhaTempBase2 (entlast))
			(setq pontoZoomTrim (polar (list (nth 0 ponto1) (nth 1 pontofim111Dif) 0.0) (* (/ pi 2)3) 3))
			(command "zoom" "c" pontoZoomTrim 30)
			;(command "trim" linhaTempBase2 "" pontoZoomTrim "")
			(setq pontoZoomTrim (polar (list (nth 0 ponto1) (nth 1 pontofim1Dif) 0.0) (/ pi 2) 3))
			(command "line" pontofim1Dif pontofim2Dif "" )
			(setq linhaTempBase (entlast))
			(command "zoom" "c" pontoZoomTrim 10)
			;(command "trim" linhaTempBase "" pontoZoomTrim "")
			(command "erase" linhaTempBase2 "")
			(command "erase" linhaTempBase "")
			
			
		)
	)
	
	(setq ponto1 ponto2)
	(if (and (> (nth 0 ponto1) (nth 0 pontofim1Dif))(<  (nth 0 ponto1)(nth 0 pontofim2Dif) ))
		(progn
			(princ "\n 3")
			
			
			(setq pontofim111Dif (polar pontoDifusor 3.904374334612493 62.24468247110053 ))
			(setq pontofim222Dif (polar pontoDifusor 5.520454024845189 62.24796498051362 ))
			(command "layer" "m" "linhaTemp" "c" "red" "" "")
			(command "line" pontofim111Dif pontofim222Dif "" )
			(setq linhaTempBase2 (entlast))
			
			(setq pontoZoomTrim (polar (list (nth 0 ponto1) (nth 1 pontofim111Dif) 0.0) (* (/ pi 2)3) 3))
			(command "zoom" "c" pontoZoomTrim 10)
			(command "trim" linhaTempBase2 "" pontoZoomTrim "")
			
			
			
			(setq pontoZoomTrim (polar (list (nth 0 ponto1) (nth 1 pontofim1Dif) 0.0) (/ pi 2) 3))
			(command "line" pontofim1Dif pontofim2Dif "" )
			(setq linhaTempBase (entlast))
			(command "zoom" "c" pontoZoomTrim 10)
			(command "trim" linhaTempBase "" pontoZoomTrim "")
			
			
			(command "erase" linhaTempBase2 "")
			(command "erase" linhaTempBase "")
		)
	)
	
	(setq ponto1 (polar ponto2 pi 50))
	(if (and (> (nth 0 ponto1) (nth 0 pontofim1Dif))(<  (nth 0 ponto1)(nth 0 pontofim2Dif) ))
		(progn
			(princ "\n 4")
			
			(setq pontofim111Dif (polar pontoDifusor 3.904374334612493 62.24468247110053 ))
			(setq pontofim222Dif (polar pontoDifusor 5.520454024845189 62.24796498051362 ))
			(command "layer" "m" "linhaTemp" "c" "red" "" "")
			(command "line" pontofim111Dif pontofim222Dif "" )
			(setq linhaTempBase2 (entlast))
			
			(setq pontoZoomTrim (polar (list (nth 0 ponto1) (nth 1 pontofim111Dif) 0.0) (* (/ pi 2)3) 3))
			(command "zoom" "c" pontoZoomTrim 10)
			;(command "trim" linhaTempBase2 "" pontoZoomTrim "")
			
			(setq pontoZoomTrim (polar (list (nth 0 ponto1) (nth 1 pontofim1Dif) 0.0) (/ pi 2) 3))
			(command "line" pontofim1Dif pontofim2Dif "" )
			(setq linhaTempBase (entlast))
			(command "zoom" "c" pontoZoomTrim 10)
			;(command "trim" linhaTempBase "" pontoZoomTrim "")
			
			(command "erase" linhaTempBase2 "")
			(command "erase" linhaTempBase "")
		)
	)
	
)


(defun desenha_perimetro_difusor(pinicio angSomar)
	(setq sel nil)
	(setq sel (ssadd))
	(command "layer" "m" "temp_layer_perimetro1" "c" "51" "" "")
	(command "line" pinicio (polar pinicio (+ (/ pi 2) angSomar) 11.1942) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar pinicio (+ (/ pi 2) angSomar) 11.1942))
	(command "line" ultmPonto (polar ultmPonto (+ 0 angSomar) 3) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ 0 angSomar) 3))
	(command "line" ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) 12.9127) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) 12.9127))
	(command "line" ultmPonto (polar ultmPonto (+ pi angSomar) 3) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ pi angSomar) 3))
	(command "line" ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) (+ 0.2210 959.8148)) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) (+ 0.2210 959.8148)))
	(command "line" ultmPonto (polar ultmPonto (+ 0 angSomar) 3) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ 0 angSomar) 3))
	(command "line" ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) 12.9127) "")
	(setq sel (ssadd (entlast) sel))
	
	sel
)

(defun desenha_perimetro_difusor2 (pinicio angSomar)
	(setq sel nil)
	(setq sel (ssadd))
	(command "layer" "m" "temp_layer_perimetro1" "c" "51" "" "")
	(command "line" pinicio (polar pinicio (+ (/ pi 2) angSomar) 11.1942) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar pinicio (+ (/ pi 2) angSomar) 11.1942))
	(command "line" ultmPonto (polar ultmPonto (+ pi angSomar) 3) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ pi angSomar) 3))
	(command "line" ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) 12.9127) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) 12.9127) )
	(command "line" ultmPonto (polar ultmPonto (+ 0 angSomar) 3) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ 0 angSomar) 3) )
	(command "line" ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) (+ 0.2210 959.8148)) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) (+ 0.2210 959.8148)) )
	(command "line" ultmPonto (polar ultmPonto (+ pi angSomar) 3) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ pi angSomar) 3) )
	(command "line" ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) 12.9127) "")
	(setq sel (ssadd (entlast) sel))
	(setq ultmPonto (polar ultmPonto (+ (/ pi 2) angSomar) 12.9127) )
	
	
	sel
)

(defun faz_trim_final_lastros(pontoDif)
	
	;(command "layer" "m" "NOME_DA_LAYER" "c" "251" "" "")
	;(command "circle" pontoDif 20)
	
	(setq pontofim1Dif (polar pontoDif 2.378861371255418  62.24796498051237 ))
	(setq pontofim2Dif (polar pontoDif 0.7627816810226796 62.24468247109927 ))
	
	(setq pontofim111Dif (polar pontoDif 3.904374334612493 62.24468247110053 ))
	(setq pontofim222Dif (polar pontoDif 5.520454024845189 62.24796498051362 ))
	
	
	(setq qtdElemArray (vl-list-length lista_coordenadas_lastros))
	
	(while (> qtdElemArray 0)
		
		(setq coordLastro (nth (- qtdElemArray 1) lista_coordenadas_lastros))
		(setq coordLastro1 (nth 0 coordLastro))
		(setq coordLastro2 (nth 1 coordLastro))
		;(command "layer" "m" "NOME_DA_LAYER" "c" "211" "" "")
		;(command "circle" coordLastro1 20)
		
		(if (and (> (nth 0 coordLastro1) (nth 0 pontofim1Dif))(< (nth 0 coordLastro1) (nth 0 pontofim2Dif)))
			(progn
				(command "layer" "m" "NOME_DA_LAYER" "c" "211" "" "")
				
				(setq objPeri (desenha_perimetro_difusor pontofim2Dif 0))
				(setq objPeri2 (desenha_perimetro_difusor2 pontofim1Dif pi ))
				(command "move" objPeri2 "" pontofim1Dif pontofim222Dif)
				
				
				(command "zoom" "c" coordLastro1 10)
				(command "trim" objPeri "" coordLastro1 "")
				
				(command "zoom" "c" coordLastro1 10)
				(command "trim" objPeri2 "" coordLastro1 "")
				
				(command "erase" objPeri "")
				(command "erase" objPeri2 "")
				
			)
		)
		
		(if (and (> (nth 0 coordLastro2) (nth 0 pontofim1Dif))(< (nth 0 coordLastro2) (nth 0 pontofim2Dif)))
			(progn
				(command "layer" "m" "NOME_DA_LAYER" "c" "211" "" "")
				
				(setq objPeri (desenha_perimetro_difusor2 pontofim1Dif 0))
				(setq objPeri2 (desenha_perimetro_difusor pontofim1Dif pi ))
				(command "move" objPeri2 "" pontofim1Dif pontofim111Dif)
				
				(command "zoom" "c" coordLastro2 10)
				(command "trim" objPeri "" coordLastro2 "")
				
				(command "zoom" "c" coordLastro2 10)
				(command "trim" objPeri2 "" coordLastro2 "")
				
				(command "erase" objPeri "")
				(command "erase" objPeri2 "")
			)
		)
		
		
		(setq qtdElemArray (- qtdElemArray 1))
	)
	
)

(defun desenha_obs_gancho(ponto / lastPoint)
	(command "layer" "m" "OBS_GANCHO" "c" "red" "" "")
	(command "insert" "C:\\bfdias\\blocos\\configGancho_bloco.dwg" ponto "" "" "")
	
	;(command "line" ponto (polar ponto sam_paraBaixo 30) "")
	;(command "change" (entlast) "" "p" "c" "bylayer" "lw" 0.5 "")
	;(command "line" ponto (polar ponto (- sam_paraBaixo (/ pi 4)) 30) "")
	;(command "change" (entlast) "" "p" "c" "bylayer" "lw" 0.5 "")
	;(command "line" ponto (polar ponto (- sam_paraBaixo (/ (/ pi 4) 2)) 75) "")
	;(command "change" (entlast) "" "p" "c" "bylayer" "lw" 0.5 "")
	;(setq lastPoint (polar ponto (- sam_paraBaixo (/ (/ pi 4) 2)) 75))
	;(setq lastPoint (polar lastPoint sam_paraBaixo 15))
	;(command "text" "bc" lastPoint 10 0 "Configurar os ganchos")
	;(command "change" (entlast) "" "p" "c" "bylayer" "lw" 0.2 "")
	
)


(defun funcao_faz_trim(pontoBase ang dist / nextPoint objEncontrado )
	
	(setq nextPoint (polar pontoBase ang dist))
	(command "zoom" "c" nextPoint 10)
	(setq objEncontrado (ssget "C" (polar nextPoint (* (/ pi 4) 5) 1) (polar nextPoint (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
	(if (/= objEncontrado nil)
		(progn
			(command "erase" objEncontrado "")
		)
	)
)


(defun doButton(a)
	(cond
		((= a 1)(alert "Button 1 was pressed!"))
		((= a 2)(alert "Button 2 was pressed!"))
		((= a 3)(alert "Button 3 was pressed!"))
	)
)


(defun saveVars()
	(setq comprimento (atof (get_tile "comprimento")))
	(setq tamTubo (atof (get_tile "comLastro")))
	(setq qtdDeLastros (atof (get_tile "qtdLastros")))
	(setq modeloLastro (atof (get_tile "quartCinque")))
	(setq larTuboInterno (atof (get_tile "larCentral")))
	(setq qtdDifusor (atof (get_tile "quantidadeDif"))) ;34 e 20, apresentam as colisões
)



(defun procuraObjRichard1(XFixo / listaObjRich1)
	(setq listaObjRich1 nil)
	(setq all (ssget "x" (List (cons 8 "BF-02-SA-GRA"))))
	
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))
			(princ "\n--------------------------")
			(princ "\n--------------------------")
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq tipo (strcase (cdr (assoc 0 (entget obj)))))
				
				(if (= tipo "LINE")
					(progn
						(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
						(setq coord (cdr (assoc 10 (entget obj))))
						(setq coord2 (cdr (assoc 11 (entget obj))))
						(setq angulo (angle coord coord2))
						;(if (and  (= coord XFixo)   (or (= angulo (/ pi 2)) (= angulo (* (/ pi 2) 3)))    )
						;	(progn
						;		(setq listaObjRich1 (cons (list obj) listaObjRich1))
						;	)
						;)
						
						(if  (and  (= (rtos (nth 0 coord) 2 1) (rtos XFixo 2 1))  (or (=  (rtos angulo 2 3)  (rtos  (/ pi 2) 2 3))   (= (rtos angulo 2 3)   (rtos (* (/ pi 2) 3) 2 3)  )) )
							(progn
								(setq listaObjRich1 (cons (list obj) listaObjRich1))
							)
						)
						
						
						(if (or (= angulo (/ pi 2)) (= angulo (* (/ pi 2) 3)))
							(progn
								;(alert (strcat (rtos (nth 0 coord) 2 1) "---" (rtos XFixo 2 1) "!"))
							)
						)
					)
				)
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	listaObjRich1
)



(defun resgataLinhasXdata(xdataName layerName / lista_elementos)
	
	(setq all (ssget "x" (List (cons 8 layerName))))
	;(setq all (ssget "x" '((-4 . "<AND") (8 . "ESPECIAL")(0 . "TEXT")(-4 . "AND>"))))
	(setq lista_elementos nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))
			(princ "\n--------------------------")
			(princ "\n--------------------------")
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName2 (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(setq valor_  (GetId obj "nome"))
				(if (/= valor_ nil)
					(progn
						(if (= (strcase valor_) (strcase xdataName))
							(progn
								(setq lista_elementos (cons (list obj) lista_elementos))
							)
						)
					)
				)
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	lista_elementos
)


(defun coeficiente_linear(coeficienteAngular coord)
	
	(setq x (nth 0 coord))
	(setq y (nth 1 coord))
	
	(setq b (- y (* coeficienteAngular x)))
	
	b
)

(defun coeficiente_angular(p1 p2)
	(setq x1 (nth 0 p1))
	(setq x2 (nth 0 p2))
	(setq y1 (nth 1 p1))
	(setq y2 (nth 1 p2))
	
	(setq deltaY (- y2 y1))
	(setq deltaX (- x2 x1))
	
	(setq coeficiente (/ deltaY deltaX))
	
	coeficiente
)

(defun grava_informacoes_coordenadas (coord / arq)
	(setq txtNameFile "COORD_DIFUSORES.txt")
	(setq x (rtos (nth 0 coord) 2 10))
	(setq y (rtos (nth 1 coord) 2 10))
	
	
	(setq arq (open (strcat (strcat "C:\\bfdias\\air grid\\txt\\" IDDESENHO "\\") txtNameFile) "w"))
	(write-line (strcat x y) arq)
	
	
	(close arq)
)


(defun c:dgrid()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq IDDESENHO (rtos (getvar "CDATE") 2 8))
	(vl-mkdir (strcat "C:\\BF_LISP_CONF\\AIR GRID\\" IDDESENHO))
	
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
					(action_tile "accept" "(saveVars)(done_dialog 2)")
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
	
	(setq larTuboInternoD2 (/ larTuboInterno 2))
	(princ "\n*****************************")
	(setq pontoInsercao (getpoint "\nDefina o ponto de inserção do sistema air grid: "))
	
	
	(setq TXTSAVEPI (strcat "PONTO DE INSERCAO---"(rtos (nth 0 pontoInsercao) 2 10) ","  (rtos (nth 1 pontoInsercao) 2 10)))
	(setq txtNameFile "INFORMACOES.txt")
	(setq arq (open (strcat (strcat "C:\\BF_LISP_CONF\\AIR GRID\\" IDDESENHO "\\") txtNameFile) "w"))
	(write-line (strcat TXTSAVEPI) arq)
	(write-line (strcat "COMPRIMENTO---" (rtos comprimento 2 10)) arq)
	(write-line (strcat "COMPRIMENTO DO LASTRO---" (rtos tamTubo 2 10)) arq)
	(write-line (strcat "QUANTIDADE DE LASTROS---" (rtos qtdDeLastros 2 10)) arq)
	(write-line (strcat "MODELO LASTRO---" (rtos modeloLastro 2 10)) arq)
	(write-line (strcat "LARGURA DO TUBO INTERNO---" (rtos larTuboInterno 2 10)) arq)
	(write-line (strcat "QUANTIDADE DE DIFUSORES---" (rtos qtdDifusor 2 10)) arq)
	(close arq)
	
	
	(if (= modeloLastro 4.0)
		(progn
			(setq modeloLastro 40)
		)
	)
	(if (= modeloLastro 5.0)
		(progn
			(setq modeloLastro 50)
		)
	)
	
	
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	
	(command "line" pontoInsercao (polar pontoInsercao (/ pi 2) 2180.0) "")
	(command "line" pontoInsercao (polar pontoInsercao 0 comprimento) "")
	(command "line" (polar pontoInsercao (/ pi 2) 2180.0)  (polar (polar pontoInsercao (/ pi 2) 2180.0) 0 comprimento) "")
	(command "line" (polar pontoInsercao 0 comprimento) (polar (polar pontoInsercao (/ pi 2) 2180.0) 0 comprimento) "")
	
	(setq p1 (polar pontoInsercao 0.785628 70.7107))
	(setq p2 (polar (polar pontoInsercao (/ pi 2) 2180.0) 5.49802 70.7107))
	(setq p3 (polar (polar (polar pontoInsercao (/ pi 2) 2180.0) 0 comprimento) 3.92739 70.6989))
	(setq p4 (polar (polar pontoInsercao 0 comprimento) 2.35603 70.6828))
	
	(command "line" p1 p2 "")
	(command "line" p2 p3 "")
	(command "line" p3 p4 "")
	(command "line" p1 p4 "")
	
	;Pequena linha
	(command "line" pontoInsercao p1 "")
	(command "line" (polar pontoInsercao (/ pi 2) 2180.0) p2 "")
	(command "line" (polar (polar pontoInsercao (/ pi 2) 2180.0) 0 comprimento) p3 "")
	(command "line"  (polar pontoInsercao 0 comprimento) p4 "")
	
	(setq pTuboFimBase (polar p1 0 tamTubo))
	(setq pTuboFimTopo (polar p2 0 tamTubo))
	
	;Mostra a distancia do lastro, para ajudar na hora que fazer
	; o desenho da lisp desenha_agir_grid3
	(command "layer" "m" "linha_aux_air3" "c" "cyan" "" "")
	(command "line" p1  pTuboFimBase "")
	(command "text" "bc" (sam_metade p1 pTuboFimBase) 22 0 (rtos (distance p1 pTuboFimBase) 2 12))
	
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	(command "line" pTuboFimBase pTuboFimTopo "")
	(command "line" (polar pTuboFimBase 0 50) (polar pTuboFimTopo 0 50) "")
	
	(setq pTuboFimBase2 (polar p4 pi tamTubo))
	(setq pTuboFimTopo2 (polar p3 pi tamTubo))
	(command "line" pTuboFimBase2 pTuboFimTopo2 "")
	(command "line" (polar pTuboFimBase2 pi 50) (polar pTuboFimTopo2 pi 50) "")
	
	;Coloca os lastros <------------
	(setq lista_coordenadas_lastros nil) ;Essa lista serve para armazernar a posição dos lastros
	;Ela será útil na hora que eu for fazer o trim nos difusores =D 02/8/2013
	(setq contadorLastros 0)
	(setq distanciaEntreLastros (/ (distance p1 p2) qtdDeLastros))
	(while (< contadorLastros qtdDeLastros)
		
		(command "line"  (polar (polar pTuboFimBase2 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (/ pi 2) (/ modeloLastro 2) )   (polar (polar  (polar pTuboFimBase2 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  )  (/ pi 2) (/ modeloLastro 2) ) 0 tamTubo)     "")
		(CriarLink (entlast) "nome" (strcat "Lastror" IDDESENHO))
		(setq lista_coordenadas_lastros (cons (list (polar (polar pTuboFimBase2 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (/ pi 2) (/ modeloLastro 2) )   (polar (polar  (polar pTuboFimBase2 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  )  (/ pi 2) (/ modeloLastro 2) ) 0 tamTubo)) lista_coordenadas_lastros))
		
		(command "line"  (polar (polar pTuboFimBase2 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (* (/ pi 2) 3) (/ modeloLastro 2)  )      (polar (polar (polar pTuboFimBase2 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (* (/ pi 2) 3) (/ modeloLastro 2)  ) 0 tamTubo)    "")
		(CriarLink (entlast) "nome" (strcat "Lastror" IDDESENHO))
		(setq lista_coordenadas_lastros (cons (list (polar (polar pTuboFimBase2 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (* (/ pi 2) 3) (/ modeloLastro 2)  )      (polar (polar (polar pTuboFimBase2 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (* (/ pi 2) 3) (/ modeloLastro 2)  ) 0 tamTubo)) lista_coordenadas_lastros))
		
		(command "line"  (polar (polar p1 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (/ pi 2) (/ modeloLastro 2) )   (polar (polar  (polar p1 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  )  (/ pi 2) (/ modeloLastro 2) ) 0 tamTubo)     "")
		(CriarLink (entlast) "nome" (strcat "Lastror" IDDESENHO))
		(setq lista_coordenadas_lastros (cons (list (polar (polar p1 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (/ pi 2) (/ modeloLastro 2) )    (polar (polar  (polar p1 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  )  (/ pi 2) (/ modeloLastro 2) ) 0 tamTubo)      ) lista_coordenadas_lastros))
		
		(command "line"  (polar (polar p1 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (* (/ pi 2) 3) (/ modeloLastro 2)  )      (polar (polar (polar p1 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (* (/ pi 2) 3) (/ modeloLastro 2)  ) 0 tamTubo)    "")
		(CriarLink (entlast) "nome" (strcat "Lastror" IDDESENHO))
		(setq lista_coordenadas_lastros (cons (list (polar (polar p1 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (* (/ pi 2) 3) (/ modeloLastro 2)  )      (polar (polar (polar p1 (/ pi 2) (+ (* distanciaEntreLastros contadorLastros) (/ distanciaEntreLastros 2) )  ) (* (/ pi 2) 3) (/ modeloLastro 2)  ) 0 tamTubo)) lista_coordenadas_lastros))
		
		(setq contadorLastros (+ contadorLastros 1))
	)
	
	
	(setq lista_linhas_cruzamento nil)
	;Cruzamento
	(setq crz1b (polar (polar pTuboFimBase 0 50) (/ pi 2) 29.5513))
	(setq crz2b (polar (polar pTuboFimTopo2 pi 50) pi 29.5513))
	
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	(command "line" crz1b crz2b "")
	(setq ultimo (entlast))
	(CriarLink ultimo "nome" (strcat "James" IDDESENHO))
	(setq lista_linhas_cruzamento (cons (list crz1b crz2b) lista_linhas_cruzamento))
	(setq tubo1 (entlast))
	(setq crz11b (polar (polar pTuboFimTopo2 pi 50) (* (/ pi 2) 3) 46.8861))
	(setq crz22b (polar (polar pTuboFimBase 0 50) 0 46.8861))
	(command "line" crz11b crz22b "")
	(setq lista_linhas_cruzamento (cons (list crz11b crz22b) lista_linhas_cruzamento))
	
	(setq tubo11 (entlast))
	(CriarLink tubo11 "nome" (strcat "James" IDDESENHO))
	
	(setq crz1 (polar (polar pTuboFimTopo 0 50) 0 46.8861 ))
	(setq crz2 (polar (polar pTuboFimBase2 pi 50) (/ pi 2) 46.8861 ))
	(command "line" crz1 crz2 "")
	(CriarLink (entlast) "nome" (strcat "Solaris" IDDESENHO))
	(setq lista_linhas_cruzamento (cons (list crz1 crz2) lista_linhas_cruzamento))
	
	
	(setq crz11 (polar (polar pTuboFimTopo 0 50) (* (/ pi 2) 3) 29.5513 ))
	(setq crz22 (polar (polar pTuboFimBase2 pi 50) pi 29.5513 ))
	(command "line" crz11 crz22 "")
	(CriarLink (entlast) "nome" (strcat "Solaris" IDDESENHO))
	(setq lista_linhas_cruzamento (cons (list crz11 crz22) lista_linhas_cruzamento))
	
	(setq ttt1 nil)
	(setq ttt1 (ssadd))
	(setq ttt1 (ssadd tubo1 ttt1))
	(setq ttt1 (ssadd tubo11 ttt1))
	
	(setq zoomInicial (viewextents))
	;Trim
	(setq pontoZoomTrim (polar crz1 (angle crz1 crz2) (/ (distance crz1 crz2) 2)))
	(command "zoom" "c" pontoZoomTrim 10)
	(command "trim" ttt1 "" pontoZoomTrim "")
	
	(setq pontoZoomTrim (polar crz11 (angle crz11 crz22) (/ (distance crz11 crz22) 2)))
	(command "zoom" "c" pontoZoomTrim 10)
	(command "trim" ttt1 "" pontoZoomTrim "")
	
	;Trim - Faz linha auxiliar
	(command "layer" "m" "linha_auxiliar2" "c" "65" "" "")
	(command "line" crz1 crz2 "")
	(setq tubo2 (entlast))
	(command "line" crz11 crz22 "")
	(setq tubo22 (entlast))
	(setq ttt2 nil)
	(setq ttt2 (ssadd))
	(setq ttt2 (ssadd tubo2 ttt2))
	(setq ttt2 (ssadd tubo22 ttt2))
	
	(setq pontoZoomTrim (polar crz11b (angle crz11b crz22b) (/ (distance crz11b crz22b) 2)))
	(command "zoom" "c" pontoZoomTrim 10)
	(command "trim" ttt2 "" pontoZoomTrim "")
	
	(setq pontoZoomTrim (polar crz1b (angle crz1b crz2b) (/ (distance crz1b crz2b) 2)))
	(command "zoom" "c" pontoZoomTrim 10)
	(command "trim" ttt2 "" pontoZoomTrim "")
	(command "erase" ttt2 "")
	
	;Insere meio
	(setq centroCirculo (polar pontoInsercao 1.5475 1090.33))
	(setq centroCirculo (polar pontoInsercao 1.705424219170264 1099.987550259601))
	
	(command "layer" "m" "BF-03-SA-SUP" "c" "250" "" "")
	(command "circle" centroCirculo larTuboInternoD2)
	(command "line" (polar centroCirculo (/ pi 2) larTuboInternoD2) (polar (polar centroCirculo (/ pi 2) larTuboInternoD2) 0 (+ (- comprimento 25.4) 173.0421) ) "")
	(setq linhaTrim1 (entlast))
	(CriarLink linhaTrim1 "IDDGRID" IDDESENHO)
	(command "line" (polar centroCirculo (* (/ pi 2) 3) larTuboInternoD2) (polar (polar centroCirculo (* (/ pi 2) 3) larTuboInternoD2) 0 (+ (- comprimento 25.4) 173.0421)  ) "")
	(setq linhaTrim2 (entlast))
	(CriarLink linhaTrim2 "IDDGRID" IDDESENHO)
	(setq pontoDetalheFinal1 (polar (polar centroCirculo (/ pi 2) larTuboInternoD2) 0 (+ (- comprimento 25.4) 173.0421)  ))
	(setq pontoDetalheFinal2 (polar (polar centroCirculo (* (/ pi 2) 3) larTuboInternoD2) 0 (+ (- comprimento 25.4) 173.0421)  ))
	
	(command "line" pontoDetalheFinal1 (polar pontoDetalheFinal1 0 5) "" )
	(command "line" pontoDetalheFinal2 (polar pontoDetalheFinal2 0 5) "")
	(command "line" (polar pontoDetalheFinal1 0 5) (polar pontoDetalheFinal2 0 5) "")
	
	;Inserte detalheGrid1_bloco.dwg
	(setq pDetalhe1 (polar (polar (polar pontoDetalheFinal2 0 5) (/ pi 2) (/ (distance (polar pontoDetalheFinal1 0 5) (polar pontoDetalheFinal2 0 5)) 2)  ) (* (/ pi 2) 3) 22.6705  ))
	(setq pDetalhe1 (polar pDetalhe1 sam_paraCima 22.67050000000017))
	(setq dist23P (* larTuboInterno 0.23))
	(setq pFimTubo (polar pDetalhe1 sam_paraBaixo (/ larTuboInterno 2)))
	(setq pontoInsert1 (polar pFimTubo sam_paraCima dist23P))
	(command "insert" "C:\\bfdias\\blocos\\detalheGrid1_bloco.dwg" pontoInsert1 "" "" "" "")
	
	(setq outExt (polar pFimTubo sam_paraCima larTuboInterno))
	(setq pontoInsert2 (polar outExt sam_paraBaixo (+ dist23P 18.63000000000466)))
	(command "insert" "C:\\bfdias\\blocos\\detalheGrid1_bloco.dwg" pontoInsert2 "" "" "" "")
	
	
	
	(setq baseTrim1 nil)
	(setq baseTrim1 (ssadd))
	(setq baseTrim1 (ssadd linhaTrim1 baseTrim1))
	(setq baseTrim1 (ssadd linhaTrim2 baseTrim1))
	
	(setq pontoZoomTrim (polar p1 (angle p1 p2) (/ (distance p1 p2) 2)))
	(command "zoom" "c" pontoZoomTrim 30)
	(command "trim" baseTrim1 "" pontoZoomTrim "")
	
	(setq pontoZoomTrim (polar p3 (angle p3 p4) (/ (distance p3 p4) 2)))
	(command "zoom" "c" pontoZoomTrim 30)
	(command "trim" baseTrim1 "" pontoZoomTrim "")
	
	(setq pontoZoomTrim (polar (polar pTuboFimBase 0 50) (angle (polar pTuboFimBase 0 50) (polar pTuboFimTopo 0 50)) (/ (distance (polar pTuboFimBase 0 50) (polar pTuboFimTopo 0 50)) 2)))
	(command "zoom" "c" pontoZoomTrim 100)
	(command "trim" baseTrim1 "" pontoZoomTrim "")
	
	(setq pontoZoomTrim (polar pTuboFimBase (angle pTuboFimBase pTuboFimTopo) (/ (distance pTuboFimBase pTuboFimTopo) 2)))
	(command "zoom" "c" pontoZoomTrim 100)
	(command "trim" baseTrim1 "" pontoZoomTrim "")
	
	(setq pontoZoomTrim (polar pTuboFimBase2 (angle pTuboFimBase2 pTuboFimTopo2) (/ (distance pTuboFimBase2 pTuboFimTopo2) 2)))
	(command "zoom" "c" pontoZoomTrim 100)
	(command "trim" baseTrim1 "" pontoZoomTrim "")
	
	(setq pontoZoomTrim (polar (polar pTuboFimBase2 pi 50) (angle (polar pTuboFimBase2 pi 50) (polar pTuboFimTopo2 pi 50)) (/ (distance (polar pTuboFimBase2 pi 50) (polar pTuboFimTopo2 pi 50)) 2)))
	(command "zoom" "c" pontoZoomTrim 100)
	(command "trim" baseTrim1 "" pontoZoomTrim "")
	
	;faz_janela_____COORD_TIPO_PROPRIEDADE_TAMANHO
	(setq Linhas (faz_janela  (polar centroCirculo 0 (/ (distance centroCirculo (polar centroCirculo 0 (- comprimento 25.4)) ) 2))  8  "BF-02-SA-GRA" 400 ))
	(setq pontoRange (polar centroCirculo 0 (/ (distance centroCirculo (polar centroCirculo 0 (- comprimento 25.4)) ) 2)))
	(setq valorRange1 (+ (nth 1 pontoRange) larTuboInternoD2 ))
	(setq valorRange2 (- (nth 1 pontoRange) larTuboInternoD2 ))
	
	(if (/= Linhas nil)
		(progn
			(setq qtd (- (sslength Linhas) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))
			(princ "\n--------------------------")
			(princ "\n--------------------------")
			(while (>= qtd 0)
				(setq obj (ssname Linhas qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq tipoObje (strcase (cdr (assoc 0 (entget obj)))))
				
				(if (= tipoObje "LINE")
					(progn
						(setq coord (cdr (assoc 10 (entget obj))))
						(setq coord2 (cdr (assoc 11 (entget obj))))
						
						(setq y1 (nth 1 coord))
						(if (and (> y1 valorRange2)(< y1 valorRange1))
							(progn
								(setq pontoTrim (polar coord (angle coord coord2) 0.5))
								(command "zoom" "c" pontoTrim 100)
								(command "trim" baseTrim1 "" pontoTrim "")
								(command "trim" baseTrim1 "" pontoTrim "")
							)
						)
						(setq y1 (nth 1 coord2))
						(if (and (> y1 valorRange2)(< y1 valorRange1))
							(progn
								(setq pontoTrim (polar coord2 (angle coord2 coord) 0.5))
								(command "zoom" "c" pontoTrim 100)
								(command "trim" baseTrim1 "" pontoTrim "")
								(command "trim" baseTrim1 "" pontoTrim "")
							)
						)
						
					)
				)
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	
	(command "layer" "m" "BF-SA-06-LCE" "c" "red" "" "")
	(command "line" centroCirculo (polar centroCirculo 0 (+ (- comprimento 25.4) 173.0421) ) "")
	
	;Inserção dos difusores
	(setq qtdDifusorMetade (/ qtdDifusor 2))
	(setq distanciaEntreDifusor (/ comprimento qtdDifusorMetade))
	(setq distInicio (/ distanciaEntreDifusor 2))
	
	;Ponto inserção difusor
	(setq pontoInsDifusor (polar pontoInsercao (/ pi 2)  (/ 2180 2)))
	
	(setq contador3 0)
	(setq lista_difusores_invertidos nil)
	(setq listaPontoDifusores nil)
	
	;Abre o arquivo para a gravação da coordenada dos difusores
	(setq txtNameFile "COORD_DIFUSORES.txt")
	(setq arq (open (strcat (strcat "C:\\BF_LISP_CONF\\AIR GRID\\" IDDESENHO "\\") txtNameFile) "w"))
	
	(while (> qtdDifusorMetade 0)
		
		(command "layer" "m" "difusor" "c" "30" "" "")
		(command "insert" "C:\\bfdias\\blocos\\90X1000 3.dwg" (polar (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ) 1.63749 1087.9) "" "" "" "")
		(setq difusores1 (entlast))
		
		(setq x (rtos (nth 0 (polar (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ) 1.63749 1087.9)) 2 10))
		(setq y (rtos (nth 1 (polar (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ) 1.63749 1087.9)) 2 10))
		(write-line (strcat x "---" y) arq)
		
		
		
		;(CriarLink difusores1 "nome" "Difusor")
		(command "insert" "C:\\bfdias\\blocos\\90X1000 3.dwg" (polar (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ) 1.63749 1087.9) "" "" "" "")
		(setq ultDifusor (entlast))
		;(CriarLink ultDifusor "nome" "DifusorInvertido")
		(command "rotate" ultDifusor "" (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ) 180)
		(setq listaPontoDifusores (cons (polar (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ) 1.63749 1087.9) listaPontoDifusores ))
		
		
		;larTuboInterno
		(setq pTeste1 (polar (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ) 1.63749 1087.9))
		(setq pTeste1 (polar pTeste1 4.781823256798633 1044.992354141086))
		
		
		(setq lista_difusores_invertidos (cons (list ultDifusor) lista_difusores_invertidos))
		(faz_o_trim_nos_difusores_air_grid (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ))
		;lista_coordenadas_lastros
		;lista_linhas_cruzamento
		(faz_trim_nas_linhas_cruzamento (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ))
		(ajusta_trim_fim_base pTuboFimBase pTuboFimBase2 (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ))
		(faz_trim_final_lastros (polar   pontoInsDifusor  0  (+ distInicio (* distanciaEntreDifusor contador3)) ))
		
		
		
		(if (< larTuboInterno 85.9458)
			(progn
				;Desenha Detalhe Conforme mostra a figura imgsuporte.png
				;(command "circle" pTeste1 25)
				(setq altura1 (- 42.97289009315500 (/ larTuboInterno 2)))
				
				(command "layer" "m" "BF-02-SA-DET" "c" "252" "" "")
				(setq inicio (polar pTeste1 pi 15))
				(command "line" inicio (polar inicio sam_paraBaixo altura1) "")
				(setq lastPoint2 (polar inicio sam_paraBaixo altura1))
				(command "line" lastPoint2 (polar lastPoint2 0 30) "")
				(setq lastPoint2 (polar lastPoint2 0 30))
				(command "line" lastPoint2 (polar lastPoint2 sam_paraCima altura1) "")
				(setq lastPoint2 (polar lastPoint2 sam_paraCima altura1))
				(command "line" lastPoint2 (polar lastPoint2 pi 30) "")
				
				;0.06828412686081719
				;(+ altura1 0.06828412686081719)
				(setq pTeste1 (polar pTeste1 sam_paraBaixo (+ 42.97289009315500 (/ larTuboInterno 2))))
				(setq inicio (polar pTeste1 pi 15))
				(command "line" inicio (polar inicio sam_paraBaixo (+ altura1 0.06828412686081719)) "")
				(setq lastPoint2 (polar inicio sam_paraBaixo (+ altura1 0.06828412686081719)))
				(command "line" lastPoint2 (polar lastPoint2 0 30) "")
				(setq lastPoint2 (polar lastPoint2 0 30))
				(command "line" lastPoint2 (polar lastPoint2 sam_paraCima (+ altura1 0.06828412686081719)) "")
				(setq lastPoint2 (polar lastPoint2 sam_paraCima (+ altura1 0.06828412686081719)))
				(command "line" lastPoint2 (polar lastPoint2 pi 30) "")
			)
			(progn
				;54.16707240713003
				(if (<= (/ larTuboInterno 2) 54.16707240713003 )
					(progn
						
						(command ".explode" difusores1  "")
						(setq difusores1 (entlast))
						
						(command ".explode" difusores1  "")
						(setq difusores1 (entlast))
						;difusores1
						
						(setq altura2 (- (/ larTuboInterno 2) 42.97289009315500))
						(setq pontoInicio (polar pTeste1 sam_paraCima altura2))
						(command "line" (polar pontoInicio pi 50) (polar pontoInicio 0 50) "")
						(setq linhaAuxS5 (entlast))
						
						(setq sel nil)
						(setq sel (ssadd))
						(setq sel (ssadd linhaAuxS5 sel))
						
						(command "zoom" "c" pTeste1 10)
						(setq objEncontrado (ssget "C" (polar pTeste1 (* (/ pi 4) 5) 1) (polar pTeste1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
						(if (/= objEncontrado nil)
							(progn
								(command "erase" objEncontrado "")
							)
						)
						
						(setq pontoZoom1 (polar pTeste1 3.141592653589793 45.00000000000000))
						(command "zoom" "c" pontoZoom1 5)
						(command "trim" sel "" pontoZoom1 "")
						
						(setq pontoZoom1 (polar pTeste1 3.141592653589793 39.24995181974373))
						(command "zoom" "c" pontoZoom1 5)
						(command "trim" sel "" pontoZoom1 "")
						
						(setq pontoZoom1 (polar pTeste1 3.141592653589793 26.98059381041094))
						(command "zoom" "c" pontoZoom1 5)
						(command "trim" sel "" pontoZoom1 "")
						
						(setq pontoZoom1 (polar pTeste1 3.141592653589793 7.240727348384098))
						(command "zoom" "c" pontoZoom1 0.5)
						(command "trim" sel "" pontoZoom1 "")
						
						(setq pontoZoom1 (polar pTeste1 0 7.235545623785583))
						(command "zoom" "c" pontoZoom1 0.5)
						(command "trim" sel "" pontoZoom1 "")
						
						(setq pontoZoom1 (polar pTeste1 0 26.98059381119674))
						(command "zoom" "c" pontoZoom1 0.5)
						(command "trim" sel "" pontoZoom1 "")
						
						(setq pontoZoom1 (polar pTeste1 0 39.24477009513066))
						(command "zoom" "c" pontoZoom1 0.5)
						(command "trim" sel "" pontoZoom1 "")
						
						(setq pontoZoom1 (polar pTeste1 0 45.00000000000000))
						(command "zoom" "c" pontoZoom1 0.5)
						(command "trim" sel "" pontoZoom1 "")
						
						(command "erase" sel "")
						
						(if (= (/ larTuboInterno 2) 54.16707240713003 )
							(progn
								;(command "circle" pTeste1 12)
								(setq pp1 (polar pTeste1 sam_paraCima 11.19418231397139))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 0 45.00000000000000))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								(setq pp1 (polar pTeste1 0 39.24477009514522))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 0 26.98059381121129))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 0 7.235545623785583))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 3.141592653589793 7.240727348384098))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 3.141592653589793 26.98059381041094))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 3.141592653589793 45.00000000000000))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								(setq pp1 (polar pTeste1 3.141592653589793 39.24995181974373))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
							)
						)
						
						;BF-01-SA-DIF
						
					)
					(progn
						;67.07974068929615
						(if (<= (/ larTuboInterno 2) 67.07974068929615)
							(progn
								
								(command ".explode" difusores1  "")
								(setq difusores1 (entlast))
								(command ".explode" difusores1  "")
								(setq difusores1 (entlast))
								;difusores1
								
								
								;(setq sel nil)
								;(setq sel (ssadd))
								;(setq sel (ssadd linhaAuxS5 sel))
								
								
								
								(setq pp1 (polar pTeste1 sam_paraCima 11.19418231397139))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								
								
								(setq pp1 (polar pTeste1 0 45.00000000000000))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								(setq pp1 (polar pTeste1 0 39.24477009514522))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 0 26.98059381121129))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 0 7.235545623785583))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 3.141592653589793 7.240727348384098))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 3.141592653589793 26.98059381041094))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pp1 (polar pTeste1 3.141592653589793 45.00000000000000))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								(setq pp1 (polar pTeste1 3.141592653589793 39.24995181974373))
								(command "zoom" "c" pp1 10)
								(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pTemp (polar pTeste1 sam_paraCima (- (/ larTuboInterno 2) 42.97289009315500)))
								(sam_linha (polar pTemp pi 90)(polar pTemp 0 90) )
								(setq selL (entlast))
								
								(setq pontoZoom1 (polar pTeste1 2.912475667139024 49.28802813745537))
								(command "zoom" "c" pontoZoom1 0.5)
								(command "trim" selL "" pontoZoom1 "")
								
								(setq pontoZoom1 (polar pTeste1 0.2291169864507684 49.28802813745537))
								(command "zoom" "c" pontoZoom1 0.5)
								(command "trim" selL "" pontoZoom1 "")
								
								(command "erase" selL "")
								
								;(command "erase" sel "")
								
								(if (= (/ larTuboInterno 2) 67.07974068929615)
									(progn
										
										(setq pz1 (polar pTeste1 sam_paraCima 24.10685059614116))
										(command "zoom" "c" pz1 10)
										(setq objEncontrado (ssget "C" (polar pz1 (* (/ pi 4) 5) 1) (polar pz1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										(setq pz1 (polar pTeste1 2.912475667139024 49.28802813745537))
										(command "zoom" "c" pz1 10)
										(setq objEncontrado (ssget "C" (polar pz1 (* (/ pi 4) 5) 1) (polar pz1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										(setq pz1 (polar pTeste1 0.229116986450768 49.28802813745537))
										(command "zoom" "c" pz1 10)
										(setq objEncontrado (ssget "C" (polar pz1 (* (/ pi 4) 5) 1) (polar pz1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										
										
									)
									
								)
							)
							(progn
								(if (> (/ larTuboInterno 2) 67.07974068929615)
									(progn
										
										
										(command ".explode" difusores1  "")
										(setq difusores1 (entlast))
										(command ".explode" difusores1  "")
										(setq difusores1 (entlast))
										;difusores1
										
										
										;(setq sel nil)
										;(setq sel (ssadd))
										;(setq sel (ssadd linhaAuxS5 sel))
										
										
										(setq pp1 (polar pTeste1 sam_paraCima 11.19418231397139))
										(command "zoom" "c" pp1 10)
										(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										
										(setq pp1 (polar pTeste1 0 45.00000000000000))
										(command "zoom" "c" pp1 10)
										(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										(setq pp1 (polar pTeste1 0 39.24477009514522))
										(command "zoom" "c" pp1 10)
										(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										
										(setq pp1 (polar pTeste1 0 26.98059381121129))
										(command "zoom" "c" pp1 10)
										(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										
										(setq pp1 (polar pTeste1 0 7.235545623785583))
										(command "zoom" "c" pp1 10)
										(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										
										(setq pp1 (polar pTeste1 3.141592653589793 7.240727348384098))
										(command "zoom" "c" pp1 10)
										(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										
										(setq pp1 (polar pTeste1 3.141592653589793 26.98059381041094))
										(command "zoom" "c" pp1 10)
										(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										
										(setq pp1 (polar pTeste1 3.141592653589793 45.00000000000000))
										(command "zoom" "c" pp1 10)
										(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										(setq pp1 (polar pTeste1 3.141592653589793 39.24995181974373))
										(command "zoom" "c" pp1 10)
										(setq objEncontrado (ssget "C" (polar pp1 (* (/ pi 4) 5) 1) (polar pp1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										
										(setq pTemp (polar pTeste1 sam_paraCima (- (/ larTuboInterno 2) 42.97289009315500)))
										(sam_linha (polar pTemp pi 90)(polar pTemp 0 90) )
										(setq selL (entlast))
										
										(setq pontoZoom1 (polar pTeste1 2.912475667139024 49.28802813745537))
										(command "zoom" "c" pontoZoom1 0.5)
										(command "trim" selL "" pontoZoom1 "")
										
										(setq pontoZoom1 (polar pTeste1 0.2291169864507684 49.28802813745537))
										(command "zoom" "c" pontoZoom1 0.5)
										(command "trim" selL "" pontoZoom1 "")
										
										(command "erase" selL "")
										
										
										(setq pz1 (polar pTeste1 sam_paraCima 24.10685059614116))
										(command "zoom" "c" pz1 10)
										(setq objEncontrado (ssget "C" (polar pz1 (* (/ pi 4) 5) 1) (polar pz1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										(setq pz1 (polar pTeste1 2.912475667139024 49.28802813745537))
										(command "zoom" "c" pz1 10)
										(setq objEncontrado (ssget "C" (polar pz1 (* (/ pi 4) 5) 1) (polar pz1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										(setq pz1 (polar pTeste1 0.229116986450768 49.28802813745537))
										(command "zoom" "c" pz1 10)
										(setq objEncontrado (ssget "C" (polar pz1 (* (/ pi 4) 5) 1) (polar pz1 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
										(if (/= objEncontrado nil)
											(progn
												(command "erase" objEncontrado "")
											)
										)
										
										(setq pLinhaT (polar pTeste1 sam_paraCima (- (/ larTuboInterno 2)  42.97289009315500) ))
										(sam_linha (polar pLinhaT pi 55)(polar pLinhaT 0 55))
										(setq objLT2 (entlast))
										
										(setq pontoZoom1 (polar pTeste1 0.4918041295567653 51.05036969175490))
										(command "zoom" "c" pontoZoom1 0.5)
										(command "trim" objLT2 "" pontoZoom1 "")
										
										(setq pontoZoom1 (polar pTeste1 0.5509275409898414 46.05133713290210))
										(command "zoom" "c" pontoZoom1 0.5)
										(command "trim" objLT2 "" pontoZoom1 "")
										
										(setq pontoZoom1 (polar pTeste1 0.7292565364432357 36.17933657568257))
										(command "zoom" "c" pontoZoom1 0.5)
										(command "trim" objLT2 "" pontoZoom1 "")
										
										(setq pontoZoom1 (polar pTeste1 1.279481440281160 25.16721974356217))
										(command "zoom" "c" pontoZoom1 0.5)
										(command "trim" objLT2 "" pontoZoom1 "")
										
										(setq pontoZoom1 (polar pTeste1 1.862857784381969 25.17286185064144))
										(command "zoom" "c" pontoZoom1 0.5)
										(command "trim" objLT2 "" pontoZoom1 "")
										
										(setq pontoZoom1 (polar pTeste1 2.412336117129405 36.17933657498811))
										(command "zoom" "c" pontoZoom1 0.5)
										(command "trim" objLT2 "" pontoZoom1 "")
										
										(setq pontoZoom1 (polar pTeste1 2.590888057135204 46.06805526936939))
										(command "zoom" "c" pontoZoom1 0.5)
										(command "trim" objLT2 "" pontoZoom1 "")
										
										(setq pontoZoom1 (polar pTeste1 2.649788524033027 51.05036969175490))
										(command "zoom" "c" pontoZoom1 0.5)
										(command "trim" objLT2 "" pontoZoom1 "")
										
										
										
										;Exclui linha trim
										(command "erase" objLT2 "")
									)
								)
							)
						)
					)
				)
			)
		)
		
		
		
		(setq contador3 (+ contador3 1))
		(setq qtdDifusorMetade (- qtdDifusorMetade 1))
	)
	
	
	
	;Fecha o arquivo para a gravação da coordenada dos difusores
	(close arq)
	
	(setq pontoICruz (polar pontoInsercao 0 (/ tamTubo 2)))
	(verifica_se_nao_existe_difusor_nessa_posicao pontoICruz)
	
	
	;Se o ganho ficar no meio do difusor, ele andará para frente, a metade da distancia entre os difusores
	(if (= sobrescreveu 1)
		(progn
			(setq pontoICruz (polar pontoICruz 0 (/ distanciaEntreDifusor 2)))
		)
	)
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	;Faz Gancho 1
	(setq g1 (polar pontoICruz (/ pi 2) 5))
	(setq g11 (polar g1 (/ pi 2)  39.9174 ))
	(setq g111 (polar g1 0 5.88899))
	(setq g1111 (polar g111 (/ pi 2) 39.9174))
	
	(command "line" g1 g11 "")
	(command "line" g1 g111 "")
	(command "line" g111 g1111 "")
	(command "line" g11 g1111 "")
	
	;Faz linha auxiliar gancho info
	(command "layer" "m" "linha_aux_air3" "c" "cyan" "" "")
	(command "line" pontoInsercao (polar g1 sam_paraBaixo 5) "" )
	(command "text" "bc" (sam_metade pontoInsercao (polar g1 sam_paraBaixo 5)) 22 0 (rtos (distance pontoInsercao (polar g1 sam_paraBaixo 5)) 2 12))
	
	(desenha_obs_gancho (polar g1 sam_paraBaixo 5))
	
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	
	;Faz Gancho 2
	(setq g2 (polar pontoICruz (/ pi 2) (+ 2125 10)))
	(setq g22 (polar g2 (/ pi 2)  39.9174 ))
	(setq g222 (polar g2 0 5.88899))
	(setq g2222 (polar g222 (/ pi 2) 39.9174))
	
	(command "line" g2 g22 "")
	(command "line" g2 g222 "")
	(command "line" g222 g2222 "")
	(command "line" g22 g2222 "")
	
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	(command "insert" "C:\\bfdias\\blocos\\configGancho2.dwg" (polar g2222 1.946550115643416 7.275987476547421) "" "" "")
	
	
	;Faz Gancho 3
	(setq pontoBase2 (polar pontoInsercao 0 comprimento))
	
	(setq pontoICruz (polar pontoBase2 pi (/ tamTubo 2)))
	;Se o ganho ficar no meio do difusor, ele andará para frente, a metade da distancia entre os difusores
	(if (= sobrescreveu 1)
		(progn
			(setq pontoICruz (polar pontoICruz pi (/ distanciaEntreDifusor 2)))
		)
	)
	
	(setq g1 (polar pontoICruz (/ pi 2) 5))
	(setq g11 (polar g1 (/ pi 2)  39.9174 ))
	(setq g111 (polar g1 pi 5.88899))
	(setq g1111 (polar g111 (/ pi 2) 39.9174))
	
	(command "line" g1 g11 "")
	(command "line" g1 g111 "")
	(command "line" g111 g1111 "")
	(command "line" g11 g1111 "")
	
	
	(desenha_obs_gancho g1)
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	
	;Faz Gancho 4
	(setq g2 (polar pontoICruz (/ pi 2) (+ 2125 10)))
	(setq g22 (polar g2 (/ pi 2)  39.9174 ))
	(setq g222 (polar g2 pi 5.88899))
	(setq g2222 (polar g222 (/ pi 2) 39.9174))
	
	(command "line" g2 g22 "")
	(command "line" g2 g222 "")
	(command "line" g222 g2222 "")
	(command "line" g22 g2222 "")
	
	(command "layer" "m" "BF-02-SA-GRA" "c" "65" "" "")
	(command "insert" "C:\\bfdias\\blocos\\configGancho2.dwg" (polar g2222 1.176850911954535 9.183145601367932) "" "" "")
	
	
	
	(ajusta_difusores_invertidos lista_difusores_invertidos)
	
	;Fazendo o trim no tubo do meio
	(setq pontoinicial (polar pontoInsercao sam_paraCima 1090.034142063428))
	(setq linhaTrim1 (polar pontoinicial sam_paraBaixo (/ larTuboInterno 2)))
	(setq linhaTrim2 (polar pontoinicial sam_paraCima (/ larTuboInterno 2)))
	
	(command "layer" "m" "linhaAuxiliar" "c" "241" "" "")
	(command "line" (polar linhaTrim1 pi 40) (polar linhaTrim1 0 40) "")
	(setq linhaSave1 (entlast))
	(command "line" (polar linhaTrim2 pi 40) (polar linhaTrim2 0 40) "")
	(setq linhaSave2 (entlast))
	
	(setq sel nil)
	(setq sel (ssadd))
	(setq sel (ssadd linhaSave1 sel))
	(setq sel (ssadd linhaSave2 sel))
	
	(setq pontoZoomTrim  pontoinicial )
	(command "zoom" "c" pontoZoomTrim 10)
	(command "trim" sel "" pontoZoomTrim "")
	(command "erase" sel "")
	
	(command "layer" "m" "BF-03-SA-SUP" "c" "250" "" "")
	(command "line" linhaTrim1 linhaTrim2 "")
	
	(if (and (> (/ larTuboInterno 2) 43.0412) (<= (/ larTuboInterno 2) 54.23535653442741) )
		(progn
			(setq qtdElemArray2 (vl-list-length lista_difusores_invertidos))
			(while (> qtdElemArray2 0)
				
				(setq objDif2 (nth 0 (nth (- qtdElemArray2 1) lista_difusores_invertidos)))
				
				(setq coordDif1 (cdr (assoc 10 (entget objDif2))))
				(setq pTeste3 (polar coordDif1 1.501362050380953 1044.992354141086))
				
				(command ".explode" objDif2  "")
				(setq difusores2 (entlast))
				(command ".explode" difusores2  "")
				
				(command "layer" "m" "linhaAuxiliar" "c" "241" "" "")
				
				(command "line" (polar (polar pTeste3 sam_paraBaixo (- (/ larTuboInterno 2) 43.0412)) pi 55) (polar (polar pTeste3 sam_paraBaixo (- (/ larTuboInterno 2) 43.0412)) 0 55) ""  )
				(setq objLinhatrim (entlast))
				
				
				(setq pontoTrim (polar pTeste3 3.143597249477096 45.00009041425649))
				(command "zoom" "c" pontoTrim 0.5)
				(command "trim" objLinhatrim "" pontoTrim "")
				
				(setq pontoTrim (polar pTeste3 3.144779911794807 39.25020234991793))
				(command "zoom" "c" pontoTrim 0.5)
				(command "trim" objLinhatrim "" pontoTrim "")
				
				(setq pontoTrim (polar pTeste3 3.144538362440836 26.98070180808681))
				(command "zoom" "c" pontoTrim 0.5)
				(command "trim" objLinhatrim "" pontoTrim "")
				
				(setq pontoTrim (polar pTeste3 3.156606516938775 7.241587985214233))
				(command "zoom" "c" pontoTrim 0.5)
				(command "trim" objLinhatrim "" pontoTrim "")
				
				(setq pontoTrim (polar pTeste3 6.276366510259009 7.235693658946182))
				(command "zoom" "c" pontoTrim 0.5)
				(command "trim" objLinhatrim "" pontoTrim "")
				
				(setq pontoTrim (polar pTeste3 6.279086022791529 26.98080789635472))
				(command "zoom" "c" pontoTrim 0.5)
				(command "trim" objLinhatrim "" pontoTrim "")
				
				(setq pontoTrim (polar pTeste3 6.280496362690464 39.24486881254061))
				(command "zoom" "c" pontoTrim 0.5)
				(command "trim" objLinhatrim "" pontoTrim "")
				
				(setq pontoTrim (polar pTeste3 6.281295375643293 45.00008036654687))
				(command "zoom" "c" pontoTrim 0.5)
				(command "trim" objLinhatrim "" pontoTrim "")
				
				
				(command "erase" objLinhatrim "")
				;BF-01-SA-DIF
				(command "zoom" "c" pTeste3 10)
				(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
				(if (/= objEncontrado nil)
					(progn
						(command "erase" objEncontrado "")
					)
				)
				(setq qtdElemArray2 (- qtdElemArray2 1))
			)
		)
		(progn
			(if (and 
					 (>  (/ larTuboInterno 2) 54.23535653442741)
					 (<= (/ larTuboInterno 2) 67.14802481616061)
				)
				(progn
					(setq qtdElemArray2 (vl-list-length lista_difusores_invertidos))
					
					(while (> qtdElemArray2 0)
				
						(setq objDif2 (nth 0 (nth (- qtdElemArray2 1) lista_difusores_invertidos)))
						
						(setq coordDif1 (cdr (assoc 10 (entget objDif2))))
						
						(setq pontoNrstM (polar coordDif1 1.570796326794896 1085.515524126684))
						
						
						(setq pontoIniLAux (polar pontoNrstM sam_paraBaixo (/ larTuboInterno 2)))
						
						(setq pTeste3 (polar coordDif1 1.501362050380953 1044.992354141086))
						
						(command ".explode" objDif2  "")
						(setq difusores2 (entlast))
						(command ".explode" difusores2  "")
						
						(command "layer" "m" "linhaAuxiliar" "c" "241" "" "")
						
						(command "zoom" "c" pTeste3 10)
						(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
						(if (/= objEncontrado nil)
							(progn
								(command "erase" objEncontrado "")
							)
						)
						
						(setq pTeste4 (polar pTeste3 sam_paraBaixo 11.19418231397867))
						(command "zoom" "c" pTeste4 10)
						(setq objEncontrado (ssget "C" (polar pTeste4 (* (/ pi 4) 5) 1) (polar pTeste4 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
						(if (/= objEncontrado nil)
							(progn
								(command "erase" objEncontrado "")
							)
						)
						
						(setq nextPoint (polar pTeste3 3.269664332507054 45.37159214697815))
						(command "zoom" "c" nextPoint 10)
						(setq objEncontrado (ssget "C" (polar nextPoint (* (/ pi 4) 5) 1) (polar nextPoint (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
						(if (/= objEncontrado nil)
							(progn
								(command "erase" objEncontrado "")
							)
						)
						
						(funcao_faz_trim pTeste3 3.269664332507054 45.37159214697815)
						(funcao_faz_trim pTeste3 3.279777329137465 39.62994984847854)
						(funcao_faz_trim pTeste3 3.337905259476462 27.50834912555687)
						(funcao_faz_trim pTeste3 3.787444824565011 9.069717714482367)
						(funcao_faz_trim pTeste3 5.724111491647671 8.532848711564280)
						(funcao_faz_trim pTeste3 6.066725201600665 27.62456868928542)
						(funcao_faz_trim pTeste3 6.154121300111731 39.57181383520353)
						(funcao_faz_trim pTeste3 6.160257183825612 45.34215881682898)
						
						(sam_linha pontoIniLAux (polar pontoIniLAux 0 153))
						(setq objLTrim (entlast))
						
						(setq pontoTrim (polar pontoIniLAux 0 24.50000000000000))
						(setq pontoTrim (polar pontoTrim sam_paraCima (- (/ larTuboInterno 2) 54.23535653442741)))
						
						;(command "circle" pontoTrim 8)
						
						(command "zoom" "c" pontoTrim 0.5)
						(command "trim" objLTrim "" pontoTrim "")
						
						(setq pontoTrim (polar pontoTrim 0 96))
						(command "zoom" "c" pontoTrim 0.5)
						(command "trim" objLTrim "" pontoTrim "")
						
						(if (= (/ larTuboInterno 2) 67.14802481616061)
							(progn
								
								(setq linhaApagar (sam_metade pontoIniLAux (polar pontoIniLAux 0 153)))
								
								;(command "circle" linhaApagar 10)
								;(command "circle" pontoTrim 10)
								
								(command "zoom" "c" linhaApagar 10)
								(setq objEncontrado (ssget "C" (polar linhaApagar (* (/ pi 4) 5) 1) (polar linhaApagar (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(command "zoom" "c" pontoTrim 10)
								(setq objEncontrado (ssget "C" (polar pontoTrim (* (/ pi 4) 5) 1) (polar pontoTrim (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								(setq pontoTrim (polar pontoTrim pi 96))
								(command "zoom" "c" pontoTrim 10)
								(setq objEncontrado (ssget "C" (polar pontoTrim (* (/ pi 4) 5) 1) (polar pontoTrim (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								
								
							)
						)
						(command "erase" objLTrim "")
						
						(setq qtdElemArray2 (- qtdElemArray2 1))
					)
				)
				(progn
					(if (> (/ larTuboInterno 2) 67.14802481616061)
						(progn
							(setq qtdElemArray2 (vl-list-length lista_difusores_invertidos))
							(while (> qtdElemArray2 0)
								
								(setq objDif2 (nth 0 (nth (- qtdElemArray2 1) lista_difusores_invertidos)))
								
								(setq coordDif1 (cdr (assoc 10 (entget objDif2))))
								(setq pTeste3 (polar coordDif1 1.501362050380953 1044.992354141086))
								
								(command ".explode" objDif2  "")
								(setq difusores2 (entlast))
								(command ".explode" difusores2  "")
								
								(command "layer" "m" "linhaAuxiliar" "c" "241" "" "")
								
								(setq proxPonto (polar coordDif1 sam_paraCima 1085.515524126684))
								
								;(command "circle" proxPonto 12)
								
								(setq pTeste3 (polar proxPonto 5.897898255486054 126.79528778865584))
								(command "zoom" "c" pTeste3 5)
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pTeste3 (polar proxPonto 5.875081495226366 121.7405546393814))
								(command "zoom" "c" pTeste3 5)
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pTeste3 (polar proxPonto 5.836464729698701 110.3043811078188))
								(command "zoom" "c" pTeste3 5)
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pTeste3 (polar proxPonto 5.821706275014174 134.5774913839022))
								(command "zoom" "c" pTeste3 5)
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pTeste3 (polar proxPonto 5.742940665315606 92.97479317931523))
								(command "zoom" "c" pTeste3 5)
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pTeste3 (polar proxPonto 5.375045110862396 85.17421703899072))
								(command "zoom" "c" pTeste3 5)
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pTeste3 (polar proxPonto 5.096880455450778 65.31804227569491))
								(command "zoom" "c" pTeste3 5)
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pTeste3 (polar proxPonto 5.647723707140793 81.08520190575399))
								(command "zoom" "c" pTeste3 5)
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								(setq pTeste3 (polar proxPonto 5.300108682914016 65.17049771674584))
								(command "zoom" "c" pTeste3 5)
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								
								(setq pTeste3 (polar proxPonto 5.468995473600383 66.31143249297020))
								(command "zoom" "c" pTeste3 5)
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								
								(setq pTeste3 (polar proxPonto 5.321565361015593 58.10663281146506))
								
								(command "zoom" "c" pTeste3 5)
								
								
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								
								(setq pTeste3 (polar proxPonto 5.235781550519644 55.01965189709857))
								
								(command "zoom" "c" pTeste3 5)
								
								
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								
								(setq pTeste3 (polar proxPonto 5.619317693519842 69.85302567562815))
								
								(command "zoom" "c" pTeste3 5)
								
								
								(setq objEncontrado (ssget "C" (polar pTeste3 (* (/ pi 4) 5) 1) (polar pTeste3 (/ pi 4) 1) (list (cons 8 "BF-SA-01-DIF"))))
								(if (/= objEncontrado nil)
									(progn
										(command "erase" objEncontrado "")
									)
								)
								
								;(setq proxPonto (polar coordDif1 sam_paraCima 1085.515524126684))
								;Fazer a linha trim
								
								(setq Pp1 (polar proxPonto sam_paraBaixo (/ larTuboInterno 2)))
								(sam_linha Pp1 (polar Pp1 0 140))
								(setq objLinhatrim (entlast))
								
								(setq pontoTrim (polar proxPonto 5.764006039257827 135.3333190190491))
								(command "zoom" "c" pontoTrim 0.5)
								(command "trim" objLinhatrim "" pontoTrim "")
								
								(setq pontoTrim (polar proxPonto 5.742071886370347 130.3615646320426))
								(command "zoom" "c" pontoTrim 0.5)
								(command "trim" objLinhatrim "" pontoTrim "")
								
								(setq pontoTrim (polar proxPonto 5.689432331940619 120.0195775189284))
								(command "zoom" "c" pontoTrim 0.5)
								(command "trim" objLinhatrim "" pontoTrim "")
								
								(setq pontoTrim (polar proxPonto 5.583230933080882 104.2375329850987))
								(command "zoom" "c" pontoTrim 0.5)
								(command "trim" objLinhatrim "" pontoTrim "")
								
								(setq pontoTrim (polar proxPonto 5.483468106121147 93.63059182821460))
								(command "zoom" "c" pontoTrim 0.5)
								(command "trim" objLinhatrim "" pontoTrim "")
								
								(setq pontoTrim (polar proxPonto 5.101094870081596 72.56105867965075))
								(command "zoom" "c" pontoTrim 0.5)
								(command "trim" objLinhatrim "" pontoTrim "")
								
								
								(setq pontoTrim (polar proxPonto 5.172083099537577 74.92624987455787))
								(command "zoom" "c" pontoTrim 0.5)
								(command "trim" objLinhatrim "" pontoTrim "")
								
								(setq pontoTrim (polar proxPonto 5.308153834717327 81.12412578554858))
								(command "zoom" "c" pontoTrim 0.5)
								(command "trim" objLinhatrim "" pontoTrim "")
								;5.308153834717327
								
								(command "erase" objLinhatrim "")
								
								(setq qtdElemArray2 (- qtdElemArray2 1))
							)
						)
					)
				)
			)
		)
	)
	
	
	(command "zoom" "w" (nth 0 zoomInicial) (nth 1 zoomInicial))
	
	(setq qtd (vl-list-length lista_coordenadas_lastros))
	(while (> qtd 0)
		(setq coord (nth (- qtd 1) lista_coordenadas_lastros))
		(setq coord1 (nth 0 coord))
		(setq coord2 (nth 1 coord))
		
		
		(setq qtd (- qtd 1))
	)
	
	
	
	;pontoInsercao
	;tamTubo
	;49.98852231825003
	
	
	;Largura dos difusores: 90.00000000001454
	
	
	
	;x1 e x2 do Richard
	(setq x1R (nth 0 (polar pontoInsercao 0 (+ tamTubo 49.98852231825003))))
	(setq listaObjRich1 (procuraObjRichard1 x1R))
	(setq x2R (nth 0 (polar pontoInsercao 0 (+ tamTubo (+ 49.98852231825003 50)))))
	(setq listaObjRich2 (procuraObjRichard1 x2R))
	
	
	;x1, e x2 Kylie
	(setq x1K (nth 0 (polar pontoInsercao 0 (-  comprimento (+ tamTubo  49.97206525006913 50) )) ))
	(setq listaObjKylie1 (procuraObjRichard1 x1K))
	(setq x2K (nth 0 (polar pontoInsercao 0 (- comprimento (+ tamTubo  49.97206525006913 ) ))))
	(setq listaObjKylie2 (procuraObjRichard1 x2K))
	
	
	(if (>= larTuboInterno  86.01406431317082 )
		(progn
			
			;Verifica o Richard1
			(setq qtd (vl-list-length listaPontoDifusores))
			(while (> qtd 0)
				(setq coord  (nth (- qtd 1) listaPontoDifusores) )
				(setq pontoX (nth 0 coord))
				(setq pontoX1 (+  pontoX 27.5))
				(setq pontoX2 (+  pontoX 27.5 90))
				
				
				(setq qtd2 (vl-list-length listaObjKylie2))
				(while (> qtd2 0)
					(setq objRich  (nth 0 (nth (- qtd2 1) listaObjKylie2) ))
					(setq coordRich (cdr (assoc 10 (entget objRich))))
					(if (/= coordRich nil)
						(progn
							(setq xRich (nth 0 coordRich))
							(if (and (>= xRich pontoX1)(<= xRich pontoX2))
								(progn
									(command "erase" objRich "")
								)
							)
						)
					)
					(setq qtd2 (- qtd2 1))
				)
				
				(setq qtd2 (vl-list-length listaObjKylie1))
				(while (> qtd2 0)
					(setq objRich  (nth 0 (nth (- qtd2 1) listaObjKylie1) ))
					(setq coordRich (cdr (assoc 10 (entget objRich))))
					(if (/= coordRich nil)
						(progn
							(setq xRich (nth 0 coordRich))
							(if (and (>= xRich pontoX1)(<= xRich pontoX2))
								(progn
									(command "erase" objRich "")
								)
							)
						)
					)
					(setq qtd2 (- qtd2 1))
				)
				
				(setq qtd2 (vl-list-length listaObjRich1))
				(while (> qtd2 0)
					(setq objRich  (nth 0 (nth (- qtd2 1) listaObjRich1) ))
					(setq coordRich (cdr (assoc 10 (entget objRich))))
					(if (/= coordRich nil)
						(progn
							(setq xRich (nth 0 coordRich))
							(if (and (>= xRich pontoX1)(<= xRich pontoX2))
								(progn
									(command "erase" objRich "")
								)
							)
						)
					)
					(setq qtd2 (- qtd2 1))
				)
				
				(setq qtd2 (vl-list-length listaObjRich2))
				(while (> qtd2 0)
					(setq objRich  (nth 0 (nth (- qtd2 1) listaObjRich2) ))
					(setq coordRich (cdr (assoc 10 (entget objRich))))
					
					(if (/= coordRich nil)
						(progn
							
							(setq xRich (nth 0 coordRich))
							(if (and (>= xRich pontoX1)(<= xRich pontoX2))
								(progn
									(command "erase" objRich "")
								)
							)
						)
					)
					(setq qtd2 (- qtd2 1))
				)
				(setq qtd (- qtd 1))
			)
		)
	)
	
	
	
	;Faz o trim no James
	(setq lstJames (resgataLinhasXdata (strcat "James" IDDESENHO) "BF-02-SA-GRA"))
	(setq qtd (vl-list-length listaPontoDifusores))
	(while (> qtd 0)
		(setq pontoLastro (nth (- qtd 1) listaPontoDifusores))
		(setq x1R  (nth 0  (polar pontoLastro 0 27.5)   ))
		(setq x2R  (nth 0  (polar pontoLastro 0 (+ 27.5 90)) ))
		(setq qtd2 (vl-list-length lstJames))
		(while (> qtd2 0)
			(setq objJames (nth 0 (nth (- qtd2 1) lstJames)))
			(setq coord1 (cdr (assoc 10 (entget objJames))))
			(setq coord2 (cdr (assoc 11 (entget objJames))))
			(setq coord1X (nth 0 (cdr (assoc 10 (entget objJames)))))
			(setq coord2X (nth 0 (cdr (assoc 11 (entget objJames)))))
			(setq resp (verificaRangeNumero x1R x2R coord1X "A"))
			(if (= resp "SIM")
				(progn
					(setq dist1 (modulo (- x1R coord1X)))
					(setq dist2 (modulo (- x2R coord1X)))
					(princ (strcat "\n" (rtos dist1 2 3) " --- " (rtos dist2 2 3)  ))
					(if (and (> dist1 1) (> dist2 1))
						(progn
							(if (< coord1X coord2X)
								(progn
									(setq novoX x2R)
								)
								(progn
									(setq novoX x1R)
								)
							)
							;Coeficiente angular
							(setq mm1 (coeficiente_angular coord1 coord2))
							;Retorna o coeficiente linear
							(setq bb1 (coeficiente_linear mm1 coord1))
							(setq novoY (+ (* mm1 novoX) bb1))
							(setq novaCoord (list novoX novoY 0.0))
							(setq elist (entget objJames))
							(setq elist (subst (cons 10 novaCoord)(assoc 10 elist) elist))
							(entmod elist)
						)
					)
				)
			)
			(setq resp (verificaRangeNumero x1R x2R coord2X "A"))
			(if (= resp "SIM")
				(progn
					(setq dist1 (modulo (- x1R coord2X)))
					(setq dist2 (modulo (- x2R coord2X)))
					(princ (strcat "\n" (rtos dist1 2 3) " --- " (rtos dist2 2 3)  ))
					(if (and (> dist1 1)(> dist2 1) )
						(progn
							;(command "circle" coord2 22)
							(if (< coord2X coord1X)
								(progn
									(setq novoX x2R)
								)
								(progn
									(setq novoX x1R)
								)
							)
							;Coeficiente angular
							(setq mm1 (coeficiente_angular coord1 coord2))
							;Retorna o coeficiente linear
							(setq bb1 (coeficiente_linear mm1 coord1))
							(setq novoY (+ (* mm1 novoX) bb1))
							(setq novaCoord (list novoX novoY 0.0))
							(setq elist (entget objJames))
							(setq elist (subst (cons 11 novaCoord)(assoc 11 elist) elist))
							(entmod elist)
						)
					)
				)
			)
			(setq qtd2 (- qtd2 1))
		)
		(setq qtd (- qtd 1))
	)
	
	;Faz o trim na Solaris
	(setq lstJames (resgataLinhasXdata (strcat "Solaris" IDDESENHO) "BF-02-SA-GRA"))
	(setq qtd (vl-list-length listaPontoDifusores))
	(while (> qtd 0)
		(setq pontoLastro (nth (- qtd 1) listaPontoDifusores))
		(setq x1R  (nth 0  (polar pontoLastro 0 27.5)   ))
		(setq x2R  (nth 0  (polar pontoLastro 0 (+ 27.5 90) )   ))
		(setq qtd2 (vl-list-length lstJames))
		(while (> qtd2 0)
			(setq objJames (nth 0 (nth (- qtd2 1) lstJames)))
			(setq coord1 (cdr (assoc 10 (entget objJames))))
			(setq coord2 (cdr (assoc 11 (entget objJames))))
			(setq coord1X (nth 0 (cdr (assoc 10 (entget objJames)))))
			(setq coord2X (nth 0 (cdr (assoc 11 (entget objJames)))))
			(setq resp (verificaRangeNumero x1R x2R coord1X "A"))
			(if (= resp "SIM")
				(progn
					(setq dist1 (modulo (- x1R coord1X)))
					(setq dist2 (modulo (- x2R coord1X)))
					(princ (strcat "\n" (rtos dist1 2 3) " --- " (rtos dist2 2 3)  ))
					(if (and (> dist1 1)(> dist2 1))
						(progn
							(if (< coord1X coord2X)
								(progn
									(setq novoX x2R)
								)
								(progn
									(setq novoX x1R)
								)
							)
							;Coeficiente angular
							(setq mm1 (coeficiente_angular coord1 coord2))
							;Retorna o coeficiente linear
							(setq bb1 (coeficiente_linear mm1 coord1))
							(setq novoY (+ (* mm1 novoX) bb1))
							(setq novaCoord (list novoX novoY 0.0))
							(setq elist (entget objJames))
							(setq elist (subst (cons 10 novaCoord)(assoc 10 elist) elist))
							(entmod elist)
						)
					)
				)
			)
			(setq resp (verificaRangeNumero x1R x2R coord2X "A"))
			(if (= resp "SIM")
				(progn
					(setq dist1 (modulo (- x1R coord2X)))
					(setq dist2 (modulo (- x2R coord2X)))
					(princ (strcat "\n" (rtos dist1 2 3) " --- " (rtos dist2 2 3)  ))
					(if (and (> dist1 1)(> dist2 1) )
						(progn
							;(command "circle" coord2 22)
							(if (< coord2X coord1X)
								(progn
									(setq novoX x2R)
								)
								(progn
									(setq novoX x1R)
								)
							)
							;Coeficiente angular
							(setq mm1 (coeficiente_angular coord1 coord2))
							;Retorna o coeficiente linear
							(setq bb1 (coeficiente_linear mm1 coord1))
							(setq novoY (+ (* mm1 novoX) bb1))
							(setq novaCoord (list novoX novoY 0.0))
							(setq elist (entget objJames))
							(setq elist (subst (cons 11 novaCoord)(assoc 11 elist) elist))
							(entmod elist)
						)
					)
				)
			)
			(setq qtd2 (- qtd2 1))
		)
		(setq qtd (- qtd 1))
	)
	
	
	(setq lstLastror (resgataLinhasXdata (strcat "Lastror" IDDESENHO) "BF-02-SA-GRA"))
	(setq pontoYmTI (nth 1 (polar pontoInsercao sam_paraCima 1090.034142063428) ))
	(setq pontoYmTIMax (+ pontoYmTI (/ larTuboInterno 2) ) )
	(setq pontoYmTIMin (- pontoYmTI (/ larTuboInterno 2) ) )
	
	
	(setq qtd (vl-list-length lstLastror))
	(while (> qtd 0)
		(setq obj (nth (- qtd 1) lstLastror))
		(setq obj (nth 0 obj))
		(setq coord (cdr (assoc 10 (entget obj))))
		(setq coordY (nth 1 coord))
		
		;range1 ---- meio ----- range2  / bolinha("A" aberto ou "F" fechado)
		(setq resposta (verificaRangeNumero pontoYmTIMin pontoYmTIMax coordY "F"))
		(if (= resposta "SIM")
			(progn
				(command "erase" obj "")
			)
		)
		;verificaRangeNumero
		;(setq coord2 (nth 1 coord))
		;(command "circle" coord1 22)
		;(command "circle" coord2 22)
		(setq qtd (- qtd 1))
	)
	
	
	;lista_coordenadas_lastros
	
	
	(princ)
)

















