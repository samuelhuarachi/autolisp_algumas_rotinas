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


(defun desenha_SA (ponto / linha1 linha2 texto1)	
	(command "layer" "m" "BF-05A-SA-TXT" "c" "251" "" "")
	(command "line" ponto (polar ponto 0.727222 95.9857) "")
	(setq linha1 (entlast))
	(command "line" (polar ponto 0.727222 95.9857) (polar (polar ponto 0.727222 95.9857) 0 265.045) "")
	(setq linha2 (entlast))
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" (polar (polar ponto 0.727222 95.9857) 1.0472 21.613) 137.2 0 "SA")
	(setq texto1 (entlast))
	
	(setq sel nil)
	(setq sel (ssadd))
	(setq sel (ssadd linha1 sel))
	(setq sel (ssadd linha2 sel))
	(setq sel (ssadd texto1 sel))
	(command "move" sel "" ponto (polar ponto 5.76667 143.93))
)

(defun desenha_SA_vertical(ponto)
	
	(command "layer" "m" "BF-05A-SA-TXT" "c" "251" "" "")
	(command "line" (polar ponto 1.57861 39.1608) (polar (polar ponto 1.57861 39.1608) 1.91955 133.376) "")
	(command "line" (polar (polar ponto 1.57861 39.1608) 1.91955 133.376) (polar (polar (polar ponto 1.57861 39.1608) 1.91955 133.376) pi 246.27)  "")
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" (polar (polar (polar (polar ponto 1.57861 39.1608) 1.91955 133.376) pi 246.27) 1.11016 20.8953) 137.2 0 "SA")
)


(defun c:isa()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq quantidade (atof (getstring "\nQuantidade: ")))
	(setq distancia (atof (getstring "\nDistância entre os pontos: ")))
	(setq ponto (getpoint "\nPonto de inserção: "))
	
	(setq posicao "")
	(while (and (/= posicao "H")(/= posicao "V"))
		(setq posicao (strcase (getstring "\nHorizontal ou vertical? [H/V]")))
		
		(if (and (/= posicao "H")(/= posicao "V"))
			(progn
				(alert "Entre com uma posição válida")
			)
		)
	)
	
	
	(if (= posicao "H")
		(progn
			(setq angulo (* (/ 0 pi) 180))
		)
		(progn
			(setq angulo (* (/ (/ pi 2) pi) 180))
		)
	)
	
	(setq ponto2 (polar ponto 3.68378 96.8968))
	(setq contador1 0)
	
	(setq zoomInicial (viewextents))
	
	(while (< contador1 quantidade)
		;BF-03-SA-SUP , 253
		
		;Desenha as Legendas AB nos pontos de intersecção
		(setq PontoInserçãoSA (polar (polar (polar ponto2 0 (* distancia contador1)) 0.3448417174743497 147.9075265313429) 2.612270868250036 143.4851930989352))
		(if (= angulo 0)
			(progn
				(command "layer" "m" "BF-03-SA-SUP" "c" "253" "" "")
				(command "insert" "C:\\bfdias\\blocos\\SUPORTE ANCORAGEM PLANTA HOR.dwg" (polar ponto 0 (* distancia contador1)) "" "" "")
				(setq objSA (entlast))
				(desenha_SA PontoInserçãoSA)
				
				
				;(command "line"  (polar  (polar ponto2 0 (* distancia contador1)) (/ pi 2) 65)   (polar (polar ponto2 0 (* distancia contador1)) 0.361317 183.872)  "")
				(command "line"  (polar  (polar ponto2 0 (* distancia contador1)) (/ pi 2)  70.99982688404281  )    (polar (polar (polar ponto2 0 (* distancia contador1)) (/ pi 2) 70.99982688404281) 0 170)   "")
				(setq elemtrim1 (entlast))
				
				;(command "line"  (polar  (polar ponto2 0 (* distancia contador1)) (/ pi 2)  28.99975339469293  )    (polar (polar ponto2 0 (* distancia contador1)) 0.200748 175.525)   "")
				(command "line"  (polar  (polar ponto2 0 (* distancia contador1)) (/ pi 2)  28.99975339469293  )    (polar (polar (polar ponto2 0 (* distancia contador1)) (/ pi 2) 28.99975339469293) 0 170)   "")
				(setq elemtrim2 (entlast))
				
				(setq sel1 nil)
				(setq sel1 (ssadd))
				(setq sel1 (ssadd elemtrim1 sel1))
				(setq sel1 (ssadd elemtrim2 sel1))
				
				(setq pontoZoomTrim (polar (polar ponto2 0 (* distancia contador1)) 1.03864 55.185))
				(command "zoom" "c" pontoZoomTrim 10)
				(command "trim" sel1 "" pontoZoomTrim "")
				
				(setq pontoZoomTrim (polar (polar ponto2 0 (* distancia contador1)) 0.34157 146.461))
				(command "zoom" "c" pontoZoomTrim 10)
				(command "trim" sel1 "" pontoZoomTrim "")
				
				(command "erase" sel1 "")
			)
			(progn
				(command "layer" "m" "BF-03-SA-SUP" "c" "253" "" "")
				;(command "insert" "C:\\bfdias\\blocos\\sa_vertical.dwg" (polar ponto 0 (* distancia contador1)) "" "" "")
				(command "insert" "C:\\bfdias\\blocos\\SUPORTE ANCORAGEM PLANTA.dwg" (polar ponto 0 (* distancia contador1)) "" "" "")
				(setq objSA (entlast))
				(desenha_SA_vertical (polar ponto 0 (* distancia contador1)))
				
				(command "line" (polar (polar ponto 0 (* distancia contador1)) 1.810296237271333 88.52687446240180) (polar (polar ponto 0 (* distancia contador1)) 4.472890507096755 88.52684339465140) "")
				(setq elemtrim1 (entlast))
				(command "line" (polar (polar ponto 0 (* distancia contador1)) 1.333948979759979 89.49857284604917)(polar (polar ponto 0 (* distancia contador1)) 4.949239154990543 89.49863393034098) "")
				(setq elemtrim2 (entlast))
				
				
				(setq sel1 nil)
				(setq sel1 (ssadd))
				(setq sel1 (ssadd elemtrim1 sel1))
				(setq sel1 (ssadd elemtrim2 sel1))
				
				;(command "circle" (polar ponto 0 (* distancia contador1)) 10)
				(setq pontoZoomTrim (polar (polar ponto 0 (* distancia contador1)) (/ pi 2) 54.9997 ))
				(command "zoom" "c" pontoZoomTrim 10)
				(command "trim" sel1 "" pontoZoomTrim "")
				
				(setq pontoZoomTrim (polar (polar ponto 0 (* distancia contador1)) (* (/ pi 2) 3) 55.0001 ))
				(command "zoom" "c" pontoZoomTrim 10)
				(command "trim" sel1 "" pontoZoomTrim "")
				
				(command "erase" sel1 "")
			)
		)
		
		
		(setq contador1 (+ contador1 1))
	)
	
	
	(command "zoom" "w" (nth 0 zoomInicial) (nth 1 zoomInicial))
	(princ)
)














