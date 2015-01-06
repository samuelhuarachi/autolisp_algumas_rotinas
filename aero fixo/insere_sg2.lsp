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


(defun c:isg2()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq qtdbf (atof (getstring "\nQuantidade: ")))
	(setq espaco (atof (getstring "\nEspaço: ")))
	(setq pontoInsert (getpoint "\nPonto de inserção: "))
	
	(setq contador 0)
	(setq zoomInicial (viewextents))
	(while (< contador qtdbf)
		
		(setq pRed (polar (polar (polar pontoInsert 0 (* espaco contador)) 4.21457 130.886) (/ pi 2) 14.9999))
		
		(command "layer" "m" "bfsa2_layer" "c" "cyan" "" "")
		(command "insert" "C:\\bfdias\\blocos\\SUP SG CORTE 1.dwg" (polar (polar pRed 1.08719 134.414) (* (/ pi 2) 3) 15) "" "" "")
		
		
		(setq elementoSA (entlast))
		
		
		(if (< contador (- qtdbf 1))
			(progn
				;Insere Cota
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear"  (polar (polar pontoInsert 0 (* espaco contador)) 4.71239 115.0) (polar (polar pontoInsert 0 (* espaco (+ contador 1)))4.71239 115.0)  (polar  (polar (polar (polar pontoInsert 0 (* espaco contador))4.71239 115.0)   0  (/ (distance  (polar (polar pontoInsert 0 (* espaco contador)) 4.71239 115.0)  (polar (polar pontoInsert 0 (* espaco (+ contador 1))) 4.71239 115.0) ) 2 ) )  (* (/ pi 2) 3) 1170  )  )
			)
		)
		
		;Desenha Linhas Suportes
		(command "layer" "m" "linha_suporte" "c" "cyan" "" "")
		(command "line" (polar (polar (polar pRed 1.08719 134.414) (* (/ pi 2) 3) 15) 3.21764 52.6522) (polar (polar (polar pRed 1.08719 134.414) (* (/ pi 2) 3) 15) 4.22878 134.414)  "")
		(setq linhaSuporte1 (entlast))
		(command "line"  (polar (polar (polar pRed 1.08719 134.414) (* (/ pi 2) 3) 15) 6.20714 52.6522)   (polar (polar (polar pRed 1.08719 134.414) (* (/ pi 2) 3) 15) 5.196  134.414 )  "")
		(setq linhaSuporte2 (entlast))
		
		(setq sel1 nil)
		(setq sel1 (ssadd))
		(setq sel1 (ssadd linhaSuporte1 sel1))
		(setq sel1 (ssadd linhaSuporte2 sel1))
		(setq pontoZoomTrim  (polar (polar (polar pRed 1.08719 134.414) (* (/ pi 2) 3) 15) (* (/ pi 2) 3) 60.3998  ))
		(command "zoom" "c" pontoZoomTrim 10)
		
		
		(command "trim" sel1 "" pontoZoomTrim "")
		(command "erase" sel1 "")
		
		
		(command "line" (polar (polar (polar pontoInsert 0 (* espaco contador)) 4.21457 130.886) 0.624023 61.6117) (polar (polar (polar (polar pontoInsert 0 (* espaco contador)) 4.21457 130.886) 0.624023 61.6117)  1.5708 185.0 ) "" )
		(setq linhaSuporte1 (entlast))
		
		(command "line" (polar (polar (polar pontoInsert 0 (* espaco contador)) 4.21457 130.886) 0.44752 83.1925)   (polar (polar (polar (polar pontoInsert 0 (* espaco contador)) 4.21457 130.886) 0.44752 83.1925)  1.5708 185.0) "" )
		(setq linhaSuporte2 (entlast))
		
		(setq sel1 nil)
		(setq sel1 (ssadd))
		(setq sel1 (ssadd linhaSuporte1 sel1))
		(setq sel1 (ssadd linhaSuporte2 sel1))
		
		
		(setq pontoZoomTrim (polar (polar (polar pontoInsert 0 (* espaco contador)) 4.21457 130.886) 1.2158  179.812   ) )
		(command "zoom" "c" pontoZoomTrim 100)
		(command "trim" sel1 "" pontoZoomTrim "")
		(command "erase" sel1 "")
		
		
		(command "zoom" "w" (nth 0 zoomInicial) (nth 1 zoomInicial))
		
		(setq contador (+ contador 1))
	)
	
)












