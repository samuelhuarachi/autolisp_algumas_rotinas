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


(defun c:isa2()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq qtdbf (atof (getstring "\nQuantidade: ")))
	(setq espaco (atof (getstring "\nEspaço: ")))
	(setq pontoInsert (getpoint "\nPonto de inserção: "))
	
	(setq contador 0)
	(setq zoomInicial (viewextents))
	(while (< contador qtdbf)
		;4.21457
		;130.886
		(command "layer" "m" "bfsa2_layer" "c" "cyan" "" "")
		(command "insert" "C:\\bfdias\\blocos\\aefixa2_sa.dwg" (polar (polar pontoInsert 0 (* espaco contador)) 4.535876305266107 120.7306419147060) "" "" "")
		
		;(getstring "sdfasfsa")
		(setq elementoSA (entlast))
		(if (< contador (- qtdbf 1))
			(progn
				;Insere Cota
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear"  (polar (polar pontoInsert 0 (* espaco contador)) 4.71239 115.0) (polar (polar pontoInsert 0 (* espaco (+ contador 1)))4.71239 115.0)  (polar  (polar (polar (polar pontoInsert 0 (* espaco contador))4.71239 115.0)   0  (/ (distance  (polar (polar pontoInsert 0 (* espaco contador)) 4.71239 115.0)  (polar (polar pontoInsert 0 (* espaco (+ contador 1))) 4.71239 115.0) ) 2 ) )  (* (/ pi 2) 3) 500  )  )
			)
		)
		
		
		;Desenha Linhas Suportes
		(command "line" (polar (polar pontoInsert 0 (* espaco contador)) 1.840663562594390 79.51884196976952)  (polar (polar pontoInsert 0 (* espaco contador)) 4.535876305266107 120.7306419147060)  "" )
		(setq linhaSuporte1 (entlast))
		
		(command "line"  (polar (polar pontoInsert 0 (* espaco contador)) 1.305783754240726 79.41313637060995)  (polar (polar pontoInsert 0 (* espaco contador)) 4.885638087591250 120.6610454833741)  "" )
		(setq linhaSuporte2 (entlast))
		
		(setq sel1 nil)
		(setq sel1 (ssadd))
		(setq sel1 (ssadd linhaSuporte1 sel1))
		(setq sel1 (ssadd linhaSuporte2 sel1))
		
		
		(setq pontoZoomTrim (polar (polar pontoInsert 0 (* espaco contador))  1.570658201306931 56.12220713790460 ) )
		
		(command "zoom" "c" pontoZoomTrim 10)
		(command "trim" sel1 "" pontoZoomTrim "")
		
		(setq pontoZoomTrim (polar (polar pontoInsert 0 (* espaco contador))  sam_parabaixo  53.89434280158835 ) )
		(command "zoom" "c" pontoZoomTrim 10)
		(command "trim" sel1 "" pontoZoomTrim "")
		
		
		
		(command "erase" sel1 "")
		(setq contador (+ contador 1))
	)
	
	
	(command "zoom" "w" (nth 0 zoomInicial) (nth 1 zoomInicial))
)












