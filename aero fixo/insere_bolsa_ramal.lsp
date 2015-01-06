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





(defun c:iabf()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq quantidade (atof (getstring "\nQuantidade: ")))
	(setq distancia (atof (getstring "\nDistância entre os pontos: ")))
	;(setq ponto (getpoint "\nPonto de inserção: "))
	
	
	(command "layer" "m" "bolsa ramal" "c" "253" "" "")
	(command "insert" "C:\\bfdias\\blocos\\BOLSA RAMAL.dwg" (list 0.0 0.0 0.0) "" "" "")
	(setq objBolsaRamal (entlast))
	
	(setq coord (cdr (assoc 10 (entget objBolsaRamal))))
	
	(princ "\n--------------===============")
	(princ "\n--------------===============")
	(princ "\n--------------===============")
	(princ "\n--------------===============")
	(princ "\nDefina o ponto para inserir a bolsa ramal")
	
	(command "move" objBolsaRamal "" coord  PAUSE)
	
	(setq coord (cdr (assoc 10 (entget objBolsaRamal))))
	
	
	(setq multxDist 0)
	(setq zoomInicial (viewextents))
	(while (> quantidade 0)
		(setq quantidade (- quantidade 1))
		
		(setq multxDist (+ multxDist 1))
		(command "layer" "m" "bolsa ramal" "c" "253" "" "")
		(command "line" (polar coord 0 70) (polar coord pi 70) "")
		(setq objL1 (entlast))
		(command "line" (polar coord 4.03303 102.831) (polar coord 5.4558 108.672) "")
		(setq objL2 (entlast))
		
		(setq sel1 nil)
		(setq sel1 (ssadd))
		(setq sel1 (ssadd objL1 sel1))
		(setq sel1 (ssadd objL2 sel1))
		
		(setq pontoZoomTrim (polar coord 3.84893 72.3594))
		(command "zoom" "c" pontoZoomTrim 10)
		(command "trim" sel1 "" pontoZoomTrim "")
		
		
		(setq pontoZoomTrim (polar coord 5.60031 70.898))
		(command "zoom" "c" pontoZoomTrim 10)
		(command "trim" sel1 "" pontoZoomTrim "")
		
		(command "erase" sel1 "")
		
		
		(if (/= quantidade 0)
			(progn
				(command "layer" "m" "bolsa ramal" "c" "253" "" "")
				(command "insert" "C:\\bfdias\\blocos\\BOLSA RAMAL.dwg" (polar coord 0 distancia) "" "" "")
				(setq objBolsaRamal (entlast))
				(setq coord (cdr (assoc 10 (entget objBolsaRamal))))
				
			)
		)
		
		
		
		
		
	)
	
	
	
	
	
	
	
	(command "zoom" "w" (nth 0 zoomInicial) (nth 1 zoomInicial))
	(princ)
)







