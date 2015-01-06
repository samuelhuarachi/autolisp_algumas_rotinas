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


(defun c:daf3()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq comprimento (atof (getstring "\nLargura: "))) ;VAR IMPORTANTE
	(setq qtd (atof (getstring "\nQuantidade de difusores por ramal: ")))
	(setq pontoInsert1 (getpoint "\nPonto inserção: "))
	
	
	(command "layer" "m" "linha_base" "c" "cyan" "" "")
	(command "line" pontoInsert1 (polar pontoInsert1 0 comprimento) "")
	
	
	(setq comprimento2 (- comprimento 600))
	(setq distDif (/ comprimento2 (- qtd 1)))
	(setq contador 0)
	(setq pontoIncialDif (polar pontoInsert1 0 300))
	;Marca ponto dos difusores
	(while (> qtd 0)
		
		(command "layer" "m" "ponto_dos_difusores" "c" "241" "" "")
		(command "circle" (polar pontoIncialDif 0 (* distDif contador)) 20)
		;(command "insert" "C:\\bfdias\\blocos\\DIFUSOR CORTE 2.dwg" (polar pontoIncialDif 0 (* distDif contador)) "" "" "")
		
		
		(if (/= qtd 1)
			(progn
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" (polar pontoIncialDif 0 (* distDif contador)) (polar pontoIncialDif 0 (* distDif (+ contador 1))) (polar (polar (polar pontoIncialDif 0 (* distDif contador)) 0 (/ (distance (polar pontoIncialDif 0 (* distDif contador)) (polar pontoIncialDif 0 (* distDif (+ contador 1)))) 2) ) (* (/ pi 2) 3) 550))
			)
			(progn
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" (polar pontoIncialDif 0 (* distDif contador))(polar pontoInsert1 0 comprimento) (polar (polar (polar pontoIncialDif 0 (* distDif contador)) 0 (/ (distance (polar pontoIncialDif 0 (* distDif contador)) (polar pontoInsert1 0 comprimento)) 2) ) (* (/ pi 2) 3) 550))
			)
		)
		(if (= contador 0)
			(progn
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" pontoInsert1 (polar pontoIncialDif 0 (* distDif contador)) (polar (polar pontoInsert1 0 (/ (distance pontoInsert1 (polar pontoIncialDif 0 (* distDif contador)) ) 2) ) (* (/ pi 2) 3) 550))
				
				(setq distanciaReal (distance (polar pontoIncialDif 0 (* distDif contador)) (polar pontoIncialDif 0 (* distDif (+ contador 1)))))
				(setq distanciaReal2 (rtos distanciaReal 2 20))
				
				(command "layer" "m" "Linha_Distancia_Fixa3" "c" "green" "" "")
				(command "line" (polar pontoIncialDif 0 (* distDif contador)) (polar pontoIncialDif 0 (* distDif (+ contador 1))) "")
				
				(command "text" "bc" (polar (polar pontoIncialDif 0 (* distDif contador)) 0 (/ (distance (polar pontoIncialDif 0 (* distDif contador)) (polar pontoIncialDif 0 (* distDif (+ contador 1)))) 2)) 25 0 distanciaReal2)
				
			)
		)
		
		
		(setq contador (+ contador 1))
		(setq qtd (- qtd 1))
	)
	
	
	
	(princ)
)







