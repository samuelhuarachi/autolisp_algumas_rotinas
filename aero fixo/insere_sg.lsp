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


(defun c:isg()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq quantidade (atof (getstring "\nQuantidade: ")))
	(setq distancia (atof (getstring "\nDistancia: ")))
	(setq horvert (strcase (getstring "\nHorizontal --- , ou Vertical | : [h/v]?")))
	(if (and (/= horvert "V")(/= horvert "H"))
		(progn
			(alert (strcat "Você selecionou uma opção inválida (" horvert ")! O programa será encerrado!"))
			(exit)
		)
	)
	(setq pontoInsercao (getpoint "\nPonto de inserção: "))
	
	
	(setq contador 0)
	
	(while (< contador quantidade)
		
		
		(if (= horvert "H")
			(progn
				(command "layer" "m" "bfsg_layer" "c" "cyan" "" "")
				(command "Insert" "C:\\bfdias\\blocos\\SUPORTE GUIA PLANTA2.dwg" (polar pontoInsercao 0 (* distancia contador)) "" "" "")
				
			)
		)
		
		(if (= horvert "V")
			(progn
				(command "layer" "m" "bfsg_layer" "c" "cyan" "" "")
				(command "Insert" "C:\\bfdias\\blocos\\SUPORTE GUIA PLANTA.dwg" (polar pontoInsercao 0 (* distancia contador)) "" "" "")
				(setq objSG (entlast))
				;(command "rotate" objSG "" pontoInsercao (polar  pontoInsercao (/ pi  2) 40) )
				(setq coord (cdr (assoc 10 (entget objSG))))
				;(command "move" objSG "" coord (polar coord (/ pi 2) 2))
				
			)
		)
		
		
		
		
		
		(setq contador (+ contador 1))
	)
	
	
)


















