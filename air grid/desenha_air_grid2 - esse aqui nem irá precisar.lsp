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





(defun c:dd()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq quantidade (atof (getstring "\nQuantidade: ")))
	(setq distancia (atof (getstring "\nDistancia entre os pontos: ")))
	(setq pontoInsercao (getpoint  "\nPonto de inserção:  "))
	(setq vezes 0)
	
	(while (> quantidade 0)
		(command "layer" "m" "corte2Difusor_layer" "c" "white" "" "")
		(command "insert" "C:\\bfdias\\blocos\\corte2Difusor_bloco.dwg" (polar pontoInsercao 0 (* distancia vezes)) "" "" "")
	
		(setq vezes (+ vezes 1))
		(setq quantidade (- quantidade 1))
	)
	
	
	(princ)
)







