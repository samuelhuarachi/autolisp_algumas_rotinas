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


(defun c:agridcont()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(command ".textstyle" "BF-CA-TXT")
	(setq lastroTipo nil)
	(while (and (/= lastroTipo "4")(/= lastroTipo "5"))
		(prompt "\n40x40 ou 50x50? [4/5]")
		(setq lastroTipo (getstring))
		(if (and (/= lastroTipo "4")(/= lastroTipo "5"))
			(progn
				(alert "Voce escolheu uma opcao invalida. Escolha 4 ou 5.")
			)
		)
	)
		
	
	(setq pontoInsercao (getpoint "\nPonto de insercao: "))
	
	(command "layer" "m" "BF-05-SA-TXT" "")
	
	(command "text" pontoInsercao 1.7907 0 "05.02")
	(command "text" "bc" (polar pontoInsercao 0 17.47229293093684) 1.7907 0 "--.--.--.-----")
	(command "text" (polar pontoInsercao 0 28.731492759) 1.7907 0 "PERFIL BORRACHA")
	(command "text" "bc" (polar (polar pontoInsercao 0 28.731492759) 0 112.31733527) 1.7907 0 "06 pç.")
	
	
	(setq pontoInsercao (polar pontoInsercao sam_parabaixo 3.581361499670720))
	(command "text" pontoInsercao 1.7907 0 "04.02")
	(command "text" "bc" (polar pontoInsercao 0 17.47229293093684) 1.7907 0 "02.99.99.00850 ")
	(command "text" (polar pontoInsercao 0 28.731492759) 1.7907 0 "GRANALAHA DE AÇO")
	(command "text" "bc" (polar (polar pontoInsercao 0 28.731492759) 0 112.31733527) 1.7907 0 "?? Kg.")
	(command "text" "bc" (polar (polar (polar pontoInsercao 0 28.731492759) 0 112.31733527) 0 14.49793696294) 1.7907 0 "?? Kg.")
	
	(setq pontoInsercao (polar pontoInsercao sam_parabaixo 3.581361499670720))
	(command "text" pontoInsercao 1.7907 0 "03.02")
	(command "text" "bc" (polar pontoInsercao 0 17.47229293093684) 1.7907 0 "02.02.04.00044 ")
	(if (= lastroTipo "4")
		(progn
			(setq extensao1 " 40 X 40")
		)
		(progn
			(setq extensao1 " 50 X 50")
		)
	)
	(command "text" (polar pontoInsercao 0 28.731492759) 1.7907 0 (strcat "TUBO INOX" extensao1) )
	(command "text" "bc" (polar (polar pontoInsercao 0 28.731492759) 0 112.31733527) 1.7907 0 "?? m.")
	(command "text" "bc" (polar (polar (polar pontoInsercao 0 28.731492759) 0 112.31733527) 0 14.49793696294) 1.7907 0 "?? m.")
	
	
	
	(setq pontoInsercao (polar pontoInsercao sam_parabaixo 3.581361499670720))
	(command "text" pontoInsercao 1.7907 0 "02.02")
	(command "text" "bc" (polar pontoInsercao 0 17.47229293093684) 1.7907 0 "02.02.02.00034 ")
	(command "text" (polar pontoInsercao 0 28.731492759) 1.7907 0 "BARRA REDONDA INOX Ø 3/8\" "  )
	(command "text" "bc" (polar (polar pontoInsercao 0 28.731492759) 0 112.31733527) 1.7907 0 "1,4 m.")
	(command "text" "bc" (polar (polar (polar pontoInsercao 0 28.731492759) 0 112.31733527) 0 14.49793696294) 1.7907 0 "?? m.")
	
	(setq pontoInsercao (polar pontoInsercao sam_parabaixo 3.581361499670720))
	(command "text" pontoInsercao 1.7907 0 "01.02")
	(command "text" "bc" (polar pontoInsercao 0 17.47229293093684) 1.7907 0 "02.02.02.00064 ")
	(command "text" (polar pontoInsercao 0 28.731492759) 1.7907 0 "CANTONEIRA INOX 2\" X 1/4\"" )
	(command "text" "bc" (polar (polar pontoInsercao 0 28.731492759) 0 112.31733527) 1.7907 0 "?? m.")
	(command "text" "bc" (polar (polar (polar pontoInsercao 0 28.731492759) 0 112.31733527) 0 14.49793696294) 1.7907 0 "?? m.")
	
	
	
	(princ)
)







