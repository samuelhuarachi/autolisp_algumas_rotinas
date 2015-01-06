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


(defun c:destl()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq resp (strcase (getstring "\nTaque ou lagoa? [T/L]")))
	
	(if (= resp "T")
		(progn
			(princ "\nDesenhando tanque...")
			(setq comprimento (atof (getstring "\nComprimento: ")))
			(setq largura (atof (getstring "\nLargura: ")))
			(setq varOffset (atof (getstring "\nOffset: ")))
			(setq pontoInsercao (getpoint "\nPonto inserção: "))
			
			(setq pontoOffset (polar pontoInsercao (+ pi (/ pi 4))  100))
			
			(command "layer" "m" "BF-07-SA-EST" "c" "43" "" "")
			(command "line" pontoInsercao (polar pontoInsercao 0 comprimento) "")
			(setq l1 (entlast))
			(command "line" pontoInsercao (polar pontoInsercao (/ pi 2) largura) "")
			(setq l2 (entlast))
			(command "line" (polar pontoInsercao (/ pi 2) largura) (polar (polar pontoInsercao (/ pi 2) largura) 0 comprimento) "")
			(setq l3 (entlast))
			(command "line" (polar pontoInsercao 0 comprimento) (polar (polar pontoInsercao 0 comprimento) (/ pi 2) largura) "")
			(setq l4 (entlast))
			
			(setq sel1 (ssadd))
			(setq sel1 (ssadd l1 sel1))
			(setq sel1 (ssadd l2 sel1))
			(setq sel1 (ssadd l3 sel1))
			(setq sel1 (ssadd l4 sel1))
			
			(command "pedit" l1 "y" "J" sel1 "" "")
			
			(command "offset" varOffset pontoInsercao pontoOffset "")
		)
	)
	(if (= resp "L")
		(progn
			(princ "\nDesenhando lagoa...")
			
			(setq comprimentoE (atof (getstring "\nComprimento Externo: ")))
			(setq comprimentoI (atof (getstring "\nComprimento Interno: ")))
			(setq larguraE (atof (getstring "\nLargura Externa: ")))
			(setq larguraI (atof (getstring "\nLargura Interna: ")))
			(setq pontoInsercao (getpoint "\nPonto inserção: "))
			
			(command "layer" "m" "BF-07-SA-EST" "c" "43" "" "")
			(command "line" pontoInsercao (polar pontoInsercao 0 comprimentoE) "")
			(setq l1 (entlast))
			(command "line" pontoInsercao (polar pontoInsercao (/ pi 2) larguraE) "")
			(setq l2 (entlast))
			(command "line" (polar pontoInsercao (/ pi 2) larguraE) (polar (polar pontoInsercao (/ pi 2) larguraE) 0 comprimentoE) "")
			(setq l3 (entlast))
			(command "line" (polar pontoInsercao 0 comprimentoE) (polar (polar pontoInsercao 0 comprimentoE) (/ pi 2) larguraE) "")
			(setq l4 (entlast))
			
			(setq xSomar (/ (- comprimentoE comprimentoI) 2))
			(setq ySomar (/ (- larguraE larguraI) 2))
			
			(setq pontoInsercao2 (list (+ (nth 0 pontoInsercao) xSomar) (+ (nth 1 pontoInsercao) ySomar) 0.0))
			(command "line" pontoInsercao2 (polar pontoInsercao2 0 comprimentoI) "")
			(setq l11 (entlast))
			(command "line" pontoInsercao2 (polar pontoInsercao2 (/ pi 2) larguraI) "")
			(setq l22 (entlast))
			(command "line" (polar pontoInsercao2 (/ pi 2) larguraI) (polar (polar pontoInsercao2 (/ pi 2) larguraI) 0 comprimentoI) "")
			(setq l33 (entlast))
			(command "line" (polar pontoInsercao2 0 comprimentoI) (polar (polar pontoInsercao2 0 comprimentoI) (/ pi 2) larguraI) "")
			(setq l44 (entlast))
			
			;Faz linhas ligações
			(command "line" pontoInsercao pontoInsercao2 "")
			(command "line" (polar pontoInsercao (/ pi 2) larguraE) (polar pontoInsercao2 (/ pi 2) larguraI) "")
			(command "line" (polar (polar pontoInsercao (/ pi 2) larguraE) 0 comprimentoE) (polar (polar pontoInsercao2 (/ pi 2) larguraI) 0 comprimentoI) "")  
			(command "line" (polar pontoInsercao 0 comprimentoE) (polar pontoInsercao2 0 comprimentoI) "")
			
		)
	)
	
	(if (and (/= resp "T")(/= resp "L"))
		(progn
			
			(alert (strcat "Opção inválida '" resp "'"))
		)
	)
	
	
	(princ)
)



















