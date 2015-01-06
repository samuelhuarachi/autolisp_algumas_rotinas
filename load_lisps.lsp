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





(defun c:ll()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq directoryName "C:\\bfdias")
	
	(setq listaArquivos (reverse(vl-directory-files directoryName "*.lsp")))
	(setq qtd (vl-list-length listaArquivos))
	
	
	;(setq resp (strcase (getstring "\n[T]odos / [E]scolher")))
	(setq resp "E")
	(if (and (/= resp "T")(/= resp "E"))
		(progn
			(setq resp "E")
		)
	)
	
	(setq contLisp 1)
	(while (> qtd 0)
		(setq arqName (nth (- qtd 1) listaArquivos))
		
		
		(if (/= arqName "load_lisps.lsp")
			(progn
				
				(if (= resp "T")
					(progn
						(load (strcat directoryName "\\" arqName))
						(princ (strcat "\nO lisp " arqName " foi carregado no sistema!"))
						
					)
				)
				(if (= resp "E")
					(progn
						(princ (strcat "\n" (itoa contLisp) " - " arqName ))
					)
				)
				
				
				
				
			)
		)
		
		
		(setq contLisp (+ contLisp 1))
		(setq qtd (- qtd 1))
	)
	
	(if (= resp "E")
		(progn
			(setq resp2 (getstring "\nQual lisp você irá carregar? "))
			
			(setq arqName (nth (- (atoi resp2) 1) (reverse listaArquivos)))
			(load (strcat directoryName "\\" arqName))
			(princ (strcat "\nO lisp " arqName " foi carregado no sistema!"))
			
		)
	)
	
	(princ "\nFim do processo!")
	(princ)
)







