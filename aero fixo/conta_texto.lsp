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
	
	(princ "\Selecione a estrutura")
	(setq all (ssget '((-4 . "<OR")(8 . "*")(-4 . "OR>"))))
	
	(princ "\nDigite o texto ")
	(setq procurado3 (getstring))
	
	(setq listaCoordenada nil)
	(setq qtdSA 0)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq tipo1 (strcase (cdr (assoc 0 (entget obj)))))
				(if (= tipo1 "TEXT")
					(progn
						(setq textoValue (strcase (cdr (assoc 1 (entget obj)))))
						
						(if (= textoValue procurado3)
							(progn
								(setq coord (cdr (assoc 10 (entget obj))))
								(setq x1 (rtos (car coord) 2 3))
								(setq y1 (rtos (cadr coord) 2 3))
								(setq chave1 (strcat x1 y1))
								
								(setq procura (assoc chave1 listaCoordenada))
								(if (= procura nil)
									(progn
										
										(setq qtdSA (+ qtdSA 1))
										(setq listaCoordenada (cons (list chave1) listaCoordenada))
									)
									(progn
										
										
									)
								)
								
							)
						)
						
						
					)
				)
				
				;(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				;(setq coord (cdr (assoc 10 (entget obj))))
				;(setq x1 (rtos (car coord) 2 3))
				;(setq y1 (rtos (cadr coord) 2 3))
				
			
				(setq qtd (- qtd 1))
			)
		)
	)
	
	(princ (strcat "\nQuantida de elementos encontrados: " (itoa qtdSA)))
	
	(princ)
)







