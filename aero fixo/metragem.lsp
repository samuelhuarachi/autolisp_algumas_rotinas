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



(defun retornaQtdTE3(coord1 coord2)
	
	;(setq all (ssget "x" (List (cons 2 "TE 3_bloco"))))
	(setq all (ssget "x" (List (cons 2 "TE PLANTA 3"))))
	
	(setq yFixo (rtos (nth 1 coord1) 2 3))
	(setq listaEncontrados nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq yFixo2 (rtos (nth 1 coord) 2 3))
				(if (and  (= yFixo2 yFixo) (> (nth 0 coord) (nth 0 coord1) ) (< (nth 0 coord) (nth 0 coord2) ) )
					(progn
						(setq listaEncontrados (cons (list obj) listaEncontrados))
					)
				)
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
	listaEncontrados
)

(defun retornaQtdTE4(coord1 coord2)
	
	(setq all (ssget "x" (List (cons 2 "TE PLANTA 4"))))
	
	(setq yFixo (rtos (nth 1 coord1) 2 3))
	(setq listaEncontrados nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq yFixo2 (rtos (nth 1 coord) 2 3))
				(if (and  (= yFixo2 yFixo) (> (nth 0 coord) (nth 0 coord1) ) (< (nth 0 coord) (nth 0 coord2) ) )
					(progn
						(setq listaEncontrados (cons (list obj) listaEncontrados))
					)
				)
				
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
	listaEncontrados
)

(defun c:dd()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	(setq all (ssget '((-4 . "<OR") (8 . "BF-08-SA-LCE")  (-4 . "OR>"))))
	(if (/= all nil)
		(progn
			(setq obj1 (ssname all 0))
			(setq coord1 (cdr (assoc 10 (entget obj1))))
			(setq coord2 (cdr (assoc 11 (entget obj1))))
			(if (< (nth 1 coord1) (nth 1 coord2))
				(progn
					
				)
				(progn
					(setq save1 coord2)
					(setq coord2 coord1)
					(setq coord1 save1)
				)
			)
			(setq xFixo (nth 0 coord1))
			(setq x1 (rtos xFixo 2 3))
			(setq xFixo2 x1)
			
			(setq allBolsaRamal (ssget "x" (List (cons 8 "BOLSA RAMAL"))))
			
			(setq listaRamalFind nil)
			(if (/= allBolsaRamal nil)
				(progn
					(setq qtd2 (- (sslength allBolsaRamal) 1))
					(while (>= qtd2 0)
						(setq objRamal (ssname allBolsaRamal qtd2))
						(setq coordRamal (cdr (assoc 10 (entget objRamal))))
						
						(if (and (> (nth 1 coordRamal) (nth 1 coord1)) (< (nth 1 coordRamal) (nth 1 coord2))  )
							(progn
								(setq xFixo (nth 0 coordRamal))
								(setq x1 (rtos xFixo 2 3))
								(setq xFixoRamal x1)
								
								(if (= xFixoRamal xFixo2)
									(progn
										(setq listaRamalFind (cons coordRamal listaRamalFind))
									)
								)
								
								
							)
						)
						
						(setq qtd2 (- qtd2 1))
					)
				)
			)
			
			
			(princ (strcat "\nLargura: " (rtos (distance coord1 coord2) 2 3)) )
			
			(setq quantidadeBolsa (vl-list-length listaRamalFind))
			
			(princ (strcat "\nQuantidade de bolsa ramal encontrado " (itoa quantidadeBolsa)))
			
			(setq totalTubulacaoV (+ (- (distance coord1 coord2) (+ 84.99966131060501 85.02980823078543 )) (* quantidadeBolsa 70)))
			
			
			(princ (strcat "\nMetragem " (rtos totalTubulacaoV 2 5)))
			
			
		)
	)
	
	(princ "\nSelecione")
	
	;Precisa localizar a entrada de ar
	(setq all (ssget '((-4 . "<OR") (8 . "BF-08-SA-LCE")  (-4 . "OR>"))))
	(if (/= all nil)
		(progn
			(setq obj1 (ssname all 0))
			(setq coord1 (cdr (assoc 10 (entget obj1))))
			(setq coord2 (cdr (assoc 11 (entget obj1))))
			
			;Define xmenor e xmaior
			(if (< (nth 0 coord1) (nth 0 coord2) )
				(progn
					
				)
				(progn
					(setq coordTemp coord2)
					(setq coord2 coord1)
					(setq coord1 coordTemp)
				)
			)
			
			(setq qtdTE3 (retornaQtdTE3 coord1 coord2))
			(setq qtdTE4 (retornaQtdTE4 coord1 coord2))
			
			(vl-list-length qtdTE3)
			(vl-list-length qtdTE4)
			
			(princ (strcat "\nComprimento: " (rtos (distance coord1 coord2) 2 3)) )
			
			(setq total (- (distance coord1 coord2) (+ 65.00029253331013 64.99996120308060 (* (vl-list-length qtdTE3) 160) (* (vl-list-length qtdTE4) 160) )  ))
			
			(princ (strcat "\nQuantidade de ramais " (rtos (+ (vl-list-length qtdTE3) (vl-list-length qtdTE4) 2) 2 0)))
			(princ (strcat "\nTotal de metragem encontrados: " (rtos total 2 3) ) )
			
			
			
			
		)
	)
	
	
	
	(princ)
)







