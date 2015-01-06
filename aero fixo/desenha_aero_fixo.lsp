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

(defun desenha_AB (ponto)
	
	(command "layer" "m" "BF-05A-SA-TXT" "c" "251" "" "")
	
	(command "line" ponto (polar ponto 0.727222 95.9857) "")
	(command "line" (polar ponto 0.727222 95.9857) (polar (polar ponto 0.727222 95.9857) 0 265.045) "")
	
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" (polar (polar ponto 0.727222 95.9857) 1.0472 21.613) 137.2 0 "AB")
)
 
 

(defun saveVars()
	(setq comprimento(atof (get_tile "comprimento")))
	(setq largura    (atof (get_tile "largura")))
	(setq qtd        (atof (get_tile "qtd")))
	(setq qtdColunas (atof (get_tile "qtdColunas")))
)

(defun c:aefixo()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	;DCL start
	(if(not(setq dcl_id (load_dialog "C:\\bfdias\\aero fixo\\dcl\\desenha_aero_fixo.dcl")))
		(progn
			(alert "O arquivo da DCL não pode ser carregado!")
			(exit)
		)
		(progn
			;;;--- Load the DCL definition inside the DCL file
			(if (not (new_dialog "SAMPLE1" dcl_id))
				(progn
					(alert "A definição SAMPLE1 não pode ser encontrada dentro do arquivo DCL!")
					(exit)
				)
				;;;--- Else, the definition was loaded
				(progn
					(action_tile "accept" "(saveVars)(done_dialog 2)")
					;;;--- If an action event occurs, do this function
					(action_tile "cancel" "(done_dialog 1)")
					;;;--- Display the dialog box
					(setq ddiag  (start_dialog))
					;;;--- Unload the dialog box
					(unload_dialog dcl_id)
					
					;;;--- If the user pressed the Cancel button
					(if(= ddiag 1)(exit))
					;;;--- If the user pressed the Okay button
					(if(= ddiag 2)
						(progn
							;.(princ "\n The user pressed Okay!")
						)
					)
				)
			)
		)
	)
	
	
	;(setq comprimento (atof (getstring "\nComprimento: "))) ;VAR IMPORTANTE
	;(setq largura (atof (getstring "\nLargura: "))) ;VAR IMPORTANTE
	;(setq qtd (atof (getstring "\nQuantidade de difusores por ramal: ")))
	;(setq qtdColunas (atof (getstring "\nQuantidade de ramal: ")))
	
	
	(setq p1 (getpoint "\nPonto inserção "))
	
	;(setq tamanhoLinha (- largura (+ 300.001 246.723)))
	(setq tamanhoLinha (- largura (+ 300.001 300.002)))
	
	
	
	(setq pontoInicioBase p1)
	(setq pontoMeio (polar (polar pontoInicioBase (* (/ pi 2) 3) 300.002) 0 (/ comprimento 2))) ;VAR IMPORTANTE (Meio da base, horizontal)
	
	
	(setq pontoMeioVertical (polar (polar (polar p1 (* (/ pi 2) 3) 300.002) (* (/ pi 2) 3) 300.002) (/ pi 2) (/ largura 2))) ;(Meio Vertical )
	(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
	(command "dimlinear" (polar p1 (* (/ pi 2) 3) 300.002) (polar (polar p1 (* (/ pi 2) 3) 300.002) (/ pi 2) largura) (polar pontoMeioVertical pi 1300))
	
	
	(command "layer" "m" "BF-08-SA-LCE" "c" "red" "" "")
	(command "line" (polar p1 (* (/ pi 2) 3) 300.002) (polar (polar p1 (* (/ pi 2) 3) 300.002) (/ pi 2) largura) "")	
	(command "line" (polar (polar p1 (* (/ pi 2) 3) 300.002) (/ pi 2) largura) (polar (polar (polar p1 (* (/ pi 2) 3) 300.002) (/ pi 2) largura) 0 comprimento) "")
	
	
	
	
	(setq distanciaEntreAsBolinhas (/ tamanhoLinha (- qtd 1)))
	
	
	;Configura as Colunas
	(setq InicioBase (polar p1 (*(/ pi 2) 3) 300.002))
	(command "layer" "m" "BF-08-SA-LCE" "c" "red" "" "")
	(command "line" InicioBase (polar InicioBase 0 comprimento) "")
	
	;VAR Help
	(setq PontoFinalBase (polar InicioBase 0 comprimento)) ;inferior
	(setq PontoInsercaoTID (polar PontoFinalBase 2.77905 155.081))
	(setq PontoFinalBaseSuperior (polar (polar InicioBase 0 comprimento) (/ pi 2) largura))
	(setq PontoInsercaoTSD (polar PontoFinalBaseSuperior 2.77923 155.07))
	
	;Faz primeira cota inferior lado esquerdo
	(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
	(command "dimlinear" InicioBase (polar InicioBase 0 (/ comprimento (- qtdColunas 1))) (polar   (polar InicioBase 0 (/ (distance InicioBase (polar InicioBase 0 (/ comprimento (- qtdColunas 1)))) 2)) (* (/ pi 2) 3) 1300))
	
	
	
	;Desenha Linha Vermelha Ultima Coluna
	(command "layer" "m" "BF-08-SA-LCE" "c" "red" "" "")
	(command "line" PontoFinalBase PontoFinalBaseSuperior "")
	
	;Inserção da TID (tubulação inferior direita)
	(command "layer" "m" "TID_layer" "c" "white" "" "")
	(command "insert" "C:\\bfdias\\blocos\\curva planta 3.dwg" (polar (polar (polar InicioBase 0 comprimento) 3.49304 159.765) 0 4.999473022058198) "" "" "")
	
	
	;Inserção da TSD (Tubulação superior direita)
	(command "layer" "m" "TSD_layer" "c" "white" "" "")
	(command "insert" "C:\\bfdias\\blocos\\curva planta 4.dwg" PontoInsercaoTSD "" "" "")
	
	
	;Inserção das "bolinhas" Última Coluna
	(setq pontoInicialBolinhas1 (polar PontoFinalBase (/ pi 2) 300.002))
	(setq contador3 0)
	
	
	;distanciaEntreAsBolinhas 
	;Faz linha auxiliar que será utilizada para ver a distancia exata entre os ramais.
	;Com essa informações conseguimos colocar os suportes de maneira exata no desenho, tanto
	;no aereo fixa1, quanto o fixa2, ou 3.
	(setq p1aux1 (polar InicioBase (/ pi 2) 800))
	(setq p2aux1 (polar p1aux1 0 (/ comprimento (- qtdColunas 1))))
	
	(command "layer" "m" "linhaAux2" "c" "cyan" "" "")
	(command "line" p1aux1 p2aux1 "")
	(command "text" "bc" (polar p1aux1 0 (/ (distance p1aux1 p2aux1) 2)) 40 0 (rtos (/ comprimento (- qtdColunas 1)) 2 18))
	
	
	(setq zoomInicial (viewextents))
	
	(while (< contador3 qtd)
		(setq pontoInserBolinha (polar pontoInicialBolinhas1 (/ pi 2) (* distanciaEntreAsBolinhas contador3)))
		
		;Inserção das bolinhas
		(command "layer" "m" "0" "c" "white" "" "")
		(command "insert" "C:\\bfdias\\blocos\\DIF.BLKS_bloco.dwg" pontoInserBolinha "" "" "")
		
		(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
		;Tubulação TIE
		(setq p1Inicio (polar pontoInserBolinha 2.00188 127.5))
		(setq p2Inicio (polar pontoInserBolinha 1.13971 127.5)) ;5.14348 127.5
		(setq Destino (polar pontoInserBolinha (/ pi 2) distanciaEntreAsBolinhas))
		(setq p1Fim (polar Destino 4.2813 127.5))
		(setq p2Fim (polar Destino 5.14348 127.5))
		
		
		
		;(command "layer" "m" "samuelTeste6" "c" "221" "" "")
		;(command "donut" 25 20 p1Inicio "")
		
		(if (/= (+ contador3 1) qtd)
			(progn
				(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "") ;<<<<<<<<<<<----------------------------------- faz vertical
				(command "line" (polar p1Inicio  pi 1.72295) (polar  p1Fim  pi 1.72295) "")
				(command "line" (polar p2Inicio  0 1.72295)  (polar p2Fim  0 1.72295) "")
				
				
				;Help6 inicio image
				(if (= contador3 (- qtd 2))
					(progn
						
						(command "line"
						(polar (polar  p1Fim  pi 1.72295)  (/ pi 2)  230.863)
						(polar  (polar (polar  p1Fim  pi 1.72295)  (/ pi 2)  230.863) (/ pi 2)  50.9749)
						"")
						
						(command "line"
						(polar (polar p2Fim  0 1.72295)  (/ pi 2)  230.863)
						(polar  (polar (polar p2Fim  0 1.72295)  (/ pi 2)  230.863) (/ pi 2)  50.9749)
						"")
					)
				)
				;Help6 fim image
				
				
				;Help5 inicio image
				(if (= contador3 0)
					(progn
						
						(command "line"
						(polar (polar p1Inicio  pi 1.72295)  (* (/ pi 2) 3)  230.863)
						(polar  (polar (polar p1Inicio  pi 1.72295)  (* (/ pi 2) 3)  230.863) (* (/ pi 2) 3)  50.9749)
						"")
						
						(command "line"
						(polar (polar p2Inicio  0 1.72295)  (* (/ pi 2) 3)  230.863)
						(polar  (polar (polar p2Inicio  0 1.72295)  (* (/ pi 2) 3)  230.863) (* (/ pi 2) 3)  50.9749)
						"")
						
					)
				)
				;Help5 fim image
				
			)
		)
		
		
		(setq contador3 (+ contador3 1))
	)
	
	
	
		;Cortando a base
	(setq contador2 1)
	(while (< contador2 qtdColunas)
		
		;Inicio base inferior do desenho
		(setq pontoInterseccao (polar InicioBase 0 (* (/ comprimento (- qtdColunas 1)) contador2)))
		(if (/= (+ contador2 1) qtdColunas)
			(progn
				
				(if (< (nth 0 pontoInterseccao)(nth 0 pontoMeio))
					(progn
						;Inserção da intersecção inferior
						(command "layer" "m" "interseccao_layer" "c" "20" "" "")
						;(command "insert" "C:\\bfdias\\blocos\\interseccao_bloco.dwg" pontoInterseccao "" "" "")
						(command "insert" "C:\\bfdias\\blocos\\TE PLANTA 3.dwg" pontoInterseccao "" "" "")
						
						
						
					)
					(progn
						;Inserção da intersecção inferior 2
						(command "layer" "m" "interseccao_layer2" "c" "20" "" "")
						(command "insert" "C:\\bfdias\\blocos\\TE PLANTA 4.dwg" pontoInterseccao "" "" "")
						
					)
				)
				
				
				;Desenha as Legendas AB nos pontos de intersecção
				(setq PontoInserçãoAB (polar pontoInterseccao 1.07861 137.55))
				;(desenha_AB PontoInserçãoAB)
				
				
				;Inserção das cotas inferiores (Base inferior)
				(setq pontoInterseccaoProximo (polar pontoInterseccao 0 (/ comprimento (- qtdColunas 1))))
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" pontoInterseccao pontoInterseccaoProximo (polar (polar pontoInterseccao 0 (/ (distance pontoInterseccao pontoInterseccaoProximo) 2)) (* (/ pi 2) 3) 1300))
				
				
				(setq contadorBolinhas 0)
				(setq pontoInicialBolinhas (polar pontoInterseccao (/ pi 2) 300.002))
				(while (< contadorBolinhas qtd)
					
					;Inserção das bolinhas
					(command "layer" "m" "0" "c" "white" "" "")
					(command "insert" "C:\\bfdias\\blocos\\DIF.BLKS_bloco.dwg" (polar pontoInicialBolinhas (/ pi 2) (* distanciaEntreAsBolinhas contadorBolinhas)) "" "" "")
					
					
					
					(if (/= (+ contadorBolinhas 1) qtd)
						(progn
							;Coloca Tubulação;Desenha tubulação
							(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "") ;<<<<<<<<<<<----------------------------------- faz vertical
							;Próximo ponto
							(setq proxPonto (polar pontoInicialBolinhas (/ pi 2) (* distanciaEntreAsBolinhas (+ contadorBolinhas 1))))
							(command "line" (polar (polar (polar pontoInicialBolinhas (/ pi 2) (* distanciaEntreAsBolinhas contadorBolinhas)) 2.00188 127.5)  pi 1.72295) (polar  (polar proxPonto 4.2813 127.5)  pi 1.72295) "")
							(command "line" (polar (polar (polar pontoInicialBolinhas (/ pi 2) (* distanciaEntreAsBolinhas contadorBolinhas)) 1.13971 127.5)  0 1.72295)  (polar (polar proxPonto 5.14348 127.5)  0 1.72295) "")
							
							
							;Help3 image
							(if (= contadorBolinhas (- qtd 2))
								(progn
									(command "line" (polar (polar  (polar proxPonto 4.2813 127.5)  pi 1.72295)  (/ pi 2)  230.863) (polar  (polar (polar  (polar proxPonto 4.2813 127.5)  pi 1.72295)  (/ pi 2)  230.863) (/ pi 2)  (+ 3.9983 50.9749)) "")
									(command "line" (polar (polar (polar proxPonto 5.14348 127.5)  0 1.72295)  (/ pi 2)  230.863) (polar  (polar (polar (polar proxPonto 5.14348 127.5)  0 1.72295)   (/ pi 2)  230.863) (/ pi 2)  (+ 3.9983 50.9749)) "")
									
								)
							)
							;Help3 image FIM
							
							
							
							;Help4 image
							(if (= contadorBolinhas 0)
								(progn
									(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
									(command "line" (polar (polar (polar (polar pontoInicialBolinhas (/ pi 2) (* distanciaEntreAsBolinhas contadorBolinhas)) 2.00188 127.5)  pi 1.72295)  (* (/ pi 2) 3)  230.863)    (polar  (polar (polar (polar (polar pontoInicialBolinhas (/ pi 2) (* distanciaEntreAsBolinhas contadorBolinhas)) 2.00188 127.5)  pi 1.72295)  (* (/ pi 2) 3)  230.863) (* (/ pi 2) 3)  (+ 3.9983 50.9749)) "")
									(command "line" (polar (polar (polar (polar pontoInicialBolinhas (/ pi 2) (* distanciaEntreAsBolinhas contadorBolinhas)) 1.13971 127.5)  0 1.72295)  (* (/ pi 2) 3)  230.863)    (polar  (polar (polar (polar (polar pontoInicialBolinhas (/ pi 2) (* distanciaEntreAsBolinhas contadorBolinhas)) 1.13971 127.5)  0 1.72295)  (* (/ pi 2) 3)  230.863) (* (/ pi 2) 3)  (+ 3.9983 50.9749)) "")
									
								)
							)
							;Help4 image FIM
							
							
						)
					)
					
					
					(setq contadorBolinhas (+ contadorBolinhas 1))
				)
				
				;Desenha Linha vermelha que corta a tubulação (VERTICAL)
				(command "layer" "m" "BF-08-SA-LCE" "c" "red" "" "")
				(command "line" pontoInterseccao (polar pontoInterseccao (/ pi 2) largura) "")
				
				
				
				(if (< (nth 0 (polar pontoInterseccao (/ pi 2) largura))(nth 0 pontoMeio))
					(progn
						;Inserção das intersecções superiores
						(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
						(command "insert" "C:\\bfdias\\blocos\\TE PLANTA.dwg" (polar pontoInterseccao (/ pi 2) largura) "" "" "")
						
						
						
					)
					(progn
						;Inserção das intersecções superiores 2
						(command "layer" "m" "interseccaoinv_layer2" "c" "white" "" "")
						(command "insert" "C:\\bfdias\\blocos\\TE PLANTA 2.dwg" (polar pontoInterseccao (/ pi 2) largura) "" "" "")
					)
				)
				
				
				;Início da TUBULAÇÃO Superior (Horizontal)
				(if (< (nth 0 (polar pontoInterseccao (/ pi 2) largura)) (nth 0 pontoMeio))
					(progn
						(setq p1Inicio (polar (polar pontoInterseccao (/ pi 2) largura) 0.351445 159.765))
						(setq p2Inicio (polar (polar pontoInterseccao (/ pi 2) largura) 5.93174 159.765))
					)
					(progn
						;(setq p1Inicio (polar (polar pontoInterseccao (/ pi 2) largura) 0.463648 122.984))
						(setq p1Inicio (polar (polar pontoInterseccao (/ pi 2) largura) 0.4600386611756409 123.8790434709063))
						;(setq p2Inicio (polar (polar pontoInterseccao (/ pi 2) largura) 5.81954 122.984))
						(setq p2Inicio (polar (polar pontoInterseccao (/ pi 2) largura) 5.823148770759927 123.8789130502216))
						
						
					)
				)
				
				(setq Destino (polar (polar pontoInterseccao (/ pi 2) largura) 0 (/ comprimento (- qtdColunas 1))))
				
				
				(if (< (nth 0 Destino) (nth 0 pontoMeio))
					(progn
						;(setq p1Fim (polar Destino 2.67795 122.984))
						(setq p1Fim (polar Destino 2.681555137226070 123.8789732006731))
						;(setq p2Fim (polar Destino 3.60524 122.984))
						(setq p2Fim (polar Destino 3.601630803094360 123.8790120638279))
						
					)
					(progn
						(setq p1Fim (polar Destino 2.79015 159.765))
						(setq p2Fim (polar Destino 3.49304 159.765))
					)
				)
				
				;(princ (strcat "\n111--> " (itoa contador2)))
				;(princ (strcat "\n222--> " (rtos qtdColunas 2 0)))
				
				;(alert (strcat (itoa (+ contador2 2)) "/" (rtos qtdColunas 2 0)))
				(if (= (itoa (+ contador2 2)) (rtos qtdColunas 2 0))
					(progn
						(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
						(command "line" p1Inicio PontoInsercaoTSD "")
						(command "line" p2Inicio (polar PontoInsercaoTSD (* (/ pi 2) 3) 110.0) "")
					)
					(progn
						(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
						(command "line" p1Inicio p1Fim "")
						(command "line" p2Inicio p2Fim "")
					)
				)
			)
		)
		
		
		
		;Início da TUBULAÇÃO Inferior (Horizontal)
		
		; se o ponto x do ponto de inserção for menor que o ponto do meio,
		;signigica que o objeto está do lado esquerdo do desenho, senão
		;o desenho estará do lado direito, e o cálculo para achar a outra estremidade do tubo é outra
		(if (< (nth 0 pontoInterseccao) (nth 0 pontoMeio))
			(progn
				(setq p1Inicio (polar pontoInterseccao 0.351445 159.765))
				(setq p2Inicio (polar pontoInterseccao 5.93174 159.765))
				
			)
			(progn
				;(setq p1Inicio (polar pontoInterseccao 0.463648 122.984))
				(setq p1Inicio (polar pontoInterseccao 0.4600375163605608 123.8789732003491))
				;(setq p2Inicio (polar pontoInterseccao 5.81954 122.984))
				(setq p2Inicio (polar pontoInterseccao 5.823147790815657 123.8789732005559))
			)
		)
		
		(setq Destino (polar InicioBase 0 (* (/ comprimento (- qtdColunas 1)) (+ contador2 1))))]
		; se o ponto x do ponto de inserção for menor que o ponto do meio,
		;signigica que o objeto está do lado esquerdo do desenho, senão
		;o desenho estará do lado direito, e o cálculo para achar a outra estremidade do tubo é outra
		(if (< (nth 0 Destino) (nth 0 pontoMeio))
			(progn
				;(setq p1Fim (polar Destino 2.67795 122.984))
				(setq p1Fim (polar Destino 2.681555137229440 123.8789732004013))
				;(setq p2Fim (polar Destino 3.60524 122.984))
				(setq p2Fim (polar Destino 3.601630803094568 123.8790120637758))
				
				
			)
			(progn
				(setq p1Fim (polar Destino 2.79015 159.765))
				(setq p2Fim (polar Destino 3.49304 159.765))
			)
		)
		(if (and (/= (+ contador2 2) qtdColunas) (< (+ contador2 1) qtdColunas) )
			(progn
				(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
				(command "line" p1Inicio p1Fim "")
				(command "line" p2Inicio p2Fim "")
				
			)
			(progn
				(if (< (+ contador2 1) qtdColunas)
					(progn
						(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
						(command "line" p1Inicio (polar p1Fim 0 4.999473022058198) "")
						(command "line" p2Inicio (polar p2Fim 0 4.999473022058198) "")
					)
				)
				
			)
		)
		
		
		
		(setq contador2 (+ contador2 1))
	)
	
	
	
	
	(setq contador1 0)
	(while (< contador1 qtd)
		;Iniciando a inserção das bolinhas...
		(command "layer" "m" "0" "c" "white" "" "")
		(command "insert" "C:\\bfdias\\blocos\\DIF.BLKS_bloco.dwg" (polar p1 (/ pi 2) (* distanciaEntreAsBolinhas contador1)) "" "" "")
		(setq objBolinha (entlast))
		(setq coord (cdr (assoc 10 (entget objBolinha))))
		
		
		;Instalação do bloco TIE (tubulação inferior esquerda)
		(if (= contador1 0)
			(progn
				
				(command "layer" "m" "tie_layer" "c" "white" "" "")
				(setq ponto1Temp (polar coord 4.22243 127.5))
				(setq ponto2Temp (polar ponto1Temp 5.36342 132.923))
				;(command "insert" "C:\\bfdias\\blocos\\TIE_bloco.dwg" ponto2Temp "" "" "")
				(command "insert" "C:\\bfdias\\blocos\\curva planta 2.dwg" (polar InicioBase 5.92064 155.081) "" "" "")
				
				
				;Tubulação TIE
				(setq p1Inicio (polar (polar InicioBase 5.92064 155.081) (/ pi 2) 110))
				(setq p2Inicio (polar InicioBase 5.92064 155.081))
				
				(setq Destino (polar InicioBase 0 (* (/ comprimento (- qtdColunas 1)) 1)))
				(setq p1Fim (polar Destino 2.67795 122.984))
				(setq p2Fim (polar Destino 3.60524 122.984))
				
				
				(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
				(command "line" p1Inicio p1Fim "")
				(command "line" p2Inicio p2Fim "")
				
				
			)
		)
		
		;Desenha tubulação
		(if (/= (+ contador1 1) qtd)
			(progn
				;Desenha tubulação
				(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")  ;<<<<<<<<<<<-----------------------------------   Faz tubulação da vertical
				
				;Próximo ponto
				(setq proxPonto (polar p1 (/ pi 2) (* distanciaEntreAsBolinhas (+ contador1 1))))  
				(command "line" (polar (polar coord 2.00188 127.5)  pi 1.72295) (polar  (polar proxPonto 4.2813 127.5)  pi 1.72295) "")
				(command "line" (polar (polar coord 1.13971 127.5)  0 1.72295)  (polar (polar proxPonto 5.14348 127.5)  0 1.72295) "")
				
				
				
				;Help1 inicio image
				(if (= contador1 (- qtd 2))
					(progn
						(command "line" (polar  (polar  (polar proxPonto 4.2813 127.5)  pi 1.72295) (/ pi 2)  230.863) (polar (polar  (polar  (polar proxPonto 4.2813 127.5)  pi 1.72295) (/ pi 2)  230.863)  (/ pi 2)  50.9749) "")
						(command "line" (polar  (polar (polar proxPonto 5.14348 127.5)  0 1.72295) (/ pi 2)  230.863) (polar (polar (polar (polar proxPonto 5.14348 127.5)  0 1.72295) (/ pi 2)  230.863)  (/ pi 2)  50.9749) "")
					)
				)
				;Help1 fim image
				
				
				;Help2 inicio image
				(if (= contador1 0)
					(progn
						(command "line" (polar  (polar (polar coord 2.00188 127.5)  pi 1.72295) (* (/ pi 2) 3)  230.863) (polar (polar  (polar (polar coord 2.00188 127.5)  pi 1.72295) (* (/ pi 2) 3)  230.863)  (* (/ pi 2) 3)  50.9749) "")
						(command "line" (polar  (polar (polar coord 1.13971 127.5)  0 1.72295) (* (/ pi 2) 3)  230.863) (polar (polar  (polar (polar coord 1.13971 127.5)  0 1.72295) (* (/ pi 2) 3)  230.863)  (* (/ pi 2) 3)  50.9749) "")
					)
				)
				;Help2 fim image
				
				
			)
			(progn
				;Instalação do bloco TSE (tubulação superior esquerda)...
				(command "layer" "m" "tse_layer" "c" "white" "" "")
				(command "insert" "C:\\bfdias\\blocos\\curva planta 1.dwg" (polar (polar (polar coord 2.09218 130.5) 5.45552 14.7799 ) (/ pi 2) 63.7240) "" "" "")
				
				
				;Insere tubulação superior direita
				(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
				
				(command "line" (polar (polar (polar (polar coord 2.09218 130.5) 5.45552 14.7799 ) (/ pi 2) 63.7240) 0.757128 275.174)  
				
				;(polar (polar (polar (polar (polar coord 2.09218 130.5) 5.45552 14.7799 ) (/ pi 2) 63.7240) 0.757128 275.174) 0 (- (/ comprimento (- qtdColunas 1)) 255.0002))  
				(polar (polar (polar (polar (polar coord 2.09218 130.5) 5.45552 14.7799 ) (/ pi 2) 63.7240) 0.757128 275.174) 0 (- (/ comprimento (- qtdColunas 1)) 256.001))  

				"")
				
				
				(command "line" (polar (polar (polar (polar (polar coord 2.09218 130.5) 5.45552 14.7799 ) (/ pi 2) 63.7240) 0.757128 275.174) (* (/ pi 2) 3) 109.9997)  
				
				(polar 
				
				;(polar (polar (polar (polar (polar coord 2.09218 130.5) 5.45552 14.7799 ) (/ pi 2) 63.7240) 0.757128 275.174) 0 (- (/ comprimento (- qtdColunas 1)) 255.0002)) 
				(polar (polar (polar (polar (polar coord 2.09218 130.5) 5.45552 14.7799 ) (/ pi 2) 63.7240) 0.757128 275.174) 0 (- (/ comprimento (- qtdColunas 1)) 256.001)) 
				
				(* (/ pi 2) 3) 110)  "")
				
				
				
				
				;(setq arq (polar (polar (polar (polar (polar coord 2.09218 130.5) 5.45552 14.7799 ) (/ pi 2) 63.7240) 0.757128 275.174) 0 (- (/ comprimento (- qtdColunas 1)) 255.0002)))
				
			)
		)
		(setq contador1 (+ contador1 1))
	)
	
	
	
	
	
	
	
	;Inserção do tubo de ar - fim
	(princ "\n======================")
	(princ "\n======================")
	(princ "\nO processo foi finalizado!")
	(princ)
)