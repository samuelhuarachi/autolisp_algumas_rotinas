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



(defun calcula_extreminadas(all)
	
	
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))
			(princ "\n--------------------------")
			(princ "\n--------------------------")
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq blocoName (strcase (cdr (assoc 2 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				
				
				(if (= blocoName "CURVA 1_BLOCO2")
					(progn
						(setq p1 coord)
					)
				)
				(if (= (strcase blocoName) "CURVA 2_BLOCO2")
					(progn
						(setq p2 coord)
					)
				)
				
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	(list p1 p2)
)

(defun pega_pontos_internos(all)
	
	(setq lista_pontos_internos nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))
			(princ "\n--------------------------")
			(princ "\n--------------------------")
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq blocoName (strcase (cdr (assoc 2 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(if (or (= blocoName "TE 1_BLOCO2")(= blocoName "TE 2_BLOCO2"))
					(progn
						(setq lista_pontos_internos (cons coord lista_pontos_internos))
					)
				)
				
				
				
				
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	lista_pontos_internos
)

(defun procura_coordenada_mais_prox(ponto)
	
	;lista_pontos_internos
	(setq qtdElem (vl-list-length lista_pontos_internos))
	(setq distanciaV 1000000)
	(setq pontoProc nil)
	(while (> qtdElem 0)
		
		(setq coordA (nth (- qtdElem 1) lista_pontos_internos))
		
		(setq distanciaAtual (distance ponto coordA))
		(if (< distanciaAtual distanciaV)
			(progn
				
				(setq pontoProc coordA)
				(setq distanciaV distanciaAtual)
			)
		)
		
		(setq qtdElem (- qtdElem 1))
	)
	
	pontoProc
)

(defun define_pontos_cortes(all)
	(setq lista_cortes nil)
	(setq conta 0)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))
			(princ "\n--------------------------")
			(princ "\n--------------------------")
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq blockName (strcase (cdr (assoc 2 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				
				(if (= layerName "BF-05A-SA-TXT")
					(progn
						
						(setq procuraPalavraColar (vl-string-search "COLAR" blockName))
						
						;Se existir a palavra colar, na etiqueta,
						;significa que estamos em um ponto de corte
						;e essa coordenada, será armazenada da lista!
						(if (/= procuraPalavraColar nil)
							(progn
								(setq conta (+ conta 1))
								(setq coordMaisProx (procura_coordenada_mais_prox coord))
								(setq x1 (rtos (car coordMaisProx) 2 3))
								(setq y1 (rtos (cadr coordMaisProx) 2 3))
								(setq lista_cortes (cons (list (strcat x1 y1) coordMaisProx) lista_cortes))
							)
						)
					)
				)
				
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	lista_cortes
)

(defun gerar_novo_desenho()
	
	(setq pontoMeio (polar (nth 0 pExtr) 0 (/ (distance (nth 0 pExtr)(nth 1 pExtr)) 2)))
	(setq paraBaixo (* (/ pi 2) 3))
	(setq paraCima (/ pi 2))
	
	;Insere primeira extremidade
	(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
	(command "insert" "C:\\bfdias\\blocos\\CURVA 1_bloco2.dwg" (polar (nth 0 pExtr) paraBaixo 1200) "" "" "")
	
	;Faz a primeira cota, bem pequena
	(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
	(setq pontoLinear2 (polar (polar (nth 0 pExtr) paraBaixo 1200) 0 55))
	(setq pontoLinear1 (polar (polar(nth 0 pExtr) paraBaixo 1200) pi 10))
	(command "dimlinear" pontoLinear2 pontoLinear1 (polar (sam_metade pontoLinear1 pontoLinear2) paraBaixo 337))
	
	
	
	;Iniciando caminhada pelos pontos internos!!!
	(setq ListapontosInternos (reverse ListapontosInternos))
	(setq tamanhoLista (vl-list-length ListapontosInternos))
	(setq metadeLista (/ tamanhoLista 2))
	
	(setq tamanhoListaSave (vl-list-length ListapontosInternos))
	(setq somar (+ 322 60))
	(setq vezes 0)
	(setq flagCortar 0)
	
	
	
	(setq coordFechamento1 (polar (polar (nth 0 pExtr) paraBaixo 1200) 2.408777551803688 13.45362404706884))
	(setq quantidadeIntersec 0)
	
	(while (> tamanhoLista 0)
	
		(setq coord1 (nth (- tamanhoLista 1) ListapontosInternos))
		
		
		(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
		(command "insert" "C:\\bfdias\\blocos\\TE 1_bloco2.dwg" (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) "" "" "")
		(setq blocoFormatoDeT (entlast))
		(setq quantidadeIntersec (+ quantidadeIntersec 1))
		
		;Desenha tubulação
		(setq pTub1 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes))  2.677949999999937 122.9839999999915))
		(setq pTub2 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes))  3.605240262582721 122.9837387654206))
		
		
		
		;Significa que estamos desenhando a primeira tubulação
		;Lembrando que estamos desenhando a tubulação da frente para trás (destino<----- inicio)
		(if (= tamanhoLista tamanhoListaSave)
			(progn
				
				(setq pTub1Destino1 (polar (polar (polar (nth 0 pExtr)paraBaixo 1200) 0 (* somar vezes)) 0.7571280619864615 275.1744900967334))
				(setq pTub1Destino2 (polar pTub1Destino1 paraBaixo 110))
				
				
				(command "layer" "m" "tubulacao1" "c" "30" "" "")
				(command "line" pTub1 pTub1Destino1 "")
				(setq tubulacao1 (entlast))
				(command "line" pTub2 pTub1Destino2 "")
				(setq tubulacao2 (entlast))
				
				;Inserir cotações
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(setq ponto2323213 (polar (polar (nth 0 pExtr)paraBaixo 1200) 0 (* somar vezes)))
				(setq ponto2323213 (polar ponto2323213 0 50.0004))
				
				(command "dimlinear" ponto2323213  (polar(polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130) (polar (polar ponto2323213 0  (/(distance ponto2323213 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130)   )2) ) (*(/ pi 2)3) 337))
				
			)
			(progn
				(setq pontoAnterior (polar (polar (nth tamanhoLista ListapontosInternos) paraBaixo 1200) 0 (* somar vezes)))
				
				(if (= flagCortar 0)
					(progn
						(setq pTub1Destino1 (polar pontoAnterior 0.3514450000000195 159.7649999999971))
						(setq pTub1Destino2 (polar pontoAnterior 5.931740513170166 159.7654530854054))
					)
					(progn
						(setq pTub1Destino1 (polar (polar pontoAnterior 0.3514450000000195 159.7649999999971) pi 50))
						(setq pTub1Destino2 (polar(polar pontoAnterior 5.931740513170166 159.7654530854054)pi 50))
						
						;Fechar a tubulação cortada
						(command "layer" "m" "tubulacao1" "c" "30" "" "")
						(command "line" pTub1Destino1 pTub1Destino2 "")
						
						;coordFechamento1
						(setq coordFechamento2 (polar pontoAnterior pi 232))
						
						(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
						
						(command "dimlinear" coordFechamento1 coordFechamento2 (polar (polar coordFechamento1 0 (/ (distance coordFechamento1 coordFechamento2) 2)) paraBaixo 750)  )
					 	
						(setq coordFechamento1 (polar pontoAnterior 0 99.9998))
						(setq quantidadeIntersec 0)
					)
				)
				(command "layer" "m" "tubulacao1" "c" "30" "" "")
				(command "line" pTub1 pTub1Destino1 "")
				(setq tubulacao1 (entlast))
				(command "line" pTub2 pTub1Destino2 "")
				(setq tubulacao2 (entlast))
				
				;inserir contações
				
				;(setq pontoMeio (polar pontoMeio 0 (* somar vezes)))
				(if (= flagCortar 0 )
					(progn
						
						(setq cProcM1 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130))
						(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
						(setq cProcM2 (polar pontoAnterior paraBaixo 130))
						(command "dimlinear" (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130)  (polar pontoAnterior paraBaixo 130) (polar (polar (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130) 0  (/(distance (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130) (polar pontoAnterior paraBaixo 130))2) ) (*(/ pi 2)3) 337))
						(setq ultimaCota (entlast))
						
					)
					(progn
						
						(setq cProcM1 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130))
						(setq cProcM2 pTub1Destino2)
						(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
						(command "dimlinear" (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130)  pTub1Destino2 (polar (polar (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130) 0  (/(distance (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130) pTub1Destino2)2) ) (*(/ pi 2)3) 337))
						(setq ultimaCota (entlast))
						
					)
				)
				(setq flagCortar 0)
				
			)
		)
		
		
		
		
		;Nessa parte do código vamos verificar se existe ponto de corte
		;Caso houver ponto de corte
		;vamos andar um pouco mais para frente, e fazer as cotações, e o corte(colar)
		;propriamente dito.
		(setq x1 (rtos (car coord1) 2 3))
		(setq y1 (rtos (cadr coord1) 2 3))
		(setq chave (strcat x1 y1))
		(setq procura (assoc chave ListapontosCortes))
		(if (/= procura nil)
			(progn
				(setq flagCortar 1)
				(setq vezes (+ vezes 1))
				
				
				
			)
		)
		
		
		;vamos abortar!
		;Nessa última parte do desenho, estamos bem no meio,
		;e podemos inserir a entrada de ar correspondente
		(if (< tamanhoLista (+ metadeLista 1))
			(progn
				
				(setq tamanhoListaBackup tamanhoLista)
				(setq tamanhoLista -100)
				;Apagamos o último blocoformatoT inserido
				(command "erase" blocoFormatoDeT "")
				
				
				;Inserimos o novo bloco T
				(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
				(command "insert" "C:\\bfdias\\blocos\\TE 2_bloco2.dwg" (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) "" "" "")
				
				;Agora vamos configurar a tubulação
				;Apagamos as últimas
				(command "erase" tubulacao1 "")
				(command "erase" tubulacao2 "")
				
				;Os 2 pontos destinos serão os mesmos
				;Vamos só configurar os pontos de origem,
				(setq pTub1 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) 2.790147859585483 159.7654530851060))
				(setq pTub2 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) 3.493037447599233 159.7654530854066))
				
				(command "layer" "m" "tubulacao1" "c" "30" "" "")
				(command "line" pTub1 pTub1Destino1 "")
				(command "line" pTub2 pTub1Destino2 "")
				
				
				(insere_entrada_ar_ae_fixa1 (polar (polar pTub1Destino2 0 (/ (distance pTub2 pTub1Destino2) 2)) paraCima (/ 109.9999 2)) tipoBloco123)
				
				;Ajusta UltimaCota
				(command "erase" ultimaCota "")
				
				;Faz as cotas que ficam na entrada de ar,
				;ela são dividas em 2
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" cProcM1 (sam_metade cProcM1 cProcM2) (polar (sam_metade cProcM1 (sam_metade cProcM1 cProcM2)) paraBaixo 337))
				(command "dimlinear" cProcM2 (sam_metade cProcM1 cProcM2) (polar (sam_metade cProcM1 (sam_metade cProcM1 cProcM2)) paraBaixo 337))
				
				;(command "dimlinear" (polar pontoAnterior paraBaixo 130) (sam_metade (polar pontoAnterior paraBaixo 130) cProcM1) (polar (sam_metade (polar pontoAnterior paraBaixo 130) (sam_metade (polar pontoAnterior paraBaixo 130) cProcM1)) paraBaixo 337))
				
				;(command "circle" (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) 20)
				
				(setq ponto1 (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)))
				(setq proxPonto (polar (polar (nth (- tamanhoListaBackup 2) ListapontosInternos)paraBaixo 1200) 0 (* somar vezes))  )
				(setq pTub1Destino1 (polar (polar ponto1 0.3514450000000195 159.7649999999971) pi 40))
				(setq pTub1Destino2 (polar (polar ponto1 5.931740513170166 159.7654530854054) pi 40))
				
				
				;Fazendo a parte final do desenho
				(setq tamanhoListaBackup (- tamanhoListaBackup 1))
				(while (> quantidadeIntersec 1)
					
					
					(setq coord1 (nth (- tamanhoListaBackup 1) ListapontosInternos))
					(setq coordAnterior (nth tamanhoListaBackup ListapontosInternos))
					(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
					(command "insert" "C:\\bfdias\\blocos\\TE 2_bloco2.dwg" (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) "" "" "")
					(setq blocoFormatoDeT (entlast))
					
					(setq coordOrigem1 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) 2.790147859585472 159.7654530851067))
					(setq coordDestino1 (polar (polar (polar coordAnterior paraBaixo 1200) 0 (* somar vezes)) 0.4636475733356884 122.9837365710983))
					
					(setq coordOrigem2 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) 3.493037447599223 159.7654530854060))
					(setq coordDestino2 (polar (polar (polar coordAnterior paraBaixo 1200) 0 (* somar vezes)) 5.819537698181227 122.9837387640625))
					
					
					(command "layer" "m" "tubulacao1" "c" "30" "" "")
					(command "line" coordOrigem1 coordDestino1 "")
					(command "line" coordOrigem2 coordDestino2 "")
					
					
					(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
					(command "dimlinear" (polar (polar (polar coordAnterior paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130) (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes))paraBaixo 130) (polar (sam_metade (polar (polar coordAnterior paraBaixo 1200) 0 (* somar vezes)) (sam_metade (polar (polar coordAnterior paraBaixo 1200) 0 (* somar vezes)) (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)))) paraBaixo 467))
					
				
				
					(setq tamanhoListaBackup (- tamanhoListaBackup 1))
					(setq quantidadeIntersec (- quantidadeIntersec 1))
				)
				
				;Desenha ultimo tubo
				(setq coord1 (nth (- tamanhoListaBackup 1) ListapontosInternos))
				(setq coordAnterior (nth tamanhoListaBackup ListapontosInternos))
				
				(setq coordOrigem1 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) 2.790147859585472 159.7654530851067))
				(setq coordDestino1 (polar (polar (polar coordAnterior paraBaixo 1200) 0 (* somar vezes)) 0.4636475733356884 122.9837365710983))
				
				(setq coordOrigem2 (polar (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) 3.493037447599223 159.7654530854060))
				(setq coordDestino2 (polar (polar (polar coordAnterior paraBaixo 1200) 0 (* somar vezes)) 5.819537698181227 122.9837387640625))
				
				
				(command "layer" "m" "tubulacao1" "c" "30" "" "")
				(command "line" (polar coordOrigem1 0 50) coordDestino1 "")
				(command "line" (polar coordOrigem2 0 50) coordDestino2 "")
				(command "line" (polar coordOrigem1 0 50) (polar coordOrigem2 0 50) "")
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" (polar (polar (polar coordAnterior paraBaixo 1200) 0 (* somar vezes)) paraBaixo 130) (polar coordOrigem2 0 50) (polar (sam_metade (polar (polar coordAnterior paraBaixo 1200) 0 (* somar vezes)) (sam_metade (polar (polar coordAnterior paraBaixo 1200) 0 (* somar vezes)) (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)))) paraBaixo 467))
				
				(command "dimlinear" coordFechamento1 (polar (polar coordOrigem2 0 50)paraCima 55) (polar (sam_metade coordFechamento1 (polar (polar coordOrigem2 0 50)paraCima 55)) paraBaixo 750))
				;(command "circle" coordFechamento1 20)
				;(command "circle" (polar (polar coordOrigem2 0 50)paraCima 55) 20)
				
				
				;(command "circle" pTub1Destino1 20)
				;(command "zoom" "c" proxPonto 10)
				;coordFechamento1
			)
		)
		
		;Decrementa
		(setq tamanhoLista (- tamanhoLista 1))
	)
	
)

(defun define_o_tipo_bloco(all)
	
	(setq tipo22 "1")
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))
			(princ "\n--------------------------")
			(princ "\n--------------------------")
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq blocoName (strcase (cdr (assoc 2 (entget obj)))))
				
				(if (= blocoName "DESCIDA 60X100 PLANTA_BLOCO")
					(progn
						(setq tipo22 "1")
						
					)
				)
				(if (= blocoName "DESCIDA 100 PLANTA_BLOCO")
					(progn
						(setq tipo22 "2")
					)
				)
				(if (= blocoName "DESCIDA 150X100 PLANTA_BLOCO")
					(progn
						(setq tipo22 "3")
					)
				)
				
				
				;(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	tipo22
)


;Manda uma lista do tipo ((list 0 0 0)(list 1 1 1))
(defun faz_circulo_ponto(lst)
	(setq quantidadeElem (vl-list-length lst) )
	(while (> quantidadeElem 0 )
		
		(setq coord (nth (- quantidadeElem 1) lst))
		(command "layer" "m" "layer_circulo_teste" "c" "cyan" "" "")
		(command "circle" coord 10)
		
		(setq quantidadeElem (- quantidadeElem 1))
	)
)


(defun pontos_pra_ca(all posea)
	
	(setq Xea (nth 0 posea))
	
	(setq lista_pontos_praCa nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq blocoName (strcase (cdr (assoc 2 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(if (and (or (= blocoName "TE 1_BLOCO2")(= blocoName "TE 2_BLOCO2")) (< (nth 0 coord) Xea))
					(progn
						(setq lista_pontos_praCa (cons coord lista_pontos_praCa))
					)
				)
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
	lista_pontos_praCa
)

(defun pontos_pra_la(all posea)
	
	(setq Xea (nth 0 posea))
	
	(setq lista_pontos_praLa nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq blocoName (strcase (cdr (assoc 2 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(if (and (or (= blocoName "TE 1_BLOCO2")(= blocoName "TE 2_BLOCO2")) (> (nth 0 coord) Xea))
					(progn
						(setq lista_pontos_praLa (cons coord lista_pontos_praLa))
					)
				)
				
				(setq qtd (- qtd 1))
			)
		)
	)
	
	lista_pontos_praLa
)

(defun posicaoEntradaAr(all)
	(setq posEA nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq blocoName (strcase (cdr (assoc 2 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				;aefixo_entar12
				(if (= blocoName "AEFIXO_ENTAR12")
					(progn
						(setq posEA coord)
					)
				)
				
				(setq qtd (- qtd 1))
			)
		)
	)
	posEA
)


(defun desenhaCanoPraCa(pAtual pProx Corte)
	(setq p1 (polar pAtual 0 149.9999999984284))
	(setq p1 (polar p1 sam_paracima 55))
	(setq p2 (polar (polar pAtual 0 149.9999999984284) sam_parabaixo 55))
	
	
	(setq pp1 (polar pProx pi 110.0000000015134))
	(setq pp2 (polar pp1 sam_paracima 55))
	(setq pp3 (polar pp1 sam_parabaixo 55))
	
	(if (= Corte 1)
		(progn
			
			(setq pp2 (polar pp2 0 50.00050620327238))
			(setq pp3 (polar pp3 0 50.00050620327238))
			(sam_Anim2 pp2 pp3)
			(command "line" pp2 pp3 "")
		)
	)
	
	
	
	(sam_Anim2 p1 pp2)
	(command "line" p1 pp2 "")
	(sam_Anim2 p2 pp3)
	(command "line" p2 pp3 "")
)

(defun sam_Anim2(ponto1 ponto2 / distancia1 angulo1)
	
	(setq distancia1 (distance ponto1 ponto2))
	(setq angulo1 (angle ponto1 ponto2))
	
	(setq distCurrent 0)
	(while (< distCurrent distancia1)
		
		(command "line" ponto1 (polar ponto1 angulo1 distCurrent) "")
		
		(setq obj1 (entlast))
		(command "erase" obj1 "")
		
		
		(setq distCurrent (+ distCurrent 1))
	)
)


(defun desenhaCanoPraLa(pAnterior pAtual Corte)
	(setq p1 (polar pAnterior 0 110.0000000015134))
	(setq p1 (polar p1 sam_paracima 55))
	(setq p2 (polar (polar pAnterior 0 110.0000000015134) sam_parabaixo 55))
	
	
	(setq pp1 (polar pAtual pi 149.9999999984866))
	(setq pp2 (polar pp1 sam_paracima 55))
	(setq pp3 (polar pp1 sam_parabaixo 55))
	
	
	(if (= Corte 1)
		(progn
			;(setq p1 (polar p1 0 50.00050620327238))
			;(setq p2 (polar p2 0 50.00050620327238))
			
			(setq p1 (polar p1 0 (- 300 50.00050620327238)))
			(setq p2 (polar p2 0 (- 300 50.00050620327238)))
			
			(sam_Anim2 p1 p2)
			(command "line" p1 p2 "")
		)
	)
	
	(sam_Anim2 p1 pp2)
	(command "line" p1 pp2 "")
	(sam_Anim2 p2 pp3)
	(command "line" p2 pp3 "")
)



(defun desenhaCanoPraCaEntradaAr(pAtual pProx Corte)
	(setq p1 (polar pAtual 0 149.9999999984284))
	(setq p1 (polar p1 sam_paracima 55))
	(setq p2 (polar (polar pAtual 0 149.9999999984284) sam_parabaixo 55))
	
	
	(setq pp1 (polar pProx pi 140.0000000001164))
	(setq pp2 (polar pp1 sam_paracima 55))
	(setq pp3 (polar pp1 sam_parabaixo 55))
	
	(if (= Corte 1)
		(progn
			(setq pp2 (polar pp2 0 50.00050620327238))
			(setq pp3 (polar pp3 0 50.00050620327238))
			(sam_Anim2 pp2 pp3)
			(command "line" pp2 pp3 "")
		)
	)
	
	(sam_Anim2 p1 pp2)
	(command "line" p1 pp2 "")
	(sam_Anim2 p2 pp3)
	(command "line" p2 pp3 "")
)



(defun sam_animacao1 (ponto1 ponto2 pathBloconame)
	
	(setq distancia1 (distance ponto1 ponto2))
	(setq angulo1 (angle ponto1 ponto2))
	
	(setq distCurrent 1)
	
	(while (< distCurrent distancia1)
		
		(command "insert" pathBloconame (polar ponto1 angulo1  distCurrent) "" "" "")
	
		(setq obj1 (entlast))
		(command "erase" obj1 "")
		
		
		(setq distCurrent (+ distCurrent 3))
	)
)

(defun parte1()
	(setq ListapontosInternos (pega_pontos_internos estrutura1))
	(setq ListapontosInternos (samuelBubbleSort ListapontosInternos))
	;A funcao define pontos cortes, precisa da lista de pontos internos atualizada
	;Ou seja, devemos executar a funcao pega pontos internos, e depois organizar essa lista
	;com a funcao samuelBubbleSort
	(setq ListapontosCortes (define_pontos_cortes estrutura1))
	
	
	(setq ponto1 (nth 0 pExtr))
	(setq inicio (polar ponto1 1.213659761190833 143.0247432903805))
	
	(setq angulo2 (angle inicio pontoInsercao))
	(setq distancia2 (distance inicio pontoInsercao))
	
	
	(sam_animacao1 inicio (polar pontoInsercao (+ pi 1.213659761190833) 143.0247432903805 ) "C:\\bfdias\\blocos\\CURVA 1_bloco2.dwg")
	;CURVA 1_bloco2
	(command "insert" (strcat "C:\\bfdias\\blocos\\CURVA 1_bloco2.dwg") (polar pontoInsercao (+ pi 1.213659761190833) 143.0247432903805 ) "" "" "")
	
	;(command "circle" pontoInsercao 22)
	;(command "circle" (list 456617.0 181469.0 0.0) 22)
	
	(setq pontoCortesFound 0)
	(setq qtd (vl-list-length listaPostaPraCa))
	
	
	(command "layer" "m" "tubulacao1" "c" "30" "" "")
	(desenhaCanoPraCa (polar (polar ponto1 angulo2 distancia2) 1.213662621988349 143.0245906164936) (polar (nth (- qtd 1) listaPostaPraCa) angulo2  distancia2  ) 0)
	
	(while (> qtd 0)
		(setq coord (polar (nth (- qtd 1) listaPostaPraCa) angulo2  distancia2  )  )
		
		;(command "circle" coord 10)
		
		(sam_animacao1 (nth (- qtd 1) listaPostaPraCa) (polar coord 0 (* 300 pontoCortesFound)) "C:\\bfdias\\blocos\\TE 1_bloco2.dwg")
		(command "insert" (strcat "C:\\bfdias\\blocos\\TE 1_bloco2.dwg") (polar coord 0 (* 300 pontoCortesFound)) "" "" "")
		
		;(command "circle" (polar coord (+ pi angulo2) distancia2) 33)
		
		(setq pontoAtual (polar coord 0 (* 300 pontoCortesFound)))
		(setq proxPonto nil)
		
		(if (> qtd 1)
			(progn
				(if (/= (nth (- qtd 2) listaPostaPraCa) nil)
					(progn
						(setq proxPonto (polar (polar (nth (- qtd 2) listaPostaPraCa) angulo2  distancia2) 0 (* 300 pontoCortesFound)))
					)
				)
			)
		)
		
		
		(setq souCorte 0) ;Gera a variável que verifica se somos ponto de corte
		
		;Verifica se é ponto de corte
		(setq resposta (assoc (strcat (rtos (car   (polar coord (+ pi angulo2) distancia2)  ) 2 3)(rtos (cadr (polar coord (+ pi angulo2) distancia2)) 2 3)) ListapontosCortes))
		(if (/= resposta nil)
			(progn
				(setq pontoCortesFound (+ pontoCortesFound 1))
				(setq souCorte 1)
			)
		)
		
		
		
		(if (/= proxPonto nil)
			(progn
				(desenhaCanoPraCa pontoAtual proxPonto souCorte)
			)
		)
		
		;(getstring "samuel...")
		
		(setq qtd (- qtd 1))
	)
	
	;entradaAr
	(setq coord (polar (polar entradaAr angulo2  distancia2 ) 0 (* 300 pontoCortesFound)) )
	
	(sam_animacao1 entradaAr coord "C:\\bfdias\\blocos\\aefixo_entar12.dwg")
	(command "insert" (strcat "C:\\bfdias\\blocos\\aefixo_entar12.dwg") coord "" "" "")
	
	(if (= souCorte 1)
		(progn
			(setq coord (polar (polar entradaAr angulo2  distancia2 ) 0 (* 300 (- pontoCortesFound 1))) )
		)
	)
	
	(desenhaCanoPraCaEntradaAr pontoAtual coord souCorte)
	
	
	;Get entrada de ar point 
	(if (= souCorte 1)
		(progn
			(setq pontoInsercaoEA (polar coord 0 300))
		)
		(progn
			(setq pontoInsercaoEA coord)
		)
	)
	
	;Realizando a conversão dos pontos pra la
	(setq pontoAnterior (polar pontoInsercaoEA 0 29.99999999842840))
	(setq qtd (vl-list-length listaPostaPraLa))
	(while (> qtd 0)
		(setq coord (polar (nth (- qtd 1) listaPostaPraLa) angulo2  distancia2  )  )
		
		
		;Verifica se é ponto de corte
		(setq souCorte 0) ;Gera a variável que verifica se somos ponto de corte
		(setq resposta (assoc (strcat (rtos (car   (polar coord (+ pi angulo2) distancia2)  ) 2 3)(rtos (cadr (polar coord (+ pi angulo2) distancia2)) 2 3)) ListapontosCortes))
		(if (/= resposta nil)
			(progn
				(setq pontoCortesFound (+ pontoCortesFound 1))
				(setq souCorte 1)
			)
		)
		
		(sam_animacao1 (nth (- qtd 1) listaPostaPraLa) (polar coord 0 (* 300 pontoCortesFound)) "C:\\bfdias\\blocos\\TE 2_bloco2.dwg" )
		(command "insert" (strcat "C:\\bfdias\\blocos\\TE 2_bloco2.dwg") (polar coord 0 (* 300 pontoCortesFound)) "" "" "")
		
		;(command "circle" (polar coord (+ pi angulo2) distancia2) 33)
		
		(setq pontoAtual (polar coord 0 (* 300 pontoCortesFound)))
		
		
		(setq proxPonto nil)
		(if (> qtd 1)
			(progn
				(if (/= (nth (- qtd 2) listaPostaPraLa) nil)
					(progn
						(setq proxPonto (polar (polar (nth (- qtd 2) listaPostaPraLa) angulo2  distancia2) 0 (* 300 pontoCortesFound)))
					)
				)
			)
		)
		
		(desenhaCanoPraLa pontoAnterior pontoAtual souCorte)
		
		(setq pontoAnterior pontoAtual)
		
		(setq qtd (- qtd 1))
	)
	
	
	(sam_animacao1 (nth 1 pExtr) (polar (polar (nth 1 pExtr) angulo2 distancia2)0 (* 300 pontoCortesFound)) "C:\\bfdias\\blocos\\CURVA 2_bloco2.dwg" )
	(command "insert" (strcat "C:\\bfdias\\blocos\\CURVA 2_bloco2.dwg") (polar (polar (nth 1 pExtr) angulo2 distancia2)0 (* 300 pontoCortesFound)) "" "" "")
	
	(desenhaCanoPraLa pontoAnterior (polar (polar (polar (nth 1 pExtr) angulo2 distancia2)0 (* 300 pontoCortesFound)) 5.931743316535905 159.7652888632005) 0)
	
	
)

(defun c:ac11()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq estrutura1 (ssget '((-4 . "<OR") (2 . "aefixo_entar12") (8 . "BF-05A-SA-TXT")(2 . "CURVA 2_bloco2")(2 . "TE 2_bloco2") (2 . "CURVA 1_bloco2")(2 . "TE 1_bloco2")(-4 . "OR>"))))
	
	(setq pExtr (calcula_extreminadas estrutura1))
	(setq entradaAr (posicaoEntradaAr estrutura1))
	(setq listaPostaPraCa(pontos_pra_ca estrutura1 entradaAr))
	(setq listaPostaPraLa(pontos_pra_la estrutura1 entradaAr))
	
	(setq qtd (vl-list-length listaPostaPraLa))
	(while (> qtd 0)
		(setq coord (nth (- qtd 1) listaPostaPraLa))
		;(command "layer" "m" "samuel2" "c" "yellow" "" "")
		;(command "circle" coord 10)
		(setq qtd (- qtd 1))
	)
	
	(setq qtd (vl-list-length listaPostaPraCa))
	(while (> qtd 0)
		(setq coord (nth (- qtd 1) listaPostaPraCa))
		;(command "layer" "m" "ronaldo" "c" "cyan" "" "")
		;(command "circle" coord 10)
		(setq qtd (- qtd 1))
	)
	
	(setq pontoInsercao (getpoint "\nDefina o ponto de inserção: "))
	(parte1) ;desenha pra CA, incluindo a entrada de ar
	
	
	(princ "\n*************************")
	(princ "\n*************************")
	(princ "\n*************************")
	(princ "\n*************************")
	(princ "\nB&F Dias - A lisp aefixa_corte11 foi executada")
	(princ "\n*************************")
	
	(princ)
)







