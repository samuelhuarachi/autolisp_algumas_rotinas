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
	(command "insert" "C:\\bfdias\\blocos\\CURVA 1_bloco.dwg" (polar (nth 0 pExtr) paraBaixo 1200) "" "" "")
	
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
		(command "insert" "C:\\bfdias\\blocos\\TE 1_bloco.dwg" (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) "" "" "")
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
				(command "insert" "C:\\bfdias\\blocos\\TE 2_bloco.dwg" (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) "" "" "")
				
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
				
				
				;(insere_entrada_ar_ae_fixa1 (polar (polar pTub1Destino2 0 (/ (distance pTub2 pTub1Destino2) 2)) paraCima (/ 109.9999 2)) tipoBloco123)
				
				
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
					(command "insert" "C:\\bfdias\\blocos\\TE 2_bloco.dwg" (polar (polar coord1 paraBaixo 1200) 0 (* somar vezes)) "" "" "")
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

(defun gerar_novo_desenho2()
	(setq paraBaixo (* (/ pi 2) 3))
	(setq paraCima (/ pi 2))
	
	
	;Iniciando caminhada pelos pontos internos!!!
	(setq ListapontosInternos (reverse ListapontosInternos))
	(setq tamanhoLista (vl-list-length ListapontosInternos))
	
	;Insere primeira extremidade e faz tubulacao
	;(nth 0 pExtr)
	(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
	(command "insert" "C:\\bfdias\\blocos\\CURVA 1_bloco2.dwg" (polar (nth 0 pExtr) paraBaixo 1000) "" "" "")
	
	(setq p1 (polar (polar (nth 0 pExtr) paraBaixo 1000) 0.7571280619864615 275.1744900967334))
	(setq p2 (polar (polar (nth 0 pExtr) paraBaixo 1000) 0.3761886146527706 215.0372060830378))
	
	(setq primeiroPontoMeio (polar (nth (- tamanhoLista 1) ListapontosInternos) paraBaixo 1000))
	(setq p1Dest (polar primeiroPontoMeio 2.677949999999960 122.9839999999900))
	(setq p2Dest (polar primeiroPontoMeio 3.605240262582741 122.9837387654218))
	
	(command "layer" "m" "tubulacao1" "c" "30" "" "")
	(command "line" p1 p1Dest "")
	(command "line" p2 p2Dest "")
	
	
	;Pontos de orientacao para fazer as 
	;cotas
	(setq ponto1Bloco (polar (polar (nth 0 pExtr) paraBaixo 1000) 1.213659761191033 143.0247432903737 ))
	(setq pontoBlocoAnterior ponto1Bloco)
	
	(setq ponto1BlocoFechamento (polar ponto1Bloco 4.264866556046900 138.6545479828249))
	
	
	(setq andarDistancia 142)
	(setq vezes 0)
	(while (> tamanhoLista 0)
		(setq pontoMeio (sam_metade (nth 0 pExtr) (nth 1 pExtr)))
		(setq coord1 (nth (- tamanhoLista 1) ListapontosInternos))
		(setq x1 (rtos (car coord1) 2 3))
		(setq y1 (rtos (cadr coord1) 2 3))
		(setq chave (strcat x1 y1))
		(setq procura (assoc chave ListapontosCortes))
		
		
		(setq coord1 (polar coord1 paraBaixo 1000))
		
		;Antes do meio
		(if (and (< (nth 0 coord1) (nth 0 pontoMeio)) )
			(progn
				(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
				(command "insert" "C:\\bfdias\\blocos\\TE 1_bloco2.dwg" (polar coord1 0 (* andarDistancia vezes)) "" "" "")
				
				(setq pontoMeio (polar pontoMeio 0 (* andarDistancia vezes)))
				;(command "circle" pontoMeio 30)
				;Se procura for diferente de nil
				;significa que aqui temos um ponto de corte =D
				(if (/= procura nil)
					(progn
						(setq vezes (+ vezes 1))
						
						(setq proxPonto (polar(nth (- tamanhoLista 2) ListapontosInternos)paraBaixo 1000))
						(setq proxPonto (polar proxPonto 0 (* andarDistancia vezes)))
						
						(if (and (< (nth 0 proxPonto) (nth 0 pontoMeio)) )
							(progn
								(setq p1 (polar proxPonto 2.677949999999964 122.9839999999898))
								(setq p2 (polar proxPonto 3.605240262588165 122.9837387640633))
							)
							(progn
								(setq p1 (polar proxPonto 2.790149999999994 159.7649999999972))
								(setq p2 (polar proxPonto 3.493037447600107 159.7654530853958))
							)
						)
						
						(setq p1Dest (polar (polar (polar coord1 0 (* andarDistancia (- vezes 1))) 0.3514447940043099 159.7654530851060) 0 (- 142 50)))
						(setq p2Dest (polar (polar (polar coord1 0 (* andarDistancia (- vezes 1))) 5.931740513170155 159.7654530854060) 0 (- 142 50)))
						
						(command "layer" "m" "tubulacao1" "c" "30" "" "")
						(command "line" p1 p1Dest "")
						(command "line" p2 p2Dest "")
						(command "line" p1Dest p2Dest "")
						
						
						;Insercao das cotas
						(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
						(command "dimlinear" pontoBlocoAnterior (polar coord1 0 (* andarDistancia (- vezes 1)))  (polar (sam_metade pontoBlocoAnterior (polar coord1 0 (* andarDistancia (- vezes 1)))) paraBaixo 500) )
						(setq pontoBlocoAnterior (sam_metade p1Dest p2Dest))
						
						
						(setq ponto2BlocoFechamento (polar (polar coord1 0 (* andarDistancia (- vezes 1))) 0 150))
						(command "dimlinear"  ponto1BlocoFechamento ponto2BlocoFechamento (polar (sam_metade ponto1BlocoFechamento ponto2BlocoFechamento) paraBaixo 1000 )  )
						;ponto1BlocoFechamento
						
						(setq ponto1BlocoFechamento (sam_metade p1Dest p2Dest))
						
						
					)
					;Senao procura = nil, nao ocorreu o corte por enquanto... =D
					(progn
						;Desenha tutubulação
						(setq p1 (polar (polar coord1 0 (* andarDistancia vezes)) 0.3514447940043099 159.7654530851060))
						(setq p2 (polar (polar coord1 0 (* andarDistancia vezes)) 5.931740513170155 159.7654530854060))
						
						(setq proxPonto (polar(nth (- tamanhoLista 2) ListapontosInternos)paraBaixo 1000))
						(if (and (< (nth 0 proxPonto) (nth 0 pontoMeio)) )
							(progn
								(setq p1Dest (polar (polar proxPonto 0 (* andarDistancia vezes)) 2.677949999999964 122.9839999999898))
								(setq p2Dest (polar (polar proxPonto 0 (* andarDistancia vezes)) 3.605240262582741 122.9837387654218))
								
							)
							(progn
								(setq p1Dest (polar (polar proxPonto 0 (* andarDistancia vezes)) 2.790149999999994 159.7649999999972))
								(setq p2Dest (polar (polar proxPonto 0 (* andarDistancia vezes)) 3.493037447600107 159.7654530853958))
								
							)
						)
						
						
						(command "layer" "m" "tubulacao1" "c" "30" "" "")
						(command "line" p1 p1Dest "")
						(command "line" p2 p2Dest "")
						(command "line" p2 p2Dest "")
						
						;Insercao das cotas
						(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
						(command "dimlinear" pontoBlocoAnterior (polar coord1 0 (* andarDistancia vezes))  (polar (sam_metade pontoBlocoAnterior (polar coord1 0 (* andarDistancia vezes))) paraBaixo 500) )
						(setq pontoBlocoAnterior (polar coord1 0 (* andarDistancia vezes)))
						
					)
				)
			)
			;Depois do meio
			(progn
				(setq pontoMeio (polar pontoMeio 0 (* andarDistancia vezes)))
				(if (/= procura nil)
					(progn
						(setq vezes (+ vezes 1))
						
					)
				)
				
				(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
				(command "insert" "C:\\bfdias\\blocos\\TE 2_bloco2.dwg" (polar coord1 0 (* andarDistancia vezes)) "" "" "")
				
				(setq pontoAnterior (nth tamanhoLista ListapontosInternos))
				(setq pontoAnterior (polar pontoAnterior paraBaixo 1000))
				(setq pontoAnterior (polar pontoAnterior 0 (* andarDistancia vezes)))
				
				
				(if (= procura nil)
					(progn
						;Inserção das cotas
						(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
						(command "dimlinear" pontoBlocoAnterior (polar coord1 0 (* andarDistancia vezes))  (polar (sam_metade pontoBlocoAnterior (polar coord1 0 (* andarDistancia vezes))) paraBaixo 500) )
						(setq pontoBlocoAnterior (polar coord1 0 (* andarDistancia vezes)))
						
					)
					(progn
						;Inserção das cotas
						(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
						(command "dimlinear" pontoBlocoAnterior (polar (polar coord1 0 (* andarDistancia vezes)) pi 242)  (polar (sam_metade pontoBlocoAnterior (polar(polar coord1 0 (* andarDistancia vezes))pi 242) ) paraBaixo 500) )
						(setq pontoBlocoAnterior (polar coord1 0 (* andarDistancia vezes)))
						
						;Inserção das cotas - BLOCO
						(setq ponto2BlocoFechamento (polar (polar coord1 0 (* andarDistancia vezes)) pi 242))
						(command "dimlinear"  ponto1BlocoFechamento ponto2BlocoFechamento (polar (sam_metade ponto1BlocoFechamento ponto2BlocoFechamento) paraBaixo 1000 )  )
						(setq ponto1BlocoFechamento (polar ponto2BlocoFechamento 0 92))
						
						
					)
				)
				
				
				(if (and (> (nth 0 pontoAnterior) (nth 0 pontoMeio)) )
					(progn
						
						
						;Desenho a tubulação
						(setq p1 (polar (polar coord1 0 (* andarDistancia vezes)) 2.790147859585483 159.7654530851060))
						(setq p2 (polar (polar coord1 0 (* andarDistancia vezes)) 3.493037447600107 159.7654530853958))
						
						(setq p1Dest (polar pontoAnterior 0.4636476089920219 122.9837387636728))
						(setq p2Dest (polar pontoAnterior 5.819537698181214 122.9837387640633))
						
						(if (= procura nil)
							(progn
								(command "layer" "m" "tubulacao1" "c" "30" "" "")
								(command "line" p1 p1Dest "")
								(command "line" p2 p2Dest "")
								
								;(command "circle" (polar coord1 0 (* andarDistancia vezes)) 20)
								
								
							)
							(progn
								(setq pontoAnterior (nth tamanhoLista ListapontosInternos))
								(setq pontoAnterior (polar pontoAnterior paraBaixo 1000))
								(setq pontoAnterior (polar pontoAnterior 0 (* andarDistancia (- vezes 1))))
								(command "layer" "m" "tubulacao1" "c" "30" "" "")
								;(command "circle" pontoAnterior 20)
								
								;Desenho a tubulação
								(setq p1 (polar pontoAnterior 0.4636476089920219 122.9837387636728))
								(setq p2 (polar pontoAnterior 5.819537698181214 122.9837387640633))
						
								
								(setq pontoDest1 (polar (polar coord1 0 (* andarDistancia (- vezes 1))) 2.790147859585483 159.7654530851060))
								
								(setq pontoDest2 (polar (polar coord1 0 (* andarDistancia (- vezes 1))) 3.493037447600107 159.7654530853958))
								
								(command "line" p1 (polar pontoDest1 0 50) "")
								(command "line" p2 (polar pontoDest2 0 50) "")
								(command "line" (polar pontoDest1 0 50) (polar pontoDest2 0 50) "")
								
							)
						)
					)
				)
			)
		)
		
		(setq tamanhoLista (- tamanhoLista 1))
	)
	
	
	;Faz a última cota
	(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
	(command "dimlinear" pontoBlocoAnterior (polar(polar (polar (polar (nth 1 pExtr) paraBaixo 1000)0 (* andarDistancia vezes)) paraBaixo (/ 110 2))0 149.9997)  (polar (sam_metade pontoBlocoAnterior (polar (polar (polar (polar (nth 1 pExtr) paraBaixo 1000)0 (* andarDistancia vezes)) paraBaixo (/ 110 2))0 149.9997)   ) paraBaixo 500) )
	
	
	(setq ponto2BlocoFechamento (polar  (polar(polar (polar (polar (nth 1 pExtr) paraBaixo 1000)0 (* andarDistancia vezes)) paraBaixo (/ 110 2))0 149.9997) 5.159910906125301 138.6543760582107))
	(command "dimlinear"  ponto1BlocoFechamento ponto2BlocoFechamento (polar (sam_metade ponto1BlocoFechamento ponto2BlocoFechamento) paraBaixo (- 1000 62.5002) )  )
	
	
	;Insere ultima extremidade
	(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
	(command "insert" "C:\\bfdias\\blocos\\CURVA 2_bloco2.dwg" (polar (polar (nth 1 pExtr) paraBaixo 1000)0 (* andarDistancia vezes)) "" "" "")
	
	
	(setq ultimoPontoInterno (polar (polar (nth 0 ListapontosInternos) paraBaixo 1000)0 (* andarDistancia vezes)) )
	;(command "circle" ultimoPontoInterno 30)
	
	(setq p1 (polar ultimoPontoInterno 0.4636479999999961 122.9840000000011))
	(setq p2 (polar ultimoPontoInterno 5.819537698181214 122.9837387640633))
	(setq p1Dest (polar (polar (nth 1 pExtr) paraBaixo 1000)0 (* andarDistancia vezes)))
	(setq p2Dest (polar (polar (polar (nth 1 pExtr) paraBaixo 1000)0 (* andarDistancia vezes)) paraBaixo 110))
	
	(command "layer" "m" "tubulacao1" "c" "30" "" "")
	(command "line" p1 p1Dest "")
	(command "line" p2 p2Dest "")
	
)

(defun c:ac22()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq estrutura1 (ssget '((-4 . "<OR")(2 . "DESCIDA 60X100 PLANTA_bloco")(2 . "DESCIDA 100 PLANTA_bloco")(2 . "DESCIDA 150X100 PLANTA_bloco")(8 . "BF-05A-SA-TXT")(2 . "CURVA 2_bloco2")(2 . "TE 2_bloco2") (2 . "CURVA 1_bloco2")(2 . "TE 1_bloco2")(-4 . "OR>"))))
	
	;pExtr = pontos da extremidade
	(setq pExtr (calcula_extreminadas estrutura1))
	
	(setq ListapontosInternos (pega_pontos_internos estrutura1))
	(setq ListapontosInternos (samuelBubbleSort ListapontosInternos))
	
	;Definir pontos de corte
	(setq ListapontosCortes (define_pontos_cortes estrutura1))
	
	
	(setq tipoBloco123 (define_o_tipo_bloco estrutura1))
	
	
	;Hora de desenhar
	(gerar_novo_desenho2)
	
	
	;(command "circle" (list 193650.0 16144.6 0.0) 20)
	
	
	
	(princ)
)







