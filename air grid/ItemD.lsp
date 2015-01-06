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



(defun percorre_selecao(all / lista_ronnie)
	(setq lista_ronnie nil)
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
				(setq tipo (strcase (cdr (assoc 0 (entget obj)))))
				
				(if (= tipo "CIRCLE")
					(progn
						(setq raio  (cdr (assoc 40 (entget obj))))
						(setq coordenada  (cdr (assoc 10 (entget obj))))
						(if (= raio 3)
							(progn
								(setq lista_ronnie (cons coordenada lista_ronnie))
							)
						)
					)
				)
				
				
				;(setq coord (cdr (assoc 10 (entget obj))))
				;(setq x1 (rtos (car coord) 2 3))
				;(setq y1 (rtos (cadr coord) 2 3))
				
				
				
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	lista_ronnie
)


(defun procura_ponto_insercao(all)
	
	(setq xMin 10000000)
	(setq yMin 10000000)
	
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
				(setq tipo (strcase (cdr (assoc 0 (entget obj)))))
				
				
				(if (and (= tipo "LINE") (= layerName "BF-02-SA-GRA"))
					(progn
						(setq coord (cdr (assoc 10 (entget obj))))
						(setq x1  (car coord))
						(setq y1  (cadr coord))
						
						(if (< x1 xMin)
							(progn
								(setq xMin x1)
							)
						)
						(if (< y1 yMin)
							(progn
								(setq yMin y1)
							)
						)
						
						(setq coord2 (cdr (assoc 10 (entget obj))))
						(setq x2  (car coord2))
						(setq y2  (cadr coord2))
						(if (< x2 xMin)
							(progn
								(setq xMin x2)
							)
						)
						(if (< y2 yMin)
							(progn
								(setq yMin y2)
							)
						)
						
						
						
						
					)
				)
				
				
				
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	
	(list xMin yMin 0.0)
)

(defun drig_get_id(all / idDgrid)
	(setq idDgrid nil)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))
			(princ "\n--------------------------")
			(princ "\n--------------------------")
			(while (>= qtd 0)
				(setq obj (ssname all qtd))
				
				(setq valueA (GetId obj "IDDGRID"))
				(if (/= valueA nil)
					(progn
						(setq idDgrid valueA)
					)
				)
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	idDgrid
)


(defun dgrid_retorna_informacoes(label1 IDDGRID / retorno)
	;PATH_DGRID_TXT
	(setq retorno "")
	
   (setq ARQUIVO_CSV (open (strcat PATH_DGRID_TXT IDDGRID "\\INFORMACOES.txt" ) "r")
         LINHA_CSV (read-line ARQUIVO_CSV)
         LISTA_LINHA nil
         LISTA_CSV nil
   )

   (while (/= LINHA_CSV nil)
    (setq TAMANHO_DA_LINHA (strlen LINHA_CSV)
          C1 1
          CAMPO ""
          LETRA "."
          FLAG 0
          CONTA 1
    )

    (while (= FLAG 0)

     (while (and (/= LETRA ";") (/= (strlen LETRA) 0))

      (setq LETRA (substr LINHA_CSV C1 1))

      (if (/= LETRA ";")
       (setq CAMPO (strcat CAMPO LETRA))
      )

      (if (> C1 TAMANHO_DA_LINHA)
       (setq FLAG 1)
      )

      (setq C1 (+ C1 1))
     )

     (setq LISTA_LINHA (cons (strcase CAMPO) LISTA_LINHA))

     (setq LETRA "."
           CAMPO ""
     )
    )
	(setq LISTA_LINHA (reverse LISTA_LINHA))
	(setq qtdString (strlen (nth 0 LISTA_LINHA)))
	
	(if (> qtdString 3)
		(progn
			(setq stringValue (nth 0 LISTA_LINHA))
			
			(setq valor (bf_split stringValue "---"))
			(if (= (nth 0 (nth 0 valor)) label1)
				(progn
					(setq retorno (nth 0 (nth 1 valor)))
				)
			)
		)
	)
    (setq LINHA_CSV (read-line ARQUIVO_CSV))
    (setq LISTA_LINHA nil)
   )
   (close ARQUIVO_CSV)
	retorno
)


;utilizar a função "(vl-string-search "smauel---gomes---huarachi" "---")" para tentar 
;resolver esse algoritmo de uma forma mais fácil
(defun bf_split(word split1)
	(setq tamString (strlen word)) ;Pega o tamanho da string
	(setq contador 1)
	(setq stringCorrent "")
	(setq tamSplit (strlen split1)) ; tamanho da palavra do delimitador
	(setq letraSplit (substr split1 1 1)) ;Pega a primeira letra do split
	(setq listaRetorno nil)
	(setq posicaoFixa 1)
	(setq varStr 1)
	(setq letraCorrente "")
	(while (> tamString 0)
		(setq letraCorrente (substr word varStr 1))
		(if (= letraCorrente letraSplit) ;Verifica se a primeira letra corresponde a primeira letra do delimitador
			(progn
				;Verifica se os próximos são iguais ao delimitador
				(setq splitVerify (substr word varStr tamSplit))
				(if (= splitVerify split1)
					(progn
						;Temos o delimitador
						(setq listaRetorno (cons (list stringCorrent ) listaRetorno))
						(setq posicaoFixa (+ posicaoFixa contador (- tamSplit 1)))
						(setq contador 1)
						(setq tamString (- tamString tamSplit))
						(setq varStr (+ varStr tamSplit))
					)
				)
			)
			(progn
				(setq stringCorrent (substr word posicaoFixa contador))
			)
		)
		(setq contador (+ contador 1))
		(setq varStr (+ varStr 1))
		(setq tamString (- tamString 1))
	)
	(setq listaRetorno (cons (list stringCorrent) listaRetorno))
	(reverse listaRetorno)
)


(defun desenha_marcacao(ponto)
	
	(command "layer" "m" "BF-07-SA-LCE" "c" "red" "" "")
	(sam_linha (polar ponto sam_parabaixo 14.92285590553365) (polar (polar ponto sam_parabaixo 14.92285590553365) sam_paracima 29.84571181106730))
	(sam_linha (polar ponto pi 14.92285590553365) (polar (polar ponto pi 14.92285590553365) 0 29.84571181106730))
	(command "layer" "m" "BF-SA-09-SUP" "c" "251" "" "")
	(command "circle" ponto 3)

	
)

(defun c:itemd()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	;(setq comprimento (atof (getstring "\nComprimento")))
	;(setq qtdDifusores (atof (getstring "\nQuantidade de difusores")))
	
	(setq selecao (ssget))
	(if (/= selecao nil)
		(progn
			(setq lista_ronnie (percorre_selecao selecao))
		)
	)
	
	(setq qtd (vl-list-length lista_ronnie))
	(while (> qtd 0)
		(setq coordenada (nth (- qtd 1) lista_ronnie))
		
		;(command "circle" coordenada 4)
		
		(setq qtd (- qtd 1))
	)
	
	
	;Agrupar os ronnies - acha a distancia Y maior entre os dois pontos
	(setq qtd (vl-list-length lista_ronnie))
	(setq yMaiorDif 10)
	(setq coordenada1 (nth (- qtd 1) lista_ronnie))
	(setq yCoordenada1 (nth 1 coordenada1))
	(setq qtd (- qtd 1))
	(while (> qtd 0)
		(setq coordenada2 (nth (- qtd 1) lista_ronnie))
		(setq yCoordenada2 (nth 1 coordenada2))
		(setq diferenca (modulo (- yCoordenada1 yCoordenada2)))
		(if (> diferenca yMaiorDif)
			(progn
				(setq yMaiorDif diferenca)
			)
		)
		(setq coordenada1 coordenada2)
		(setq yCoordenada1 yCoordenada2)
		
		(setq qtd (- qtd 1))
	)
	
	;Agrupar os ronnies -  Separa os ronnies que estão em cima, dos que estão em baixo
	(setq listaDeUmLado nil)
	(setq listaDeOutroLado nil)
	(setq qtd (vl-list-length lista_ronnie))
	(setq difRange 80)
	(setq coordenada1 (nth (- qtd 1) lista_ronnie))
	(setq listaDeUmLado (cons coordenada1 listaDeUmLado))
	(setq yFixo (nth 1 coordenada1))
	(setq qtd (- qtd 1))
	(while (> qtd 0)
		(setq coordenadaAtual (nth (- qtd 1) lista_ronnie))
		(setq YAtual (nth 1 coordenadaAtual))
		(setq diferenca (modulo (- yFixo YAtual)))
		(if (> diferenca 80)
			(progn
				(setq listaDeOutroLado (cons coordenadaAtual listaDeOutroLado))
			)
			(progn
				(setq listaDeUmLado (cons coordenadaAtual listaDeUmLado))
			)
		)
		(setq qtd (- qtd 1))
	)
	
	
	(setq qtd (vl-list-length listaDeOutroLado))
	(while (> qtd 0)
		(setq coordenada (nth (- qtd 1) listaDeOutroLado))
		;(command "layer" "m" "samuel1" "c" "blue" "" "")
		;(command "circle" coordenada 4)
		
		(setq qtd (- qtd 1))
	)
	
	(setq qtd (vl-list-length listaDeUmLado))
	(while (> qtd 0)
		(setq coordenada (nth (- qtd 1) listaDeUmLado))
		;(command "layer" "m" "renata1" "c" "red" "" "")
		;(command "circle" coordenada 4)
		
		(setq qtd (- qtd 1))
	)
	
	(setq pontoInsercao (procura_ponto_insercao selecao))
	
	;(command "layer" "m" "insertpoint" "c" "241" "" "")
	;(command "circle" pontoInsercao 22)
	
	
	
	
	(setq IDDGRID (drig_get_id selecao))
	
	;PATH_DGRID_TXT
	(setq comprimento (dgrid_retorna_informacoes "COMPRIMENTO" IDDGRID))
	(if (/= comprimento "") ;Senão encontrar o comprimento no txt, achar o comprimento automaticamente
		(progn
			(setq lista1 (samuelBubbleSort listaDeUmLado)) ;Acabei de ordenar a lista
			(setq qtd (vl-list-length lista1))
			(setq pontoInsercaoAtual (getpoint "\nDefina o ponto de inserção "))
			
			(setq distanciaBaseDif (- (nth 0 (nth 0 lista1))  (nth 0 pontoInsercao)  )  )
			
			
			(setq coord1 (nth (- qtd 1) lista1))
			;(command "layer" "m" "pontoInsercao" "c" "blue" "" "") ;Ponto de inserção
			;(command "circle" pontoInsercaoAtual 22)
			;(command "layer" "m" "bolinhas" "c" "241" "" "") ;Primeira bolinha(ou primeira posição do difusor)
			;(command "circle" (polar pontoInsercaoAtual 0 (+ distanciaBaseDif 57.5)) 22)
			(setq coordDifCurrent (polar (polar pontoInsercaoAtual 0 distanciaBaseDif ) sam_paracima 19.49166489901836))
			;Desenha a marcação
			(desenha_marcacao coordDifCurrent)
			
			
			;Posição do primeiro difusor
			(setq PRIMEIRO_DIFUSOR_COORD (polar pontoInsercaoAtual 0  distanciaBaseDif ))
			(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
			(command "dimlinear" (polar pontoInsercaoAtual sam_paracima 50.01146909992531) PRIMEIRO_DIFUSOR_COORD (polar (sam_metade pontoInsercaoAtual PRIMEIRO_DIFUSOR_COORD) sam_parabaixo (- 265 19.49166489901836) ) )
			
			
			(setq qtd (- qtd 1))
			(setq pontoFinal (polar pontoInsercaoAtual 0 (atof comprimento)))
			;(command "layer" "m" "pontoFinalEstrutura" "c" "blue" "" "")
			;(command "circle" pontoFinal 22) ;Faz o ponto final da estrutura
			;30.51742865520281
			
			
			;Inicializando o desenho da estrutura - "Item C"
			;BF-03-SA-CNT
			(command "layer" "m" "BF-03-SA-CNT" "c" "251" "" "")
			(command "line" (polar pontoInsercaoAtual sam_paracima 50.01146909992531) (polar pontoFinal sam_paracima 50.01146909992531) "")
			
			
			;(command "line" pontoInsercaoAtual (polar pontoInsercaoAtual 0.7856275717126317 70.71067811858305) "")
			;(command "line" pontoFinal (polar pontoFinal (+ (* (- (/ pi 2) 0.7856275717126317) 2) 0.7856275717126317) 70.71067811858305) "")
			
			(command "line" (polar pontoInsercaoAtual sam_paracima 50.01146909992531)  (polar (polar pontoInsercaoAtual sam_paracima 50.01146909992531) (+ (* (- (/ pi 2) 0.7856275717126317) 2) 0.7856275717126317 pi) 70.71067811858305) "")
			(command "line" (polar pontoFinal sam_paracima 50.01146909992531) (polar (polar pontoFinal sam_paracima 50.01146909992531) (+ 0.7856275717126317 pi) 70.71067811858305) "")
			
			
			(command "line"  (polar (polar pontoInsercaoAtual sam_paracima 50.01146909992531) (+ (* (- (/ pi 2) 0.7856275717126317) 2) 0.7856275717126317 pi) 70.71067811858305)  (polar (polar pontoFinal sam_paracima 50.01146909992531) (+ 0.7856275717126317 pi) 70.71067811858305)  "")
			
			
			(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
			(command "dimlinear" (polar pontoInsercaoAtual sam_paracima 50.01146909992531)  (polar pontoFinal sam_paracima 50.01146909992531)  (polar (sam_metade pontoInsercaoAtual pontoFinal) sam_parabaixo 520) )
			
			
			(setq coordAnterior PRIMEIRO_DIFUSOR_COORD)
			(while (> qtd 0)
				(setq coordAtual (nth (- qtd 1) lista1))
				(setq distancia (distance coord1 coordAtual))
				;(command "layer" "m" "bolinhas" "c" "241" "" "")
				;(command "circle" (polar pontoInsercaoAtual 0 (+ distancia distanciaBaseDif 57.5) ) 22) ;pega o ponto dos difusores intermediários
				
				(setq coordDifCurrent (polar pontoInsercaoAtual 0 (+ distancia distanciaBaseDif) ))
				(setq coordDifCurrent (polar coordDifCurrent sam_paracima 19.49166489901836))
				;Desenha a marcação
				(desenha_marcacao coordDifCurrent)
				
				(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
				(command "dimlinear" coordAnterior coordDifCurrent (polar (sam_metade coordAnterior coordDifCurrent) sam_parabaixo 265) )
				
				
				(setq coordAnterior coordDifCurrent)
				
				(setq qtd (- qtd 1))
			)
			
			(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
			(command "dimlinear" coordAnterior  (polar pontoFinal sam_paracima 50.01146909992531) (polar(sam_metade coordAnterior  pontoFinal) sam_parabaixo (- 265 9.745832449509180)) )
			
			;Faz a legenda logo abaixo do desenho da estrutura
			(command "layer" "m" "BF-SA-05-TXT" "c" "252" "" "")
			(command "text" "s" "BF-01-TXT" (polar pontoInsercaoAtual sam_parabaixo 650) 70 0 "ITEM D")
			(setq lastPoint (polar pontoInsercaoAtual sam_parabaixo 650))
			(command "text" "s" "BF-01-TXT" (polar lastPoint sam_parabaixo 116) 70 0 "QUANTIDADE: 01 PEÇA POR GRADE")
			(setq lastPoint (polar lastPoint sam_parabaixo 116))
			(command "text" "s" "BF-01-TXT" (polar lastPoint sam_parabaixo 116) 70 0 "TOTAL:")
			
		)
		(progn
			(alert "O comprimento não foi encontrado! O programa será finalizado!")
		)
	)
	
	
	
	
	(princ)
)







