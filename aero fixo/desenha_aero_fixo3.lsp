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



(defun percorre_estrutura_fixa3(all)
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
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
)

(defun conta_quantidade_de_difusores(all / qtd_difusores)
	(setq qtd_difusores 0)
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
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(if (= layerName "PONTO_DOS_DIFUSORES")
					(progn
						
						(setq qtd_difusores (+ qtd_difusores 1))
						
					)
				)
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	qtd_difusores
)

(defun carrega_posicao_difusores(all / lista_posicao_difusores)
	
	(setq lista_posicao_difusores nil)
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
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(if (= layerName "PONTO_DOS_DIFUSORES")
					(progn
						
						(setq lista_posicao_difusores (cons coord lista_posicao_difusores))
						
					)
				)
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	lista_posicao_difusores
	
	
)

(defun bubble (L)
  (cond ( (null (cdr L)) L)
        ( (> (car L) (cadr L))
          (cons (car L) (bubble (cdr L))))
        ( t (cons (cadr L)
            (bubble (cons (car L) (cddr L)))))
  )
)


;Desenvolvido por Samuel Gomes Huarachi - 13/08/2013 - 9:30 + ou -
;Voce passa um array, e passa a posicao que será trocada com seu sucessor
;Ex      0 1 2 3 4 5 6 7 8 9      Vou trocar na posicao 5, irá retornar:
;0 1 2 3 4 6 5 7 8 9 => O 5 foi "para frente", e o 6 foi "para tras"
;Fiz essa função para me auxiliar na função bubble sort que eu desenvolvi
(defun trocar_elementos_de_posicao (array2 pos / qtd novoArray contador2)
	(setq novoArray nil)
	(setq qtd (- (vl-list-length array2) 1))
	(setq contador2 0)
	(while (<= contador2 qtd )
		
		(if (= contador2 pos)
			(progn
				(setq novoArray (cons (nth (+ contador2 1) array2) novoArray))
				(setq novoArray (cons (nth contador2 array2) novoArray))
				
				
				(setq contador2 (+ contador2 1))
			)
			(progn
				(setq novoArray (cons (nth contador2 array2) novoArray))
			)
		)
		(setq contador2 (+ contador2 1))
	)
	(reverse novoArray)
)



;Função desenvolvida por Samuel Gomes Huarachi - 13/08/2013 - 9:45AM
;Afim de ordenar um array pelo eixo do x, na ordem crescente (Bubble Sort)
;Formato do array: (  (x y z)  (1 2 3)  (4 7 1) (9 2 1) )
;Função lenta, funciona bem até 200 elementos no array, depois disso fica muito lenta =(
(defun samuelBubbleSort(array1 / qtdElemArray HouveTroca contadorPos Px Px2 )
	(setq qtdElemArray (- (vl-list-length array1) 1))
	
	(setq HouveTroca T)
	(while (= HouveTroca T)
		(setq HouveTroca nil)
		(setq contadorPos 0)
		
		(while (< contadorPos qtdElemArray)
			
			(setq Px (nth 0 (nth contadorPos array1)))
			(setq Px2 (nth 0 (nth (+ contadorPos 1) array1)))
			
			(if (> Px Px2)
				(progn
					(setq array1 (trocar_elementos_de_posicao array1 contadorPos))
					(setq HouveTroca T)
				)
			)
			
			(setq contadorPos (+ contadorPos 1))
		)
		(setq qtdElemArray (- qtdElemArray 1))
	)
	array1
)

;Modo de usar a função 'rnd'
;(fix(* 200 (rnd)))
(defun rnd (/ modulus multiplier increment random)
  (if (not seed)
    (setq seed (getvar "DATE"))
  )
  (setq modulus    65536
        multiplier 25173
        increment  13849
        seed  (rem (+ (* multiplier seed) increment) modulus)
        random     (/ seed modulus)
  )
)


(defun define_mover_ea (opcao / move1)
	
	(if (= opcao "1")
		(progn
			(setq move1 0)
		)
	)
	(if (= opcao "2")
		(progn
			(setq move1 0)
		)
	)
	(if (= opcao "3")
		(progn
			(setq move1 -3.8546)
		)
	)
	
	move1
)

(defun define_bloco_entrada_ar3 (opcao / bloco_entrada_ar)
	
	(if (= opcao "1")
		(progn
			(setq bloco_entrada_ar "DESCIDA 60X100 CORTE 2")
		)
	)
	(if (= opcao "2")
		(progn
			(setq bloco_entrada_ar "DESCIDA 100 CORTE 2")
		)
	)
	(if (= opcao "3")
		(progn
			(setq bloco_entrada_ar "DESCIDA 150X100 CORTE 2")
		)
	)
	
	bloco_entrada_ar
)

(defun retorna_posicao_linha_base(all)

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
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(if (= layerName "LINHA_BASE")
					(progn
						(setq coord2 (cdr (assoc 11 (entget obj))))
						
						(if (< (nth 0 coord)(nth 0 coord2))
							(progn
								
								(setq posicaoBase (list coord coord2))
								
							)
							(progn
								(setq posicaoBase (list coord2 coord))
							)
						)
						
						
					)
				)
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
	
	posicaoBase
)

(defun converte_difusores()
	
	(setq qtdElemArray (vl-list-length listaPosDifusores_Sort))
	(setq contador 1)
	
	(setq zoomInicial (viewextents))
	(while (> qtdElemArray 0)
		(setq coordDif (nth (- contador 1) listaPosDifusores_Sort))
		
		
		(command "layer" "m" "difusorTeste" "c" "red" "" "")
		(command "insert" (strcat "C:\\bfdias\\blocos\\DIFUSOR CORTE 2.dwg") coordDif "" "" "")
		
		
		;Aplicando o trim
		(setq pTrim (polar coordDif 2.50762 92.8511))
		(command "layer" "m" "linhaTrimaAux" "c" "blue" "" "")
		(command "line" (polar pTrim (/ pi 2) 10) (polar pTrim (* (/ pi 2) 3) 10) "")
		(setq p1trim (entlast))
		
		(setq pTrim2 (polar coordDif 0.634695 92.7594))
		(command "layer" "m" "linhaTrimaAux" "c" "blue" "" "")
		(command "line" (polar pTrim2 (/ pi 2) 10) (polar pTrim2 (* (/ pi 2) 3) 10) "")
		(setq p2trim (entlast))
		
		(setq sel (ssadd))
		(setq sel (ssadd p1trim sel))
		(setq sel (ssadd p2trim sel))
		
		(setq pontoZoomTrim  (polar coordDif (/ pi 2) 55)   )
		(command "zoom" "c" pontoZoomTrim 10)
		(command "trim" sel "" pontoZoomTrim "")
		
		(command "erase" sel "")
		
		
		(setq contador (+ contador 1))
		(setq qtdElemArray (- qtdElemArray 1))
	)
	
)
(defun c:daf31()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(princ "\nSelecione uma entrada de ar")
	(princ "\n[1] - 60x100")
	(princ "\n[2] - 100")
	(princ "\n[3] - 150x100")
	
	(setq entradaAR (getstring "\n"))
	
	(if (and (/= entradaAR "1")(/= entradaAR "2")(/= entradaAR "3"))
		(progn
			(alert "Voce selecionou um entrada de ar invalida! O programa sera encerrado")
			(end)
		)
	)
	
	(princ "\nSelecione os elementos da estrutura: ")
	(setq elementos (ssget '((-4 . "<OR")(8 . "linha_base")(8 . "ponto_dos_difusores")(-4 . "OR>"))))
	
	(setq bloco_entrada_ar "")
	(if (/= elementos nil)
		(progn
			
			(setq bloco_entrada_ar (define_bloco_entrada_ar3 entradaAR))
			(setq bloco_entrada_ar "TE LAMINADO CORTE 2")
			
			
			(setq mover (define_mover_ea entradaAR))
			(setq listaPosDifusores (carrega_posicao_difusores elementos))
			(setq listaPosDifusores_Sort (samuelBubbleSort listaPosDifusores)) ;work
			(setq listaPosicaoInicialFinalBase (retorna_posicao_linha_base elementos))
			
			(command "layer" "m" "bfesquerda_layer" "c" "white" "" "")
			
			(command "insert" (strcat "C:\\bfdias\\blocos\\" bloco_entrada_ar ".dwg") (polar (nth 0 listaPosicaoInicialFinalBase) (/ pi 2) mover) "" "" "")
			
			(command "insert" (strcat "C:\\bfdias\\blocos\\CURVA CORTE.dwg") (polar (nth 1 listaPosicaoInicialFinalBase) (/ pi 2) 4.6092) "" "" "")
			
			
			(if (= entradaAR "1")
				(progn
					(setq pontoIncialTubo1 (polar (nth 0 listaPosicaoInicialFinalBase) 0.360336 155.989))
					(setq pontoIncialTubo2 (polar (nth 0 listaPosicaoInicialFinalBase) 5.92285 155.989))
				)
				
			)
			(if (= entradaAR "2")
				(progn
					(setq pontoIncialTubo1 (polar (nth 0 listaPosicaoInicialFinalBase) 0.374334 150.416))
					(setq pontoIncialTubo2 (polar (nth 0 listaPosicaoInicialFinalBase) 5.90885 150.416))
				)
			)
			(if (= entradaAR "3")
				(progn
					(setq pontoIncialTubo1 (polar (nth 0 listaPosicaoInicialFinalBase) 0.374334 150.416))
					(setq pontoIncialTubo2 (polar (nth 0 listaPosicaoInicialFinalBase) 5.90885 150.416))
				)
			)
			
			(command "layer" "m" "BF-04-SA-TUB" "c" "30" "" "")
			(command "line" pontoIncialTubo1 (polar (nth 1 listaPosicaoInicialFinalBase) 2.75725 146.685)    "")
			(command "line" pontoIncialTubo2 (polar (nth 1 listaPosicaoInicialFinalBase)   3.52594 146.685) "")
			
			(converte_difusores)
			
			
			;(setq qtdElemArray (vl-list-length listaPosDifusores))
			
			(command "erase" elementos "")
			(command "zoom" "w" (nth 0 zoomInicial)(nth 1 zoomInicial))
			
			
			(command "layer" "m" "BF-08-SA-LCE" "c" "red" "" "")
			(command "line" (nth 0 listaPosicaoInicialFinalBase)(nth 1 listaPosicaoInicialFinalBase) "")
		)
	)
	;BF-08-SA-LCE
	
	
	(princ)
)







