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

(defun saveVarsAefixaCorte1()
	(setq qtdRamais  (atof (get_tile "qtdbf")))
	(setq comprimento (atof (get_tile "comprimento3")))
)

(defun c:ac1()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	;DCL start
	(if (not(setq dcl_id (load_dialog "C:\\bfdias\\aero fixo\\dcl\\aefixa_corte1.dcl")))
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
					(action_tile "accept" "(saveVarsAefixaCorte1)(done_dialog 2)")
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
	
	(setq zoomInicial3 (viewextents))
	
	;(setq comprimento (atof (getstring "\nComprimento? ")))
	;(setq qtdRamais (atof (getstring "\nQuantidade de ramais? ")))
	
	
	(setq distRamais (/ comprimento (- qtdRamais 1)))
	
	(setq ponto_insercao (getpoint "\nDefina um ponto de inserção: "))
	(setq extremidade2 (polar ponto_insercao 0 comprimento))
	(setq pontoMeio (polar ponto_insercao 0 (/ comprimento 2)))
	
	
	(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
	(command "insert" "C:\\bfdias\\blocos\\CURVA 1_bloco2.dwg" ponto_insercao "" "" "")
	(setq obj1 (entlast))
	(command "move" obj1 "" ponto_insercao (polar ponto_insercao 4.355252414780826 143.0247432903737))
	
	
	
	
	;Insere Intersecções
	(setq vezes 1)
	(while (> (- qtdRamais 2) 0)
		(setq p1 (polar ponto_insercao 0 (* distRamais vezes) ))
		
		
		(if (< (nth 0 p1) (nth 0 pontoMeio))
			(progn
				(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
				(command "insert" "C:\\bfdias\\blocos\\TE 1_bloco2.dwg" p1 "" "" "")
				
				
			)
			(progn
				(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
				(command "insert" "C:\\bfdias\\blocos\\TE 2_bloco2.dwg" p1 "" "" "")
				
				
			)
		)
		
		
		(if (= vezes 1)
			(progn
				(setq pontoT1 (polar p1 2.67795 122.984))
				(setq pontoT1_Destino (polar (polar p1 pi distRamais) 0.351445 159.765))
				
				
				(command "layer" "m" "tubulacao1" "c" "30" "" "")
				(command "line" pontoT1 pontoT1_Destino "")
				
				
				(setq pontoT2 (polar p1 3.605240262582741 122.9837387654218))
				(setq pontoT2_Destino (polar (polar p1 pi distRamais) 5.931738837113715 159.7650861174637))
				
				
				(command "layer" "m" "tubulacao1" "c" "30" "" "")
				(command "line" pontoT2 pontoT2_Destino "")
				
				
				;Faz a primeira conta
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" p1  (polar p1 pi distRamais) (polar (polar p1 0  (/(distance p1 (polar p1 pi distRamais))2) ) (*(/ pi 2)3) 337))
				
				
			)
		)
		
		
		;Inserção da tubulação
		(setq p1_prox (polar ponto_insercao 0 (* distRamais (+ vezes 1)) ))
		
		
		(if (and (< (nth 0 p1) (nth 0 pontoMeio))(> (nth 0 p1_prox) (nth 0 pontoMeio)))
			(progn
				
				;Faz a cota no centro
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" p1  pontoMeio (polar (polar p1 0  (/(distance p1 pontoMeio)2) ) (*(/ pi 2)3) 337))
				(command "dimlinear" pontoMeio  p1_prox (polar (polar p1 0  (/(distance pontoMeio pontoMeio)2) ) (*(/ pi 2)3) 337))
				
				
			)
			(progn
				;Faz as cotas medianas
				(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
				(command "dimlinear" p1  p1_prox (polar (polar p1 0  (/(distance p1 p1_prox)2) ) (*(/ pi 2)3) 337))
				
			)
		)
		
		
		
		(if (< (nth 0 p1)(nth 0 pontoMeio))
			(progn
				(setq pontoT1 (polar p1 0.351445 159.765))
				(setq pontoT2 (polar p1 5.931740513170155 159.7654530854060))
			)
			(progn
				(setq pontoT1 (polar p1 0.463648 122.984))
				(setq pontoT2 (polar p1 5.819537698181214 122.9837387640633))
			)
		)
		(if (< (nth 0 p1_prox)(nth 0 pontoMeio))
			(progn
				(setq pontoT1_Destino (polar p1_prox 2.67795 122.984))
				(setq pontoT2_Destino (polar p1_prox 3.605240262588151 122.9837387640625))
			)
			(progn
				(setq pontoT1_Destino (polar p1_prox 2.79015 159.765))
				(setq pontoT2_Destino (polar p1_prox 3.493037447600107 159.7654530853958))
			)
		)
		
		
		(command "layer" "m" "tubulacao1" "c" "30" "" "")
		(command "line" pontoT1 pontoT1_Destino "")
		(command "line" pontoT2 pontoT2_Destino "")
		
		
		
		
		
		(setq vezes (+ vezes 1))
		(setq qtdRamais (- qtdRamais 1))
	)
	
	(command "layer" "m" "interseccaoinv_layer" "c" "white" "" "")
	(command "insert" "C:\\bfdias\\blocos\\CURVA 2_bloco2.dwg" extremidade2 "" "" "")
	(setq obj1 (entlast))
	(command "move" obj1 "" extremidade2 (polar extremidade2 2.79015 159.765))
	
	
	(command "layer" "m" "layer_t" "c" "white" "" "")
	(command "line" ponto_insercao extremidade2 "")
	
	;(insere_entrada_ar_ae_fixa1 pontoMeio tipoEntradaAr)
	
	
	(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
	(command "insert" "C:\\bfdias\\blocos\\aefixo_entar12.dwg" pontoMeio "" "" "" "")
	
	(command "layer" "m" "linha_aux" "c" "cyan" "" "")
	(command "circle" pontoMeio 60)
	(setq objCircle1 (entlast))
	(setq pontoTrim (polar pontoMeio 1.814160039890400 56.66970959877810))
	(command "zoom" "c" pontoTrim 10)
	(command "trim" objCircle1 "" pontoTrim "")
	(command "zoom" "c" (polar pontoMeio (+ 1.814160039890400 pi) 56.66970959877810) 10)
	(command "trim" objCircle1 "" (polar pontoMeio (+ 1.814160039890400 pi) 56.66970959877810) "")
	(command "erase"  objCircle1 "")
	
	(sam_linha (polar pontoMeio 0.5743048301754257 101.2422836566303) (polar pontoMeio 5.708880477005608 101.2422836565355) )
	(setq linha1 (entlast))
	(sam_linha (polar pontoMeio 0.4048917862856102 152.3154621173125) (polar pontoMeio 5.878293520892219 152.3154621174272))
	(setq linha2 (entlast))
	(setq linhasAdd nil)
	(setq linhasAdd (ssadd))
	(setq linhasAdd (ssadd linha1 linhasAdd))
	(setq linhasAdd (ssadd linha2 linhasAdd))
	
	(setq pontoTrim (polar pontoMeio 0.4497884829061774 126.5017170823583))
	(command "zoom" "c" pontoTrim 10)
	(command "trim" linhasAdd "" pontoTrim "")
	(setq pontoTrim (polar pontoMeio 5.835879638814807 127.1564843953733))
	(command "zoom" "c" pontoTrim 10)
	(command "trim" linhasAdd "" pontoTrim "")
	(command "erase"  linhasAdd "")
	;----------
	
	(sam_linha (polar pontoMeio (+ 0.5743048301754257 pi) 101.2422836566303) (polar pontoMeio (+ 5.708880477005608 pi) 101.2422836565355) )
	(setq linha1 (entlast))
	(sam_linha (polar pontoMeio (+ 0.4048917862856102 pi) 152.3154621173125) (polar pontoMeio (+ 5.878293520892219 pi) 152.3154621174272))
	(setq linha2 (entlast))
	(setq linhasAdd nil)
	(setq linhasAdd (ssadd))
	(setq linhasAdd (ssadd linha1 linhasAdd))
	(setq linhasAdd (ssadd linha2 linhasAdd))
	
	(setq pontoTrim (polar pontoMeio (+ 0.4497884829061774 pi) 126.5017170823583))
	(command "zoom" "c" pontoTrim 10)
	(command "trim" linhasAdd "" pontoTrim "")
	(setq pontoTrim (polar pontoMeio (+ 5.835879638814807 pi) 127.1564843953733))
	(command "zoom" "c" pontoTrim 10)
	(command "trim" linhasAdd "" pontoTrim "")
	
	(command "erase" linhasAdd "")
	
	
	
	;Insere cota de ponta a ponta
	(command "layer" "m" "BF-06-SA-COT" "c" "250" "" "")
	(command "dimlinear" (polar (polar ponto_insercao pi 60) (* (/ pi 2) 3) 110)  (polar (polar extremidade2 0 60) (* (/ pi 2) 3) 110 ) (polar   (polar (polar ponto_insercao pi 55) 0 (/ (distance (polar ponto_insercao pi 55)  (polar extremidade2 0 60)) 2))  (*(/ pi 2)3)  590  ) )
	
	
	
	(command "zoom" "w" (nth 0 zoomInicial3) (nth 1 zoomInicial3))
	(princ)
)







