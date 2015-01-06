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


(defun procura_ponto_insercao(all)
	(setq xMin 10000000)
	(setq yMin 10000000)
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))(princ "\n--------------------------")
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
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))(setq qtd (- qtd 1))
			)
		)
	)
	(list xMin yMin 0.0)
)

(defun porcentagem(qtd)
	
	(while (> qtd 0)
		
		
		
		(setq qtd (- qtd 1))
	)
	
)

;Recebe como parâmetro o comprimento
(defun desenha_item_e(c)
	(setq pntIns (getpoint "\nDefina o ponto de inserção ")) ;Ponto de inserção
	
	(setq proxPnt (polar pntIns (/ pi 4) (/ 25 (sin (/ pi 4)))))
	(setq proxPnt2 (polar pntIns  (/ pi -4)   (/ 25 (sin (/ pi 4)))))
	(command "layer" "m" "BF-SA-03-CNT" "c" "251" "" "")
	(sam_linha pntIns proxPnt)
	(sam_linha pntIns proxPnt2)
	
	(setq outExt (polar pntIns 0 c)) ;Outra extremidade
	(setq proxPnt3 (polar outExt (* (/ pi 4) 3) (/ 25 (sin (/ pi 4)))))
	(setq proxPnt4 (polar outExt  (* (/ pi -4) 3)  (/ 25 (sin (/ pi 4)))))
	
	(sam_linha outExt proxPnt3)
	(sam_linha outExt proxPnt4)
	
	(sam_linha proxPnt proxPnt3)
	(sam_linha proxPnt2 proxPnt4)
	
	(command "layer" "m" "BF-07-SA-COT" "c" "250" "" "")
	(command "dimlinear" pntIns outExt (polar (sam_metade pntIns outExt) sam_parabaixo 265) )
	
	;Faz a legenda logo abaixo do desenho da estrutura
	(command "layer" "m" "BF-SA-05-TXT" "c" "252" "" "")
	(command "text" "s" "BF-01-TXT" (polar pntIns sam_parabaixo (+ 265 180.0114690999071)) 70 0 "ITEM E")
	(setq lastPoint (polar pntIns sam_parabaixo (+ 265 180.0114690999071)))
	(command "text" "s" "BF-01-TXT" (polar lastPoint sam_parabaixo 116) 70 0 "QUANTIDADE: 01 PEÇA POR GRADE")
	(setq lastPoint (polar lastPoint sam_parabaixo 116))
	(command "text" "s" "BF-01-TXT" (polar lastPoint sam_parabaixo 116) 70 0 "TOTAL:")


)

(defun c:iteme()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	(princ "\nSelecione a estrutura air grid ")
	(setq selecao (ssget))
	(if (/= selecao nil)
		(progn
			(setq IDDGRID (drig_get_id selecao))
			
			(setq lastroCompr (atof (dgrid_retorna_informacoes "COMPRIMENTO DO LASTRO" IDDGRID)))
			(setq dgridComprimento (atof (dgrid_retorna_informacoes "COMPRIMENTO" IDDGRID)))
			
			;Cálculo da extremidade do James
			(setq pontoInsercao (procura_ponto_insercao selecao))
			(setq andarAte (+ 49.98852231825003 50.00000000000000 lastroCompr))
			;50.01150598540698
			(setq newPoint (polar pontoInsercao 0 andarAte))
			(setq P1ExtJames (polar newPoint sam_paracima 50.01150598540698)) ;Ponto 1 extremidade do James
			;(command "circle" P1ExtJames 22)
			
			;Cálculo da extremidade da Solaris
			;49.97206525008369
			(setq andarAte2 (+ 49.97206525008369 50.00000000000000 lastroCompr))
			(setq POTEX (polar pontoInsercao 0 dgridComprimento)) ;Ponto da outra extremidade do dgrid
			(setq P1ExtSolaris (polar POTEX pi andarAte2)) ;Ponto 1 extremidade da Solaris
			(setq P1ExtSolaris (polar P1ExtSolaris sam_paracima 2129.988376545054))
			;(command "circle" P1ExtSolaris 24)
			
			(setq comprimentoITE (distance P1ExtJames P1ExtSolaris)) ;Comprimento do item E
			
			(desenha_item_e comprimentoITE)
			
			;2129.988376545054
		)
	)
	
	
	(princ "\n...")
	(princ)
)







