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


(defun c:isgtrim()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(setq sel (ssget '((2 . "SUPORTE GUIA PLANTA,SUPORTE GUIA PLANTA2,bfsg_bloco,bfsg_bloco2_bloco"))))
	
	(if (/=  sel nil)
		(progn
			
			(setq qtd (- (sslength sel) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))
			(princ "\n--------------------------")
			(princ "\n--------------------------")
			(setq zoomInicial (viewextents))
			(while (>= qtd 0)
				(setq obj (ssname sel qtd))
				(setq layerName (strcase (cdr (assoc 8 (entget obj)))))
				(setq blocoName (strcase (cdr (assoc 2 (entget obj)))))
				(setq coord (cdr (assoc 10 (entget obj))))
				(setq x1 (rtos (car coord) 2 3))
				(setq y1 (rtos (cadr coord) 2 3))
				
				(if (=  blocoName "SUPORTE GUIA PLANTA")
					(progn
						(setq ponto1 (polar coord 0 21))
						(setq ponto2 (polar coord pi 21))
						
						(command "layer" "m" "linha_aux2" "c" "red" "" "")
						(command "line" (polar ponto1 sam_paracima 57) (polar ponto1 sam_parabaixo 57) "")
						(setq l1 (entlast))
						(command "line" (polar ponto2 sam_parabaixo 57) (polar ponto2 sam_paracima 57) "")
						(setq l2 (entlast))
						(setq sel1 nil)
						(setq sel1 (ssadd))
						(setq sel1 (ssadd l1 sel1))
						(setq sel1 (ssadd l2 sel1))
						
						(setq pontoZoomTrim (polar coord (+ 6.283184074698814 (/ pi 2)) 54.88298244138226))
						
						(command "zoom" "c" pontoZoomTrim 10)
						(command "trim" sel1 "" pontoZoomTrim "")
						
						(setq pontoZoomTrim (polar coord (+ pi (/ pi 2)) 54.99980631161701))
						
						(command "zoom" "c" pontoZoomTrim 10)
						(command "trim" sel1 "" pontoZoomTrim "")
						
						(command "erase" sel1 "")
					)
				)
				
				(if (= blocoName "SUPORTE GUIA PLANTA2")
					(progn
						
						(setq ponto1 (polar coord sam_paracima 21))
						(setq ponto2 (polar coord sam_parabaixo 21))
						
						(command "layer" "m" "linha_aux2" "c" "red" "" "")
						(command "line" (polar ponto1 pi 57) (polar ponto1 0 57) "")
						(setq l1 (entlast))
						(command "line" (polar ponto2 pi 57) (polar ponto2 0 57) "")
						(setq l2 (entlast))
						(setq sel1 nil)
						(setq sel1 (ssadd))
						(setq sel1 (ssadd l1 sel1))
						(setq sel1 (ssadd l2 sel1))
						
						(setq pontoZoomTrim (polar coord 6.283184074698814 54.88298244138226))
						
						(command "zoom" "c" pontoZoomTrim 10)
						(command "trim" sel1 "" pontoZoomTrim "")
						
						(setq pontoZoomTrim (polar coord pi 54.99980631161701))
						
						(command "zoom" "c" pontoZoomTrim 10)
						(command "trim" sel1 "" pontoZoomTrim "")
						
						(command "erase" sel1 "")
						
					)
				)
				
				
				
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
			(command "zoom" "w" (nth 0 zoomInicial) (nth 1 zoomInicial))
			
		)
	)
	
)










