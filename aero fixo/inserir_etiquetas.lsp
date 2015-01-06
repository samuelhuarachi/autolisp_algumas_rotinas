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





(defun c:inet()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	;(setq resposta (getstring "\n[1] - Colar na obra"))
	
	
	(if (= resposta2 "1")
		(progn
			
			
			(command "layer" "m" "BF-05A-SA-TXT" "c" "251" "" "")
			(command "line" pontoInsercao (polar pontoInsercao 1.047197551196507 214.5105000670548) "")
			(command "line" (polar pontoInsercao 1.047197551196507 214.5105000670548)  (polar (polar pontoInsercao 1.047197551196507 214.5105000670548) 0 718.5) "")
			
			
			
			(command "text" (polar pontoInsercao 0.8037993611789921 285.1111610401940) 123.1200 0 "COLAR")
			(command "text" (polar pontoInsercao 0.4079856367375379 131.1566852449645) 123.1200 0 "NA OBRA")
			
			
		)
	)
	
	(setq textoValue (strcase (getstring T "\nValor: ")))
	(setq pontoInsercao (getpoint "\nDefina o ponto de insercao: "))
	
	
	(command "layer" "m" "BF-05A-SA-TXT" "c" "251" "" "")
	(command "line" pontoInsercao (polar pontoInsercao 1.047197551196507 214.5105000670548) "")
	(setq linha1 (entlast))
	
	(setq textoInsertPoint (polar pontoInsercao 1.047197551196507 214.5105000670548))
	
	(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
	(command "text" "s" "BF-01-TXT" textoInsertPoint 100 0 textoValue)
	(setq texto1 (entlast))
	(setq obj_texto (entlast))
	;calculo do tamanho do texto INICIO
	(setq obj_texto (tdir obj_texto "C"))
	(setq ponto_insert_demanda2 (cdr (assoc 10 (entget obj_texto))))
	(setq tamanho_text (* (distance textoInsertPoint ponto_insert_demanda2) 2))
	(setq obj_texto (tdir obj_texto "L"))
	(command "move" obj_texto "" ponto_insert_demanda2 textoInsertPoint)
	(command "move" obj_texto "" textoInsertPoint (polar textoInsertPoint (/ pi 4) 25))
	;cálculo do tamanho do texto FIM
	
	(command "layer" "m" "BF-05A-SA-TXT" "c" "251" "" "")
	
	(command "line" 
	(polar pontoInsercao 1.047197551196507 214.5105000670548) 
	(polar (polar pontoInsercao 1.047197551196507 214.5105000670548)  0 (+ tamanho_text 40)) "")
	
	
	(setq linha2 (entlast))
	
	
	
	(setq inverter (strcase (getstring "\nInverter? [i]")))
	(if (= inverter "I")
		(progn
		
			(command "erase" linha1 "")
			(command "erase" linha2 "")
			(command "erase" texto1 "")
			
			(command "layer" "m" "BF-05A-SA-TXT" "c" "251" "" "")
			(command "line" pontoInsercao (polar pontoInsercao (- pi 1.047197551196507) 214.5105000670548) "")
			(setq linha1 (entlast))
			(command "line" (polar pontoInsercao (- pi 1.047197551196507) 214.5105000670548)   (polar (polar pontoInsercao (- pi 1.047197551196507) 214.5105000670548) pi (+ tamanho_text 40)) "")
			(setq linha2 (entlast))
			(command "layer" "m" "BF-05-SA-TXT" "c" "252" "" "")
			;(command "text" (polar (polar (polar pontoInsercao (- pi 1.047197551196507) 214.5105000670548) pi (+ tamanho_text 40)) (/ pi 4) 25) 100 0 textoValue)
			
			(command "text" "s" "BF-01-TXT"  (polar (polar (polar pontoInsercao (- pi 1.047197551196507) 214.5105000670548) pi (+ tamanho_text 40)) (/ pi 4) 25) 100 0 textoValue)
			(setq texto1 (entlast))
			
			
		)
	)
	
	
	
	(setq sel (ssadd))
	(setq sel (ssadd linha1 sel))
	(setq sel (ssadd linha2 sel))
	(setq sel (ssadd texto1 sel))
	
	
	(setq lista (vl-string->list textoValue))
	(setq lista (vl-remove (ascii ",") lista))
	(setq lista (vl-remove (ascii "/") lista))
	(setq textoValue (strcase (vl-list->string lista)))
	
	(setq id1 (rtos (getvar "CDATE") 2 8))
	(command "_.-Block" (strcat textoValue id1) pontoInsercao sel "")
	(command "layer" "m" "BF-05A-SA-TXT" "c" "251" "" "")
	(command "_.-insert" (strcat textoValue id1) pontoInsercao "" "" "")
	(setq objTexto (entlast))
	(command "erase" sel "")
	
	
	(CriarLink objTexto "INFO_TEXTO" textoValue)
	
	
	(princ)
)







