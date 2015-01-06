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


(defun c:entar2()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	;(setq tipoEntradaAr (getstring "\n [1] - 60x100, [2] - 100, [3] - 150x100 : "))
	;(if (and (/= tipoEntradaAr "1")(/= tipoEntradaAr "2")(/= tipoEntradaAr "3") )
	;	(progn
	;		(alert "Você escolheu uma opção inválida!")
	;		(exit)
	;	)
	;)
	
	(setq ponto_insercao (getpoint "\nDefina um ponto de inserção: "))
	
	(setq zoomInicial (viewextents))
	
	
	(setq ponto_insercao (polar ponto_insercao (/ pi 2) 23.6001))
	(command "layer" "m" "100 corte1" "c" "30" "" "")
	;(command "insert" "C:\\bfdias\\blocos\\DESCIDA 150X100 CORTE 1_bloco.dwg" ponto_insercao "" "" "" "")
	(command "insert" "C:\\bfdias\\blocos\\TE LAMINADO CORTE 1.dwg" (polar ponto_insercao sam_parabaixo 23.69392525780131) "" "" "" "")
	
	(setq ponto_insercao (polar ponto_insercao sam_parabaixo 23.69392525780131))
	
	(command "layer" "m" "linha_aux" "c" "cyan" "" "")
	(sam_linha (polar ponto_insercao 0.4324077755712193 143.1782106317187) (polar ponto_insercao 5.850459632049786 143.1992284694708))
	(setq linha1T (entlast))
	(sam_linha (polar ponto_insercao 2.709184878024366 143.1782106335156) (polar ponto_insercao 3.574318328713797 143.1992284712675))
	(setq linha2T (entlast))
	
	(setq linhasAdd nil)
	(setq linhasAdd (ssadd))
	(setq linhasAdd (ssadd linha1T linhasAdd))
	(setq linhasAdd (ssadd linha2T linhasAdd))
	
	(setq pontoTrim (polar ponto_insercao 1.546862960803718 56.15489388307977))
	
	(command "zoom" "c" pontoTrim 10)
	(command "trim" linhasAdd "" pontoTrim "")
	
	(setq pontoTrim (polar ponto_insercao sam_parabaixo 53.86118828027974))
	(command "zoom" "c" pontoTrim 10)
	(command "trim" linhasAdd "" pontoTrim "")
	
	(command "erase" linhasAdd "")

	
	
	;(if (= tipoEntradaAr "2")
	;	(progn
	;		(command "layer" "m" "100 corte1" "c" "30" "" "")
	;		(command "insert" "C:\\bfdias\\blocos\\DESCIDA 100 CORTE 1_bloco.dwg" ponto_insercao "" "" "" "")
	;		
	;		(command "layer" "m" "linha_aux" "c" "cyan" "" "")
	;		(setq linha1 (polar ponto_insercao 0.404892 152.315))
	;		(setq linha2 (polar ponto_insercao 5.87829 152.315))
	;		(setq linha3 (polar ponto_insercao 2.7367 152.315))
	;		(setq linha4 (polar ponto_insercao 3.54648 152.315))
	;		
	;		
	;		(command "layer" "m" "linha_aux" "c" "cyan" "" "")
	;		(command "line" linha1 linha2 "")
	;		(setq li1 (entlast))
	;		(command "line" linha3 linha4 "")
	;		(setq li2 (entlast))
	;		
	;		(setq ttt2 nil)
	;		(setq ttt2 (ssadd))
	;		(setq ttt2 (ssadd li1 ttt2))
	;		(setq ttt2 (ssadd li2 ttt2))
	;		
	;		(command "zoom" "c" (polar linha3 5.99332 22.3904) 10)
	;		(command "trim" ttt2 "" (polar linha3 5.99332 22.3904) "")
	;		
	;		(command "zoom" "c" (polar linha4 0.193543 18.7178) 10)
	;		(command "trim" ttt2 "" (polar linha4 0.193543 18.7178) "")
	;		
	;		
	;		
	;		(command "erase" ttt2 "")
	;		
	;	)
	;)
	
	;(if (= tipoEntradaAr "1")
	;	(progn
	;		
	;		(command "layer" "m" "60x100 corte1" "c" "30" "" "")
	;		(command "insert" "C:\\bfdias\\blocos\\DESCIDA 60X100 CORTE 1_bloco.dwg" ponto_insercao "" "" "" "")
	;		
	;		(command "layer" "m" "linha_aux" "c" "cyan" "" "")
	;		
	;		(setq linha1 (polar ponto_insercao 0.432408 143.178))
	;		(setq linha2 (polar ponto_insercao 5.85078 143.178))
	;		(setq linha3 (polar ponto_insercao 2.70918 143.178))
	;		(setq linha4 (polar ponto_insercao 3.574 143.178))
	;		
	;		(command "layer" "m" "linha_aux" "c" "cyan" "" "")
	;		(command "line" linha1 linha2 "")
	;		(setq li1 (entlast))
	;		(command "line" linha3 linha4 "")
	;		(setq li2 (entlast))
	;		
	;		(setq ttt2 nil)
	;		(setq ttt2 (ssadd))
	;		(setq ttt2 (ssadd li1 ttt2))
	;		(setq ttt2 (ssadd li2 ttt2))
	;		
	;		(command "zoom" "c" (polar linha3 5.99332 22.3904) 10)
	;		(command "trim" ttt2 "" (polar linha3 5.99332 22.3904) "")
	;		
	;		(command "zoom" "c" (polar linha4 0.193543 18.7178) 10)
	;		(command "trim" ttt2 "" (polar linha4 0.193543 18.7178) "")
	;		
	;		
	;		
	;		(command "erase" ttt2 "")
	;	)
	;)
	
	
	
	
	(command "zoom" "w" (nth 0 zoomInicial) (nth 1 zoomInicial))
	(princ)
)







