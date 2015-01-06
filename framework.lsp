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
;verificaRangeNumero ;range1 ---- meio ----- range2  / bolinha("A" aberto ou "F" fechado)

;cuidado ao usar a variavel contador

(setvar "cmdecho" 0)
 
;i - Configuração do serial number
(vl-load-com)
;(setq serialNumber "A472E172")
;(setq serialNumberDos (dos_serialno "c:"))
;(if (/= serialNumber serialNumberDos)
;	(progn
;		(princ "\nError: Invalid serial number")
;		(exit)
;	)
;)
;f - Configuração do serial number


;Inicia variaveis globais
(setq ultima_funcao "")
(setq STATUS_CONNECTION 0)  ;Se a conexão estiver fechada = 0, se estiver aberta = 1;
(setq DEFAULT_MODULE_VBA "Module1")
(setq DEFAULT_CLOSE_CONNECTION_FUNCTION_NAME "fechacon")
;FIM


(setq PATH_DGRID_TXT "C:\\BF_LISP_CONF\\AIR GRID\\")

(defun modulo (num)
	(if (< num 0)
		(progn
			(setq num (* num -1))
		)
	)
	num
)


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

(defun samuelBubbleSort(array1 / qtdElemArray HouveTroca contadorPos Px Px2 ) 
;(defun samuelBubbleSort(array1)
   (setq qtdElemArray (- (vl-list-length array1) 1)) 
    
   (setq HouveTroca T) 
   (while (= HouveTroca T) 
      (setq HouveTroca nil)
      (setq contadorPos 0) 
       
      (while (< contadorPos qtdElemArray)
          
         (setq Px (nth 0 (nth contadorPos array1)))
         ;(setq Px (nth 0 (nth contadorPos pontosInternos)))
         (setq Px2 (nth 0 (nth (+ contadorPos 1) array1))) 
         ;(setq Px2 (nth 0 (nth (+ contadorPos 1) pontosInternos)))
          
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


(defun c:v ()
 
 
 (if (setq ent (car (entsel)))
   ;(entget ent '("*"))
   (assoc -3 (entget ent '("*")))
 )
)


(setq sam_paraBaixo (* (/ pi 2) 3))
(setq sam_paraCima (/ pi 2))

(defun sam_metade(c1 c2)
	(polar c1 (angle c1 c2) (/ (distance c1 c2) 2))
)

(defun sam_linha(c1 c2)
	(command "line" c1 c2 "")
)



(defun insere_entrada_ar_ae_fixa1(ponto_insercao tipoEntradaAr)
	
	(setq zoomInicial (viewextents))

			(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
			;(command "insert" "C:\\bfdias\\blocos\\DESCIDA 60X100 PLANTA_bloco.dwg" ponto_insercao "" "" "" "")
			(command "insert" "C:\\bfdias\\blocos\\aefixo_entar1.dwg" ponto_insercao "" "" "" "")
			
			(command "layer" "m" "linha_aux" "c" "cyan" "" "")
			(command "circle" ponto_insercao 60)
			(setq objCircle1 (entlast))
			(setq pontoTrim (polar ponto_insercao 1.814160039890400 56.66970959877810))
			(command "zoom" "c" pontoTrim 10)
			(command "trim" objCircle1 "" pontoTrim "")
			(command "zoom" "c" (polar ponto_insercao (+ 1.814160039890400 pi) 56.66970959877810) 10)
			(command "trim" objCircle1 "" (polar ponto_insercao (+ 1.814160039890400 pi) 56.66970959877810) "")
			(command "erase"  objCircle1 "")
			
			(sam_linha (polar ponto_insercao 0.5743048301754257 101.2422836566303) (polar ponto_insercao 5.708880477005608 101.2422836565355) )
			(setq linha1 (entlast))
			(sam_linha (polar ponto_insercao 0.4048917862856102 152.3154621173125) (polar ponto_insercao 5.878293520892219 152.3154621174272))
			(setq linha2 (entlast))
			(setq linhasAdd nil)
			(setq linhasAdd (ssadd))
			(setq linhasAdd (ssadd linha1 linhasAdd))
			(setq linhasAdd (ssadd linha2 linhasAdd))
			
			(setq pontoTrim (polar ponto_insercao 0.4497884829061774 126.5017170823583))
			(command "zoom" "c" pontoTrim 10)
			(command "trim" linhasAdd "" pontoTrim "")
			(setq pontoTrim (polar ponto_insercao 5.835879638814807 127.1564843953733))
			(command "zoom" "c" pontoTrim 10)
			(command "trim" linhasAdd "" pontoTrim "")
			(command "erase"  linhasAdd "")
			;----------
			
			(sam_linha (polar ponto_insercao (+ 0.5743048301754257 pi) 101.2422836566303) (polar ponto_insercao (+ 5.708880477005608 pi) 101.2422836565355) )
			(setq linha1 (entlast))
			(sam_linha (polar ponto_insercao (+ 0.4048917862856102 pi) 152.3154621173125) (polar ponto_insercao (+ 5.878293520892219 pi) 152.3154621174272))
			(setq linha2 (entlast))
			(setq linhasAdd nil)
			(setq linhasAdd (ssadd))
			(setq linhasAdd (ssadd linha1 linhasAdd))
			(setq linhasAdd (ssadd linha2 linhasAdd))
			
			(setq pontoTrim (polar ponto_insercao (+ 0.4497884829061774 pi) 126.5017170823583))
			(command "zoom" "c" pontoTrim 10)
			(command "trim" linhasAdd "" pontoTrim "")
			(setq pontoTrim (polar ponto_insercao (+ 5.835879638814807 pi) 127.1564843953733))
			(command "zoom" "c" pontoTrim 10)
			(command "trim" linhasAdd "" pontoTrim "")
			
			(command "erase" linhasAdd "")
			
	;(if (= tipoEntradaAr "2")
	;	(progn
	;	
	;		(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
	;		(command "insert" "C:\\bfdias\\blocos\\DESCIDA 100 PLANTA_bloco.dwg" ponto_insercao "" "" "" "")
	;		
	;		(setq linha1 (polar ponto_insercao 0.404892 152.315))
	;		(setq linha2 (polar ponto_insercao 5.87829 152.315))
	;		(setq linha3 (polar ponto_insercao 2.7367 152.315))
	;		(setq linha4 (polar ponto_insercao 3.54648 152.315))
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
	;		(command "zoom" "c" (polar (polar ponto_insercao (/ pi 2) 54.9997) 0 12) 10)
	;		(command "trim" ttt2 "" (polar (polar ponto_insercao (/ pi 2) 54.9997) 0 12) "")
	;		
	;		(command "zoom" "c"   (polar (polar ponto_insercao (* (/ pi 2) 3) 55.0001) 0 12)   10)
	;		(command "trim" ttt2 "" (polar (polar ponto_insercao (* (/ pi 2) 3) 55.0001) 0 12) "")
	;		
	;		(command "erase" ttt2 "")
	;	)
	;)
	;(if (= tipoEntradaAr "3")
	;	(progn
	;		
	;		(command "layer" "m" "BF-02-SA-DET" "c" "30" "" "")
	;		(command "insert" "C:\\bfdias\\blocos\\DESCIDA 150X100 PLANTA_bloco.dwg" ponto_insercao "" "" "" "")
	;		
	;		(setq linha1 (polar (polar ponto_insercao 0.184324 300.083) (/ pi 2) 5))
	;		(setq linha2 (polar (polar ponto_insercao 6.09886 300.083)  (* (/ pi 2) 3) 5))
	;		(setq linha3 (polar (polar ponto_insercao 2.95727 300.083) (/ pi 2) 5 ))
	;		(setq linha4 (polar (polar ponto_insercao 3.32592 300.083) (* (/ pi 2) 3) 5  ))
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
	;		(command "zoom" "c" (polar (polar ponto_insercao (/ pi 2) 54.9997) 0 12) 10)
	;		(command "trim" ttt2 "" (polar (polar ponto_insercao (/ pi 2) 54.9997) 0 12) "")
	;		
	;		
	;		(command "zoom" "c"   (polar (polar ponto_insercao (* (/ pi 2) 3) 55.0001) 0 12)   10)
	;		(command "trim" ttt2 "" (polar (polar ponto_insercao (* (/ pi 2) 3) 55.0001) 0 12) "")
	;		
	;		(command "erase" ttt2 "")
	;	)
	;)
	
	(command "zoom" "w" (nth 0 zoomInicial) (nth 1 zoomInicial))
)




(defun GetKey ( / result )
    (vl-catch-all-apply
       '(lambda ( )
            (while 
                (null 
                    (eq 2 
                        (car 
                            (setq result (grread nil 8))
                        )
                    )
                )
            )
        )
    )
    (if result (chr (cadr result)) "\e")
)

(defun remove_espaco_string (string)
	(setq string (vl-string-left-trim " " string))
	(setq string (vl-string-right-trim " " string))
	string
)

(defun remove_letras_string(string1 / lista string1)
 (setq ultima_funcao "remove_letras_string")
 (setq lista (vl-string->list string1))
 (setq lista (vl-remove (ascii "-") lista))
 (setq lista (vl-remove (ascii "a") lista))
 (setq lista (vl-remove (ascii "b") lista))
 (setq lista (vl-remove (ascii "c") lista))
 (setq lista (vl-remove (ascii "d") lista))
 (setq lista (vl-remove (ascii "e") lista))
 (setq lista (vl-remove (ascii "f") lista))
 (setq lista (vl-remove (ascii "g") lista))
 (setq lista (vl-remove (ascii "h") lista))
 (setq lista (vl-remove (ascii "i") lista))
 (setq lista (vl-remove (ascii "j") lista))
 (setq lista (vl-remove (ascii "k") lista))
 (setq lista (vl-remove (ascii "l") lista))
 (setq lista (vl-remove (ascii "m") lista))
 (setq lista (vl-remove (ascii "n") lista))
 (setq lista (vl-remove (ascii "o") lista))
 (setq lista (vl-remove (ascii "p") lista))
 (setq lista (vl-remove (ascii "q") lista))
 (setq lista (vl-remove (ascii "r") lista))
 (setq lista (vl-remove (ascii "s") lista))
 (setq lista (vl-remove (ascii "t") lista))
 (setq lista (vl-remove (ascii "u") lista))
 (setq lista (vl-remove (ascii "v") lista))
 (setq lista (vl-remove (ascii "x") lista))
 (setq lista (vl-remove (ascii "y") lista))
 (setq lista (vl-remove (ascii "w") lista))
 (setq lista (vl-remove (ascii "z") lista))
 (setq lista (vl-remove (ascii "A") lista))
 (setq lista (vl-remove (ascii "B") lista))
 (setq lista (vl-remove (ascii "C") lista))
 (setq lista (vl-remove (ascii "D") lista))
 (setq lista (vl-remove (ascii "E") lista))
 (setq lista (vl-remove (ascii "F") lista))
 (setq lista (vl-remove (ascii "G") lista))
 (setq lista (vl-remove (ascii "H") lista))
 (setq lista (vl-remove (ascii "I") lista))
 (setq lista (vl-remove (ascii "J") lista))
 (setq lista (vl-remove (ascii "K") lista))
 (setq lista (vl-remove (ascii "L") lista))
 (setq lista (vl-remove (ascii "M") lista))
 (setq lista (vl-remove (ascii "N") lista))
 (setq lista (vl-remove (ascii "O") lista))
 (setq lista (vl-remove (ascii "P") lista))
 (setq lista (vl-remove (ascii "Q") lista))
 (setq lista (vl-remove (ascii "R") lista))
 (setq lista (vl-remove (ascii "S") lista))
 (setq lista (vl-remove (ascii "T") lista))
 (setq lista (vl-remove (ascii "U") lista))
 (setq lista (vl-remove (ascii "V") lista))
 (setq lista (vl-remove (ascii "X") lista))
 (setq lista (vl-remove (ascii "Y") lista))
 (setq lista (vl-remove (ascii "W") lista))
 (setq lista (vl-remove (ascii "Z") lista))
 
 (setq string1 (vl-list->string lista))
 string1
)


(defun distancia_ponto_reta(ponto1 ponto2 ponto_insercao / result a b c)
 
 (setq a (- (nth 1 ponto1)(nth 1 ponto2)))
 (setq b (- (nth 0 ponto2)(nth 0 ponto1)))
 (setq c (- (* (nth 0 ponto1) (nth 1 ponto2)) (* (nth 0 ponto2) (nth 1 ponto1)) ))
 
 (setq result (/ (+ (* a (nth 0 ponto_insercao)) (* b (nth 1 ponto_insercao)) c)   (sqrt (+ (* a a) (* b b))) ))
 
 (if (< result 0)
  (progn
   (setq result (* result -1))
  )
 )
 result
)

(defun tdir(obj opcao)
	;(setq j (getstring "\nLeft/Center/Middle/Right  "))
	;(setq j (strcase j))
	(setq j opcao)
	(setq e (entget obj))
	(setq p11 (assoc 11 e))
	(setq p10 (assoc 10 e))
	(setq f (assoc 72 e))
	(setq en e)
	
	(if (= (cdr f) 0) (setq plin (cons 11 (cdr p10))))
	(if (= (cdr f) 0) (setq en (subst plin p11 en)))
	
	(if (and (= j "L") (/= (cdr f) 0))
		(progn
			(setq p10 (cons 10 (cdr p11)))
			(setq en (subst p1on p10 en))
		)
	)
	
	(IF (= J "L") (setq fn 0))
	(IF (= J "C") (setq fn 1))
	(IF (= J "M") (setq fn 4))
	(IF (= J "R") (setq fn 2))
	
	(setq fn (cons 72 fn))
	(setq en (subst fn f en))
	(entmod en)
	
    obj
)


(defun remove_numeros_string (string1 / lista string1)
 (setq ultima_funcao "remove_numeros_string")
 (setq lista (vl-string->list string1))
 (setq lista (vl-remove (ascii "0") lista))
 (setq lista (vl-remove (ascii "1") lista))
 (setq lista (vl-remove (ascii "2") lista))
 (setq lista (vl-remove (ascii "3") lista))
 (setq lista (vl-remove (ascii "4") lista))
 (setq lista (vl-remove (ascii "5") lista))
 (setq lista (vl-remove (ascii "6") lista))
 (setq lista (vl-remove (ascii "7") lista))
 (setq lista (vl-remove (ascii "8") lista))
 (setq lista (vl-remove (ascii "9") lista))
 (setq string1 (vl-list->string lista))
 string1
)

(defun CriarLink (ent link codigo / entl xd new_entl)
 (setq ultima_funcao "CriarLink")
 (if (not (tblsearch "APPID" link)) (regapp link))
 (if (= (type codigo) 'STR)
  (setq tipo_dado 1000)
  (setq tipo_dado 1071)
 )
 (setq entl (entget ent))
 (setq xd (list (list -3 (list link (cons tipo_dado codigo)))))
 (setq new_entl (append entl xd))
 (entmod new_entl)
 (entupd (cdr (assoc -1 new_entl)))
)

(defun GetId (entd nome_link / codigo nome links retorno codlog)
 (setq ultima_funcao "GetId")
 (setq retorno nil)
 (if entd
  (progn   
   (if (setq Links  (assoc -3 (entget entd '("*"))))
	(progn
	 (assoc codlog (cdr links))   
	 (if (assoc nome_link (cdr links))
	 ;(assoc "Samuel" (cdr links))
	 
	  (setq retorno (cdr (car (cdr (assoc nome_link (cdr links))))))
	  ;(setq retorno (cdr (car (cdr (assoc "Samuel" (cdr links))))))
	 )
	)
   )
  )
 )
 retorno
)

(defun faz_janela(COORD TIPO PROPRIEDADE TAMANHO / PONTO1 PONTO2 JANELA)
     (setq ultima_funcao "faz_janela")
	 (command "zoom" "c" COORD 30)
	 (setq PONTO1 (polar COORD (/ pi 4) TAMANHO))
	 (setq PONTO2 (polar COORD (* (/ pi 4) 5) TAMANHO))

	 (command "zoom" "w" PONTO1 PONTO2)
	 (setq JANELA (ssget "C" PONTO1 PONTO2 (list (cons TIPO PROPRIEDADE))))
	 JANELA
)

(defun desenha_linha(p1 p2 layer cor)
 (setq ultima_funcao "desenha_linha")
 (command "layer" "m" layer "c" cor "" "")
 (command "line" p1 p2 "")
 
)

;ponto - array
;raio - double
;layer - string
;cor - string
(defun desenha_circulo(ponto raio layer cor)
 (setq ultima_funcao "desenha_circulo")
 (command "layer" "m" layer "c" cor "" "")
 (command "circle" ponto raio)
)


(defun quantidade_elementos_array(array1 / cont pass valor)
 (setq ultima_funcao "quantidade_elementos_array")
 (setq cont 0)
 (setq pass 0)
 (setq valor (nth pass array1))
 
 (while (/= valor nil)
  (setq cont (+ cont 1))
  
  (setq pass (+ pass 1))
  (setq valor (nth pass array1))
 )
 cont
)

(defun pega_propriedade_objeto(obj prop / ppp)
  (setq ultima_funcao "pega_propriedade_objeto")
  (setq ppp (cdr (assoc prop (entget obj))))
  ppp
)

(defun gerar_lista_elementos(layer tipo elementos / lista contador casa_decimais coordf qual_coord selecao cont_aux qtd oque_pegar lista_temp obj valor_prop) 
 (setq ultima_funcao "gerar_lista_elementos")
 (setq lista nil)
 
 (setq elementos (reverse elementos))
 ;(setq contador (quantidade_elementos_array elementos))
 (setq contador (length elementos))
 
 (setq selecao (ssget "x" (list (cons 8 layer)(cons 0 tipo))))
 (if (/= selecao nil)
  (progn
  
   (setq qtd (- (sslength selecao) 1))
   
   (while (>= qtd 0)
    (setq obj (ssname selecao qtd))
	(setq cont_aux contador)
	
	(setq lista_temp nil)
	(while (> cont_aux 0)
	 (setq oque_pegar (nth 0 (nth (- cont_aux 1) elementos)))
	 (cond
	      ( 
			(= oque_pegar "objeto")
			(setq lista_temp (cons obj lista_temp))
		  )
          ( 
			(= oque_pegar "coordenada")
			(setq valor_prop (pega_propriedade_objeto obj 10))
			(setq lista_temp (cons valor_prop lista_temp))
		  )
		  ( 
			(= oque_pegar "blockname")
			(setq valor_prop (pega_propriedade_objeto obj 2))
			(setq lista_temp (cons valor_prop lista_temp))
		  )
		  ( 
			(= oque_pegar "layername")
			(setq valor_prop (pega_propriedade_objeto obj 8))
			(setq lista_temp (cons valor_prop lista_temp))
		  )
		  ( 
			(= oque_pegar "angulo")
			(setq valor_prop (pega_propriedade_objeto obj 50))
			(setq lista_temp (cons valor_prop lista_temp))
		  )
		  ( 
			(= oque_pegar "distancia")
			(setq valor_prop (pega_propriedade_objeto obj 42))
			(setq lista_temp (cons valor_prop lista_temp))
		  )
		  ( 
			(= oque_pegar "texto")
			(setq valor_prop (pega_propriedade_objeto obj 1))
			(setq lista_temp (cons valor_prop lista_temp))
		  )
		  
		  ( 
			(= oque_pegar "coordformat")
			(setq casa_decimais (nth 1 (nth (- cont_aux 1) elementos)))
			(setq qual_coord (nth 2 (nth (- cont_aux 1) elementos)))
			(setq valor_prop (pega_propriedade_objeto obj qual_coord))
			(setq coordf (arredonda_coordenada valor_prop casa_decimais))
			(setq lista_temp (cons coordf lista_temp))
		  )
		  ( 
			(= oque_pegar "atributo")
			(setq posica_atributo (nth 1 (nth (- cont_aux 1) elementos)))
			(setq valor_atributo (retorna_attrib obj posica_atributo))
			(setq lista_temp (cons valor_atributo lista_temp))
		  )
		  (
			(= oque_pegar "xdata")
			
			(setq xdata_value (nth 1 (nth (- cont_aux 1) elementos)))
			(setq xdata_value (GetId obj xdata_value))
			(setq lista_temp (cons xdata_value lista_temp))
		  )
	 )
	 (setq cont_aux (- cont_aux 1))
	)
	
	(setq lista (cons lista_temp lista))
	
	(setq qtd (- qtd 1))
   )
   
   
  )
  (progn
   (setq lista nil)
  )
 )
 
 
 
 lista
)

(defun retorna_attrib (BLOCO_AUX NUM_ATTRIB)
  (setq ultima_funcao "retorna_attrib")
  (setq VALOR_ATTRIB nil)
  (setq EL BLOCO_AUX)
  (setq CONTADOR 1)
  (while (<= CONTADOR NUM_ATTRIB)
    (setq EL (entnext EL))
    (setq CONTADOR (1+ CONTADOR))
  )
  (setq VALOR_ATTRIB (cdr (assoc 1 (entget EL))))
  VALOR_ATTRIB
)





(defun gera_layer (nome_layer cor)
 (setq ultima_funcao "gera_layer")
 (command "layer" "m" nome_layer "c" cor "" "")
)

(defun arredonda_coordenada(coord casas / x y)
 (setq ultima_funcao "arredonda_coordenada")
 (setq x (rtos (car coord) 2 casas))
 (setq y (rtos (cadr coord) 2 casas))
 (setq coord (strcat x y))
 
 coord
)

(defun exibir_porcentagem (qtd)
	(setq ultima_funcao "exibir_porcentagem")
	(prompt (strcat "\rQuantidade de elementos faltando: *********" (itoa qtd) "*********"))
)

(vl-load-com)

(defun verxdata()
 (setq ultima_funcao "verxdata")
 (if (setq ent (car (entsel)))
   ;(entget ent '("*"))
   (assoc -3 (entget ent '("*")))
 )
)




(defun *ERROR* (MSG / cont)
 ;(SETVAR "OSMODE" R-OSMODE)
 
 (setq cont 10)
 (while (> cont 1)
  (prompt "\n===============================")
  (setq cont (- cont 1))
 )
 (prompt "\n===========   ERRO   ==========")
 (prompt "\n===============================")
 
 (if (and (/= ultima_funcao nil)(/= ultima_funcao ""))
  (progn
   (prompt "\n") (princ (strcat " Última função executada: '" ultima_funcao "'"))
  )
 )
 
 (prompt "\n") (princ (strcat " " (strcase MSG) ""))
 (prompt "\n===============================")
 
 (if (= STATUS_CONNECTION 1)
	(progn
		(prompt "\nFechando a conexão...")
		(command "_-vbarun" (strcat DEFAULT_MODULE_VBA "." DEFAULT_CLOSE_CONNECTION_FUNCTION_NAME))
		(prompt "\nOK!")
	)
 )
 
 (princ)
)

(defun ViewExtents (/ A B C D X)
  (setq ultima_funcao "ViewExtents")
  (setq B (getvar "VIEWSIZE")
        A (* B (/ (car (getvar "SCREENSIZE")) (cadr (getvar "SCREENSIZE"))))
        X (trans (getvar "VIEWCTR") 1 2)
        C (trans (list (- (car X) (/ A 2.0)) (+ (cadr X) (/ B 2.0))) 2 1)
        D (trans (list (+ (car X) (/ A 2.0)) (- (cadr X) (/ B 2.0))) 2 1)
  );setq
  (list C D)
)


;Essa função pega os atributos do bloco e coloca dentro do xdata do mesmo
;Modo de usar: (funcitonCarimbaXdata "Postes" "INSERT") ou (funcitonCarimbaXdata "Primaria" "INSERT")
(defun funcitonCarimbaXdata(layerName ObjectType / qtd el objAbstract tipoObjeto tipoObjeto VALOR_ATTRIB
 CONTADOR sair labelName listaInfoPoste proc1 all)
	
	(setq all (ssget "x" (List (cons 8 layerName))))
	;(setq all (ssget "x" '((-4 . "<AND") (List (cons 8 layerName)) (List (cons 0 ObjectType))(-4 . "AND>"))))
	
	(if (/= all nil)
		(progn
			(setq qtd (- (sslength all) 1))
			(princ "\nPercorrendo objetos")
			(princ (strcat "\nQuantidade entrados: " (itoa qtd)))
			(princ "\n--------------------------")
			(princ "\n--------------------------")
			(while (>= qtd 0)
				(setq el (ssname all qtd))
				(setq objAbstract (ssname all qtd))
				
				
				(setq tipoObjeto (cdr (assoc 0 (entget el))))
				
				(if (= tipoObjeto ObjectType)
					(progn
						
						(setq VALOR_ATTRIB nil)
						(setq CONTADOR 1)
						(setq sair 0)
						
						(setq listaInfoPoste nil)
						(setq EL (entnext EL))
						(while (/= EL nil)
							(setq CONTADOR (1+ CONTADOR))
							
							(setq VALOR_ATTRIB (cdr (assoc 1 (entget EL))))
							(setq labelName (cdr (assoc 2 (entget EL))))
							
							(setq proc1 (assoc labelName listaInfoPoste))
							(if (= proc1 nil)
								(progn
									(setq listaInfoPoste (cons (list labelName VALOR_ATTRIB) listaInfoPoste))
									
									(if (and (/= labelName nil)(/= VALOR_ATTRIB nil))
										(progn
											(CriarLink objAbstract labelName VALOR_ATTRIB)
										)
									)
								)
							)
							(setq EL (entnext EL))
						)
					)
				)
				(princ (strcat "\rQuantidade restante: " (itoa qtd)))
				(setq qtd (- qtd 1))
			)
		)
	)
)

;Return SIM/NAO
;range1 ---- meio ----- range2  / bolinha("A" aberto ou "F" fechado)
(defun verificaRangeNumero(r1 r2 meio bolinha / resp)
	(if (= bolinha "F")
		(progn
			(if (and  (>= meio r1) (<= meio r2) )
				(progn
					(setq resp "SIM") ;Sim, está entre os números enviados nos parâmetros
				)
				(progn
					(setq resp "NAO") ;Não está dentro do range especificado nos parâmetros
				)
			)
		)
	)
	
	(if (= bolinha "A")
		(progn
			(if (and  (> meio r1)(< meio r2) )
				(progn
					(setq resp "SIM") ;Sim, está entre os números enviados nos parâmetros
				)
				(progn
					(setq resp "NAO") ;Não está dentro do range especificado nos parâmetros
				)
			)
		)
	)
	
	resp
)


(defun samuel_split(word split1)
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


(defun c:framework()

 (princ)
)

(princ "\n=============================")