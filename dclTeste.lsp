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


;;;--- First we need a slide name 
(setq mySlideName "C:\\bfdias\\sld\\aef3.sld")
;;;--- Second we need the key to the image control 
(setq myKey "sld")
;;;--- Next we send the slide name and the key to the update function 
;


(setq mySlideName2 "C:\\bfdias\\sld\\aef4.sld")
(setq myKey2 "sld2")


(setq mySlideName3 "C:\\bfdias\\sld\\estf3.sld")
(setq myKey3 "sld3")


(setq mySlideName4 "C:\\bfdias\\sld\\estf31.sld")
(setq myKey4 "sld4")

(setq mySlideName5 "C:\\bfdias\\sld\\agrid1.sld")
(setq myKey5 "sld5")



; NOTE: Notice mySlideName becomes sldName and myKey becomes key when passed 
;       as parameters to the upDateImage function.
;;;--- Function to update the slide
(defun upDateImage(sldName key)
  ;;;--- Get the width of the slide
  ;;;--- Get the width of the slide
  (setq width (dimx_tile key))

  ;;;--- Get the height of the slide
  (setq height (dimy_tile key))

  ;;;--- Start the slide definition
  (start_image key)

  ;;;--- Wipe out the background
  (fill_image 0 0 width height 0)

  ;;;--- Put the slide in the image area
  (slide_image 0 0 width height sldName)

  ;;;--- Finish the slide definition
  (end_image)

)



(defun c:dd()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	(if(not(setq dcl_id (load_dialog "C:\\bfdias\\DCL_TEST.dcl")))
    (progn
      (alert "The DCL file could not be loaded.")
      (exit)
    )
    (progn
      (if (not (new_dialog "DCL_TEST" dcl_id))
        (progn
          (alert "The definition could not be found inside the DCL file")
          (exit)
        )
        (progn
		  (upDateImage mySlideName myKey)
		  (upDateImage mySlideName2 myKey2)
		  (upDateImage mySlideName3 myKey3)
		  (upDateImage mySlideName4 myKey4)
		  (upDateImage mySlideName5 myKey5)
		  
          (action_tile "cancel" "(done_dialog 1)")
          (action_tile "accept" "(done_dialog 2)")
          (setq ddiag(start_dialog))
          (unload_dialog dcl_id)
		  
          (if (= ddiag 1)
            (princ "\n \n ...DCL_TEST Cancelled. \n ")
          )
          (if (= ddiag 2)
            (alert "You pressed the OKAY button!")
          )
        )
      )
    )
  )

	
  (princ)
)







