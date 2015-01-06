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


(load "C:\\bfdias\\desenha_aereo_fixa.fas")
(load "C:\\bfdias\\insere_entradaAR_AE_FIXA.fas")
(load "C:\\bfdias\\insere_sa.fas")
(load "C:\\bfdias\\insere_bolsa_ramal.fas")
(load "C:\\bfdias\\insere_sg.fas")
(load "C:\\bfdias\\insere_sg_trim.fas")
(load "C:\\bfdias\\desenha_aereo_fixa2.fas")
(load "C:\\bfdias\\insere_entradaAR_AEFIXA2.fas")
(load "C:\\bfdias\\insere_sa2.fas")
(load "C:\\bfdias\\insere_sg2.fas")
(load "C:\\bfdias\\desenha_aereo_fixa3.fas")
(load "C:\\bfdias\\df3_estrutura.fas")
(load "C:\\bfdias\\insere_sa3.fas")
(load "C:\\bfdias\\insere_sg3.fas")
(load "C:\\bfdias\\desenha_tanque_ou_lagoa.fas")
(load "C:\\bfdias\\desenha_grid.fas")
(load "C:\\bfdias\\air_grid_contagem.fas")


(defun c:carregar_lisps()
	(vl-load-com)
	(load "C:\\bfdias\\framework.fas")
	
	
	(load "C:\\bfdias\\desenha_aereo_fixa.fas")
	(load "C:\\bfdias\\insere_entradaAR_AE_FIXA.fas")
	(load "C:\\bfdias\\insere_sa.fas")
	(load "C:\\bfdias\\insere_bolsa_ramal.fas")
	(load "C:\\bfdias\\insere_sg.fas")
	(load "C:\\bfdias\\insere_sg_trim.fas")
	(load "C:\\bfdias\\desenha_aereo_fixa2.fas")
	(load "C:\\bfdias\\insere_entradaAR_AEFIXA2.fas")
	(load "C:\\bfdias\\insere_sa2.fas")
	(load "C:\\bfdias\\insere_sg2.fas")
	(load "C:\\bfdias\\desenha_aereo_fixa3.fas")
	(load "C:\\bfdias\\df3_estrutura.fas")
	(load "C:\\bfdias\\insere_sa3.fas")
	(load "C:\\bfdias\\insere_sg3.fas")
	(load "C:\\bfdias\\desenha_tanque_ou_lagoa.fas")
	(load "C:\\bfdias\\desenha_grid.fas")
	(load "C:\\bfdias\\air_grid_contagem.fas")
	
	
	(princ)
)







